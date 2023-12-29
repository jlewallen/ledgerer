use std::{
    collections::HashMap,
    ops::Mul,
    str::FromStr,
    sync::{Arc, Mutex},
};

use anyhow::Result;
use bigdecimal::BigDecimal;
use chrono::NaiveDate;
use itertools::Itertools;
use regex::Regex;
use serde_json::Value;
use tera::{Context, Function, Tera};
use thiserror::Error;

use crate::model::{AccountPath, Expression, LedgerFile, Numeric, Origin, Posting, Transaction};

use super::{TaxRule, Taxes};

#[derive(Debug)]
struct Allocated {
    account: AccountPath,
    value: Periodic,
    note: Option<String>,
}

impl Allocated {
    fn posting(&self, cycle: Cycle) -> Posting {
        Posting {
            account: self.account.clone(),
            expression: Some(Expression::Literal(Numeric::Decimal(
                self.value.amount(cycle).round(2),
            ))),
            note: self.note.clone(),
        }
    }

    fn amount(&self, cycle: Cycle) -> BigDecimal {
        self.value.amount(cycle)
    }
}

#[derive(Debug)]
enum Periodic {
    Monthly(BigDecimal),
    Yearly(BigDecimal),
}

impl Periodic {
    fn yearly_total(&self) -> BigDecimal {
        match self {
            Periodic::Monthly(value) => value.mul(BigDecimal::from(12)),
            Periodic::Yearly(value) => value.clone(),
        }
    }

    fn amount(&self, cycle: Cycle) -> BigDecimal {
        match cycle {
            Cycle::Bimonthly => self.yearly_total() / 12 / 2,
            Cycle::Monthly => self.yearly_total() / 12,
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum Cycle {
    Bimonthly,
    Monthly,
}

#[derive(Debug)]
struct AllocatedPay {
    date: NaiveDate,
    allocated: Vec<Allocated>,
    taxes: Option<Vec<TaxRule>>,
}

impl AllocatedPay {
    fn new(date: NaiveDate) -> Self {
        Self {
            date,
            allocated: Vec::default(),
            taxes: None,
        }
    }

    fn monthly(&mut self, account: impl Into<AccountPath>, value: BigDecimal, note: Option<&str>) {
        let periodic = Periodic::Monthly(value);
        self.allocated(account, periodic, note);
    }

    fn yearly(&mut self, account: impl Into<AccountPath>, value: BigDecimal, note: Option<&str>) {
        let periodic = Periodic::Yearly(value);
        self.allocated(account, periodic, note);
    }

    fn allocated(
        &mut self,
        account: impl Into<AccountPath>,
        periodic: Periodic,
        note: Option<&str>,
    ) {
        let allocated = Allocated {
            account: account.into(),
            value: periodic,
            note: note.map(|n| n.to_owned()),
        };
        self.allocated.push(allocated);
    }

    fn total(&self, cycle: Cycle) -> BigDecimal {
        self.allocated.iter().map(|alloc| alloc.amount(cycle)).sum()
    }

    fn tax(&mut self, rate: BigDecimal, account: String) -> Result<(), Error> {
        self.taxes
            .get_or_insert(Vec::default())
            .push(TaxRule::Path(Regex::new(&account)?, rate));

        Ok(())
    }

    fn transactions(
        &self,
        cycle: Cycle,
        reserve: Posting,
        available: Posting,
    ) -> Result<Vec<Transaction>> {
        Ok(vec![Transaction {
            date: self.date.clone(),
            payee: "income allocation".to_owned(),
            cleared: true,
            notes: vec![],
            postings: self
                .allocated
                .iter()
                .map(|alloc| alloc.posting(cycle))
                .chain(vec![available, reserve])
                .collect_vec(),
            mid: None,
            order: None,
            origin: Some(Origin::Generated),
        }
        .into_balanced()?])
    }
}

#[derive(Debug)]
#[allow(dead_code)]
pub struct Paycheck {
    total: BigDecimal,
    allocated: AllocatedPay,
}

#[allow(dead_code)]
impl Paycheck {
    fn new(tx: &Transaction, allocated: AllocatedPay) -> Self {
        Self {
            total: tx.magnitude().expect("income has no magnitude"),
            allocated,
        }
    }

    pub fn remaining(&self, cycle: Cycle) -> BigDecimal {
        self.total.clone() - self.allocated.total(cycle)
    }

    pub fn transactions(&self, cycle: Cycle) -> Result<Vec<Transaction>> {
        let reserve = Posting {
            account: "assets:checking:reserved".into(), // TODO Names
            expression: Some(Expression::Literal(Numeric::Decimal(-self.total.clone()))),
            note: None,
        };
        let available = Posting {
            account: "allocations:checking:available".into(), // TODO Names
            expression: None,
            note: None,
        };
        self.allocated.transactions(cycle, reserve, available)
    }

    pub fn taxes(&self) -> Option<Taxes> {
        self.allocated.taxes.clone().map(|rules| Taxes { rules })
    }
}

#[derive(Clone)]
struct DateHelpers {
    date: NaiveDate,
}

impl DateHelpers {
    fn new(date: NaiveDate) -> Self {
        Self { date }
    }

    fn parse_date(args: &HashMap<String, Value>, key: &str) -> Result<NaiveDate, Error> {
        match args.get(key) {
            Some(Value::String(value)) => {
                Ok(NaiveDate::parse_from_str(&value.to_string(), "%m/%d/%Y")?)
            }
            _ => Err(Error::Arguments()),
        }
    }

    fn before(self) -> impl Function {
        Box::new(
            move |args: &HashMap<String, Value>| -> tera::Result<Value> {
                let before = DateHelpers::parse_date(args, "before")?;
                Ok(serde_json::Value::Bool(self.date < before))
            },
        )
    }

    fn after(self) -> impl Function {
        Box::new(
            move |args: &HashMap<String, Value>| -> tera::Result<Value> {
                let after = DateHelpers::parse_date(args, "after")?;
                Ok(serde_json::Value::Bool(self.date > after))
            },
        )
    }

    fn between(self) -> impl Function {
        Box::new(
            move |args: &HashMap<String, Value>| -> tera::Result<Value> {
                let before = DateHelpers::parse_date(args, "before")?;
                let after = DateHelpers::parse_date(args, "after")?;
                Ok(serde_json::Value::Bool(
                    self.date < before && self.date >= after,
                ))
            },
        )
    }
}

pub struct PaycheckTemplate {
    name: String,
    paycheck: Arc<Mutex<Option<AllocatedPay>>>,
}

impl PaycheckTemplate {
    pub fn new(name: &str, date: NaiveDate) -> Self {
        Self {
            name: name.to_owned(),
            paycheck: Arc::new(Mutex::new(Some(AllocatedPay::new(date)))),
        }
    }

    pub fn generate(self, tx: &Transaction) -> Result<Paycheck> {
        let mut tera = Tera::default();

        let template = format!(
            "/home/jlewallen/sync/finances/income.{}.template.ledger",
            &self.name
        );

        tera.add_template_file(template, Some(&self.name))?;

        let dates = self.date_helpers();
        tera.register_function("before", dates.clone().before());
        tera.register_function("after", dates.clone().after());
        tera.register_function("between", dates.clone().between());

        tera.register_function("monthly", {
            let paycheck = self.paycheck.clone();
            move |args: &HashMap<String, Value>| -> tera::Result<Value> {
                let account = Self::get_account(args).ok_or_else(|| Error::Arguments())?;
                let value = Self::get_decimal(args, "value").ok_or_else(|| Error::Arguments())?;
                let note = Self::get_note(args);
                let mut paycheck = paycheck.lock().unwrap();
                paycheck.as_mut().unwrap().monthly(account, value, note);
                Ok(Value::Null)
            }
        });

        tera.register_function("yearly", {
            let paycheck = self.paycheck.clone();
            move |args: &HashMap<String, Value>| -> tera::Result<Value> {
                let account = Self::get_account(args).ok_or_else(|| Error::Arguments())?;
                let value = Self::get_decimal(args, "value").ok_or_else(|| Error::Arguments())?;
                let note = Self::get_note(args);
                let mut paycheck = paycheck.lock().unwrap();
                paycheck.as_mut().unwrap().yearly(account, value, note);
                Ok(Value::Null)
            }
        });

        tera.register_function("tax", {
            let paycheck = self.paycheck.clone();
            move |args: &HashMap<String, Value>| -> tera::Result<Value> {
                let account = Self::get_account(args).ok_or_else(|| Error::Arguments())?;
                let rate = Self::get_decimal(args, "rate").ok_or_else(|| Error::Arguments())?;
                let mut paycheck = paycheck.lock().unwrap();
                paycheck.as_mut().unwrap().tax(rate, account.to_string())?;
                Ok(Value::Null)
            }
        });

        let mut context = Context::new();
        context.insert("date", &dates.date.format("%Y/%m/%d").to_string());

        let text = tera.render(&self.name, &context)?;
        let _file = LedgerFile::parse_text(text, self.name.try_into()?)?;

        let allocated = self.paycheck.lock().unwrap().take().unwrap();

        Ok(Paycheck::new(tx, allocated))
    }

    fn date_helpers(&self) -> DateHelpers {
        let paycheck = self.paycheck.lock().unwrap();
        DateHelpers::new(paycheck.as_ref().unwrap().date)
    }

    fn get_account(args: &HashMap<String, Value>) -> Option<&str> {
        args.get("account").map(|v| v.as_str()).flatten()
    }

    fn get_note(args: &HashMap<String, Value>) -> Option<&str> {
        args.get("note").map(|v| v.as_str()).flatten()
    }

    fn get_decimal(args: &HashMap<String, Value>, key: &str) -> Option<BigDecimal> {
        match args.get(key).unwrap() {
            Value::Number(number) => BigDecimal::from_str(&number.to_string()).ok(),
            Value::String(str) => BigDecimal::from_str(str).ok(),
            _ => todo!(),
        }
    }
}

#[derive(Debug, Error)]
enum Error {
    #[error("arguments error")]
    Arguments(),
    #[error("date format error")]
    Chrono(#[source] chrono::ParseError),
    #[error("regex format error")]
    Regex(#[source] regex::Error),
}

impl From<chrono::ParseError> for Error {
    fn from(value: chrono::ParseError) -> Self {
        Self::Chrono(value)
    }
}

impl From<regex::Error> for Error {
    fn from(value: regex::Error) -> Self {
        Self::Regex(value)
    }
}

impl From<Error> for tera::Error {
    fn from(value: Error) -> Self {
        tera::Error::msg(format!("{:?}", value))
    }
}
