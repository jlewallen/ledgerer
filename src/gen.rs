use std::{cell::RefCell, ops::Mul, path::PathBuf, rc::Rc};

use anyhow::Result;
use bigdecimal::{BigDecimal, Signed, ToPrimitive, Zero};
use chrono::{Months, NaiveDate};
use clap::Args;
use itertools::Itertools;
use regex::Regex;
use thiserror::Error;
use tracing::*;

use crate::{
    model::{AccountPath, Expression, LedgerFile, Node, Numeric, Origin, Posting, Transaction},
    print::Printer,
};

use self::config::{
    Configuration, EnvelopeDefinition, IncomeDefinition, Names, RefundDefinition,
    SpendingDefinition,
};

mod config;
mod pay;

#[derive(Debug)]
struct Spending {
    total: BigDecimal,
    envelope: AccountPath,
    original: Transaction,
    scheduled: Option<NaiveDate>,
}

impl std::fmt::Display for Spending {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Spending({}, {})", self.total, self.envelope)
    }
}

impl Spending {
    fn short_string(&self) -> String {
        self.original.short_string()
    }

    fn affects_emergency(&self, names: &Names) -> bool {
        self.original.has_posting_for(&names.emergency)
    }

    fn available(&self, date: NaiveDate, total: BigDecimal, names: &Names) -> Transaction {
        Transactions::new(date, names, &self.original)
            .make_cover_from_available([(self.envelope.clone(), total)].into_iter())
            .unwrap()
    }

    fn emergency(&self, date: NaiveDate, total: BigDecimal, names: &Names) -> Transaction {
        Transactions::new(date, names, &self.original)
            .make_cover_from_emergency([(self.envelope.clone(), total)].into_iter())
            .unwrap()
    }

    fn early(&self, date: NaiveDate, total: BigDecimal, names: &Names) -> Transaction {
        Transactions::new(date, names, &self.original)
            .make_cover_from_early([(self.envelope.clone(), total)].into_iter())
            .unwrap()
    }

    fn scheduled(self, maximum: Option<BigDecimal>) -> Vec<Spending> {
        Self::get_payment_amounts(self.total.clone(), maximum)
            .into_iter()
            .enumerate()
            .map(|(i, payment)| Spending {
                total: payment,
                envelope: self.envelope.clone(),
                original: self.original.clone(),
                scheduled: self
                    .original
                    .date()
                    .checked_add_months(Months::new(i as u32)),
            })
            .collect_vec()
    }

    fn get_payment_amounts(total: BigDecimal, maximum: Option<BigDecimal>) -> Vec<BigDecimal> {
        match maximum {
            Some(maximum) => {
                let full_payments = (&total / &maximum).to_usize().unwrap();
                let last = total % maximum.clone();
                (0..full_payments)
                    .map(|_| maximum.clone())
                    .chain(std::iter::once(last))
                    .collect_vec()
            }
            None => vec![total],
        }
    }
}

#[derive(Debug, Clone)]
pub enum TaxRule {
    Path(Regex, BigDecimal),
}

impl TaxRule {
    fn amount(&self, tx: &Transaction) -> Option<BigDecimal> {
        match self {
            TaxRule::Path(re, rate) => {
                let total = tx
                    .iter_postings_for_re(re)
                    .flat_map(|p| p.only_positive().map(|v| v.abs()))
                    .sum::<BigDecimal>();

                // debug!("{:?} {:?}", total, rate);

                Some(total.mul(rate).round(2))
            }
        }
    }

    fn matches(&self, tx: &Transaction) -> bool {
        match self {
            TaxRule::Path(re, _) => tx.has_posting_for_re(re),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Taxes {
    rules: Vec<TaxRule>,
}

#[derive(Debug)]
enum Operation {
    AllocatePaycheck(Transaction, Option<Taxes>),
    RefundedToAvailable(Transaction),
    EnvelopeWithdrawal(Transaction),
    CoverSpending(Spending),
    CoverEmergency(Spending),
}

impl std::fmt::Display for Operation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Operation::AllocatePaycheck(tx, _) => write!(f, "{} Paycheck({})", tx.date(), tx.payee),
            Operation::RefundedToAvailable(tx) => write!(f, "{} Refund({})", tx.date(), tx.payee),
            Operation::EnvelopeWithdrawal(tx) => {
                write!(f, "{} EnvelopeWithdrawal({})", tx.date(), tx.payee)
            }
            Operation::CoverSpending(spending) => {
                write!(f, "{} {}", spending.original.date(), &spending)
            }
            Operation::CoverEmergency(spending) => {
                write!(f, "{} Emergency({})", spending.original.date(), &spending)
            }
        }
    }
}

#[derive(Debug, Clone)]
struct Covered {
    available: Option<Transaction>,
    emergency: Option<Transaction>,
    early: Option<Transaction>,
}

impl Covered {
    fn transactions(&self) -> impl Iterator<Item = &Transaction> {
        vec![
            self.early.as_ref(),
            self.available.as_ref(),
            self.emergency.as_ref(),
        ]
        .into_iter()
        .flatten()
    }

    fn to_corrective_operations(self) -> impl Iterator<Item = Operation> {
        if let Some(tx) = self.emergency {
            let covering = tx
                .postings
                .iter()
                .flat_map(|p| p.only_negative().map(|v| (p.account.clone(), -v)))
                .collect_vec();

            assert!(covering.len() == 1);

            let (envelope, total) = covering.into_iter().next().unwrap();

            let spending = Spending {
                total,
                envelope,
                original: tx,
                scheduled: None,
            };

            vec![Operation::CoverEmergency(spending)].into_iter()
        } else {
            Vec::default().into_iter()
        }
    }
}

struct Available {
    names: Names,
    available: BigDecimal,
    emergency: BigDecimal,
    early: BigDecimal,
}

impl std::fmt::Debug for Available {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "Avail={} Emerg={} Early={}",
            &self.available, &self.emergency, &self.early
        )
    }
}

impl Available {
    fn new(names: Names) -> Self {
        Self {
            names,
            available: BigDecimal::default(),
            emergency: BigDecimal::default(),
            early: BigDecimal::default(),
        }
    }

    fn update(&mut self, tx: &Transaction) {
        for value in tx
            .iter_postings_for(&self.names.available)
            .filter_map(|p| p.has_value())
        {
            self.available += value;
            assert!(self.available >= BigDecimal::zero(), "{:?}", tx);
        }

        for value in tx
            .iter_postings_for(&self.names.early)
            .filter_map(|p| p.has_value())
        {
            self.early += value;
            assert!(self.early >= BigDecimal::zero(), "{:?}", tx);
        }

        for value in tx
            .iter_postings_for(&self.names.emergency)
            .filter_map(|p| p.has_value())
        {
            self.emergency += value;
            assert!(self.emergency >= BigDecimal::zero(), "{:?}", tx);
        }
    }

    fn cover(&self, spending: &Spending, today: &NaiveDate) -> Option<Covered> {
        let affects_emergency = spending.affects_emergency(&self.names);
        let remaining = spending.total.clone();

        let (early, remaining) = match spending.scheduled.as_ref() {
            Some(date) => {
                if today >= date {
                    (None, remaining)
                } else if self.early.is_positive() {
                    let taking = std::cmp::min(remaining.clone(), self.early.clone());
                    (
                        Some(spending.early(*today, taking.clone(), &self.names)),
                        remaining - taking,
                    )
                } else {
                    return None;
                }
            }
            None => (None, remaining),
        };

        let (available, remaining) = if remaining.is_positive() && self.available.is_positive() {
            let taking = std::cmp::min(remaining.clone(), self.available.clone());
            (
                Some(spending.available(*today, taking.clone(), &self.names)),
                remaining - taking,
            )
        } else {
            (None, remaining)
        };

        let (emergency, remaining) =
            if remaining.is_positive() && self.emergency.is_positive() && emergency {
                let taking = std::cmp::min(remaining.clone(), self.emergency.clone());
                (
                    Some(spending.emergency(*today, taking.clone(), &self.names)),
                    remaining - taking,
                )
            } else {
                (None, remaining)
            };

        if !remaining.is_zero() {
            None
        } else {
            Some(Covered {
                available,
                emergency,
                early,
            })
        }
    }
}

#[derive(Clone)]
struct SharedAvailable {
    available: Rc<RefCell<Available>>,
}

impl SharedAvailable {
    fn update(&self, tx: &Transaction) {
        let mut available = self.available.borrow_mut();
        available.update(tx);
    }

    fn cover(&self, spending: &Spending, today: &NaiveDate) -> Option<Covered> {
        let available = self.available.borrow_mut();
        available.cover(spending, today)
    }
}

struct Generated {
    available: SharedAvailable,
    transactions: Vec<Transaction>,
}

impl Generated {
    fn push(&mut self, tx: Transaction) {
        self.available.update(&tx);
        self.transactions.push(tx);
    }

    fn into_iter(self) -> impl Iterator<Item = Transaction> {
        self.transactions.into_iter()
    }
}

struct Finances {
    today: Option<NaiveDate>,
    names: Names,
    matchers: Matchers,
    generated: Generated,
    available: SharedAvailable,
    taxes: Vec<Matcher>,
    pending: Vec<Operation>,
}

impl Finances {
    fn new(config: &Configuration) -> Result<Self> {
        let available = SharedAvailable {
            available: Rc::new(RefCell::new(Available::new(config.names.clone()))),
        };
        let generated = Generated {
            available: available.clone(),
            transactions: Vec::default(),
        };

        Ok(Self {
            today: None,
            names: config.names.clone(),
            matchers: Matchers::new(config)?,
            generated,
            available,
            taxes: Vec::default(),
            pending: Vec::default(),
        })
    }

    fn handle(&mut self, tx: &Transaction) -> Result<()> {
        let ops = self
            .matchers
            .matchers
            .iter()
            .chain(self.taxes.iter())
            .map(|m| m.apply(tx))
            .collect::<Result<Vec<_>>>()?
            .into_iter()
            .flatten();

        self.today = Some(*tx.date());
        self.available.update(tx);

        for op in ops {
            if self.can_apply(&op) {
                self.apply(op)?;
            } else {
                trace!("queue {:?}", op);
                self.pending.push(op);
            }
        }

        Ok(())
    }

    fn can_apply(&self, op: &Operation) -> bool {
        match op {
            Operation::AllocatePaycheck(_, _) => true,
            Operation::RefundedToAvailable(_) => true,
            Operation::EnvelopeWithdrawal(_) => true,
            Operation::CoverSpending(spending) | Operation::CoverEmergency(spending) => self
                .available
                .cover(spending, self.today.as_ref().unwrap())
                .is_some(),
        }
    }

    fn apply(&mut self, op: Operation) -> Result<()> {
        debug!("{}", &op);

        assert!(self.can_apply(&op), "{:#?}", op);

        match op {
            Operation::AllocatePaycheck(tx, taxes) => {
                self.generated.push(tx);

                if let Some(taxes) = taxes.as_ref() {
                    self.taxes = taxes
                        .rules
                        .clone()
                        .into_iter()
                        .map(|rule| Matcher {
                            op: Box::new(ApplyTaxes {
                                names: self.names.clone(),
                                rule,
                            }),
                        })
                        .collect_vec();
                }

                let mut pending = self.pending.drain(0..).collect_vec();
                let unprocessed = pending
                    .drain(0..)
                    .flat_map(|op| {
                        if self.can_apply(&op) {
                            self.apply(op).unwrap();

                            None
                        } else {
                            Some(op)
                        }
                    })
                    .collect_vec();

                self.pending = unprocessed;
            }
            Operation::CoverSpending(spending) | Operation::CoverEmergency(spending) => {
                let today = self.today.as_ref().unwrap();
                let covered = self.available.cover(&spending, today);

                if let Some(covered) = covered {
                    for tx in covered.transactions() {
                        self.generated.push(tx.clone());
                    }

                    let ops = covered.clone().to_corrective_operations();
                    for op in ops {
                        trace!("queue {:?}", op);
                        self.pending.push(op);
                    }
                } else {
                    panic!("not covered");
                }
            }
            Operation::RefundedToAvailable(tx) | Operation::EnvelopeWithdrawal(tx) => {
                self.generated.push(tx);
            }
        }

        Ok(())
    }
}

#[derive(Debug, Args)]
pub struct Command {
    #[arg(long, value_name = "CONFIG")]
    config: PathBuf,
    #[arg(long, value_name = "GENERATED")]
    generated: PathBuf,
}

pub fn execute_command(file: &LedgerFile, cmd: &Command) -> anyhow::Result<()> {
    let configuration = config::Configuration::load(&cmd.config)?;

    let sorted = file
        .iter_transactions_in_order()
        .filter(|t| !matches!(t.origin, Some(Origin::Generated)))
        .collect::<Vec<_>>();

    let mut finances = Finances::new(&configuration)?;
    for tx in sorted.iter() {
        finances.handle(tx)?;
    }

    let nodes = finances
        .generated
        .into_iter()
        .enumerate()
        .flat_map(|(i, mut tx)| {
            tx.notes.push(format!(":generated:order:{}", i));
            vec![Node::Transaction(tx), Node::EmptyLine]
        })
        .collect_vec();

    let mut writer = std::fs::File::create(&cmd.generated)?;
    let printer = Printer::default();
    printer.write_nodes(&mut writer, nodes.iter())?;

    Ok(())
}

trait Operator: std::fmt::Debug {
    fn matches(&self, tx: &Transaction) -> bool;
    fn apply(&self, tx: &Transaction) -> Result<Vec<Operation>>;
}

#[derive(Debug, Default)]
struct Noop {}

impl Operator for Noop {
    fn matches(&self, _tx: &Transaction) -> bool {
        false
    }

    fn apply(&self, _tx: &Transaction) -> Result<Vec<Operation>> {
        Ok(Vec::default())
    }
}

#[derive(Debug)]
struct AllocatePaycheck {
    names: Names,
    income: IncomeDefinition,
}

impl Operator for AllocatePaycheck {
    fn matches(&self, tx: &Transaction) -> bool {
        tx.has_posting_for_re(self.income.regex())
    }

    fn apply(&self, tx: &Transaction) -> Result<Vec<Operation>> {
        debug!("{:?} {}", tx.date, tx.payee);

        let template = pay::PaycheckTemplate::new(self.income.name(), *tx.date());
        let paycheck = template.generate(tx)?;
        let cycle = self.income.cycle();
        let transactions = paycheck.transactions(cycle, &self.names)?;

        info!(
            "{:?} {} {:#?}",
            tx.date,
            tx.payee,
            paycheck.remaining(cycle).round(2)
        );

        Ok(transactions
            .into_iter()
            .map(|tx| Operation::AllocatePaycheck(tx, paycheck.taxes()))
            .collect_vec())
    }
}

const MANUAL_TAG: &str = ":manual:";
const TAX_TAG: &str = ":tax:";

#[derive(Debug)]
struct RefundMoney {
    names: Names,
    config: RefundDefinition,
}

impl Operator for RefundMoney {
    fn matches(&self, tx: &Transaction) -> bool {
        tx.has_posting_for_re(self.config.regex())
    }

    fn apply(&self, tx: &Transaction) -> Result<Vec<Operation>> {
        // We only apply to money being deposited into the configured accounts.
        let refunded = tx
            .iter_postings_for_re(self.config.regex())
            .flat_map(|p| p.only_positive().map(|value| (p.account.clone(), value)))
            .collect_vec();

        if refunded.is_empty() {
            Ok(Vec::default())
        } else {
            Ok(vec![Operation::RefundedToAvailable(
                Transactions::new(*tx.date(), &self.names, tx).make_refund(refunded.into_iter())?,
            )])
        }
    }
}

#[derive(Debug)]
struct FillEnvelopeAndCoverSpending {
    names: Names,
    config: EnvelopeDefinition,
}

impl Operator for FillEnvelopeAndCoverSpending {
    fn matches(&self, tx: &Transaction) -> bool {
        tx.has_posting_for_re(self.config.regex())
            && !tx.notes.iter().any(|n| n.contains(MANUAL_TAG))
    }

    fn apply(&self, tx: &Transaction) -> Result<Vec<Operation>> {
        let total: BigDecimal = tx
            .iter_postings_for_re(self.config.regex())
            .flat_map(|p| p.has_value())
            .sum();

        let envelope: AccountPath = self.config.name.as_str().into();
        let envelope_withdrawal = Transactions::new(*tx.date(), &self.names, tx)
            .make_envelope_withdrawal(vec![(envelope.clone(), total.clone())].into_iter())?;

        if total.is_positive() {
            let spending = Spending {
                total,
                envelope,
                original: tx.clone(),
                scheduled: None,
            };

            Ok(vec![
                Operation::EnvelopeWithdrawal(envelope_withdrawal),
                Operation::CoverSpending(spending),
            ])
        } else {
            let refund = Transactions::new(*tx.date(), &self.names, tx)
                .make_refund(vec![(envelope.clone(), -total)].into_iter())?;

            Ok(vec![
                Operation::EnvelopeWithdrawal(envelope_withdrawal),
                Operation::RefundedToAvailable(refund),
            ])
        }
    }
}

#[derive(Debug)]
struct CoverSpending {
    config: SpendingDefinition,
    payoff_re: Regex,
}

impl Operator for CoverSpending {
    fn matches(&self, tx: &Transaction) -> bool {
        tx.has_posting_for_re(self.config.regex())
    }

    fn apply(&self, tx: &Transaction) -> Result<Vec<Operation>> {
        let maximum: Option<u64> = self
            .payoff_re
            .captures(&tx.payee)
            .map(|v| v[1].parse())
            .map_or(Ok(None), |v| v.map(Some))?;

        let covering = tx
            .iter_postings_for_re(self.config.regex())
            .flat_map(|p| p.only_negative().map(|v| (p.account.clone(), v)))
            .group_by(|(p, _)| p.clone())
            .into_iter()
            .map(|(a, v)| (a, v.into_iter().map(|(_, v)| v.abs()).sum()))
            .collect_vec();

        if !covering.is_empty() {
            let (envelope, total) = covering.into_iter().next().unwrap();

            let spending = Spending {
                total,
                envelope,
                original: tx.clone(),
                scheduled: None,
            };

            Ok(spending
                .scheduled(maximum.map(|v| v.into()))
                .into_iter()
                .map(Operation::CoverSpending)
                .collect_vec())
        } else {
            Ok(Vec::default())
        }
    }
}

#[derive(Debug)]
struct ApplyTaxes {
    names: Names,
    rule: TaxRule,
}

impl Operator for ApplyTaxes {
    fn matches(&self, tx: &Transaction) -> bool {
        if self.rule.matches(tx) {
            if tx.notes.iter().any(|n| n.contains(MANUAL_TAG)) {
                tx.notes.iter().any(|n| n.contains(TAX_TAG))
            } else {
                true
            }
        } else {
            false
        }
    }

    fn apply(&self, tx: &Transaction) -> Result<Vec<Operation>> {
        let taxed = self.rule.amount(tx).unwrap();

        let spending = Spending {
            total: taxed,
            envelope: self.names.taxes.as_str().into(),
            original: tx.clone(),
            scheduled: None,
        };

        Ok(vec![Operation::CoverSpending(spending)])
    }
}

#[derive(Debug)]
pub struct Matcher {
    op: Box<dyn Operator>,
}

impl Matcher {
    fn apply(&self, tx: &Transaction) -> Result<Vec<Operation>> {
        if self.op.matches(tx) {
            self.op.apply(tx)
        } else {
            Ok(Vec::default())
        }
    }
}

#[derive(Default)]
pub struct Matchers {
    matchers: Vec<Matcher>,
}

impl Matchers {
    fn new(config: &Configuration) -> Result<Self> {
        let matchers = config
            .income
            .clone()
            .into_iter()
            .map(|income| Matcher {
                op: Box::new(AllocatePaycheck {
                    names: config.names.clone(),
                    income,
                }),
            })
            .chain(config.refund.clone().into_iter().map(|refund| Matcher {
                op: Box::new(RefundMoney {
                    names: config.names.clone(),
                    config: refund,
                }),
            }))
            .chain(config.spending.clone().into_iter().map(|spending| Matcher {
                op: Box::new(CoverSpending {
                    config: spending,
                    payoff_re: Regex::new("payoff:(\\d+)").unwrap(),
                }),
            }))
            .chain(
                config
                    .envelopes
                    .clone()
                    .into_iter()
                    .filter(|envelope| envelope.enabled)
                    .map(|envelope| Matcher {
                        op: Box::new(FillEnvelopeAndCoverSpending {
                            names: config.names.clone(),
                            config: envelope,
                        }),
                    }),
            )
            .collect::<Vec<_>>();

        Ok(Self { matchers })
    }
}

struct Transactions<'t> {
    date: NaiveDate,
    names: &'t Names,
    tx: &'t Transaction,
}

impl<'t> Transactions<'t> {
    fn new(date: NaiveDate, names: &'t Names, tx: &'t Transaction) -> Self {
        Self { date, names, tx }
    }

    fn make(
        &self,
        description: &str,
        balanced_by: impl Into<AccountPath>,
        postings: impl Iterator<Item = (AccountPath, BigDecimal)>,
    ) -> Result<Transaction> {
        Transaction {
            date: crate::model::ParsedDate::Full(self.date),
            payee: match self.tx.mid.as_ref() {
                Some(mid) => format!("{} `{}` #{}#", description, self.tx.payee, mid),
                None => format!("{} `{}`", description, self.tx.payee),
            },
            cleared: self.tx.cleared,
            postings: postings
                .into_iter()
                .map(|(account, value)| Posting {
                    account,
                    expression: Some(Expression::Literal(Numeric::Decimal(value))),
                    note: None,
                })
                .chain(std::iter::once(Posting {
                    account: balanced_by.into(),
                    expression: None,
                    note: None,
                }))
                .collect_vec(),
            notes: Vec::default(),
            mid: None,
            order: None,
            origin: Some(Origin::Generated),
            refs: Vec::default(),
        }
        .into_balanced()
    }

    fn make_refund(
        &self,
        refunded: impl Iterator<Item = (AccountPath, BigDecimal)>,
    ) -> Result<Transaction> {
        self.make(
            "refund",
            self.names.available.as_str(),
            refunded.into_iter().map(|(a, v)| (a, -v)),
        )
    }

    fn make_envelope_withdrawal(
        &self,
        reserving: impl Iterator<Item = (AccountPath, BigDecimal)>,
    ) -> Result<Transaction> {
        let source = self
            .names
            .get_envelope_source(self.tx)
            .ok_or(Error::NoEnvelopeSource)?;

        self.make(
            "from envelope",
            source.as_str(),
            reserving.into_iter().map(|(a, v)| (a, -v)),
        )
    }

    fn make_cover_from_available(
        &self,
        spending: impl Iterator<Item = (AccountPath, BigDecimal)>,
    ) -> Result<Transaction> {
        self.make("cover", self.names.available.as_str(), spending)
    }

    fn make_cover_from_emergency(
        &self,
        spending: impl Iterator<Item = (AccountPath, BigDecimal)>,
    ) -> Result<Transaction> {
        self.make("emergency", self.names.emergency.as_str(), spending)
    }

    fn make_cover_from_early(
        &self,
        spending: impl Iterator<Item = (AccountPath, BigDecimal)>,
    ) -> Result<Transaction> {
        self.make("early", self.names.early.as_str(), spending)
    }
}

#[derive(Debug, Error)]
enum Error {
    #[error("no envelope source")]
    NoEnvelopeSource,
}
