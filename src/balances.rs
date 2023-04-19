use chrono::{DateTime, NaiveDateTime, NaiveTime, Utc};
use clap::Args;
use regex::Regex;
use std::collections::HashMap;

use crate::model::*;

#[derive(Debug, Args)]
pub struct Command {
    pattern: Option<String>,
    #[arg(short, long)]
    cleared: bool,
}

pub fn execute_command(file: &LedgerFile, cmd: &Command) -> anyhow::Result<()> {
    let compiled = cmd
        .pattern
        .clone()
        .map(|p| Regex::new(&p))
        // YES! https://users.rust-lang.org/t/convenience-method-for-flipping-option-result-to-result-option/13695/10
        // x.map_or(Ok(None), |v| v.map(Some))
        .map_or(Ok(None), |v| v.map(Some))
        .unwrap();
    let sorted = file
        .iter_transactions_in_order()
        .filter(|t| !cmd.cleared || t.cleared)
        .collect::<Vec<_>>();

    let past_only = sorted
        .iter()
        .filter(|tx| naive_to_pacific(tx.date).unwrap() < Utc::now());

    let accounts: HashMap<&str, Balances> = past_only
        .flat_map(|tx| {
            tx.postings
                .iter()
                .filter(|p| {
                    if let Some(compiled) = &compiled {
                        compiled.is_match(p.account.as_str())
                    } else {
                        true
                    }
                })
                .filter_map(|p| {
                    p.into_balances()
                        .map_or(None, |b| Some((p.account.as_str(), b)))
                })
        })
        // This is easier than trying to get this to work with group_by
        .fold(HashMap::new(), |mut acc, (a, v)| {
            if !acc.contains_key(a) {
                acc.insert(a, v);
            } else {
                *acc.get_mut(a).unwrap() += v
            }
            acc
        });

    if let Some(_max_key_len) = accounts.keys().map(|k| k.len()).max() {
        for key in accounts.keys().sorted() {
            let value = accounts.get(key).unwrap();
            for (symbol, value) in value.iter() {
                let pretty = SymbolDecimal::new(symbol, value);
                if !pretty.is_zero() {
                    println!("{:>20} {}", pretty, key);
                }
            }
        }
    }

    Ok(())
}

struct SymbolDecimal<'a> {
    symbol: &'a str,
    decimal: &'a BigDecimal,
}

impl<'a> SymbolDecimal<'a> {
    pub fn new(symbol: &'a str, decimal: &'a BigDecimal) -> Self {
        Self { symbol, decimal }
    }

    pub fn with_scale(&self) -> BigDecimal {
        match self.symbol {
            "$" => self.decimal.with_scale(2),
            _ => self.decimal.clone(),
        }
    }

    pub fn is_zero(&self) -> bool {
        self.with_scale().is_zero()
    }
}

impl<'a> std::fmt::Display for SymbolDecimal<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.symbol {
            "$" => f.pad(&format!("${}", self.decimal.with_scale(2))),
            _ => f.pad(&format!("{} {}", self.symbol, self.decimal)),
        }
    }
}

fn naive_to_pacific(date: NaiveDate) -> Result<DateTime<chrono_tz::Tz>> {
    let and_time = NaiveDateTime::new(date, NaiveTime::MIN);
    and_time
        .and_local_timezone(chrono_tz::US::Pacific)
        .single()
        .ok_or_else(|| anyhow::anyhow!("Error converting NaiveDate."))
}
