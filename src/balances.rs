use chrono::{DateTime, NaiveDateTime, NaiveTime, Utc};
use clap::Args;
use regex::Regex;
use serde::{ser::SerializeStruct, Serialize};
use std::collections::{
    hash_map::{Iter, Keys},
    HashMap,
};

use crate::model::*;

#[derive(Debug, Args)]
pub struct Command {
    pub pattern: Option<String>,
    #[arg(short, long)]
    pub cleared: bool,
    #[arg(short, long)]
    pub actual: bool,
}

#[derive(Clone)]
pub struct BalancesByAccount {
    map: HashMap<String, Balances>,
}

impl BalancesByAccount {
    pub fn abs(&self) -> BalancesByAccount {
        let mut map: HashMap<String, Balances> = HashMap::new();
        for (key, value) in self.map.iter() {
            map.insert(key.clone(), value.abs());
        }
        Self { map }
    }

    pub fn keys(&self) -> Keys<String, Balances> {
        self.map.keys()
    }

    pub fn get(&self, name: &str) -> Option<&Balances> {
        self.map.get(name)
    }

    pub fn iter(&self) -> Iter<String, Balances> {
        self.map.iter()
    }
}

pub fn calculate_balances(file: &LedgerFile, cmd: &Command) -> anyhow::Result<BalancesByAccount> {
    let declared = file.declared_accounts();

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

    let by_path = past_only
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
                .filter_map(|p| p.into_balances().map(|b| (p.account.as_str(), b)))
        })
        // This is easier than trying to get this to work with group_by
        .fold(HashMap::new(), |mut acc, (a, v)| {
            if !acc.contains_key(a) {
                acc.insert(a.to_owned(), v);
            } else {
                *acc.get_mut(a).unwrap() += v
            }
            acc
        });

    if cmd.actual {
        Ok(BalancesByAccount { map: by_path })
    } else {
        Ok(BalancesByAccount {
            map: bubble_balances_upward(&by_path, &declared),
        })
    }
}

pub fn execute_command(file: &LedgerFile, cmd: &Command) -> anyhow::Result<()> {
    let accounts = calculate_balances(file, cmd)?;

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

pub fn bubble_balances_upward(
    from: &HashMap<String, Balances>,
    declared: &HashMap<String, AccountPath>,
) -> HashMap<String, Balances> {
    let mut into = HashMap::new();
    for (account, balances) in from.iter() {
        for parent in get_parents_and_self(account) {
            if declared.contains_key(parent.as_str()) {
                if !into.contains_key(parent.as_str()) {
                    into.insert(parent, balances.clone());
                } else {
                    *into.get_mut(parent.as_str()).unwrap() += balances.clone();
                }
            }
        }
    }
    into
}

fn get_parents_and_self(p: &str) -> Vec<String> {
    if !p.is_empty() {
        std::iter::once(p.to_owned())
            .chain(get_parents(p))
            .collect()
    } else {
        vec![]
    }
}

fn get_parents(p: &str) -> Vec<String> {
    let mut parents = Vec::new();
    for (i, c) in p.chars().enumerate() {
        if c == ':' && i < p.len() {
            parents.push(p[0..i].to_owned())
        }
    }
    parents.reverse();
    parents
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_get_parents_and_self_empty() {
        let empty: Vec<String> = Vec::new();
        assert_eq!(get_parents_and_self(""), empty);
    }

    #[test]
    fn test_get_parents_and_self_single() {
        assert_eq!(get_parents_and_self("single"), vec!["single"]);
    }

    #[test]
    fn test_get_parents_and_self_two() {
        assert_eq!(
            get_parents_and_self("first:second"),
            vec!["first:second", "first"]
        );
    }

    #[test]
    fn test_get_parents_and_self_three() {
        assert_eq!(
            get_parents_and_self("first:second:third"),
            vec!["first:second:third", "first:second", "first"]
        );
    }

    #[test]
    fn test_get_parents_empty() {
        let empty: Vec<String> = Vec::new();
        assert_eq!(get_parents(""), empty);
    }

    #[test]
    fn test_get_parents_single() {
        let empty: Vec<String> = Vec::new();
        assert_eq!(get_parents("single"), empty);
    }

    #[test]
    fn test_get_parents_two() {
        assert_eq!(get_parents("first:second"), vec!["first"]);
    }

    #[test]
    fn test_get_parents_three() {
        assert_eq!(
            get_parents("first:second:third"),
            vec!["first:second", "first"]
        );
    }
}

pub struct SymbolDecimal<'a> {
    pub symbol: &'a str,
    pub decimal: &'a BigDecimal,
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

impl<'a> Serialize for SymbolDecimal<'a> {
    fn serialize<S>(&self, serializer: S) -> std::result::Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        let mut state = serializer.serialize_struct("SymbolDecimal", 2)?;
        state.serialize_field("symbol", self.symbol)?;
        state.serialize_field("display", &format!("{}", self))?;
        state.end()
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
