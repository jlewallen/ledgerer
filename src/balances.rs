use chrono::{DateTime, NaiveDateTime, NaiveTime, Utc};
use clap::Args;
use regex::Regex;
use serde::{ser::SerializeStruct, Serialize};
use std::collections::{hash_map::Iter, HashMap};

use crate::{model::*, print::optional_naive_to_pacific, register::Filter};

#[derive(Debug, Args)]
pub struct Command {
    pub pattern: Option<String>,
    #[arg(short, long)]
    pub cleared: bool,
    #[arg(short, long)]
    pub before: Option<String>,
    #[arg(short, long)]
    pub after: Option<String>,
    #[arg(long)]
    pub real: bool,
    #[arg(long = "virtual")]
    pub virt: bool,
    #[arg(long)]
    pub future: bool,

    #[arg(short, long)]
    pub actual: bool,
    #[arg(short, long)]
    pub invert: bool,
    #[arg(long)]
    pub apply_prices: bool,
    #[arg(short, long)]
    pub json: bool,
    #[arg(long)]
    pub posting_format: bool,
}

impl Command {
    fn filter(&self) -> Result<Filter> {
        let before = optional_naive_to_pacific(&self.before)?.or_else(|| {
            if self.future {
                None
            } else {
                Some(Utc::now())
            }
        });
        let after = optional_naive_to_pacific(&self.after)?;
        let pattern = self
            .pattern
            .clone()
            .map(|p| Regex::new(&p))
            .map_or(Ok(None), |v| v.map(Some))?;
        Ok(Filter {
            cleared: self.cleared,
            before,
            after,
            pattern,
            real: self.real,
            virt: self.virt,
        })
    }
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

    pub fn iter(&self) -> Iter<String, Balances> {
        self.map.iter()
    }
}

pub fn calculate_balances(file: &LedgerFile, cmd: &Command) -> anyhow::Result<BalancesByAccount> {
    let declared = file.declared_accounts();
    let filter = cmd.filter()?;

    let sorted = file
        .iter_transactions_in_order()
        .filter(|tx| filter.matches_tx(tx))
        .collect::<Vec<_>>();

    let by_path = sorted
        .iter()
        .flat_map(|tx| {
            tx.postings
                .iter()
                .filter(|p| filter.matches_posting(p))
                .filter_map(|p| {
                    p.into_balance()
                        .map(|b| (p.account.as_str(), Balances::new_from_balance(b)))
                })
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

struct DisplayBalance {
    account: String,
    value: Balance,
}

impl Serialize for DisplayBalance {
    fn serialize<S>(&self, serializer: S) -> std::result::Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        let mut state = serializer.serialize_struct("Balance", 2)?;
        state.serialize_field("account", &self.account)?;
        if let Some(value) = self.value.value() {
            state.serialize_field("value", &format!("{}", value))?;
        }
        state.end()
    }
}

pub fn execute_command(file: &LedgerFile, cmd: &Command) -> anyhow::Result<()> {
    let accounts = calculate_balances(file, cmd)?;

    let prices = file.get_prices()?;
    let balances = accounts
        .iter()
        .flat_map(|(key, value)| {
            value
                .iter()
                .map(|balance| {
                    let inverted = balance.neg();
                    let balance = if cmd.invert { &inverted } else { balance };
                    if cmd.apply_prices {
                        match balance {
                            Balance::Currency {
                                symbol: _,
                                value: _,
                            } => balance.clone(),
                            Balance::Commodity { symbol, lots } => {
                                match prices.value_of(symbol, Lot::quantity(lots)) {
                                    Some(value) => Balance::currency("$", value),
                                    None => balance.clone(),
                                }
                            }
                        }
                    } else {
                        balance.clone()
                    }
                })
                .map(|value| DisplayBalance {
                    account: key.clone(),
                    value,
                })
        })
        .sorted_unstable_by_key(|row| (row.account.clone(), row.value.to_string()))
        .collect_vec();

    if cmd.json {
        println!("{}", serde_json::to_string_pretty(&balances)?)
    } else {
        for balance in balances.iter() {
            if !balance.value.is_zero() {
                if cmd.posting_format {
                    println!("    {:76} {:>30}", balance.account, balance.value);
                } else {
                    println!("{:>30} {}", balance.value, balance.account);
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

pub fn naive_to_pacific(date: NaiveDate) -> Result<DateTime<chrono_tz::Tz>> {
    let and_time = NaiveDateTime::new(date, NaiveTime::MIN);
    and_time
        .and_local_timezone(chrono_tz::US::Pacific)
        .single()
        .ok_or_else(|| anyhow::anyhow!("Error converting NaiveDate."))
}
