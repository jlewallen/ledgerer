use bigdecimal::ToPrimitive;
use clap::Args;
use regex::Regex;
use serde::{Deserialize, Serialize};
use std::{collections::HashMap, path::PathBuf};
use tera::{to_value, try_get_value, Context, Function, Tera, Value};
use tracing::{debug, info};

use crate::{
    balances::{self, calculate_balances, BalancesByAccount, SymbolDecimal},
    model::*,
};

#[derive(Debug, Args)]
pub struct Command {
    paths: Vec<PathBuf>,
    #[arg(short, long)]
    actual: bool,
    #[arg(short, long)]
    include: Option<String>,
    #[arg(short, long)]
    pub before: Option<String>,
}

pub fn execute_command(file: &LedgerFile, cmd: &Command) -> anyhow::Result<()> {
    let calculate = |cleared| {
        calculate_balances(
            file,
            &balances::Command {
                pattern: None,
                cleared,
                actual: cmd.actual,
                invert: false,
                posting_format: false,
                before: cmd.before.clone(),
            },
        )
    };

    let everything = calculate(false)?;
    let cleared = calculate(true)?;

    debug!("initializing tera");
    let mut tera = Tera::default();
    tera.register_filter("rpad", rpad);
    tera.register_filter("lpad", lpad);
    tera.register_filter("meter", meter);
    tera.register_function("balances", balances_matching(everything, cleared));

    if let Some(include) = &cmd.include {
        info!("including {:?}", include);
        for entry in glob::glob(include)? {
            let entry = entry?;
            let name = entry.file_name().map(|f| f.to_str()).unwrap();
            debug!("including {:?} as {:?}", entry, name);
            tera.add_template_file(&entry, name)?;
        }
    }

    for path in &cmd.paths {
        debug!("loading {}", path.display());
        tera.add_template_file(path, None)?;
        info!("rendering {}", path.display());
        let context = Context::new();
        let text = tera.render(path.to_str().unwrap(), &context)?;
        println!("{}", &text);
    }

    Ok(())
}

#[derive(Debug, Deserialize, Serialize)]
struct SingleBalance {
    symbol: String,
    display: String,
    total: f32,
}

#[derive(Debug, Deserialize, Serialize)]
struct MatchedBalance {
    name: String,
    balances: Vec<SingleBalance>,
}

impl MatchedBalance {
    fn default_currency_balance(&self) -> Option<&SingleBalance> {
        self.balances.iter().filter(|b| b.symbol == "$").next()
    }
}

fn balances_matching(everything: BalancesByAccount, cleared: BalancesByAccount) -> impl Function {
    Box::new(
        move |args: &HashMap<String, Value>| -> tera::Result<Value> {
            let accounts = match args.get("cleared") {
                Some(Value::Bool(true)) => &cleared,
                _ => &everything,
            };

            let accounts = match args.get("abs") {
                Some(Value::Bool(true)) => accounts.abs(),
                _ => accounts.clone(),
            };

            match args.get("pattern") {
                Some(pattern) => {
                    let pattern = try_get_value!("balances", "pattern", String, pattern);
                    let pattern =
                        Regex::new(&pattern).map_err(|e| tera::Error::msg(e.to_string()))?;

                    Ok(to_value(
                        accounts
                            .iter()
                            .filter(|(name, _)| pattern.is_match(name))
                            .map(|(name, balances)| {
                                Ok(MatchedBalance {
                                    name: name.clone(),
                                    balances: balances
                                        .iter()
                                        .map(|(symbol, total)| SymbolDecimal::new(symbol, total))
                                        .filter(|total| !total.with_scale().is_zero())
                                        .map(|total| {
                                            Ok(SingleBalance {
                                                symbol: total.symbol.to_owned(),
                                                display: total.to_string(),
                                                total: total.decimal.to_f32().unwrap(),
                                            })
                                        })
                                        .collect::<tera::Result<Vec<_>>>()?,
                                })
                            })
                            .collect::<tera::Result<Vec<_>>>()?
                            .iter()
                            .sorted_unstable_by_key(|m| (m.name.clone()))
                            .inspect(|p| debug!("{:?}", p))
                            .collect_vec(),
                    )?)
                }
                None => Err("oops".into()),
            }
        },
    )
}

fn get_string_with_width(
    name: &str,
    value: &Value,
    args: &HashMap<String, Value>,
) -> tera::Result<(String, usize)> {
    let value = try_get_value!(name, "value", String, value);
    match args.get("width") {
        Some(width) => {
            let width = try_get_value!(name, "width", usize, width);
            Ok((value, width))
        }
        _ => Err(tera::Error::msg("Missing width parameter.")),
    }
}

pub fn rpad(value: &Value, args: &HashMap<String, Value>) -> tera::Result<Value> {
    let (value, width) = get_string_with_width("rpad", value, args)?;
    Ok(to_value(format!("{:width$}", value, width = width)).unwrap())
}

pub fn lpad(value: &Value, args: &HashMap<String, Value>) -> tera::Result<Value> {
    let (value, width) = get_string_with_width("lpad", value, args)?;
    Ok(to_value(format!("{:>width$}", value, width = width)).unwrap())
}

pub fn meter(value: &Value, _args: &HashMap<String, Value>) -> tera::Result<Value> {
    let value = try_get_value!("meter", "value", Vec<MatchedBalance>, value);
    if value.len() != 1 {
        return Err(tera::Error::msg("Expected a single balance in 'meter'"));
    }

    let balances = value.into_iter().next().unwrap();
    match balances.default_currency_balance() {
        Some(balance) => {
            let big_bucks = balance.total / 50.0;
            Ok(to_value("$".repeat(big_bucks as usize))?)
        }
        None => Err(tera::Error::msg("Missing $ balance")),
    }
}
