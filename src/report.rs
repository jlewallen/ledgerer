use clap::Args;
use regex::Regex;
use serde::Serialize;
use std::{collections::HashMap, path::PathBuf};
use tera::{to_value, try_get_value, Context, Function, Tera, Value};
use tracing::{debug, info};

use crate::{
    balances::{self, calculate_balances, SymbolDecimal},
    model::*,
};

#[derive(Debug, Args)]
pub struct Command {
    paths: Vec<PathBuf>,
    #[arg(short, long)]
    actual: bool,
    #[arg(short, long)]
    include: Option<String>,
}

pub fn execute_command(file: &LedgerFile, cmd: &Command) -> anyhow::Result<()> {
    let everything = calculate_balances(
        &file,
        &balances::Command {
            pattern: None,
            cleared: false,
            actual: cmd.actual,
        },
    )?;

    let cleared = calculate_balances(
        &file,
        &balances::Command {
            pattern: None,
            cleared: true,
            actual: cmd.actual,
        },
    )?;

    debug!("initializing tera");
    let mut tera = Tera::default();
    tera.register_filter("lpad", lpad);
    tera.register_function("balances_matching", balances_matching(everything, cleared));

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
        let context = Context::new();
        info!("rendering {}", path.display());
        let text = tera.render(path.to_str().unwrap(), &context)?;
        println!("{}", &text);
    }

    Ok(())
}

#[derive(Debug, Serialize)]
struct MatchedBalance {
    name: String,
    balances: Vec<tera::Value>,
}

fn balances_matching(
    everything: HashMap<String, Balances>,
    cleared: HashMap<String, Balances>,
) -> impl Function {
    Box::new(
        move |args: &HashMap<String, Value>| -> tera::Result<Value> {
            let accounts = match args.get("cleared") {
                Some(Value::Bool(true)) => &cleared,
                _ => &everything,
            };

            match args.get("pattern") {
                Some(pattern) => {
                    let pattern = try_get_value!("balances_matching", "pattern", String, pattern);
                    let pattern =
                        Regex::new(&pattern).map_err(|_| tera::Error::msg("bad pattern"))?;

                    Ok(to_value(
                        accounts
                            .iter()
                            .filter(|(name, _)| pattern.is_match(name))
                            .map(|(name, balances)| -> tera::Result<MatchedBalance> {
                                Ok(MatchedBalance {
                                    name: name.clone(),
                                    balances: balances
                                        .iter()
                                        .map(|(symbol, total)| SymbolDecimal::new(&symbol, total))
                                        .filter(|total| !total.with_scale().is_zero())
                                        .map(|total| Ok(to_value(total)?))
                                        .collect::<tera::Result<Vec<Value>>>()?,
                                })
                            })
                            .collect::<tera::Result<Vec<MatchedBalance>>>()?
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

pub fn lpad(value: &Value, args: &HashMap<String, Value>) -> tera::Result<Value> {
    let value = try_get_value!("lpad", "value", String, value);
    match args.get("width") {
        Some(width) => {
            let width = try_get_value!("lpad", "width", usize, width);
            Ok(to_value(format!("{:>width$}", value, width = width)).unwrap())
        }
        _ => unimplemented!(),
    }
}
