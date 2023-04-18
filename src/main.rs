use anyhow::{anyhow, Result};
use bigdecimal::{BigDecimal, Zero};
use chrono::{DateTime, NaiveDate, NaiveDateTime, NaiveTime, Utc};
use chrono_tz::US::Pacific;
use clap::{arg, Parser, Subcommand};
use itertools::Itertools;
use regex::Regex;
use std::{collections::HashMap, path::PathBuf, time::Instant};
use tracing::*;
use tracing_subscriber::prelude::*;

pub mod model;
pub mod parsing;

use crate::model::LedgerFile;

#[derive(Parser)]
#[command(author, version, about, long_about = None)]
struct Cli {
    #[arg(short, long, value_name = "FILE")]
    path: PathBuf,
    #[arg(short, long, action = clap::ArgAction::Count)]
    verbose: u8,
    #[command(subcommand)]
    command: Option<Commands>,
}

#[derive(Subcommand)]
enum Commands {
    Json,
    Balances {
        pattern: Option<String>,
        #[arg(short, long)]
        show_postings: bool,
        #[arg(short, long)]
        cleared: bool,
    },
    Print,
}

fn naive_to_pacific(date: NaiveDate) -> Result<DateTime<chrono_tz::Tz>> {
    let and_time = NaiveDateTime::new(date, NaiveTime::MIN);
    and_time
        .and_local_timezone(Pacific)
        .single()
        .ok_or_else(|| anyhow!("Error converting NaiveDate."))
}

fn main() -> Result<()> {
    fn get_rust_log() -> String {
        std::env::var("RUST_LOG").unwrap_or_else(|_| "error".into())
    }

    tracing_subscriber::registry()
        .with(tracing_subscriber::EnvFilter::new(get_rust_log()))
        .with(tracing_subscriber::fmt::layer().with_writer(std::io::stderr))
        .init();

    let cli = Cli::parse();

    let get_processed = || -> Result<LedgerFile> {
        let _span = span!(Level::INFO, "loading").entered();
        let started = Instant::now();
        let file = LedgerFile::parse(&cli.path)?;
        let loaded = file.preprocess()?.apply_automatic_transactions()?;
        let elapsed = Instant::now() - started;
        info!("loaded in {:?}ms", elapsed);
        Ok(loaded)
    };

    match &cli.command {
        Some(Commands::Json) => {
            let processed = get_processed()?;

            let sorted = processed
                .iter_transactions_in_order()
                .filter(|t| t.is_simple())
                .collect::<Vec<_>>();

            println!("{}", serde_json::to_string(&sorted)?);

            Ok(())
        }
        Some(Commands::Print) => {
            use crate::model::*;

            let processed = get_processed()?;

            let sorted = processed.sorted_nodes_iter().collect::<Vec<_>>();

            fn print_expression(expression: &Expression) -> String {
                match expression {
                    Expression::Literal(numeric) => {
                        format!("{}", numeric.to_text_format("$"))
                    }
                    Expression::Commodity(c) => match c {
                        (quantity, symbol, Some(price)) => format!(
                            "{} {} @ {}",
                            quantity.to_text_format_raw(),
                            symbol,
                            price.to_text_format("$")
                        ),
                        (quantity, symbol, None) => {
                            format!("{} {}", quantity.to_text_format_raw(), symbol)
                        }
                    },
                    Expression::Factor(n) => match &n {
                        (true, i) => format!("(-{})", i),
                        (false, i) => format!("({})", i),
                    },
                    Expression::Calculated(_) => "".to_owned(),
                }
            }

            for node in sorted {
                match node {
                    Node::Comment(text) => {
                        println!(";{}", text)
                    }
                    Node::Transaction(tx) => {
                        print!("{} ", tx.date.format("%Y/%m/%d").to_string());
                        if tx.cleared {
                            print!("* ")
                        }
                        println!("{}", tx.payee);
                        for n in tx.notes.iter() {
                            print!("    ");
                            println!("; {}", n)
                        }
                        for p in tx.postings.iter() {
                            print!("    ");
                            match &p.expression {
                                Some(Expression::Calculated(_)) | None => {
                                    print!(
                                        "{}",
                                        match &p.account {
                                            AccountPath::Real(name) => name.to_string(),
                                            AccountPath::Virtual(name) => format!("[{}]", name),
                                        }
                                    );
                                }
                                Some(expression) => {
                                    print!(
                                        "{:76}",
                                        match &p.account {
                                            AccountPath::Real(name) => name.to_string(),
                                            AccountPath::Virtual(name) => format!("[{}]", name),
                                        }
                                    );
                                    print!("{:>20}", print_expression(expression));
                                }
                            }

                            match &p.note {
                                Some(note) => println!(" ; {}", note),
                                None => println!(),
                            }
                        }
                    }
                    Node::AccountDeclaration(ap) => println!("account {}", ap.as_str()),
                    Node::TagDeclaration(tag) => println!("tag {}", tag),
                    Node::Include(including) => println!("!include {}", including),
                    Node::Included(including, _) => println!("!include {}", including),
                    Node::Generated(_) => {}
                    Node::DefaultCommodity(symbol) => println!("D {}1000.00", symbol),
                    Node::CommodityPrice(CommodityPrice {
                        date,
                        symbol,
                        expression,
                    }) => println!(
                        "P {} {} {}",
                        date.format("%Y/%m/%d").to_string(),
                        symbol,
                        print_expression(expression)
                    ),
                    Node::CommodityDeclaration(symbol) => println!("commodity {}", symbol),
                    Node::AutomaticTransaction(tx) => {
                        println!("= {}", tx.condition);
                        for n in tx.notes.iter() {
                            print!("    ");
                            println!("; {}", n)
                        }
                        for p in tx.postings.iter() {
                            print!("    ");
                            print!(
                                "{:76}",
                                match &p.account {
                                    AccountPath::Real(name) => name.to_string(),
                                    AccountPath::Virtual(name) => format!("[{}]", name),
                                }
                            );
                            print!(
                                "{:>20}",
                                match &p.expression {
                                    Some(expression) => print_expression(expression),
                                    None => "".to_owned(),
                                }
                            );
                            match &p.note {
                                Some(note) => println!(" ; {}", note),
                                None => println!(),
                            }
                        }
                    }
                    Node::EmptyLine => println!(),
                }
            }

            Ok(())
        }
        Some(Commands::Balances {
            pattern,
            show_postings: _show_postings,
            cleared,
        }) => {
            let processed = get_processed()?;
            let compiled = pattern
                .clone()
                .map(|p| Regex::new(&p))
                // YES! https://users.rust-lang.org/t/convenience-method-for-flipping-option-result-to-result-option/13695/10
                // x.map_or(Ok(None), |v| v.map(Some))
                .map_or(Ok(None), |v| v.map(Some))
                .unwrap();
            let sorted = processed
                .iter_transactions_in_order()
                .filter(|t| t.is_simple() && (!(*cleared) || t.cleared))
                .collect::<Vec<_>>();

            let past_only = sorted
                .iter()
                .filter(|tx| naive_to_pacific(tx.date).unwrap() < Utc::now());

            let accounts: HashMap<&str, BigDecimal> = past_only
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
                        .filter_map(|p| p.has_value().map(|value| (p.account.as_str(), value)))
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
                    if !value.is_zero() {
                        // println!("{:width$} {:>10}", key, value, width = max_key_len);
                        println!("{:>20} {}", value, key);
                    }
                }
            }

            Ok(())
        }

        _ => Ok(()),
    }
}
