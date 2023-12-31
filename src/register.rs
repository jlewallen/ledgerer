use clap::Args;
use ellipse::Ellipse;
use regex::Regex;
use terminal_size::{terminal_size, Width};

use crate::{balances::naive_to_pacific, model::*, print::optional_naive_to_pacific};

#[derive(Debug, Args)]
pub struct Command {
    pub pattern: Option<String>,
    #[arg(short, long)]
    pub cleared: bool,
    #[arg(short, long)]
    pub before: Option<String>,
    #[arg(short, long)]
    pub after: Option<String>,
    #[arg(short, long)]
    pub cumulative: bool,
    #[arg(long)]
    pub width: Option<u16>,
}

struct Format {
    leading_width: usize,
    name_width: usize,
    value_width: usize,
    cumulative_width: usize,
}

impl Format {
    pub fn new(cmd: &Command) -> Self {
        let fixed_spaces = 2;

        let maximum_width = match (cmd.width, terminal_size()) {
            (Some(w), _) | (None, Some((Width(w), _))) => w as usize,
            _ => 160,
        };

        assert!(maximum_width >= 60);

        let value_width = 60;
        let cumulative_width = if cmd.cumulative { value_width } else { 0 };
        let after_values = maximum_width - value_width - cumulative_width - fixed_spaces;
        let name_width = after_values / 2;
        let leading_width = after_values / 2;

        Self {
            leading_width,
            name_width,
            value_width,
            cumulative_width,
        }
    }
}

pub fn execute_command(file: &LedgerFile, cmd: &Command) -> anyhow::Result<()> {
    let after = optional_naive_to_pacific(&cmd.after)?;
    let before = optional_naive_to_pacific(&cmd.before)?;

    let compiled = cmd
        .pattern
        .clone()
        .map(|p| Regex::new(&p))
        .map_or(Ok(None), |v| v.map(Some))
        .unwrap();

    let mut cumulative = BigDecimal::zero();
    let format = Format::new(cmd);
    for tx in file
        .iter_transactions_in_order()
        .filter(|tx| !cmd.cleared || tx.cleared)
        // This also happens in 'print' and it would be nice to avoid this duplication.
        .filter(|tx| match before {
            Some(before) => naive_to_pacific(tx.date).unwrap() < before,
            None => true,
        })
        .filter(|tx| match after {
            Some(after) => {
                if tx.date == NaiveDate::MIN {
                    true
                } else {
                    naive_to_pacific(tx.date).unwrap() >= after
                }
            }
            None => true,
        })
    {
        let mut prefix = format!("{} {}", tx.date, tx.payee);
        for posting in tx.postings.iter() {
            if compiled
                .as_ref()
                .map_or(true, |c| c.is_match(posting.account.as_str()))
            {
                let formatted = match &posting.expression {
                    Some(expression) => format!("{}", expression),
                    _ => "".to_owned(),
                };

                print!(
                    "{:leading_width$} {:name_width$} {:>value_width$}",
                    prefix.as_str().truncate_ellipse(format.leading_width - 3),
                    posting.account,
                    formatted,
                    leading_width = format.leading_width,
                    name_width = format.name_width,
                    value_width = format.value_width,
                );

                let value = match &posting.expression {
                    Some(expression) => expression.to_decimal(),
                    _ => None,
                };

                if format.cumulative_width > 0 {
                    if let Some(value) = value {
                        cumulative += value;
                        print!(
                            "{:>cumulative_width$}",
                            cumulative.round(2),
                            cumulative_width = format.cumulative_width
                        );
                    }
                }

                println!();

                prefix = "".to_owned();
            }
        }
    }

    Ok(())
}
