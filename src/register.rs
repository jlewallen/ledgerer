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
}

struct Format {
    leading_width: usize,
    name_width: usize,
    value_width: usize,
}

impl Format {
    pub fn new() -> Self {
        let fixed_spaces = 2;
        match terminal_size() {
            Some((Width(w), _)) => Self {
                leading_width: 80,
                name_width: 60,
                value_width: w as usize - fixed_spaces - 80 - 60,
            },
            None => Self {
                leading_width: 80,
                name_width: 60,
                value_width: 10,
            },
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
        // YES! https://users.rust-lang.org/t/convenience-method-for-flipping-option-result-to-result-option/13695/10
        // x.map_or(Ok(None), |v| v.map(Some))
        .map_or(Ok(None), |v| v.map(Some))
        .unwrap();

    let format = Format::new();
    for tx in file
        .iter_transactions_in_order()
        .filter(|tx| !cmd.cleared || tx.cleared)
        // This also happens in 'print' and it would be nice to avoid this duplication.
        .filter(|tx| match before {
            Some(before) => naive_to_pacific(tx.date.clone()).unwrap() < before,
            None => true,
        })
        .filter(|tx| match after {
            Some(after) => {
                if tx.date == NaiveDate::MIN {
                    true
                } else {
                    naive_to_pacific(tx.date.clone()).unwrap() >= after
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
                println!(
                    "{:leading_width$} {:name_width$} {:>value_width$}",
                    prefix.as_str().truncate_ellipse(format.leading_width - 3),
                    posting.account,
                    match &posting.expression {
                        Some(expression) => format!("{}", expression),
                        _ => "".to_owned(),
                    },
                    leading_width = format.leading_width,
                    name_width = format.name_width,
                    value_width = format.value_width,
                );
                prefix = "".to_owned();
            }
        }
    }

    Ok(())
}
