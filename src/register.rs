use clap::Args;
use colored::Colorize;
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
    cumulative_width: Option<usize>,
}

impl Format {
    pub fn new(cmd: &Command) -> Self {
        let fixed_spaces = 2;

        let maximum_width = match (cmd.width, terminal_size()) {
            (Some(w), _) | (None, Some((Width(w), _))) => w as usize,
            _ => 160,
        };

        assert!(maximum_width >= 60);

        let value_width = 25;
        let cumulative_width = if cmd.cumulative {
            Some(value_width)
        } else {
            None
        };
        let after_values =
            maximum_width - value_width - cumulative_width.unwrap_or_default() - fixed_spaces;
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

#[allow(dead_code)]
struct Row<'r> {
    first_posting: bool,
    cleared: bool,
    date: &'r NaiveDate,
    payee: &'r str,
    account: &'r AccountPath,
    value: &'r Option<BigDecimal>,
    cumulative: &'r BigDecimal,
}

impl<'r> Row<'r> {
    fn format(self, format: &Format) -> String {
        let prefix = match self.first_posting {
            true => format!("{} {}", self.date, self.payee),
            false => "".to_owned(),
        };

        let formatted_value = match self.value {
            Some(value) => format!("{}", value),
            None => "".to_owned(),
        };

        match format.cumulative_width {
            Some(cumulative_width) => format!(
                "{:leading_width$} {:name_width$} {:>value_width$} {:>cumulative_width$}",
                prefix.as_str().truncate_ellipse(format.leading_width - 3),
                self.account,
                formatted_value,
                self.cumulative.round(2),
                leading_width = format.leading_width,
                name_width = format.name_width,
                value_width = format.value_width,
                cumulative_width = cumulative_width,
            ),
            None => format!(
                "{:leading_width$} {:name_width$} {:>value_width$}",
                prefix.as_str().truncate_ellipse(format.leading_width - 3),
                self.account,
                formatted_value,
                leading_width = format.leading_width,
                name_width = format.name_width,
                value_width = format.value_width,
            ),
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
        for (i, posting) in tx
            .postings
            .iter()
            .filter(|p| {
                compiled
                    .as_ref()
                    .map_or(true, |c| c.is_match(p.account.as_str()))
            })
            .enumerate()
        {
            let value = match &posting.expression {
                Some(expression) => expression.to_decimal(),
                _ => None,
            };

            if let Some(value) = value.as_ref() {
                cumulative += value;
            }

            let row = Row {
                first_posting: i == 0,
                cleared: tx.cleared,
                date: &tx.date,
                payee: &tx.payee,
                account: &posting.account,
                value: &value,
                cumulative: &cumulative,
            };

            println!(
                "{}",
                if tx.cleared {
                    row.format(&format).normal()
                } else {
                    row.format(&format).yellow()
                }
            );
        }
    }

    Ok(())
}
