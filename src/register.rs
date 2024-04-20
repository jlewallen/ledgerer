use chrono::{DateTime, Utc};
use clap::Args;
use colored::Colorize;
use ellipse::Ellipse;
use regex::Regex;
use terminal_size::{terminal_size, Width};

use crate::{balances::naive_to_pacific, model::*, print::optional_naive_to_pacific};

#[derive(Debug, Args)]
pub struct Command {
    pub pattern: Option<String>,
    #[arg(long)]
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
    #[arg(long)]
    pub width: Option<u16>,
    #[arg(short, long)]
    pub cumulative: bool,
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

pub(crate) struct Filter {
    pub(crate) cleared: bool,
    pub(crate) before: Option<DateTime<Utc>>,
    pub(crate) after: Option<DateTime<Utc>>,
    pub(crate) pattern: Option<Regex>,
    pub(crate) real: bool,
    pub(crate) virt: bool,
}

impl Filter {
    pub(crate) fn matches_tx(&self, tx: &Transaction) -> bool {
        let allow_before = match self.before {
            Some(before) => naive_to_pacific(*tx.date()).unwrap() < before,
            None => true,
        };

        let allow_after = match self.after {
            Some(after) => {
                if tx.date() == &NaiveDate::MIN {
                    true
                } else {
                    naive_to_pacific(*tx.date()).unwrap() >= after
                }
            }
            None => true,
        };

        let allow_cleared = !self.cleared || tx.cleared;

        allow_cleared && allow_before && allow_after
    }

    pub(crate) fn matches_posting(&self, posting: &Posting) -> bool {
        let allow_pattern = self
            .pattern
            .as_ref()
            .map_or(true, |c| c.is_match(posting.account.as_str()));

        let allow_real = !self.real || posting.account.is_real();

        let allow_virtual = !self.virt || posting.account.is_virtual();

        allow_pattern && allow_real && allow_virtual
    }
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
    let filter = cmd.filter()?;

    let mut cumulative = BigDecimal::zero();
    let format = Format::new(cmd);
    for tx in file
        .iter_transactions_in_order()
        .filter(|tx| filter.matches_tx(tx))
    {
        for (i, posting) in tx
            .postings
            .iter()
            .filter(|posting| filter.matches_posting(posting))
            .enumerate()
        {
            let value = match &posting.expression {
                Some(expression) => expression.to_decimal().map(|d| d.round(2)),
                _ => None,
            };

            if let Some(value) = value.as_ref() {
                cumulative += value;
            }

            let row = Row {
                first_posting: i == 0,
                cleared: tx.cleared,
                date: tx.date(),
                payee: &tx.payee,
                account: &posting.account,
                value: &value,
                cumulative: &cumulative,
            };

            println!(
                "{}",
                if tx.cleared {
                    match tx.origin {
                        Some(Origin::File) => row.format(&format).normal(),
                        Some(Origin::Automatic) => row.format(&format).blue(),
                        Some(Origin::Generated) => row.format(&format).cyan(),
                        None => row.format(&format).normal(),
                    }
                } else {
                    row.format(&format).yellow()
                }
            );
        }
    }

    Ok(())
}
