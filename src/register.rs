use clap::Args;
use ellipse::Ellipse;
use terminal_size::{terminal_size, Width};

use crate::model::*;

#[derive(Debug, Args)]
pub struct Command {
    pub pattern: Option<String>,
    #[arg(short, long)]
    pub cleared: bool,
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
    let format = Format::new();
    for tx in file
        .iter_transactions_in_order()
        .filter(|tx| !cmd.cleared || tx.cleared)
    {
        let mut prefix = format!("{} {}", tx.date, tx.payee);
        for posting in tx.postings.iter() {
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

    Ok(())
}
