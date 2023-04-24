use clap::Args;

use crate::model::*;

#[derive(Debug, Args)]
pub struct Command {
    pub pattern: Option<String>,
    #[arg(short, long)]
    pub cleared: bool,
}

fn truncate(s: &str, max_chars: usize) -> &str {
    match s.char_indices().nth(max_chars) {
        None => s,
        Some((idx, _)) => &s[..idx],
    }
}

pub fn execute_command(file: &LedgerFile, cmd: &Command) -> anyhow::Result<()> {
    for tx in file
        .iter_transactions_in_order()
        .filter(|tx| !cmd.cleared || tx.cleared)
    {
        let mut prefix = format!("{} {}", tx.date, tx.payee);
        for posting in tx.postings.iter() {
            println!(
                "{:80} {:60} {:>10}",
                truncate(&prefix, 80),
                posting.account,
                match &posting.expression {
                    Some(expression) => format!("{}", expression),
                    _ => "".to_owned(),
                }
            );
            prefix = "".to_owned();
        }
    }

    Ok(())
}
