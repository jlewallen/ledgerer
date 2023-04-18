use clap::Args;

use crate::model::LedgerFile;

#[derive(Debug, Args)]
pub struct Command {}

pub fn execute_command(file: &LedgerFile, _cmd: &Command) -> anyhow::Result<()> {
    let sorted = file
        .iter_transactions_in_order()
        .filter(|t| t.is_simple())
        .collect::<Vec<_>>();

    println!("{}", serde_json::to_string(&sorted)?);

    Ok(())
}
