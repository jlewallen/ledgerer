use clap::Args;

use crate::model::LedgerFile;

#[derive(Debug, Args)]
pub struct Command {}

pub fn execute_command(file: &LedgerFile, _cmd: &Command) -> anyhow::Result<()> {
    let sorted = file
        .iter_transactions_in_order()
        .filter(|t| t.is_simple())
        .collect::<Vec<_>>();

    serde_json::to_writer(std::io::stdout(), &sorted)?;

    Ok(())
}
