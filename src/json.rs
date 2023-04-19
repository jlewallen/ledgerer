use clap::Args;

use crate::model::LedgerFile;

#[derive(Debug, Args)]
pub struct Command {
    #[arg(short, long)]
    lint: bool,
}

pub fn execute_command(file: &LedgerFile, cmd: &Command) -> anyhow::Result<()> {
    if cmd.lint {
        crate::lint::execute_command(file, &crate::lint::Command {})?;
    }

    let sorted = file
        .iter_transactions_in_order()
        .filter(|t| t.is_simple())
        .collect::<Vec<_>>();
    serde_json::to_writer(std::io::stdout(), &sorted)?;

    Ok(())
}
