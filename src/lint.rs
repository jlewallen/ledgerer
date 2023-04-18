use clap::Args;

use crate::model::LedgerFile;

#[derive(Debug, Args)]
pub struct Command {}

pub fn execute_command(file: &LedgerFile, _cmd: &Command) -> anyhow::Result<()> {
    for tx in file.iter_transactions_in_order() {
        for n in tx.notes.iter() {
            println!("{}", n)
        }
        for p in tx.postings.iter() {
            match &p.note {
                Some(note) => println!("{}", note),
                None => {}
            }
        }
    }

    Ok(())
}
