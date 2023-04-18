use anyhow::Result;
use clap::{arg, Parser, Subcommand};
use std::{path::PathBuf, time::Instant};
use tracing::*;
use tracing_subscriber::prelude::*;

pub mod model;
pub mod parsing;

use model::LedgerFile;

mod balances;
mod json;
mod lint;
mod lots;
mod print;

#[derive(Parser)]
#[command(author, version, about, long_about = None)]
struct Cli {
    #[arg(short, long, value_name = "FILE")]
    path: PathBuf,
    #[command(subcommand)]
    command: Option<Commands>,
}

#[derive(Subcommand)]
enum Commands {
    Json(json::Command),
    Print(print::Command),
    Balances(balances::Command),
    Lots(lots::Command),
    Lint(lint::Command),
}

fn main() -> Result<()> {
    fn get_rust_log() -> String {
        std::env::var("RUST_LOG").unwrap_or_else(|_| "error".into())
    }

    tracing_subscriber::registry()
        .with(tracing_subscriber::EnvFilter::new(get_rust_log()))
        .with(tracing_subscriber::fmt::layer().with_writer(std::io::stderr))
        .init();

    let cli = Cli::parse();

    let load_ledger_file = || -> Result<LedgerFile> {
        let _span = span!(Level::INFO, "loading").entered();
        let started = Instant::now();
        let file = LedgerFile::parse(&cli.path)?;
        let loaded = file.preprocess()?.apply_automatic_transactions()?;
        let elapsed = Instant::now() - started;
        info!("loaded in {:?}ms", elapsed);
        Ok(loaded)
    };

    match &cli.command {
        Some(Commands::Json(cmd)) => json::execute_command(&load_ledger_file()?, cmd),
        Some(Commands::Print(cmd)) => print::execute_command(&load_ledger_file()?, cmd),
        Some(Commands::Balances(cmd)) => balances::execute_command(&load_ledger_file()?, cmd),
        Some(Commands::Lots(cmd)) => lots::execute_command(&load_ledger_file()?, cmd),
        Some(Commands::Lint(cmd)) => lint::execute_command(&load_ledger_file()?, cmd),
        _ => Ok(()),
    }
}
