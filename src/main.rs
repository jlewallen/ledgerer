use anyhow::Result;
use clap::{arg, Parser, Subcommand};
use std::{path::PathBuf, rc::Rc, sync::atomic::AtomicU64, time::Instant};
use tracing::*;
use tracing_subscriber::prelude::*;

pub mod model;
pub mod parsing;

use model::LedgerFile;

mod balances;
mod gen;
mod json;
mod lint;
mod lots;
mod print;
mod register;
mod report;

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
    Register(register::Command),
    Lots(lots::Command),
    Lint(lint::Command),
    Report(report::Command),
    Gen(gen::Command),
}

fn main() -> Result<()> {
    fn get_rust_log() -> String {
        std::env::var("RUST_LOG").unwrap_or_else(|_| "error".into())
    }

    tracing_subscriber::registry()
        .with(tracing_subscriber::EnvFilter::new(get_rust_log()))
        .with(
            tracing_subscriber::fmt::layer()
                .without_time()
                .with_writer(std::io::stderr),
        )
        .init();

    let cli = Cli::parse();

    let load_ledger_file = || -> Result<LedgerFile> {
        let _span = span!(Level::INFO, "loading").entered();
        let started = Instant::now();
        let file = LedgerFile::parse(&cli.path)?;
        let loaded = file
            .preprocess(Rc::new(AtomicU64::new(0)))?
            .apply_automatic_transactions()?;
        let elapsed = Instant::now() - started;
        info!("loaded in {:?}", elapsed);
        Ok(loaded)
    };

    match &cli.command {
        Some(Commands::Json(cmd)) => json::execute_command(&load_ledger_file()?, cmd),
        Some(Commands::Print(cmd)) => print::execute_command(&load_ledger_file()?, cmd),
        Some(Commands::Balances(cmd)) => balances::execute_command(&load_ledger_file()?, cmd),
        Some(Commands::Register(cmd)) => register::execute_command(&load_ledger_file()?, cmd),
        Some(Commands::Lots(cmd)) => lots::execute_command(&load_ledger_file()?, cmd),
        Some(Commands::Lint(cmd)) => lint::execute_command(&load_ledger_file()?, cmd),
        Some(Commands::Report(cmd)) => report::execute_command(&load_ledger_file()?, cmd),
        Some(Commands::Gen(cmd)) => gen::execute_command(&load_ledger_file()?, cmd),
        None => Ok(()),
    }
}
