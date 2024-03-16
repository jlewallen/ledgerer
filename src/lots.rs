use bigdecimal::BigDecimal;
use chrono::NaiveDate;
use clap::Args;
use std::{io::Write, ops::Neg};

use crate::model::{CommodityExpression, LedgerFile};

#[derive(Debug, Args)]
pub struct Command {
    symbol: Option<String>,
}

#[derive(Debug)]
pub struct Lot {
    pub date: NaiveDate,
    pub symbol: String,
    pub quantity: BigDecimal,
    pub price: Option<BigDecimal>,
}

pub fn execute_command(file: &LedgerFile, cmd: &Command) -> anyhow::Result<()> {
    let mut lots: Vec<Lot> = file
        .iter_transactions()
        .flat_map(|tx| {
            tx.postings.iter().filter_map(|p| match &p.expression {
                Some(crate::model::Expression::Commodity(CommodityExpression {
                    quantity,
                    symbol,
                    price,
                    lot_price: _,
                    date,
                })) => Some(Lot {
                    date: date.unwrap_or(tx.date().clone()),
                    symbol: symbol.to_owned(),
                    quantity: quantity.to_decimal(),
                    price: price.as_ref().map(|e| e.to_decimal()),
                }),
                _ => None,
            })
        })
        .collect();

    lots.sort_unstable_by_key(|lot| (lot.symbol.to_owned(), lot.date, lot.quantity.clone().neg()));

    let mut writer = std::io::stdout();

    for lot in lots
        .into_iter()
        .filter(|l| cmd.symbol.as_ref().map(|s| s == &l.symbol).unwrap_or(true))
    {
        if let Some(price) = lot.price {
            writeln!(
                writer,
                "{} {} {{${}}} [{}]",
                lot.quantity,
                lot.symbol,
                price,
                lot.date.format("%Y/%m/%d")
            )?
        }
    }

    Ok(())
}
