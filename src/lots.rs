use bigdecimal::BigDecimal;
use chrono::NaiveDate;
use clap::Args;
use std::io::Write;

use crate::model::{CommodityExpression, LedgerFile};

#[derive(Debug, Args)]
pub struct Command {}

#[derive(Debug)]
pub struct Lot {
    pub date: NaiveDate,
    pub symbol: String,
    pub quantity: BigDecimal,
    pub price: Option<BigDecimal>,
}

pub fn execute_command(file: &LedgerFile, _cmd: &Command) -> anyhow::Result<()> {
    let mut lots: Vec<Lot> = file
        .iter_transactions()
        .flat_map(|tx| {
            tx.postings.iter().filter_map(|p| match &p.expression {
                Some(crate::model::Expression::Commodity(CommodityExpression {
                    quantity,
                    symbol,
                    price,
                })) => Some(Lot {
                    date: tx.date,
                    symbol: symbol.to_owned(),
                    quantity: quantity.to_decimal(),
                    price: price.as_ref().map(|e| e.to_decimal()),
                }),
                _ => None,
            })
        })
        .collect();

    lots.sort_unstable_by_key(|lot| (lot.symbol.to_owned(), lot.date));

    let mut writer = std::io::stdout();

    for lot in lots {
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
