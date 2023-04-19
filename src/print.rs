use std::io::Write;

use clap::Args;

use crate::model::*;

#[derive(Debug, Args)]
pub struct Command {}

fn write_node<W>(node: &Node, mut w: W) -> Result<(), std::io::Error>
where
    W: Write,
{
    match node {
        Node::EmptyLine => writeln!(w),
        Node::Comment(text) => {
            writeln!(w, ";{}", text)
        }
        Node::Transaction(tx) => {
            let write_prefix = |w: &mut W| write!(w, "    ");
            write!(w, "{} ", tx.date.format("%Y/%m/%d"))?;
            if tx.cleared {
                write!(w, "* ")?;
            }
            writeln!(w, "{}", tx.payee)?;
            for n in tx.notes.iter() {
                write_prefix(&mut w)?;
                writeln!(w, "; {}", n)?;
            }
            for p in tx.postings.iter() {
                write_prefix(&mut w)?;
                match &p.expression {
                    Some(Expression::Calculated(_)) | None => {
                        write!(w, "{}", p.account)?;
                    }
                    Some(expression) => {
                        write!(w, "{:76}", p.account)?;
                        write!(w, "{:>20}", expression)?;
                    }
                };

                match &p.note {
                    Some(note) => writeln!(w, " ; {}", note)?,
                    None => writeln!(w)?,
                };
            }

            Ok(())
        }
        Node::AutomaticTransaction(tx) => {
            writeln!(w, "= {}", tx.condition)?;
            for n in tx.notes.iter() {
                write!(w, "    ")?;
                writeln!(w, "; {}", n)?;
            }
            for p in tx.postings.iter() {
                write!(w, "    ")?;
                match &p.expression {
                    Some(expression) => {
                        write!(w, "{:76}", p.account)?;
                        write!(w, "{:>20}", expression)?;
                    }
                    None => write!(w, "{}", p.account)?,
                }
                match &p.note {
                    Some(note) => writeln!(w, " ; {}", note),
                    None => writeln!(w),
                }?;
            }

            Ok(())
        }
        Node::CommodityPrice(CommodityPrice {
            date,
            symbol,
            expression,
        }) => writeln!(w, "P {} {} {}", date.format("%Y/%m/%d"), symbol, expression),
        Node::Include(including) | Node::Included(including, _) => {
            writeln!(w, "!include {}", including)
        }
        Node::AccountDeclaration(ap) => writeln!(w, "account {}", ap.as_str()),
        Node::TagDeclaration(tag) => writeln!(w, "tag {}", tag),
        Node::DefaultCommodity(symbol) => writeln!(w, "D {}1000.00", symbol),
        Node::CommodityDeclaration(symbol) => writeln!(w, "commodity {}", symbol),
        _ => Ok(()),
    }
}

pub fn execute_command(file: &LedgerFile, _cmd: &Command) -> anyhow::Result<()> {
    let writer = std::io::stdout();
    for node in file.sorted_nodes_iter() {
        write_node(node, &writer)?;
    }

    Ok(())
}
