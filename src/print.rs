use std::io::Write;

use chrono::{DateTime, Utc};
use clap::Args;

use crate::{balances::naive_to_pacific, model::*};

#[derive(Debug, Args)]
pub struct Command {
    #[arg(short, long)]
    pub recursive: bool,
    #[arg(short, long)]
    pub after: Option<String>,
    #[arg(short, long)]
    pub before: Option<String>,
}

pub struct Printer {
    pub recursive: bool,
    pub after: Option<DateTime<Utc>>,
    pub before: Option<DateTime<Utc>>,
}

pub fn optional_naive_to_pacific(v: &Option<String>) -> Result<Option<DateTime<Utc>>> {
    v.as_ref()
        .map_or(Ok::<Option<DateTime<Utc>>, anyhow::Error>(None), |o| {
            Ok(Some(
                naive_to_pacific(NaiveDate::parse_from_str(&o, "%m/%d/%Y")?)?.with_timezone(&Utc),
            ))
        })
}

impl Printer {
    pub fn from(cmd: &Command) -> Result<Self> {
        let after = optional_naive_to_pacific(&cmd.after)?;
        let before = optional_naive_to_pacific(&cmd.before)?;

        Ok(Self {
            after,
            before,
            recursive: cmd.recursive,
        })
    }

    fn write_node(&self, w: &mut impl Write, node: &Node) -> Result<(), std::io::Error> {
        let prefix = "    ";
        match node {
            Node::EmptyLine => writeln!(w),
            Node::Comment(text) => {
                writeln!(w, ";{}", text)
            }
            Node::Transaction(tx) => {
                write!(w, "{} ", tx.date.format("%Y/%m/%d"))?;
                if tx.cleared {
                    write!(w, "* ")?;
                }
                writeln!(w, "{}", tx.payee)?;
                for n in tx.notes.iter() {
                    write!(w, "{}", &prefix)?;
                    writeln!(w, "; {}", n)?;
                }
                for p in tx.postings.iter() {
                    write!(w, "{}", &prefix)?;
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
                    write!(w, "{}", &prefix)?;
                    writeln!(w, "; {}", n)?;
                }
                for p in tx.postings.iter() {
                    write!(w, "{}", &prefix)?;
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
            Node::Include(including) => {
                writeln!(w, "!include {}", including)
            }
            Node::Included(including, children) => {
                if self.recursive {
                    writeln!(w, "; {}", including)?;

                    self.write_nodes(w, children.iter())
                } else {
                    writeln!(w, "!include {}", including)
                }
            }
            Node::AccountDeclaration(ap) => writeln!(w, "account {}", ap.as_str()),
            Node::TagDeclaration(tag) => writeln!(w, "tag {}", tag),
            Node::DefaultCommodity(symbol) => writeln!(w, "D {}1000.00", symbol),
            Node::CommodityDeclaration(symbol) => writeln!(w, "commodity {}", symbol),
            _ => Ok(()),
        }
    }

    fn write_nodes<'a>(
        &self,
        w: &mut impl Write,
        iter: impl Iterator<Item = &'a Node>,
    ) -> Result<(), std::io::Error> {
        for node in sort_nodes(
            sortable_nodes(iter)
                .filter(|sn| match self.before {
                    Some(before) => naive_to_pacific(sn.date().clone()).unwrap() < before,
                    None => true,
                })
                .filter(|sn| match self.after {
                    Some(after) => {
                        if *sn.date() == NaiveDate::MIN {
                            true
                        } else {
                            naive_to_pacific(sn.date().clone()).unwrap() > after
                        }
                    }
                    None => true,
                }),
        ) {
            self.write_node(w, node)?;
        }

        Ok(())
    }
}

pub fn execute_command(file: &LedgerFile, cmd: &Command) -> anyhow::Result<()> {
    let mut writer = std::io::stdout();
    let printer = Printer::from(cmd)?;
    Ok(printer.write_nodes(&mut writer, file.nodes_iter())?)
}
