use clap::Args;

use crate::model::*;

#[derive(Debug, Args)]
pub struct Command {}

pub fn execute_command(file: &LedgerFile, _cmd: &Command) -> anyhow::Result<()> {
    let sorted = file.sorted_nodes_iter().collect::<Vec<_>>();

    fn print_expression(expression: &Expression) -> String {
        match expression {
            Expression::Literal(numeric) => {
                format!("{}", numeric.to_text_format("$"))
            }
            Expression::Commodity(c) => match c {
                (quantity, symbol, Some(price)) => format!(
                    "{} {} @ {}",
                    quantity.to_text_format_raw(),
                    symbol,
                    price.to_text_format("$")
                ),
                (quantity, symbol, None) => {
                    format!("{} {}", quantity.to_text_format_raw(), symbol)
                }
            },
            Expression::Factor(n) => match &n {
                (true, i) => format!("(-{})", i),
                (false, i) => format!("({})", i),
            },
            Expression::Calculated(_) => "".to_owned(),
        }
    }

    for node in sorted {
        match node {
            Node::Comment(text) => {
                println!(";{}", text)
            }
            Node::Transaction(tx) => {
                print!("{} ", tx.date.format("%Y/%m/%d").to_string());
                if tx.cleared {
                    print!("* ")
                }
                println!("{}", tx.payee);
                for n in tx.notes.iter() {
                    print!("    ");
                    println!("; {}", n)
                }
                for p in tx.postings.iter() {
                    print!("    ");
                    match &p.expression {
                        Some(Expression::Calculated(_)) | None => {
                            print!(
                                "{}",
                                match &p.account {
                                    AccountPath::Real(name) => name.to_string(),
                                    AccountPath::Virtual(name) => format!("[{}]", name),
                                }
                            );
                        }
                        Some(expression) => {
                            print!(
                                "{:76}",
                                match &p.account {
                                    AccountPath::Real(name) => name.to_string(),
                                    AccountPath::Virtual(name) => format!("[{}]", name),
                                }
                            );
                            print!("{:>20}", print_expression(expression));
                        }
                    }

                    match &p.note {
                        Some(note) => println!(" ; {}", note),
                        None => println!(),
                    }
                }
            }
            Node::AccountDeclaration(ap) => println!("account {}", ap.as_str()),
            Node::TagDeclaration(tag) => println!("tag {}", tag),
            Node::Include(including) => println!("!include {}", including),
            Node::Included(including, _) => println!("!include {}", including),
            Node::Generated(_) => {}
            Node::DefaultCommodity(symbol) => println!("D {}1000.00", symbol),
            Node::CommodityPrice(CommodityPrice {
                date,
                symbol,
                expression,
            }) => println!(
                "P {} {} {}",
                date.format("%Y/%m/%d").to_string(),
                symbol,
                print_expression(expression)
            ),
            Node::CommodityDeclaration(symbol) => println!("commodity {}", symbol),
            Node::AutomaticTransaction(tx) => {
                println!("= {}", tx.condition);
                for n in tx.notes.iter() {
                    print!("    ");
                    println!("; {}", n)
                }
                for p in tx.postings.iter() {
                    print!("    ");
                    print!(
                        "{:76}",
                        match &p.account {
                            AccountPath::Real(name) => name.to_string(),
                            AccountPath::Virtual(name) => format!("[{}]", name),
                        }
                    );
                    print!(
                        "{:>20}",
                        match &p.expression {
                            Some(expression) => print_expression(expression),
                            None => "".to_owned(),
                        }
                    );
                    match &p.note {
                        Some(note) => println!(" ; {}", note),
                        None => println!(),
                    }
                }
            }
            Node::EmptyLine => println!(),
        }
    }

    Ok(())
}
