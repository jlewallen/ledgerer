use anyhow::Result;
#[allow(unused_imports)]
use bigdecimal::{BigDecimal, Zero};
use chrono::{NaiveDateTime, NaiveTime, Utc};
use chrono_tz::US::Pacific;
use clap::{arg, Parser, Subcommand};
use itertools::Itertools;
use std::{collections::HashMap, path::PathBuf, time::Instant};
#[allow(unused_imports)]
use tracing::*;
use tracing_subscriber::prelude::*;

use crate::ledger::parsing::LedgerFile;

#[derive(Parser)]
#[command(author, version, about, long_about = None)]
struct Cli {
    #[arg(short, long, value_name = "FILE")]
    path: PathBuf,
    #[arg(short, long, action = clap::ArgAction::Count)]
    verbose: u8,
    #[command(subcommand)]
    command: Option<Commands>,
}

#[derive(Subcommand)]
enum Commands {
    Json,
    Balances { prefix: Option<String> },
}

fn main() -> Result<()> {
    fn get_rust_log() -> String {
        std::env::var("RUST_LOG").unwrap_or_else(|_| "info".into())
    }

    tracing_subscriber::registry()
        .with(tracing_subscriber::EnvFilter::new(get_rust_log()))
        .with(tracing_subscriber::fmt::layer().with_writer(std::io::stderr))
        .init();

    let cli = Cli::parse();

    let get_processed = || -> Result<LedgerFile> {
        let _span = span!(Level::INFO, "loading").entered();
        let started = Instant::now();
        let file = ledger::parsing::LedgerFile::parse(&cli.path)?;
        let loaded = file.preprocess()?.apply_automatic_transactions()?;
        let elapsed = Instant::now() - started;
        info!("loaded ledger in {:?}ms", elapsed);
        Ok(loaded)
    };

    match &cli.command {
        Some(Commands::Json) => {
            let processed = get_processed()?;

            let sorted = processed
                .iter_transactions_in_temporal_order()
                .filter(|t| t.is_simple())
                .collect::<Vec<_>>();

            println!("{}", serde_json::to_string(&sorted)?);

            Ok(())
        }
        Some(Commands::Balances { prefix }) => {
            let processed = get_processed()?;

            let sorted = processed
                .iter_transactions_in_temporal_order()
                .filter(|t| t.is_simple())
                .collect::<Vec<_>>();

            let mut accounts: HashMap<String, BigDecimal> = HashMap::new();

            for tx in sorted.iter() {
                let and_time = NaiveDateTime::new(tx.date, NaiveTime::MIN);
                let pacific = and_time.and_local_timezone(Pacific).single().unwrap();
                if pacific > Utc::now() {
                    continue;
                }

                for posting in tx.postings.iter() {
                    match posting.has_value() {
                        Some(value) => {
                            let account = posting.account.as_str();
                            let including = if let Some(prefix) = prefix {
                                account.starts_with(prefix)
                            } else {
                                true
                            };
                            if including {
                                // println!("{} {:?} {} {}", tx.date, account, tx.payee, value);
                                if !accounts.contains_key(account) {
                                    accounts.insert(account.to_owned(), value);
                                } else {
                                    *accounts.get_mut(account).unwrap() += value
                                }
                            }
                        }
                        None => {}
                    }
                }
            }

            if let Some(max_key_len) = accounts.keys().map(|k| k.len()).max() {
                for key in accounts.keys().sorted() {
                    let value = accounts.get(key).unwrap();
                    if !value.is_zero() {
                        println!("{:width$} {:>10}", key, value, width = max_key_len);
                    }
                }
            }

            Ok(())
        }
        _ => Ok(()),
    }
}

pub mod ledger {
    pub mod parsing {
        use std::{
            fs,
            path::{Path, PathBuf},
            sync::atomic::AtomicU64,
            time::Instant,
        };

        use anyhow::{anyhow, Result};

        use bigdecimal::{BigDecimal, Zero};
        use chrono::NaiveDate;
        use itertools::Itertools;
        use nom::{
            branch::alt,
            bytes::complete::{tag, take_while1},
            character::{
                self,
                complete::{alpha1, digit1, multispace0, newline},
            },
            combinator::{map, map_res, opt, recognize},
            error::ParseError,
            multi::{many0, many1},
            sequence::{delimited, pair, preceded, separated_pair, terminated, tuple},
            IResult,
        };

        use serde::{ser::SerializeStruct, Serialize};
        use tracing::*;

        #[derive(Debug, PartialEq, Clone)]
        pub enum AccountPath {
            Real(String),
            Virtual(String),
        }

        impl AccountPath {
            pub fn as_str(&self) -> &str {
                match self {
                    AccountPath::Real(p) | AccountPath::Virtual(p) => p,
                }
            }
        }

        #[derive(Debug, PartialEq)]
        pub enum Numeric {
            Negative(u64, u64),
            Positive(u64, u64),
        }

        impl Numeric {
            pub fn to_decimal(&self) -> BigDecimal {
                match self {
                    Numeric::Negative(a, b) => {
                        BigDecimal::new((-(*a as i64 * 100 + *b as i64)).into(), 2)
                    }
                    Numeric::Positive(a, b) => {
                        BigDecimal::new((*a as i64 * 100 + *b as i64).into(), 2)
                    }
                }
            }
        }

        #[derive(Debug, PartialEq)]
        pub enum Expression {
            Literal(Numeric),
            Commodity((Numeric, String, Option<Numeric>)),
            Factor((bool, u64)),
            Calculated(BigDecimal),
        }

        #[derive(Debug, PartialEq)]
        pub struct Transaction {
            pub date: NaiveDate,
            pub payee: String,
            pub cleared: bool,
            pub notes: Vec<String>,
            pub postings: Vec<Posting>,
            pub mid: Option<String>,
        }

        impl Serialize for Transaction {
            fn serialize<S>(&self, serializer: S) -> std::result::Result<S::Ok, S::Error>
            where
                S: serde::Serializer,
            {
                let mut state = serializer.serialize_struct("Transaction", 6)?;
                state.serialize_field("date", &format!("{:?}T00:00:00", &self.date))?; // TODO
                state.serialize_field("payee", &self.payee)?;
                state.serialize_field("cleared", &self.cleared)?;
                state.serialize_field("mid", &self.mid)?;
                state.serialize_field("notes", &self.notes)?;
                state.serialize_field("postings", &self.postings)?;
                state.end()
            }
        }

        impl Transaction {
            pub fn is_simple(&self) -> bool {
                !self.postings.iter().any(|p| p.has_value().is_none())
            }

            pub fn has_posting_for(&self, name: &str) -> bool {
                self.postings.iter().any(|p| p.account.as_str() == name)
            }

            pub fn iter_postings_for<'a>(
                &'a self,
                name: &'a str,
            ) -> impl Iterator<Item = &Posting> + 'a {
                self.postings
                    .iter()
                    .filter(move |p| p.account.as_str() == name)
            }

            fn into_balanced(self) -> Result<Self> {
                if self.postings.iter().any(|p| p.expression.is_none()) {
                    let total: BigDecimal = self
                        .postings
                        .iter()
                        .map(|p| p.has_value().or(Some(BigDecimal::zero())).unwrap())
                        .collect::<Vec<_>>()
                        .into_iter()
                        .sum();

                    let mut postings = self.postings;

                    for mut posting in postings.iter_mut() {
                        if posting.expression.is_none() {
                            posting.expression = Some(Expression::Calculated(-total.clone()))
                        }
                    }

                    Ok(Self {
                        date: self.date,
                        payee: self.payee,
                        cleared: self.cleared,
                        mid: self.mid,
                        notes: self.notes,
                        postings,
                    })
                } else {
                    Ok(self)
                }
            }

            fn into_with_mid(self, mid: String) -> Self {
                Self {
                    date: self.date,
                    payee: self.payee,
                    cleared: self.cleared,
                    notes: self.notes,
                    postings: self.postings,
                    mid: Some(mid),
                }
            }
        }

        #[derive(Debug, PartialEq)]
        pub struct Posting {
            pub account: AccountPath,
            pub expression: Option<Expression>,
            pub note: Option<String>,
        }

        impl Posting {
            pub fn has_expression(&self) -> bool {
                match &self.expression {
                    None => false,
                    _ => true,
                }
            }

            pub fn has_literal(&self) -> Option<&Numeric> {
                match &self.expression {
                    Some(Expression::Literal(numeric)) => Some(numeric),
                    _ => None,
                }
            }

            pub fn has_value(&self) -> Option<BigDecimal> {
                match &self.expression {
                    Some(Expression::Literal(numeric)) => Some(numeric.to_decimal()),
                    Some(Expression::Calculated(value)) => Some(value.clone()),
                    _ => None,
                }
            }
        }

        impl Serialize for Posting {
            fn serialize<S>(&self, serializer: S) -> std::result::Result<S::Ok, S::Error>
            where
                S: serde::Serializer,
            {
                let value = match self.has_value() {
                    Some(value) => Some(format!("{}", value)),
                    None => None,
                };
                let mut state = serializer.serialize_struct("Posting", 3)?;
                state.serialize_field("account", self.account.as_str())?;
                state.serialize_field("value", &value)?;
                state.serialize_field("note", &self.note)?;
                state.end()
            }
        }

        #[derive(Debug, PartialEq)]
        pub struct AutomaticTransaction {
            pub condition: String,
            pub notes: Vec<String>,
            pub postings: Vec<Posting>,
        }

        impl AutomaticTransaction {
            pub fn apply(&self, tx: &Transaction) -> Vec<Transaction> {
                tx.iter_postings_for(&self.condition)
                    .map(|p| match p.has_value() {
                        Some(value) => Transaction {
                            date: tx.date,
                            payee: tx.payee.clone(),
                            cleared: tx.cleared,
                            notes: self.notes.clone(),
                            postings: self.postings_with_value(value),
                            mid: None,
                        },
                        None => todo!(),
                    })
                    .collect_vec()
            }

            fn postings_with_value(&self, value: BigDecimal) -> Vec<Posting> {
                self.postings
                    .iter()
                    .map(|p| match p.expression {
                        Some(Expression::Factor((negative, 1))) => Posting {
                            account: p.account.clone(),
                            expression: if negative {
                                Some(Expression::Calculated(-value.clone()))
                            } else {
                                Some(Expression::Calculated(value.clone()))
                            },
                            note: p.note.clone(),
                        },
                        _ => todo!(),
                    })
                    .collect_vec()
            }
        }

        #[derive(Debug, PartialEq)]
        pub struct CommodityPrice {
            pub date: NaiveDate,
            pub symbol: String,
            pub expression: Expression,
        }

        #[derive(Debug, PartialEq)]
        pub enum Node {
            Comment(String),
            Transaction(Transaction),
            AccountDeclaration(AccountPath),
            TagDeclaration(String),
            Include(String),
            Included(Vec<Node>),
            Generated(Vec<Node>),
            DefaultCommodity(String),
            CommodityPrice(CommodityPrice),
            CommodityDeclaration(String),
            AutomaticTransaction(AutomaticTransaction),
        }

        fn unsigned_number(i: &str) -> IResult<&str, u64> {
            map_res(recognize(digit1), str::parse)(i)
        }

        fn parse_automatic_transaction(i: &str) -> IResult<&str, Node> {
            map(
                pair(
                    preceded(
                        tag("="),
                        preceded(linespace1, terminated(account_path_string, newline)),
                    ),
                    pair(
                        many0(preceded(ws(tag(";")), terminated(parse_note, newline))),
                        many1(delimited(
                            linespace1,
                            parse_posting,
                            preceded(opt(linespace1), newline),
                        )),
                    ),
                ),
                |(path, (notes, postings))| {
                    Node::AutomaticTransaction(AutomaticTransaction {
                        condition: path.into(),
                        notes: notes.iter().map(|n| n.to_string()).collect::<Vec<_>>(),
                        postings,
                    })
                },
            )(i)
        }

        fn parse_commodity_price(i: &str) -> IResult<&str, Node> {
            map(
                separated_pair(
                    preceded(tuple((tag("P"), linespace1)), date_string),
                    linespace1,
                    separated_pair(symbol, linespace1, expression),
                ),
                |(date, (symbol, expression))| {
                    Node::CommodityPrice(CommodityPrice {
                        date,
                        symbol: symbol.into(),
                        expression,
                    })
                },
            )(i)
        }

        fn symbol(i: &str) -> IResult<&str, &str> {
            recognize(many1(alpha1))(i)
        }

        fn date_string(i: &str) -> IResult<&str, NaiveDate> {
            map(
                separated_pair(
                    separated_pair(unsigned_number, tag("/"), unsigned_number),
                    tag("/"),
                    unsigned_number,
                ),
                |((year, month), day)| {
                    NaiveDate::from_ymd_opt(year as i32, month as u32, day as u32)
                        .expect("Invalid date")
                },
            )(i)
        }

        fn payee(i: &str) -> IResult<&str, &str> {
            take_while1(move |c: char| !character::is_newline(c as u8))(i)
        }

        fn parse_note(i: &str) -> IResult<&str, &str> {
            take_while1(move |c: char| !character::is_newline(c as u8))(i)
        }

        // TODO This needs tons of work
        fn account_path_string(i: &str) -> IResult<&str, &str> {
            take_while1(move |c| {
                ":abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ-".contains(c)
            })(i)
        }

        fn account_path(i: &str) -> IResult<&str, AccountPath> {
            alt((
                map(delimited(tag("["), account_path_string, tag("]")), |p| {
                    AccountPath::Virtual(p.into())
                }),
                map(account_path_string, |p| AccountPath::Real(p.into())),
            ))(i)
        }

        fn parse_transaction(i: &str) -> IResult<&str, Node> {
            map(
                pair(
                    tuple((
                        date_string,
                        opt(preceded(linespace1, tag("*"))),
                        preceded(linespace1, terminated(payee, newline)),
                    )),
                    pair(
                        many0(preceded(ws(tag(";")), terminated(parse_note, newline))),
                        many1(delimited(
                            linespace1,
                            parse_posting,
                            preceded(opt(linespace1), newline),
                        )),
                    ),
                ),
                |((date, cleared, payee), (notes, postings))| {
                    Node::Transaction(Transaction {
                        date,
                        payee: payee.to_string(),
                        cleared: cleared.is_some(),
                        mid: None,
                        notes: notes.iter().map(|n| n.to_string()).collect::<Vec<_>>(),
                        postings,
                    })
                },
            )(i)
        }

        fn numeric_literal(i: &str) -> IResult<&str, Numeric> {
            map(
                separated_pair(
                    opt(tag("-")),
                    opt(tag("$")),
                    pair(
                        opt(tag("-")),
                        pair(unsigned_number, opt(preceded(tag("."), unsigned_number))),
                    ),
                ),
                |(sign1, (sign2, factors))| {
                    if sign1.is_some() || sign2.is_some() {
                        Numeric::Negative(factors.0, factors.1.map_or(0, |f| f))
                    } else {
                        Numeric::Positive(factors.0, factors.1.map_or(0, |f| f))
                    }
                },
            )(i)
        }

        fn linespace1(i: &str) -> IResult<&str, &str> {
            take_while1(move |c| " \t".contains(c))(i)
        }

        fn basic_commodity(i: &str) -> IResult<&str, Expression> {
            map(
                tuple((
                    numeric_literal,
                    preceded(linespace1, symbol),
                    opt(preceded(
                        tuple((linespace1, tag("@"), linespace1)),
                        numeric_literal,
                    )),
                )),
                |(quantity, symbol, price)| Expression::Commodity((quantity, symbol.into(), price)),
            )(i)
        }

        fn fractional(i: &str) -> IResult<&str, Expression> {
            map(
                delimited(tag("("), pair(opt(tag("-")), unsigned_number), tag(")")),
                |(sign, factor)| Expression::Factor((sign.is_some(), factor)),
            )(i)
        }

        fn expression(i: &str) -> IResult<&str, Expression> {
            alt((
                basic_commodity,
                map(numeric_literal, Expression::Literal),
                fractional,
            ))(i)
        }

        fn parse_posting(i: &str) -> IResult<&str, Posting> {
            map(
                pair(
                    account_path,
                    pair(
                        opt(preceded(linespace1, expression)),
                        opt(preceded(
                            tuple((linespace1, tag(";"), linespace1)),
                            parse_note,
                        )),
                    ),
                ),
                |(account, (expression, note))| Posting {
                    account,
                    expression,
                    note: note.map(|f| f.into()),
                },
            )(i)
        }

        fn parse_default_commodity(i: &str) -> IResult<&str, Node> {
            map(
                separated_pair(
                    tag("D"),
                    linespace1,
                    pair(
                        tag("$"),
                        separated_pair(unsigned_number, tag("."), unsigned_number),
                    ),
                ),
                |_| Node::DefaultCommodity("$".into()),
            )(i)
        }

        // TODO This needs tons of work
        fn file_path(i: &str) -> IResult<&str, &str> {
            take_while1(move |c| {
                "/abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ.123456789-_*".contains(c)
            })(i)
        }

        fn parse_include(i: &str) -> IResult<&str, Node> {
            map(
                preceded(tuple((tag("!include"), linespace1)), file_path),
                |path| Node::Include(path.into()),
            )(i)
        }

        fn parse_account_declaration(i: &str) -> IResult<&str, Node> {
            map(
                preceded(tuple((tag("account"), linespace1)), account_path),
                |path| Node::AccountDeclaration(path),
            )(i)
        }

        fn identifier(i: &str) -> IResult<&str, &str> {
            take_while1(move |c| {
                "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ.123456789-_".contains(c)
            })(i)
        }

        fn parse_tag_declaration(i: &str) -> IResult<&str, Node> {
            map(
                preceded(tuple((tag("tag"), linespace1)), identifier),
                |path| Node::TagDeclaration(path.into()),
            )(i)
        }

        fn parse_commodity_declaration(i: &str) -> IResult<&str, Node> {
            map(
                preceded(tuple((tag("commodity"), linespace1)), symbol),
                |symbol| Node::CommodityDeclaration(symbol.into()),
            )(i)
        }

        fn remaining_text(i: &str) -> IResult<&str, &str> {
            take_while1(move |c: char| !character::is_newline(c as u8))(i)
        }

        fn parse_comment(i: &str) -> IResult<&str, Node> {
            map(
                preceded(
                    alt((tag(";"), tag("#"))),
                    terminated(remaining_text, newline),
                ),
                |text| Node::Comment(text.into()),
            )(i)
        }

        /// A combinator that takes a parser `inner` and produces a parser that also consumes both leading and
        /// trailing whitespace, returning the output of `inner`.
        fn ws<'a, F: 'a, O, E: ParseError<&'a str>>(
            inner: F,
        ) -> impl FnMut(&'a str) -> IResult<&'a str, O, E>
        where
            F: Fn(&'a str) -> IResult<&'a str, O, E>,
        {
            delimited(multispace0, inner, multispace0)
        }

        fn parse_directive(i: &str) -> IResult<&str, Node> {
            alt((
                parse_transaction,
                parse_comment,
                parse_account_declaration,
                parse_tag_declaration,
                parse_include,
                parse_default_commodity,
                parse_commodity_price,
                parse_commodity_declaration,
                parse_automatic_transaction,
            ))(i)
        }

        pub fn parse_str(i: &str) -> Result<Vec<Node>> {
            let (remaining, nodes) =
                many0(ws(parse_directive))(i).map_err(|e| anyhow!("{:?}", e))?;

            assert_eq!(remaining, "");

            Ok(nodes)
        }

        #[derive(Debug)]
        pub struct LedgerFile {
            path: PathBuf,
            nodes: Vec<Node>,
        }

        impl LedgerFile {
            pub fn parse(path: &Path) -> Result<Self> {
                info!("parsing {:?}", path);

                let data = fs::read_to_string(path)?;
                let nodes = parse_str(&data)?;

                Ok(LedgerFile {
                    path: path.to_owned(),
                    nodes,
                })
            }

            pub fn name(&self) -> Option<&str> {
                match self.path.file_name() {
                    Some(name) => name.to_str().into(),
                    None => None,
                }
            }

            pub fn apply_automatic_transactions(self) -> Result<LedgerFile> {
                let _span = span!(Level::INFO, "auto-txs").entered();
                let started = Instant::now();

                let automatics: Vec<&AutomaticTransaction> =
                    self.iter_automatic_transactions().collect_vec();
                let generated: Vec<Transaction> = self
                    .iter_transactions()
                    .flat_map(|tx| automatics.iter().flat_map(|automatic| automatic.apply(tx)))
                    .collect_vec();

                let nodes = self
                    .nodes
                    .into_iter()
                    .chain(vec![Node::Generated(
                        generated
                            .into_iter()
                            .map(|tx| Node::Transaction(tx))
                            .collect_vec(),
                    )])
                    .collect_vec();

                let elapsed = Instant::now() - started;
                info!("done in {:?}ms", elapsed);

                Ok(Self {
                    path: self.path.clone(),
                    nodes,
                })
            }

            pub fn preprocess(self) -> Result<LedgerFile> {
                debug!("preprocessing {:?}", self.path);

                let relative = self
                    .path
                    .parent()
                    .ok_or(anyhow!("Expected parent directory: {:?}", self.path))?;

                let name = self
                    .name()
                    .map_or(Err(anyhow!("Unfriendly path")), |f| Ok(f))?
                    .split(".")
                    .next()
                    .map_or(Err(anyhow!("Expected extension on path")), |f| Ok(f))?
                    .to_ascii_uppercase();

                let counter = AtomicU64::new(1);
                let new_mid = move || {
                    format!(
                        "{}-{}",
                        name,
                        counter.fetch_add(1, std::sync::atomic::Ordering::SeqCst)
                    )
                };

                let nodes = self
                    .nodes
                    .into_iter()
                    .map(|node| match node {
                        Node::Transaction(tx) => Ok(Node::Transaction(
                            tx.into_with_mid(new_mid()).into_balanced()?,
                        )),
                        Node::Include(include_path_or_glob) => Ok(Node::Included(include_glob(
                            &relative
                                .join(include_path_or_glob)
                                .to_str()
                                .ok_or(anyhow!("Unfriendly path"))?,
                        )?)),
                        _ => Ok(node),
                    })
                    .collect::<Result<Vec<_>>>()?;

                Ok(Self {
                    path: self.path,
                    nodes,
                })
            }

            pub fn iter_transactions_in_temporal_order(
                &self,
            ) -> impl Iterator<Item = &Transaction> {
                let mut everything: Vec<&Transaction> = self.iter_transactions().collect();
                everything.sort_by(|a, b| a.date.cmp(&b.date));
                everything.into_iter()
            }

            pub fn iter_transactions(&self) -> impl Iterator<Item = &Transaction> {
                fn recursively_iter_txs(
                    nodes: &Vec<Node>,
                ) -> Box<dyn Iterator<Item = &Transaction> + '_> {
                    Box::new(nodes.iter().flat_map(|node| match node {
                        Node::Transaction(tx) => Box::new(std::iter::once(tx)),
                        Node::Included(children) | Node::Generated(children) => {
                            recursively_iter_txs(children)
                        }
                        _ => Box::new(std::iter::empty::<&Transaction>()),
                    }))
                }

                recursively_iter_txs(&self.nodes)
            }

            pub fn iter_automatic_transactions(
                &self,
            ) -> impl Iterator<Item = &AutomaticTransaction> {
                fn recursively_iter_txs(
                    nodes: &Vec<Node>,
                ) -> Box<dyn Iterator<Item = &AutomaticTransaction> + '_> {
                    Box::new(nodes.iter().flat_map(|node| match node {
                        Node::AutomaticTransaction(tx) => Box::new(std::iter::once(tx)),
                        Node::Included(children) | Node::Generated(children) => {
                            recursively_iter_txs(children)
                        }
                        _ => Box::new(std::iter::empty::<&AutomaticTransaction>()),
                    }))
                }

                recursively_iter_txs(&self.nodes)
            }
        }

        fn include_glob(path: &str) -> Result<Vec<Node>> {
            use glob::glob;

            let files = glob(path)?
                .map(|entry| Ok(LedgerFile::parse(&entry?)?.preprocess()?))
                .collect::<Result<Vec<_>>>()?;

            let nodes = files
                .into_iter()
                .map(|file| Node::Included(file.nodes))
                .collect();

            Ok(nodes)
        }

        #[cfg(test)]
        mod tests {
            use super::*;

            #[test]
            fn test_parse_empty() -> Result<()> {
                assert_eq!(parse_str(r"")?, vec![]);

                Ok(())
            }

            #[test]
            fn test_parse_default_commodity() -> Result<()> {
                assert_eq!(
                    parse_str(r"D $1000.00")?,
                    vec![Node::DefaultCommodity("$".into())]
                );

                Ok(())
            }

            #[test]
            fn test_parse_price() -> Result<()> {
                assert_eq!(
                    parse_str(r"P 2021/1/29 BS $1000.00")?,
                    vec![Node::CommodityPrice(CommodityPrice {
                        date: NaiveDate::from_ymd_opt(2021, 1, 29).expect("inline date error"),
                        symbol: "BS".into(),
                        expression: Expression::Literal(Numeric::Positive(1000, 0))
                    })]
                );

                Ok(())
            }

            #[test]
            fn test_parse_include() -> Result<()> {
                assert_eq!(
                    parse_str(r"!include checking.ledger")?,
                    vec![Node::Include("checking.ledger".into())]
                );

                Ok(())
            }

            #[test]
            fn test_parse_include_wildcard() -> Result<()> {
                assert_eq!(
                    parse_str(r"!include cards/*.ledger")?,
                    vec![Node::Include("cards/*.ledger".into())]
                );

                Ok(())
            }

            #[test]
            fn test_parse_include_with_whitespace() -> Result<()> {
                assert_eq!(
                    parse_str(
                        r"
!include checking.ledger
"
                    )?,
                    vec![Node::Include("checking.ledger".into()),]
                );

                Ok(())
            }

            #[test]
            fn test_parse_transaction_basic() -> Result<()> {
                assert_eq!(
                    parse_str(
                        r"
2023/04/09 withdrawl
    assets:cash            $100.00
    assets:checking       -$100.00
"
                    )?,
                    vec![Node::Transaction(Transaction {
                        date: NaiveDate::from_ymd_opt(2023, 4, 9).unwrap(),
                        payee: "withdrawl".into(),
                        cleared: false,
                        mid: None,
                        notes: Vec::new(),
                        postings: vec![
                            Posting {
                                account: AccountPath::Real("assets:cash".into()),
                                expression: Some(Expression::Literal(Numeric::Positive(100, 0))),
                                note: None,
                            },
                            Posting {
                                account: AccountPath::Real("assets:checking".into()),
                                expression: Some(Expression::Literal(Numeric::Negative(100, 0))),
                                note: None,
                            },
                        ]
                    })]
                );

                Ok(())
            }

            #[test]
            fn test_parse_transaction_basic_twice() -> Result<()> {
                assert_eq!(
                    parse_str(
                        r"
2023/04/09 withdrawl 1
    assets:cash            $100.00
    assets:checking       -$100.00

2023/04/10 withdrawl 2
    assets:cash            $100.00
    assets:checking       -$100.00
"
                    )?,
                    vec![
                        Node::Transaction(Transaction {
                            date: NaiveDate::from_ymd_opt(2023, 4, 9).unwrap(),
                            payee: "withdrawl 1".into(),
                            cleared: false,
                            mid: None,
                            notes: Vec::new(),
                            postings: vec![
                                Posting {
                                    account: AccountPath::Real("assets:cash".into()),
                                    expression: Some(Expression::Literal(Numeric::Positive(
                                        100, 0
                                    ))),
                                    note: None,
                                },
                                Posting {
                                    account: AccountPath::Real("assets:checking".into()),
                                    expression: Some(Expression::Literal(Numeric::Negative(
                                        100, 0
                                    ))),
                                    note: None,
                                },
                            ]
                        }),
                        Node::Transaction(Transaction {
                            date: NaiveDate::from_ymd_opt(2023, 4, 10).unwrap(),
                            payee: "withdrawl 2".into(),
                            cleared: false,
                            mid: None,
                            notes: Vec::new(),
                            postings: vec![
                                Posting {
                                    account: AccountPath::Real("assets:cash".into()),
                                    expression: Some(Expression::Literal(Numeric::Positive(
                                        100, 0
                                    ))),
                                    note: None,
                                },
                                Posting {
                                    account: AccountPath::Real("assets:checking".into()),
                                    expression: Some(Expression::Literal(Numeric::Negative(
                                        100, 0
                                    ))),
                                    note: None,
                                },
                            ]
                        }),
                    ]
                );

                Ok(())
            }

            #[test]
            fn test_parse_transaction_basic_with_whitespace_after_posting() -> Result<()> {
                assert_eq!(
                    parse_str(
                        r"
2023/04/09 income
    income                      -$100.00    
    assets:checking              $100.00
"
                    )?,
                    vec![Node::Transaction(Transaction {
                        date: NaiveDate::from_ymd_opt(2023, 4, 9).unwrap(),
                        payee: "income".into(),
                        cleared: false,
                        mid: None,
                        notes: Vec::new(),
                        postings: vec![
                            Posting {
                                account: AccountPath::Real("income".into()),
                                expression: Some(Expression::Literal(Numeric::Negative(100, 0))),
                                note: None,
                            },
                            Posting {
                                account: AccountPath::Real("assets:checking".into()),
                                expression: Some(Expression::Literal(Numeric::Positive(100, 0))),
                                note: None,
                            },
                        ]
                    })]
                );

                Ok(())
            }

            #[test]
            fn test_parse_transaction_basic_with_virtual() -> Result<()> {
                assert_eq!(
                    parse_str(
                        r"
2023/04/09 income
    income                      -$100.00
    assets:checking              $100.00
    [assets:checking:reserved]  -$100.00
    [allocations:savings]        $100.00
"
                    )?,
                    vec![Node::Transaction(Transaction {
                        date: NaiveDate::from_ymd_opt(2023, 4, 9).unwrap(),
                        payee: "income".into(),
                        cleared: false,
                        mid: None,
                        notes: Vec::new(),
                        postings: vec![
                            Posting {
                                account: AccountPath::Real("income".into()),
                                expression: Some(Expression::Literal(Numeric::Negative(100, 0))),
                                note: None,
                            },
                            Posting {
                                account: AccountPath::Real("assets:checking".into()),
                                expression: Some(Expression::Literal(Numeric::Positive(100, 0))),
                                note: None,
                            },
                            Posting {
                                account: AccountPath::Virtual("assets:checking:reserved".into()),
                                expression: Some(Expression::Literal(Numeric::Negative(100, 0))),
                                note: None,
                            },
                            Posting {
                                account: AccountPath::Virtual("allocations:savings".into()),
                                expression: Some(Expression::Literal(Numeric::Positive(100, 0))),
                                note: None,
                            },
                        ]
                    }),]
                );

                Ok(())
            }

            #[test]
            fn test_parse_transaction_basic_alternative_sign_location() -> Result<()> {
                assert_eq!(
                    parse_str(
                        r"
2023/04/09 * another example
    assets:cash            $100.00
    assets:checking       $-100.00
"
                    )?,
                    vec![Node::Transaction(Transaction {
                        date: NaiveDate::from_ymd_opt(2023, 4, 9).unwrap(),
                        payee: "another example".into(),
                        notes: Vec::new(),
                        cleared: true,
                        mid: None,
                        postings: vec![
                            Posting {
                                account: AccountPath::Real("assets:cash".into()),
                                expression: Some(Expression::Literal(Numeric::Positive(100, 0))),
                                note: None,
                            },
                            Posting {
                                account: AccountPath::Real("assets:checking".into()),
                                expression: Some(Expression::Literal(Numeric::Negative(100, 0))),
                                note: None,
                            },
                        ]
                    }),]
                );

                Ok(())
            }

            #[test]
            fn test_parse_transaction_basic_cleared() -> Result<()> {
                assert_eq!(
                    parse_str(
                        r"
2023/04/09 * withdrawl with more text
    assets:cash            $100.00
    assets:checking       -$100.00
"
                    )?,
                    vec![Node::Transaction(Transaction {
                        date: NaiveDate::from_ymd_opt(2023, 4, 9).unwrap(),
                        payee: "withdrawl with more text".into(),
                        notes: Vec::new(),
                        cleared: true,
                        mid: None,
                        postings: vec![
                            Posting {
                                account: AccountPath::Real("assets:cash".into()),
                                expression: Some(Expression::Literal(Numeric::Positive(100, 0))),
                                note: None,
                            },
                            Posting {
                                account: AccountPath::Real("assets:checking".into()),
                                expression: Some(Expression::Literal(Numeric::Negative(100, 0))),
                                note: None,
                            },
                        ]
                    }),]
                );

                Ok(())
            }

            #[test]
            fn test_parse_transaction_with_catchall_posting_last() -> Result<()> {
                assert_eq!(
                    parse_str(
                        r"
2023/04/09 withdrawl
    assets:checking       -$100.00
    assets:cash
"
                    )?,
                    vec![Node::Transaction(Transaction {
                        date: NaiveDate::from_ymd_opt(2023, 4, 9).unwrap(),
                        payee: "withdrawl".into(),
                        cleared: false,
                        mid: None,
                        notes: Vec::new(),
                        postings: vec![
                            Posting {
                                account: AccountPath::Real("assets:checking".into()),
                                expression: Some(Expression::Literal(Numeric::Negative(100, 0))),
                                note: None,
                            },
                            Posting {
                                account: AccountPath::Real("assets:cash".into()),
                                expression: None,
                                note: None,
                            },
                        ]
                    }),]
                );

                Ok(())
            }

            #[test]
            fn test_parse_transaction_with_catchall_posting_first() -> Result<()> {
                assert_eq!(
                    parse_str(
                        r"
2023/04/09 withdrawl
    assets:cash
    assets:checking       -$100.00
"
                    )?,
                    vec![Node::Transaction(Transaction {
                        date: NaiveDate::from_ymd_opt(2023, 4, 9).unwrap(),
                        payee: "withdrawl".into(),
                        cleared: false,
                        mid: None,
                        notes: Vec::new(),
                        postings: vec![
                            Posting {
                                account: AccountPath::Real("assets:cash".into()),
                                expression: None,
                                note: None,
                            },
                            Posting {
                                account: AccountPath::Real("assets:checking".into()),
                                expression: Some(Expression::Literal(Numeric::Negative(100, 0))),
                                note: None,
                            },
                        ]
                    }),]
                );

                Ok(())
            }

            #[test]
            fn test_parse_transaction_with_note() -> Result<()> {
                assert_eq!(
                    parse_str(
                        r"
2023/04/09 withdrawl
    ; hello-world
    assets:cash            $100.00
    assets:checking       -$100.00
"
                    )?,
                    vec![Node::Transaction(Transaction {
                        date: NaiveDate::from_ymd_opt(2023, 4, 9).unwrap(),
                        payee: "withdrawl".into(),
                        cleared: false,
                        mid: None,
                        notes: vec!["hello-world".into()],
                        postings: vec![
                            Posting {
                                account: AccountPath::Real("assets:cash".into()),
                                expression: Some(Expression::Literal(Numeric::Positive(100, 0))),
                                note: None,
                            },
                            Posting {
                                account: AccountPath::Real("assets:checking".into()),
                                expression: Some(Expression::Literal(Numeric::Negative(100, 0))),
                                note: None,
                            },
                        ]
                    }),]
                );

                Ok(())
            }

            #[test]
            fn test_parse_transaction_with_posting_with_note() -> Result<()> {
                assert_eq!(
                    parse_str(
                        r"
2023/04/09 withdrawl
    assets:cash            $100.00 ; hello-world
    assets:checking       -$100.00
"
                    )?,
                    vec![Node::Transaction(Transaction {
                        date: NaiveDate::from_ymd_opt(2023, 4, 9).unwrap(),
                        payee: "withdrawl".into(),
                        cleared: false,
                        mid: None,
                        notes: vec![],
                        postings: vec![
                            Posting {
                                account: AccountPath::Real("assets:cash".into()),
                                expression: Some(Expression::Literal(Numeric::Positive(100, 0))),
                                note: Some("hello-world".into()),
                            },
                            Posting {
                                account: AccountPath::Real("assets:checking".into()),
                                expression: Some(Expression::Literal(Numeric::Negative(100, 0))),
                                note: None,
                            },
                        ]
                    }),]
                );

                Ok(())
            }

            #[test]
            fn test_parse_account_declaration() -> Result<()> {
                assert_eq!(
                    parse_str(r"account expenses:cash")?,
                    vec![Node::AccountDeclaration(AccountPath::Real(
                        "expenses:cash".into()
                    ),)]
                );

                Ok(())
            }

            #[test]
            fn test_parse_account_declaration_multiple() -> Result<()> {
                assert_eq!(
                    parse_str(
                        r"
account expenses:cash
account expenses:food
account [allocations:checking]
"
                    )?,
                    vec![
                        Node::AccountDeclaration(AccountPath::Real("expenses:cash".into())),
                        Node::AccountDeclaration(AccountPath::Real("expenses:food".into())),
                        Node::AccountDeclaration(AccountPath::Virtual(
                            "allocations:checking".into()
                        )),
                    ]
                );

                Ok(())
            }

            #[test]
            fn test_parse_virtual_account_declaration() -> Result<()> {
                assert_eq!(
                    parse_str(r"account [expenses:cash]")?,
                    vec![Node::AccountDeclaration(AccountPath::Virtual(
                        "expenses:cash".into()
                    ),)]
                );

                Ok(())
            }

            #[test]
            fn test_parse_tag_declaration() -> Result<()> {
                assert_eq!(
                    parse_str(r"tag income")?,
                    vec![Node::TagDeclaration("income".into())]
                );

                Ok(())
            }

            #[test]
            fn test_parse_commodity_declaration() -> Result<()> {
                assert_eq!(
                    parse_str(r"commodity VTI")?,
                    vec![Node::CommodityDeclaration("VTI".into())]
                );

                Ok(())
            }

            #[test]
            fn test_parse_transaction_with_mixed_commodities() -> Result<()> {
                assert_eq!(
                    parse_str(
                        r"
2023/04/09 opening
    assets:cash            $100.00
    assets:fake             100.00 BS
    equity:opening
"
                    )?,
                    vec![Node::Transaction(Transaction {
                        date: NaiveDate::from_ymd_opt(2023, 4, 9).unwrap(),
                        payee: "opening".into(),
                        cleared: false,
                        mid: None,
                        notes: vec![],
                        postings: vec![
                            Posting {
                                account: AccountPath::Real("assets:cash".into()),
                                expression: Some(Expression::Literal(Numeric::Positive(100, 0))),
                                note: None,
                            },
                            Posting {
                                account: AccountPath::Real("assets:fake".into()),
                                expression: Some(Expression::Commodity((
                                    Numeric::Positive(100, 0),
                                    "BS".into(),
                                    None,
                                ))),
                                note: None,
                            },
                            Posting {
                                account: AccountPath::Real("equity:opening".into()),
                                expression: None,
                                note: None,
                            },
                        ]
                    }),]
                );

                Ok(())
            }

            #[test]
            fn test_parse_transaction_with_priced_commodity() -> Result<()> {
                assert_eq!(
                    parse_str(
                        r"
2023/04/09 opening
    assets:cash            $100.00
    assets:fake             100.00 BS @ $10.00
    equity:opening
"
                    )?,
                    vec![Node::Transaction(Transaction {
                        date: NaiveDate::from_ymd_opt(2023, 4, 9).unwrap(),
                        payee: "opening".into(),
                        cleared: false,
                        mid: None,
                        notes: vec![],
                        postings: vec![
                            Posting {
                                account: AccountPath::Real("assets:cash".into()),
                                expression: Some(Expression::Literal(Numeric::Positive(100, 0))),
                                note: None,
                            },
                            Posting {
                                account: AccountPath::Real("assets:fake".into()),
                                expression: Some(Expression::Commodity((
                                    Numeric::Positive(100, 0),
                                    "BS".into(),
                                    Some(Numeric::Positive(10, 0))
                                ))),
                                note: None,
                            },
                            Posting {
                                account: AccountPath::Real("equity:opening".into()),
                                expression: None,
                                note: None,
                            },
                        ]
                    }),]
                );

                Ok(())
            }

            #[test]
            fn test_parse_automatic_transaction_simple() -> Result<()> {
                assert_eq!(
                    parse_str(
                        r"
= assets:savings:ktc
    [allocations:checking:savings:main]                 (-1)
    [assets:checking:reserved]                           (1)
"
                    )?,
                    vec![Node::AutomaticTransaction(AutomaticTransaction {
                        condition: "assets:savings:ktc".into(),
                        notes: vec![],
                        postings: vec![
                            Posting {
                                account: AccountPath::Virtual(
                                    "allocations:checking:savings:main".into()
                                ),
                                expression: Some(Expression::Factor((true, 1))),
                                note: None,
                            },
                            Posting {
                                account: AccountPath::Virtual("assets:checking:reserved".into()),
                                expression: Some(Expression::Factor((false, 1))),
                                note: None,
                            },
                        ]
                    }),]
                );

                Ok(())
            }

            #[test]
            fn test_parse_automatic_transaction_with_note() -> Result<()> {
                assert_eq!(
                    parse_str(
                        r"
= assets:savings:ktc
    ; :automatic:
    [allocations:checking:savings:main]                 (-1)
    [assets:checking:reserved]                           (1)
"
                    )?,
                    vec![Node::AutomaticTransaction(AutomaticTransaction {
                        condition: "assets:savings:ktc".into(),
                        notes: vec![":automatic:".into()],
                        postings: vec![
                            Posting {
                                account: AccountPath::Virtual(
                                    "allocations:checking:savings:main".into()
                                ),
                                expression: Some(Expression::Factor((true, 1))),
                                note: None,
                            },
                            Posting {
                                account: AccountPath::Virtual("assets:checking:reserved".into()),
                                expression: Some(Expression::Factor((false, 1))),
                                note: None,
                            },
                        ]
                    }),]
                );

                Ok(())
            }
        }
    }
}
