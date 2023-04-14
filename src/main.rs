use anyhow::Result;
use bigdecimal::{BigDecimal, Zero};
use std::{path::Path, time::Instant};
#[allow(unused_imports)]
use tracing::*;
use tracing_subscriber::prelude::*;

fn main() -> Result<()> {
    fn get_rust_log() -> String {
        std::env::var("RUST_LOG").unwrap_or_else(|_| "info".into())
    }

    tracing_subscriber::registry()
        .with(tracing_subscriber::EnvFilter::new(get_rust_log()))
        .with(tracing_subscriber::fmt::layer())
        .init();

    let args: Vec<_> = std::env::args().skip(1).collect();
    for arg in args {
        let processed = {
            let _span = span!(Level::INFO, "loading").entered();
            let started = Instant::now();
            let file = ledger::parsing::LedgerFile::parse(&Path::new(&arg))?;
            let loaded = file.preprocess()?;
            let elapsed = Instant::now() - started;
            info!("loaded ledger in {:?}ms", elapsed);
            loaded
        };

        let mut total = bigdecimal::BigDecimal::zero();
        for tx in processed.iter_transactions() {
            for posting in tx.postings.iter() {
                total += match &posting.account {
                    ledger::parsing::AccountPath::Real(name) => {
                        if name == "assets:checking" {
                            let value = posting.has_value();
                            // println!("{:?} {} {} {:?}", tx.date, name, tx.payee, value);
                            match value {
                                Some(value) => value,
                                _ => BigDecimal::zero(),
                            }
                        } else {
                            BigDecimal::zero()
                        }
                    }
                    _ => BigDecimal::zero(),
                }
            }
        }

        let sorted: Vec<_> = processed.iter_transactions_in_temporal_order().collect();

        println!("{}", serde_json::to_string(&sorted)?);
    }

    Ok(())
}

pub mod ledger {
    pub mod parsing {
        use std::{
            fs,
            path::{Path, PathBuf},
        };

        use anyhow::{anyhow, Result};

        use bigdecimal::{BigDecimal, Zero};
        use chrono::NaiveDate;
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

        #[derive(Debug, PartialEq)]
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
        }

        impl Serialize for Transaction {
            fn serialize<S>(&self, serializer: S) -> std::result::Result<S::Ok, S::Error>
            where
                S: serde::Serializer,
            {
                let mut state = serializer.serialize_struct("Transaction", 3)?;
                state.serialize_field("date", &format!("{:?}", &self.date))?;
                state.serialize_field("payee", &self.payee)?;
                state.serialize_field("cleared", &self.cleared)?;
                state.serialize_field("notes", &self.notes)?;
                state.serialize_field("postings", &self.postings)?;
                state.end()
            }
        }

        impl Transaction {
            fn balanced(self) -> Result<Self> {
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

                    Ok(Transaction {
                        date: self.date,
                        payee: self.payee,
                        cleared: self.cleared,
                        notes: self.notes,
                        postings,
                    })
                } else {
                    Ok(self)
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
                        notes: notes.iter().map(|n| n.to_string()).collect::<Vec<_>>(),
                        cleared: cleared.is_some(),
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

            pub fn preprocess(self) -> Result<LedgerFile> {
                debug!("preprocessing {:?}", self.path);

                let relative = self
                    .path
                    .parent()
                    .ok_or(anyhow!("Expected parent directory: {:?}", self.path))?;

                let nodes = self
                    .nodes
                    .into_iter()
                    .map(|node| match node {
                        Node::Transaction(tx) => Ok(Node::Transaction(tx.balanced()?)),
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
                        Node::Included(children) => recursively_iter_txs(children),
                        _ => Box::new(std::iter::empty::<&Transaction>()),
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
                        notes: Vec::new(),
                        cleared: false,
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
                            notes: Vec::new(),
                            cleared: false,
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
                            notes: Vec::new(),
                            cleared: false,
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
                        notes: Vec::new(),
                        cleared: false,
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
                        notes: Vec::new(),
                        cleared: false,
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
                        notes: Vec::new(),
                        cleared: false,
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
                        notes: Vec::new(),
                        cleared: false,
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
                        notes: vec!["hello-world".into()],
                        cleared: false,
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
                        notes: vec![],
                        cleared: false,
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
                        notes: vec![],
                        cleared: false,
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
                        notes: vec![],
                        cleared: false,
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
