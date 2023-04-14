use std::fs;

use anyhow::Result;

fn main() -> Result<()> {
    let args: Vec<_> = std::env::args().skip(1).collect();
    for arg in args {
        let data = fs::read_to_string(arg)?;
        let nodes = ledger::files::parse_str(&data)?;
        for node in nodes {
            println!("{:?}", node);
        }
    }

    Ok(())
}

pub mod ledger {
    pub mod files {
        use anyhow::{anyhow, Result};

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

        #[derive(Debug, PartialEq)]
        pub enum AccountPath {
            Real(String),
            Virtual(String),
        }

        #[derive(Debug, PartialEq)]
        pub enum Numeric {
            Negative(u64, u64),
            Positive(u64, u64),
        }

        #[derive(Debug, PartialEq)]
        pub enum PostingExpression {
            Literal(Numeric),
            Commodity((Numeric, String, Option<Numeric>)),
            Factor((bool, u64)),
        }

        #[derive(Debug, PartialEq)]
        pub struct Transaction {
            date: NaiveDate,
            payee: String,
            notes: Vec<String>,
            postings: Vec<Posting>,
            cleared: bool,
        }

        #[derive(Debug, PartialEq)]
        pub struct Posting {
            account: AccountPath,
            expression: Option<PostingExpression>,
            notes: Option<String>,
        }

        #[derive(Debug, PartialEq)]
        pub struct AutomaticTransaction {
            path: String,
            notes: Vec<String>,
            postings: Vec<Posting>,
        }

        #[derive(Debug, PartialEq)]
        pub struct CommodityPrice {
            date: NaiveDate,
            symbol: String,
            expression: PostingExpression,
        }

        #[derive(Debug, PartialEq)]
        pub enum Node {
            Comment(String),
            Transaction(Transaction),
            AccountDeclaration(AccountPath),
            TagDeclaration(String),
            Include(String),
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
                        path: path.into(),
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
                    separated_pair(symbol, linespace1, posting_expression),
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

        fn basic_commodity(i: &str) -> IResult<&str, PostingExpression> {
            map(
                tuple((
                    numeric_literal,
                    preceded(linespace1, symbol),
                    opt(preceded(
                        tuple((linespace1, tag("@"), linespace1)),
                        numeric_literal,
                    )),
                )),
                |(quantity, symbol, price)| {
                    PostingExpression::Commodity((quantity, symbol.into(), price))
                },
            )(i)
        }

        fn fractional(i: &str) -> IResult<&str, PostingExpression> {
            map(
                delimited(tag("("), pair(opt(tag("-")), unsigned_number), tag(")")),
                |(sign, factor)| PostingExpression::Factor((sign.is_some(), factor)),
            )(i)
        }

        fn posting_expression(i: &str) -> IResult<&str, PostingExpression> {
            alt((
                basic_commodity,
                map(numeric_literal, PostingExpression::Literal),
                fractional,
            ))(i)
        }

        fn parse_posting(i: &str) -> IResult<&str, Posting> {
            map(
                pair(
                    account_path,
                    pair(
                        opt(preceded(linespace1, posting_expression)),
                        opt(preceded(
                            tuple((linespace1, tag(";"), linespace1)),
                            parse_note,
                        )),
                    ),
                ),
                |(account, (expression, notes))| Posting {
                    account,
                    expression,
                    notes: notes.map(|f| f.into()),
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
                    preceded(linespace1, terminated(remaining_text, newline)),
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
                        expression: PostingExpression::Literal(Numeric::Positive(1000, 0))
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
                                expression: Some(PostingExpression::Literal(Numeric::Positive(
                                    100, 0
                                ))),
                                notes: None,
                            },
                            Posting {
                                account: AccountPath::Real("assets:checking".into()),
                                expression: Some(PostingExpression::Literal(Numeric::Negative(
                                    100, 0
                                ))),
                                notes: None,
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
                                    expression: Some(PostingExpression::Literal(
                                        Numeric::Positive(100, 0)
                                    )),
                                    notes: None,
                                },
                                Posting {
                                    account: AccountPath::Real("assets:checking".into()),
                                    expression: Some(PostingExpression::Literal(
                                        Numeric::Negative(100, 0)
                                    )),
                                    notes: None,
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
                                    expression: Some(PostingExpression::Literal(
                                        Numeric::Positive(100, 0)
                                    )),
                                    notes: None,
                                },
                                Posting {
                                    account: AccountPath::Real("assets:checking".into()),
                                    expression: Some(PostingExpression::Literal(
                                        Numeric::Negative(100, 0)
                                    )),
                                    notes: None,
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
                                expression: Some(PostingExpression::Literal(Numeric::Negative(
                                    100, 0
                                ))),
                                notes: None,
                            },
                            Posting {
                                account: AccountPath::Real("assets:checking".into()),
                                expression: Some(PostingExpression::Literal(Numeric::Positive(
                                    100, 0
                                ))),
                                notes: None,
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
                                expression: Some(PostingExpression::Literal(Numeric::Negative(
                                    100, 0
                                ))),
                                notes: None,
                            },
                            Posting {
                                account: AccountPath::Real("assets:checking".into()),
                                expression: Some(PostingExpression::Literal(Numeric::Positive(
                                    100, 0
                                ))),
                                notes: None,
                            },
                            Posting {
                                account: AccountPath::Virtual("assets:checking:reserved".into()),
                                expression: Some(PostingExpression::Literal(Numeric::Negative(
                                    100, 0
                                ))),
                                notes: None,
                            },
                            Posting {
                                account: AccountPath::Virtual("allocations:savings".into()),
                                expression: Some(PostingExpression::Literal(Numeric::Positive(
                                    100, 0
                                ))),
                                notes: None,
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
                                expression: Some(PostingExpression::Literal(Numeric::Positive(
                                    100, 0
                                ))),
                                notes: None,
                            },
                            Posting {
                                account: AccountPath::Real("assets:checking".into()),
                                expression: Some(PostingExpression::Literal(Numeric::Negative(
                                    100, 0
                                ))),
                                notes: None,
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
                                expression: Some(PostingExpression::Literal(Numeric::Positive(
                                    100, 0
                                ))),
                                notes: None,
                            },
                            Posting {
                                account: AccountPath::Real("assets:checking".into()),
                                expression: Some(PostingExpression::Literal(Numeric::Negative(
                                    100, 0
                                ))),
                                notes: None,
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
                                expression: Some(PostingExpression::Literal(Numeric::Negative(
                                    100, 0
                                ))),
                                notes: None,
                            },
                            Posting {
                                account: AccountPath::Real("assets:cash".into()),
                                expression: None,
                                notes: None,
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
                                notes: None,
                            },
                            Posting {
                                account: AccountPath::Real("assets:checking".into()),
                                expression: Some(PostingExpression::Literal(Numeric::Negative(
                                    100, 0
                                ))),
                                notes: None,
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
                                expression: Some(PostingExpression::Literal(Numeric::Positive(
                                    100, 0
                                ))),
                                notes: None,
                            },
                            Posting {
                                account: AccountPath::Real("assets:checking".into()),
                                expression: Some(PostingExpression::Literal(Numeric::Negative(
                                    100, 0
                                ))),
                                notes: None,
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
                                expression: Some(PostingExpression::Literal(Numeric::Positive(
                                    100, 0
                                ))),
                                notes: Some("hello-world".into()),
                            },
                            Posting {
                                account: AccountPath::Real("assets:checking".into()),
                                expression: Some(PostingExpression::Literal(Numeric::Negative(
                                    100, 0
                                ))),
                                notes: None,
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
                                expression: Some(PostingExpression::Literal(Numeric::Positive(
                                    100, 0
                                ))),
                                notes: None,
                            },
                            Posting {
                                account: AccountPath::Real("assets:fake".into()),
                                expression: Some(PostingExpression::Commodity((
                                    Numeric::Positive(100, 0),
                                    "BS".into(),
                                    None,
                                ))),
                                notes: None,
                            },
                            Posting {
                                account: AccountPath::Real("equity:opening".into()),
                                expression: None,
                                notes: None,
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
                                expression: Some(PostingExpression::Literal(Numeric::Positive(
                                    100, 0
                                ))),
                                notes: None,
                            },
                            Posting {
                                account: AccountPath::Real("assets:fake".into()),
                                expression: Some(PostingExpression::Commodity((
                                    Numeric::Positive(100, 0),
                                    "BS".into(),
                                    Some(Numeric::Positive(10, 0))
                                ))),
                                notes: None,
                            },
                            Posting {
                                account: AccountPath::Real("equity:opening".into()),
                                expression: None,
                                notes: None,
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
                        path: "assets:savings:ktc".into(),
                        notes: vec![],
                        postings: vec![
                            Posting {
                                account: AccountPath::Virtual(
                                    "allocations:checking:savings:main".into()
                                ),
                                expression: Some(PostingExpression::Factor((true, 1))),
                                notes: None,
                            },
                            Posting {
                                account: AccountPath::Virtual("assets:checking:reserved".into()),
                                expression: Some(PostingExpression::Factor((false, 1))),
                                notes: None,
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
                        path: "assets:savings:ktc".into(),
                        notes: vec![":automatic:".into()],
                        postings: vec![
                            Posting {
                                account: AccountPath::Virtual(
                                    "allocations:checking:savings:main".into()
                                ),
                                expression: Some(PostingExpression::Factor((true, 1))),
                                notes: None,
                            },
                            Posting {
                                account: AccountPath::Virtual("assets:checking:reserved".into()),
                                expression: Some(PostingExpression::Factor((false, 1))),
                                notes: None,
                            },
                        ]
                    }),]
                );

                Ok(())
            }
        }
    }
}
