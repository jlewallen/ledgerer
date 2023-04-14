use anyhow::Result;

fn main() -> Result<()> {
    let _nodes = ledger::files::parse_str("")?;

    Ok(())
}

pub mod ledger {
    pub mod files {
        use anyhow::{anyhow, Result};

        use nom::combinator::opt;

        use chrono::NaiveDate;
        use nom::character::complete::{alpha1, multispace0};
        use nom::character::{self};
        use nom::error::ParseError;
        use nom::multi::{many0, many1};
        use nom::sequence::{delimited, tuple};
        use nom::{
            branch::alt,
            bytes::complete::{tag, take_while, take_while1},
            character::complete::digit1,
            combinator::map,
            combinator::{map_res, recognize},
            sequence::{pair, preceded, separated_pair},
            IResult,
        };

        #[derive(Debug, PartialEq)]
        pub enum AccountPath {
            Real(String),
            Virtual(String),
        }

        #[derive(Debug, PartialEq)]
        pub enum PostingExpression {
            Currency((bool, u64, u64)),
            Commodity((bool, u64, u64, String)),
            Factor((bool, u64)),
        }

        #[derive(Debug, PartialEq)]
        pub enum Node {
            Whitespace,
            Currency(String),
            Include(String),
            Account(AccountPath),
            Price((NaiveDate, String, PostingExpression)),
            Comment(String),
            Tag(String),
            Transaction {
                date: NaiveDate,
                payee: String,
                notes: Vec<String>,
                postings: Vec<Self>,
                cleared: bool,
            },
            Posting {
                account: AccountPath,
                value: Option<PostingExpression>,
                notes: Option<String>,
            },
            AutomaticTransaction {
                path: String,
                notes: Vec<String>,
                postings: Vec<Self>,
            },
        }

        fn unsigned_number(i: &str) -> IResult<&str, u64> {
            map_res(recognize(digit1), str::parse)(i)
        }

        fn path(i: &str) -> IResult<&str, &str> {
            take_while1(move |c| {
                // TODO This needs tons of work
                "/abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ.123456789-_*".contains(c)
            })(i)
        }

        fn parse_directive(i: &str) -> IResult<&str, Node> {
            alt((
                parse_transaction,
                parse_comment,
                parse_account,
                parse_tag,
                parse_include,
                parse_currency,
                parse_price,
                parse_automatic_transaction,
            ))(i)
        }

        fn parse_automatic_transaction(i: &str) -> IResult<&str, Node> {
            map(
                pair(
                    tuple((preceded(tag("="), whitespace1), payee)), /* Not happy with payee here */
                    pair(
                        many0(preceded(ws(tag(";")), parse_note)),
                        many1(preceded(whitespace1, parse_posting)),
                    ),
                ),
                |((_, path), (notes, postings))| Node::AutomaticTransaction {
                    path: path.into(),
                    notes: notes.iter().map(|n| n.to_string()).collect::<Vec<_>>(),
                    postings: postings,
                },
            )(i)
        }

        fn parse_price(i: &str) -> IResult<&str, Node> {
            map(
                separated_pair(
                    separated_pair(tag("P"), whitespace1, date_string),
                    whitespace1,
                    separated_pair(symbol, whitespace1, posting_expression),
                ),
                |((_, date), (symbol, expression))| Node::Price((date, symbol.into(), expression)),
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

        fn parse_rest_of_comment(i: &str) -> IResult<&str, &str> {
            take_while1(move |c: char| !character::is_newline(c as u8))(i)
        }

        fn parse_note(i: &str) -> IResult<&str, &str> {
            take_while1(move |c: char| !character::is_newline(c as u8))(i)
        }

        fn account_path_string(i: &str) -> IResult<&str, &str> {
            take_while1(move |c| {
                // TODO This needs tons of work
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
                        opt(preceded(whitespace1, tag("*"))),
                        whitespace1,
                        payee,
                    )),
                    pair(
                        many0(preceded(ws(tag(";")), parse_note)),
                        many1(preceded(whitespace1, parse_posting)),
                    ),
                ),
                |((naive_date, cleared, _, payee), (notes, postings))| Node::Transaction {
                    date: naive_date,
                    payee: payee.to_string(),
                    notes: notes.iter().map(|n| n.to_string()).collect::<Vec<_>>(),
                    postings: postings,
                    cleared: cleared.is_some(),
                },
            )(i)
        }

        fn basic_dollars(i: &str) -> IResult<&str, PostingExpression> {
            map(
                separated_pair(
                    opt(tag("-")),
                    tag("$"),
                    pair(
                        opt(tag("-")),
                        separated_pair(unsigned_number, tag("."), unsigned_number),
                    ),
                ),
                |(sign1, (sign2, factors))| {
                    PostingExpression::Currency((
                        sign1.is_some() || sign2.is_some(),
                        factors.0,
                        factors.1,
                    ))
                },
            )(i)
        }

        fn basic_commodity(i: &str) -> IResult<&str, PostingExpression> {
            map(
                tuple((
                    opt(tag("-")),
                    separated_pair(unsigned_number, tag("."), unsigned_number),
                    preceded(whitespace1, symbol),
                )),
                |(sign, factors, symbol)| {
                    PostingExpression::Commodity((
                        sign.is_some(),
                        factors.0,
                        factors.1,
                        symbol.into(),
                    ))
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
            alt((basic_dollars, basic_commodity, fractional))(i)
        }

        fn parse_posting(i: &str) -> IResult<&str, Node> {
            map(
                pair(
                    account_path,
                    pair(
                        opt(preceded(whitespace1, posting_expression)),
                        opt(preceded(
                            tuple((whitespace1, tag(";"))),
                            preceded(whitespace1, parse_note),
                        )),
                    ),
                ),
                |(a, (v, n))| Node::Posting {
                    account: a,
                    value: v,
                    notes: n.map(|f| f.into()),
                },
            )(i)
        }

        fn parse_currency(i: &str) -> IResult<&str, Node> {
            map(
                separated_pair(
                    tag("D"),
                    whitespace1,
                    pair(
                        tag("$"),
                        separated_pair(unsigned_number, tag("."), unsigned_number),
                    ),
                ),
                |(_, _)| Node::Currency("$".into()),
            )(i)
        }

        fn parse_include(i: &str) -> IResult<&str, Node> {
            map(
                separated_pair(tag("!include"), whitespace1, path),
                |(_, path)| Node::Include(path.into()),
            )(i)
        }

        fn parse_account(i: &str) -> IResult<&str, Node> {
            map(
                separated_pair(tag("account"), whitespace1, account_path),
                |(_, path)| Node::Account(path.into()),
            )(i)
        }

        fn parse_comment(i: &str) -> IResult<&str, Node> {
            map(
                separated_pair(tag(";"), whitespace1, parse_rest_of_comment),
                |(_, comment)| Node::Comment(comment.into()),
            )(i)
        }

        fn parse_tag(i: &str) -> IResult<&str, Node> {
            map(
                separated_pair(tag("tag"), whitespace1, path),
                |(_, path)| Node::Tag(path.into()),
            )(i)
        }

        pub fn whitespace1(i: &str) -> IResult<&str, &str> {
            take_while1(move |c| " \n\r\t".contains(c))(i)
        }

        pub fn whitespace0(i: &str) -> IResult<&str, &str> {
            take_while(move |c| " \n\r\t".contains(c))(i)
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

        pub fn parse_str(i: &str) -> Result<Vec<Node>> {
            let (_, nodes) = many0(ws(parse_directive))(i).map_err(|e| anyhow!("{:?}", e))?;

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
            fn test_parse_standard_currency() -> Result<()> {
                assert_eq!(parse_str(r"D $1000.00")?, vec![Node::Currency("$".into())]);

                Ok(())
            }

            #[test]
            fn test_parse_price() -> Result<()> {
                assert_eq!(
                    parse_str(r"P 2021/1/29 BS $1000.00")?,
                    vec![Node::Price((
                        NaiveDate::from_ymd_opt(2021, 1, 29).expect("inline date error"),
                        "BS".into(),
                        PostingExpression::Currency((false, 1000, 0))
                    ))]
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
                    vec![Node::Transaction {
                        date: NaiveDate::from_ymd_opt(2023, 04, 09).unwrap(),
                        payee: "withdrawl".into(),
                        notes: Vec::new(),
                        cleared: false,
                        postings: vec![
                            Node::Posting {
                                account: AccountPath::Real("assets:cash".into()),
                                value: Some(PostingExpression::Currency((false, 100, 0))),
                                notes: None,
                            },
                            Node::Posting {
                                account: AccountPath::Real("assets:checking".into()),
                                value: Some(PostingExpression::Currency((true, 100, 0))),
                                notes: None,
                            },
                        ]
                    },]
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
                        Node::Transaction {
                            date: NaiveDate::from_ymd_opt(2023, 04, 09).unwrap(),
                            payee: "withdrawl 1".into(),
                            notes: Vec::new(),
                            cleared: false,
                            postings: vec![
                                Node::Posting {
                                    account: AccountPath::Real("assets:cash".into()),
                                    value: Some(PostingExpression::Currency((false, 100, 0))),
                                    notes: None,
                                },
                                Node::Posting {
                                    account: AccountPath::Real("assets:checking".into()),
                                    value: Some(PostingExpression::Currency((true, 100, 0))),
                                    notes: None,
                                },
                            ]
                        },
                        Node::Transaction {
                            date: NaiveDate::from_ymd_opt(2023, 04, 10).unwrap(),
                            payee: "withdrawl 2".into(),
                            notes: Vec::new(),
                            cleared: false,
                            postings: vec![
                                Node::Posting {
                                    account: AccountPath::Real("assets:cash".into()),
                                    value: Some(PostingExpression::Currency((false, 100, 0))),
                                    notes: None,
                                },
                                Node::Posting {
                                    account: AccountPath::Real("assets:checking".into()),
                                    value: Some(PostingExpression::Currency((true, 100, 0))),
                                    notes: None,
                                },
                            ]
                        },
                    ]
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
                    vec![Node::Transaction {
                        date: NaiveDate::from_ymd_opt(2023, 04, 09).unwrap(),
                        payee: "income".into(),
                        notes: Vec::new(),
                        cleared: false,
                        postings: vec![
                            Node::Posting {
                                account: AccountPath::Real("income".into()),
                                value: Some(PostingExpression::Currency((true, 100, 0))),
                                notes: None,
                            },
                            Node::Posting {
                                account: AccountPath::Real("assets:checking".into()),
                                value: Some(PostingExpression::Currency((false, 100, 0))),
                                notes: None,
                            },
                            Node::Posting {
                                account: AccountPath::Virtual("assets:checking:reserved".into()),
                                value: Some(PostingExpression::Currency((true, 100, 0))),
                                notes: None,
                            },
                            Node::Posting {
                                account: AccountPath::Virtual("allocations:savings".into()),
                                value: Some(PostingExpression::Currency((false, 100, 0))),
                                notes: None,
                            },
                        ]
                    },]
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
                    vec![Node::Transaction {
                        date: NaiveDate::from_ymd_opt(2023, 04, 09).unwrap(),
                        payee: "another example".into(),
                        notes: Vec::new(),
                        cleared: true,
                        postings: vec![
                            Node::Posting {
                                account: AccountPath::Real("assets:cash".into()),
                                value: Some(PostingExpression::Currency((false, 100, 0))),
                                notes: None,
                            },
                            Node::Posting {
                                account: AccountPath::Real("assets:checking".into()),
                                value: Some(PostingExpression::Currency((true, 100, 0))),
                                notes: None,
                            },
                        ]
                    },]
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
                    vec![Node::Transaction {
                        date: NaiveDate::from_ymd_opt(2023, 04, 09).unwrap(),
                        payee: "withdrawl with more text".into(),
                        notes: Vec::new(),
                        cleared: true,
                        postings: vec![
                            Node::Posting {
                                account: AccountPath::Real("assets:cash".into()),
                                value: Some(PostingExpression::Currency((false, 100, 0))),
                                notes: None,
                            },
                            Node::Posting {
                                account: AccountPath::Real("assets:checking".into()),
                                value: Some(PostingExpression::Currency((true, 100, 0))),
                                notes: None,
                            },
                        ]
                    },]
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
                    vec![Node::Transaction {
                        date: NaiveDate::from_ymd_opt(2023, 04, 09).unwrap(),
                        payee: "withdrawl".into(),
                        notes: Vec::new(),
                        cleared: false,
                        postings: vec![
                            Node::Posting {
                                account: AccountPath::Real("assets:checking".into()),
                                value: Some(PostingExpression::Currency((true, 100, 0))),
                                notes: None,
                            },
                            Node::Posting {
                                account: AccountPath::Real("assets:cash".into()),
                                value: None,
                                notes: None,
                            },
                        ]
                    },]
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
                    vec![Node::Transaction {
                        date: NaiveDate::from_ymd_opt(2023, 04, 09).unwrap(),
                        payee: "withdrawl".into(),
                        notes: Vec::new(),
                        cleared: false,
                        postings: vec![
                            Node::Posting {
                                account: AccountPath::Real("assets:cash".into()),
                                value: None,
                                notes: None,
                            },
                            Node::Posting {
                                account: AccountPath::Real("assets:checking".into()),
                                value: Some(PostingExpression::Currency((true, 100, 0))),
                                notes: None,
                            },
                        ]
                    },]
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
                    vec![Node::Transaction {
                        date: NaiveDate::from_ymd_opt(2023, 04, 09).unwrap(),
                        payee: "withdrawl".into(),
                        notes: vec!["hello-world".into()],
                        cleared: false,
                        postings: vec![
                            Node::Posting {
                                account: AccountPath::Real("assets:cash".into()),
                                value: Some(PostingExpression::Currency((false, 100, 0))),
                                notes: None,
                            },
                            Node::Posting {
                                account: AccountPath::Real("assets:checking".into()),
                                value: Some(PostingExpression::Currency((true, 100, 0))),
                                notes: None,
                            },
                        ]
                    },]
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
                    vec![Node::Transaction {
                        date: NaiveDate::from_ymd_opt(2023, 04, 09).unwrap(),
                        payee: "withdrawl".into(),
                        notes: vec![],
                        cleared: false,
                        postings: vec![
                            Node::Posting {
                                account: AccountPath::Real("assets:cash".into()),
                                value: Some(PostingExpression::Currency((false, 100, 0))),
                                notes: Some("hello-world".into()),
                            },
                            Node::Posting {
                                account: AccountPath::Real("assets:checking".into()),
                                value: Some(PostingExpression::Currency((true, 100, 0))),
                                notes: None,
                            },
                        ]
                    },]
                );

                Ok(())
            }

            #[test]
            fn test_parse_account_declaration() -> Result<()> {
                assert_eq!(
                    parse_str(r"account expenses:cash")?,
                    vec![Node::Account(AccountPath::Real("expenses:cash".into()),)]
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
                        Node::Account(AccountPath::Real("expenses:cash".into())),
                        Node::Account(AccountPath::Real("expenses:food".into())),
                        Node::Account(AccountPath::Virtual("allocations:checking".into())),
                    ]
                );

                Ok(())
            }

            #[test]
            fn test_parse_virtual_account_declaration() -> Result<()> {
                assert_eq!(
                    parse_str(r"account [expenses:cash]")?,
                    vec![Node::Account(AccountPath::Virtual("expenses:cash".into()),)]
                );

                Ok(())
            }

            #[test]
            fn test_parse_tag_declaration() -> Result<()> {
                assert_eq!(parse_str(r"tag income")?, vec![Node::Tag("income".into())]);

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
                    vec![Node::Transaction {
                        date: NaiveDate::from_ymd_opt(2023, 04, 09).unwrap(),
                        payee: "opening".into(),
                        notes: vec![],
                        cleared: false,
                        postings: vec![
                            Node::Posting {
                                account: AccountPath::Real("assets:cash".into()),
                                value: Some(PostingExpression::Currency((false, 100, 0))),
                                notes: None,
                            },
                            Node::Posting {
                                account: AccountPath::Real("assets:fake".into()),
                                value: Some(PostingExpression::Commodity((
                                    false,
                                    100,
                                    0,
                                    "BS".into()
                                ))),
                                notes: None,
                            },
                            Node::Posting {
                                account: AccountPath::Real("equity:opening".into()),
                                value: None,
                                notes: None,
                            },
                        ]
                    },]
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
                    vec![Node::AutomaticTransaction {
                        path: "assets:savings:ktc".into(),
                        notes: vec![],
                        postings: vec![
                            Node::Posting {
                                account: AccountPath::Virtual(
                                    "allocations:checking:savings:main".into()
                                ),
                                value: Some(PostingExpression::Factor((true, 1))),
                                notes: None,
                            },
                            Node::Posting {
                                account: AccountPath::Virtual("assets:checking:reserved".into()),
                                value: Some(PostingExpression::Factor((false, 1))),
                                notes: None,
                            },
                        ]
                    },]
                );

                Ok(())
            }
        }
    }
}
