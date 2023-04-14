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
        use nom::character::complete::multispace0;
        use nom::character::{self};
        use nom::error::ParseError;
        use nom::multi::{many0, many1};
        use nom::sequence::delimited;
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
        }

        #[derive(Debug, PartialEq)]
        pub enum Node {
            Whitespace,
            Include(String),
            Transaction {
                date: NaiveDate,
                payee: String,
                postings: Vec<Self>,
                cleared: bool,
            },
            Posting {
                account: AccountPath,
                value: PostingExpression,
            },
        }

        /*
        pub fn word(i: &str) -> IResult<&str, &str> {
            take_while1(move |c| "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ".contains(c))(
                i,
            )
        }

        pub fn spaces(i: &str) -> IResult<&str, &str> {
            take_while1(move |c| " \t".contains(c))(i)
        }

        pub fn noun(i: &str) -> IResult<&str, Item> {
            map(word, |s: &str| Item::Named(s.to_owned()))(i)
        }

        pub fn string_literal(i: &str) -> IResult<&str, &str> {
            delimited(tag("\""), string_inside, tag("\""))(i)
        }

        fn string_inside(i: &str) -> IResult<&str, &str> {
            take_while(|c: char| c.is_alphabetic() || c.is_whitespace())(i)
        }

        pub fn unsigned_number(i: &str) -> IResult<&str, u64> {
            map_res(recognize(digit1), str::parse)(i)
        }

        pub fn gid_reference(i: &str) -> IResult<&str, Item> {
            map(preceded(tag("#"), unsigned_number), |n| {
                Item::Gid(EntityGid::new(n))
            })(i)
        }

        pub fn surrounding_area(i: &str) -> IResult<&str, Item> {
            map(tag("area"), |_s: &str| Item::Area)(i)
        }

        pub fn noun_or_specific(i: &str) -> IResult<&str, Item> {
            alt((surrounding_area, noun, gid_reference))(i)
        }

        pub fn named_place(i: &str) -> IResult<&str, Item> {
            alt((
                gid_reference,
                map(word, |s: &str| Item::Route(s.to_owned())),
            ))(i)
        }
        */

        pub fn unsigned_number(i: &str) -> IResult<&str, u64> {
            map_res(recognize(digit1), str::parse)(i)
        }

        pub fn path(i: &str) -> IResult<&str, &str> {
            take_while1(move |c| {
                // TODO This needs tons of work
                "/abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ.123456789-_".contains(c)
            })(i)
        }

        fn parse_directive(i: &str) -> IResult<&str, Node> {
            alt((parse_transaction, parse_include))(i)
        }

        fn date_string(i: &str) -> IResult<&str, NaiveDate> {
            map(
                separated_pair(
                    separated_pair(unsigned_number, tag("/"), unsigned_number),
                    tag("/"),
                    unsigned_number,
                ),
                |((year, month), day)| {
                    println!("{:?} {:?} {:?}", year, month, day);
                    NaiveDate::from_ymd_opt(year as i32, month as u32, day as u32)
                        .expect("Invalid date")
                },
            )(i)
        }

        fn payee(i: &str) -> IResult<&str, &str> {
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
                    separated_pair(date_string, whitespace1, payee),
                    many1(preceded(whitespace1, parse_posting)),
                ),
                |((naive_date, payee), postings)| {
                    println!("{:?} {:?} {:?}", naive_date, payee, postings);
                    Node::Transaction {
                        date: naive_date,
                        payee: payee.to_string(),
                        postings: postings,
                        cleared: false,
                    }
                },
            )(i)
        }

        fn posting_expression(i: &str) -> IResult<&str, PostingExpression> {
            map(
                separated_pair(
                    opt(tag("-")),
                    tag("$"),
                    separated_pair(unsigned_number, tag("."), unsigned_number),
                ),
                |(sign, factors)| {
                    PostingExpression::Currency((sign.is_some(), factors.0, factors.1))
                },
            )(i)
        }

        fn parse_posting(i: &str) -> IResult<&str, Node> {
            map(
                separated_pair(account_path, whitespace1, posting_expression),
                |(a, v)| Node::Posting {
                    account: a,
                    value: v,
                },
            )(i)
        }

        fn parse_include(i: &str) -> IResult<&str, Node> {
            map(
                separated_pair(tag("!include"), whitespace1, path),
                |(_, path)| Node::Include(path.into()),
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
            /*
            let (_, nodes) = many0(map(
                alt((parse_directive, map(whitespace1, |_| Node::Whitespace))),
                |n| n,
            ))(i)
            .map_err(|e| anyhow!("{:?}", e))?;
            */
            /*
            terminated(separated_list0(whitespace1, parse_directive), whitespace0)(i)
                .map_err(|e| anyhow!("{:?}", e))?;
            */

            Ok(nodes)
            /*
            let item = map(separated_pair(tag("put"), spaces, noun), |(_, target)| {
                target
            });

            let (_, action) = map(
                separated_pair(
                    separated_pair(
                        item,
                        spaces,
                        pair(tag("inside"), opt(pair(spaces, tag("of")))),
                    ),
                    spaces,
                    noun,
                ),
                |(item, target)| PutInsideAction {
                    item: item.0,
                    vessel: target,
                },
            )(i)?;
            */

            // Ok(vec![])
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
            fn test_parse_include() -> Result<()> {
                assert_eq!(
                    parse_str(r"!include checking.ledger")?,
                    vec![Node::Include("checking.ledger".into())]
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
                        cleared: false,
                        postings: vec![
                            Node::Posting {
                                account: AccountPath::Real("assets:cash".into()),
                                value: PostingExpression::Currency((false, 100, 0)),
                            },
                            Node::Posting {
                                account: AccountPath::Real("assets:checking".into()),
                                value: PostingExpression::Currency((true, 100, 0)),
                            },
                        ]
                    },]
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
                        cleared: false,
                        postings: vec![
                            Node::Posting {
                                account: AccountPath::Real("income".into()),
                                value: PostingExpression::Currency((true, 100, 0)),
                            },
                            Node::Posting {
                                account: AccountPath::Real("assets:checking".into()),
                                value: PostingExpression::Currency((false, 100, 0)),
                            },
                            Node::Posting {
                                account: AccountPath::Virtual("assets:checking:reserved".into()),
                                value: PostingExpression::Currency((true, 100, 0)),
                            },
                            Node::Posting {
                                account: AccountPath::Virtual("allocations:savings".into()),
                                value: PostingExpression::Currency((false, 100, 0)),
                            },
                        ]
                    },]
                );

                Ok(())
            }
        }
    }
}
