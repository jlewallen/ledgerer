use anyhow::anyhow;
use nom::{
    branch::alt,
    bytes::complete::{tag, take_while, take_while1},
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

use crate::model::*;

#[cfg(test)]
mod tests;

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

fn parse_dated_price(i: &str) -> IResult<&str, Node> {
    map(
        terminated(
            separated_pair(
                preceded(tuple((tag("P"), linespace1)), date_string),
                linespace1,
                separated_pair(symbol, linespace1, expression),
            ),
            opt(newline),
        ),
        |(date, (symbol, expression))| {
            Node::DatedPrice(DatedPrice {
                date,
                symbol: symbol.into(),
                expression,
            })
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
    take_while1(move |c| ":abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ-".contains(c))(i)
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
                parsed_date_string,
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
            Node::ParsedTransaction(ParsedTransaction {
                date,
                payee: payee.to_string(),
                cleared: cleared.is_some(),
                notes: notes.iter().map(|n| n.to_string()).collect::<Vec<_>>(),
                refs: Vec::default(),
                postings,
                origin: Some(Origin::File),
                mid: None,
                order: None,
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
                pair(
                    unparsed_unsigned_number,
                    opt(preceded(tag("."), unparsed_unsigned_number)),
                ),
            ),
        ),
        |(sign1, (sign2, factors))| {
            if sign1.is_some() || sign2.is_some() {
                Numeric::Negative(factors.0.to_owned(), factors.1.map(|f| f.to_owned()))
            } else {
                Numeric::Positive(factors.0.to_owned(), factors.1.map(|f| f.to_owned()))
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
                linespace1,
                delimited(tag("{"), numeric_literal, tag("}")),
            )),
            opt(preceded(
                tuple((linespace1, tag("@"), linespace1)),
                numeric_literal,
            )),
            opt(preceded(
                linespace1,
                delimited(tag("["), date_string, tag("]")),
            )),
        )),
        |(quantity, symbol, maybe_lot_price, maybe_price, maybe_date)| {
            Expression::Commodity(CommodityExpression {
                quantity,
                symbol: symbol.into(),
                lot_price: maybe_lot_price.as_ref().map(|d| d.clone()),
                price: maybe_price.as_ref().map(|d| d.clone()),
                date: maybe_date.map(|d| d),
            })
        },
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
        tuple((
            account_path,
            opt(preceded(linespace1, expression)),
            opt(preceded(
                tuple((linespace1, tag(";"), linespace1)),
                parse_note,
            )),
        )),
        |(account, expression, note)| Posting {
            account,
            expression,
            note: note.map(|f| f.into()),
        },
    )(i)
}

fn parse_default_commodity(i: &str) -> IResult<&str, Node> {
    map(
        terminated(
            preceded(
                pair(tag("D"), linespace1),
                preceded(
                    tag("$"),
                    separated_pair(unsigned_number, tag("."), unsigned_number),
                ),
            ),
            opt(newline),
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

fn parse_include_directive(i: &str) -> IResult<&str, Node> {
    map(
        terminated(
            preceded(tuple((tag("!include"), linespace1)), file_path),
            opt(newline),
        ),
        |path| Node::Include(path.into()),
    )(i)
}

fn parse_account_declaration(i: &str) -> IResult<&str, Node> {
    map(
        terminated(
            preceded(preceded(tag("account"), linespace1), account_path),
            opt(newline),
        ),
        Node::AccountDeclaration,
    )(i)
}

fn identifier(i: &str) -> IResult<&str, &str> {
    take_while1(move |c| {
        "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ.123456789-_".contains(c)
    })(i)
}

fn parse_tag_declaration(i: &str) -> IResult<&str, Node> {
    map(
        terminated(
            preceded(preceded(tag("tag"), linespace1), identifier),
            opt(newline),
        ),
        |path| Node::TagDeclaration(path.into()),
    )(i)
}

fn parse_commodity_declaration(i: &str) -> IResult<&str, Node> {
    map(
        terminated(
            preceded(tuple((tag("commodity"), linespace1)), symbol),
            opt(newline),
        ),
        |symbol| Node::CommodityDeclaration(symbol.into()),
    )(i)
}

fn parse_year(i: &str) -> IResult<&str, Node> {
    map(
        terminated(
            preceded(tuple((tag("year"), linespace1)), unparsed_unsigned_number),
            opt(newline),
        ),
        |year| Node::Year(year.parse().unwrap()),
    )(i)
}

fn remaining_text(i: &str) -> IResult<&str, &str> {
    take_while(move |c: char| !character::is_newline(c as u8))(i)
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

fn parse_empty_line(i: &str) -> IResult<&str, Node> {
    map(preceded(opt(linespace1), newline), |_| Node::EmptyLine)(i)
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
        parse_empty_line,
        parse_transaction,
        parse_comment,
        parse_account_declaration,
        parse_tag_declaration,
        parse_include_directive,
        parse_default_commodity,
        parse_dated_price,
        parse_commodity_declaration,
        parse_automatic_transaction,
        parse_year,
    ))(i)
}

pub fn parse_str(i: &str) -> Result<Vec<Node>> {
    let (remaining, nodes) = many0(parse_directive)(i).map_err(|e| anyhow!("{:?}", e))?;

    assert_eq!(remaining, "");

    Ok(nodes)
}

fn symbol(i: &str) -> IResult<&str, &str> {
    recognize(tuple((many1(alpha1), many0(tag("*")))))(i)
}

fn unparsed_unsigned_number(i: &str) -> IResult<&str, &str> {
    recognize(digit1)(i)
}

fn unsigned_number(i: &str) -> IResult<&str, u64> {
    map_res(unparsed_unsigned_number, str::parse)(i)
}

fn date_string(i: &str) -> IResult<&str, NaiveDate> {
    map(
        separated_pair(
            separated_pair(unsigned_number, tag("/"), unsigned_number),
            tag("/"),
            unsigned_number,
        ),
        |((year, month), day)| {
            NaiveDate::from_ymd_opt(year as i32, month as u32, day as u32).expect("Invalid date")
        },
    )(i)
}

fn month_day_string(i: &str) -> IResult<&str, ParsedDate> {
    map(
        separated_pair(unsigned_number, tag("/"), unsigned_number),
        |(month, day)| ParsedDate::MonthDay(month as u32, day as u32),
    )(i)
}

fn parsed_date_string(i: &str) -> IResult<&str, ParsedDate> {
    alt((
        map(date_string, |v| ParsedDate::YearMonthDate(v)),
        month_day_string,
    ))(i)
}
