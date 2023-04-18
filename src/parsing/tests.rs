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
            expression: Expression::Literal(Numeric::Positive("1000".into(), Some("00".into())))
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
        vec![Node::EmptyLine, Node::Include("checking.ledger".into()),]
    );

    Ok(())
}
#[test]
fn test_parse_transaction_below_comment() -> Result<()> {
    assert_eq!(
        parse_str(
            r"
; Hello
;
2023/04/09 withdrawl
    assets:cash            $100.00
    assets:checking       -$100.00
"
            .trim_start()
        )?,
        vec![
            Node::Comment(" Hello".to_owned()),
            Node::Comment("".to_owned()),
            Node::Transaction(Transaction {
                date: NaiveDate::from_ymd_opt(2023, 4, 9).unwrap(),
                payee: "withdrawl".into(),
                cleared: false,
                mid: None,
                notes: Vec::new(),
                postings: vec![
                    Posting {
                        account: AccountPath::Real("assets:cash".into()),
                        expression: Some(Expression::Literal(Numeric::Positive(
                            "100".into(),
                            Some("00".into())
                        ))),
                        note: None,
                    },
                    Posting {
                        account: AccountPath::Real("assets:checking".into()),
                        expression: Some(Expression::Literal(Numeric::Negative(
                            "100".into(),
                            Some("00".into())
                        ))),
                        note: None,
                    },
                ]
            })
        ]
    );

    Ok(())
}

#[test]
fn test_parse_transaction_basic_no_newline() -> Result<()> {
    assert_eq!(
        parse_str(
            r"2023/04/09 withdrawl
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
                    expression: Some(Expression::Literal(Numeric::Positive(
                        "100".into(),
                        Some("00".into())
                    ))),
                    note: None,
                },
                Posting {
                    account: AccountPath::Real("assets:checking".into()),
                    expression: Some(Expression::Literal(Numeric::Negative(
                        "100".into(),
                        Some("00".into())
                    ))),
                    note: None,
                },
            ]
        })]
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
            .trim_start()
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
                    expression: Some(Expression::Literal(Numeric::Positive(
                        "100".into(),
                        Some("00".into())
                    ))),
                    note: None,
                },
                Posting {
                    account: AccountPath::Real("assets:checking".into()),
                    expression: Some(Expression::Literal(Numeric::Negative(
                        "100".into(),
                        Some("00".into())
                    ))),
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
            .trim_start()
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
                            "100".into(),
                            Some("00".into())
                        ))),
                        note: None,
                    },
                    Posting {
                        account: AccountPath::Real("assets:checking".into()),
                        expression: Some(Expression::Literal(Numeric::Negative(
                            "100".into(),
                            Some("00".into())
                        ))),
                        note: None,
                    },
                ]
            }),
            Node::EmptyLine,
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
                            "100".into(),
                            Some("00".into())
                        ))),
                        note: None,
                    },
                    Posting {
                        account: AccountPath::Real("assets:checking".into()),
                        expression: Some(Expression::Literal(Numeric::Negative(
                            "100".into(),
                            Some("00".into())
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
            .trim_start()
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
                    expression: Some(Expression::Literal(Numeric::Negative(
                        "100".into(),
                        Some("00".into())
                    ))),
                    note: None,
                },
                Posting {
                    account: AccountPath::Real("assets:checking".into()),
                    expression: Some(Expression::Literal(Numeric::Positive(
                        "100".into(),
                        Some("00".into())
                    ))),
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
            .trim_start()
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
                    expression: Some(Expression::Literal(Numeric::Negative(
                        "100".into(),
                        Some("00".into())
                    ))),
                    note: None,
                },
                Posting {
                    account: AccountPath::Real("assets:checking".into()),
                    expression: Some(Expression::Literal(Numeric::Positive(
                        "100".into(),
                        Some("00".into())
                    ))),
                    note: None,
                },
                Posting {
                    account: AccountPath::Virtual("assets:checking:reserved".into()),
                    expression: Some(Expression::Literal(Numeric::Negative(
                        "100".into(),
                        Some("00".into())
                    ))),
                    note: None,
                },
                Posting {
                    account: AccountPath::Virtual("allocations:savings".into()),
                    expression: Some(Expression::Literal(Numeric::Positive(
                        "100".into(),
                        Some("00".into())
                    ))),
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
            .trim_start()
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
                    expression: Some(Expression::Literal(Numeric::Positive(
                        "100".into(),
                        Some("00".into())
                    ))),
                    note: None,
                },
                Posting {
                    account: AccountPath::Real("assets:checking".into()),
                    expression: Some(Expression::Literal(Numeric::Negative(
                        "100".into(),
                        Some("00".into())
                    ))),
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
            .trim_start()
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
                    expression: Some(Expression::Literal(Numeric::Positive(
                        "100".into(),
                        Some("00".into())
                    ))),
                    note: None,
                },
                Posting {
                    account: AccountPath::Real("assets:checking".into()),
                    expression: Some(Expression::Literal(Numeric::Negative(
                        "100".into(),
                        Some("00".into())
                    ))),
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
            .trim_start()
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
                    expression: Some(Expression::Literal(Numeric::Negative(
                        "100".into(),
                        Some("00".into())
                    ))),
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
            .trim_start()
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
                    expression: Some(Expression::Literal(Numeric::Negative(
                        "100".into(),
                        Some("00".into())
                    ))),
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
            .trim_start()
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
                    expression: Some(Expression::Literal(Numeric::Positive(
                        "100".into(),
                        Some("00".into())
                    ))),
                    note: None,
                },
                Posting {
                    account: AccountPath::Real("assets:checking".into()),
                    expression: Some(Expression::Literal(Numeric::Negative(
                        "100".into(),
                        Some("00".into())
                    ))),
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
            .trim_start()
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
                    expression: Some(Expression::Literal(Numeric::Positive(
                        "100".into(),
                        Some("00".into())
                    ))),
                    note: Some("hello-world".into()),
                },
                Posting {
                    account: AccountPath::Real("assets:checking".into()),
                    expression: Some(Expression::Literal(Numeric::Negative(
                        "100".into(),
                        Some("00".into())
                    ))),
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
fn test_parse_account_declaration_multiple_with_empty_line() -> Result<()> {
    assert_eq!(
        parse_str(
            r"account expenses:cash

account expenses:food

account [allocations:checking]
"
        )?,
        vec![
            Node::AccountDeclaration(AccountPath::Real("expenses:cash".into())),
            Node::EmptyLine,
            Node::AccountDeclaration(AccountPath::Real("expenses:food".into())),
            Node::EmptyLine,
            Node::AccountDeclaration(AccountPath::Virtual("allocations:checking".into())),
        ]
    );

    Ok(())
}

#[test]
fn test_parse_account_declaration_multiple() -> Result<()> {
    assert_eq!(
        parse_str(
            r"account expenses:cash
account expenses:food
account [allocations:checking]"
        )?,
        vec![
            Node::AccountDeclaration(AccountPath::Real("expenses:cash".into())),
            Node::AccountDeclaration(AccountPath::Real("expenses:food".into())),
            Node::AccountDeclaration(AccountPath::Virtual("allocations:checking".into())),
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
            .trim_start()
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
                    expression: Some(Expression::Literal(Numeric::Positive(
                        "100".into(),
                        Some("00".into())
                    ))),
                    note: None,
                },
                Posting {
                    account: AccountPath::Real("assets:fake".into()),
                    expression: Some(Expression::Commodity(CommodityExpression {
                        quantity: Numeric::Positive("100".into(), Some("00".into())),
                        symbol: "BS".into(),
                        price: None,
                    })),
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
            .trim_start()
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
                    expression: Some(Expression::Literal(Numeric::Positive(
                        "100".into(),
                        Some("00".into())
                    ))),
                    note: None,
                },
                Posting {
                    account: AccountPath::Real("assets:fake".into()),
                    expression: Some(Expression::Commodity(CommodityExpression {
                        quantity: Numeric::Positive("100".into(), Some("00".into())),
                        symbol: "BS".into(),
                        price: Some(Numeric::Positive("10".into(), Some("00".into())))
                    })),
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
        vec![
            Node::EmptyLine,
            Node::AutomaticTransaction(AutomaticTransaction {
                condition: "assets:savings:ktc".into(),
                notes: vec![],
                postings: vec![
                    Posting {
                        account: AccountPath::Virtual("allocations:checking:savings:main".into()),
                        expression: Some(Expression::Factor((true, 1))),
                        note: None,
                    },
                    Posting {
                        account: AccountPath::Virtual("assets:checking:reserved".into()),
                        expression: Some(Expression::Factor((false, 1))),
                        note: None,
                    },
                ]
            }),
        ]
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
            .trim_start()
        )?,
        vec![Node::AutomaticTransaction(AutomaticTransaction {
            condition: "assets:savings:ktc".into(),
            notes: vec![":automatic:".into()],
            postings: vec![
                Posting {
                    account: AccountPath::Virtual("allocations:checking:savings:main".into()),
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
