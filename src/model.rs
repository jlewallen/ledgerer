use anyhow::anyhow;
use bigdecimal::Signed;
#[allow(unused_imports)]
use bigdecimal::ToPrimitive;
use serde::{ser::SerializeStruct, Serialize};
use std::{
    collections::HashMap,
    ops::Neg,
    path::{Path, PathBuf},
    rc::Rc,
    str::FromStr,
    sync::atomic::AtomicU64,
    time::Instant,
};
use thiserror::Error;
use tracing::{debug, info, span, Level};

pub use anyhow::Result;
pub use bigdecimal::{BigDecimal, Zero};
pub use chrono::NaiveDate;
pub use itertools::Itertools;

#[derive(Debug, Clone)]
pub enum AccountPath {
    Real(String),
    Virtual(String),
}

impl PartialEq for AccountPath {
    fn eq(&self, other: &Self) -> bool {
        self.as_str() == other.as_str()
    }
}

impl AccountPath {
    pub fn as_str(&self) -> &str {
        match self {
            AccountPath::Real(p) | AccountPath::Virtual(p) => p,
        }
    }

    pub fn is_real(&self) -> bool {
        match self {
            AccountPath::Real(_) => true,
            AccountPath::Virtual(_) => false,
        }
    }

    pub fn is_virtual(&self) -> bool {
        match self {
            AccountPath::Real(_) => false,
            AccountPath::Virtual(_) => true,
        }
    }
}

impl From<&str> for AccountPath {
    fn from(value: &str) -> Self {
        if value.starts_with("allocations") || value.ends_with("reserved") {
            Self::Virtual(value.to_owned())
        } else {
            Self::Real(value.to_owned())
        }
    }
}

impl std::fmt::Display for AccountPath {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            AccountPath::Real(name) => f.pad(name),
            AccountPath::Virtual(name) => f.pad(&format!("[{}]", name)),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Numeric {
    Negative(String, Option<String>),
    Positive(String, Option<String>),
    Decimal(BigDecimal),
}

impl Numeric {
    pub fn to_decimal(&self) -> BigDecimal {
        match self {
            Numeric::Negative(a, Some(b)) => {
                BigDecimal::from_str(&format!("-{}.{}", a, b)).expect("Malformed Numeric")
            }
            Numeric::Positive(a, Some(b)) => {
                BigDecimal::from_str(&format!("{}.{}", a, b)).expect("Malformed Numeric")
            }
            Numeric::Negative(a, None) => {
                BigDecimal::from_str(&format!("-{}", a)).expect("Malformed Numeric")
            }
            Numeric::Positive(a, None) => {
                BigDecimal::from_str(&a.to_string()).expect("Malformed Numeric")
            }
            Numeric::Decimal(d) => d.clone(),
        }
    }

    pub fn to_text_format(&self, symbol: &str) -> String {
        match self {
            Numeric::Negative(a, Some(b)) => format!("-{}{}.{:02}", symbol, a, b),
            Numeric::Positive(a, Some(b)) => format!("{}{}.{:02}", symbol, a, b),
            Numeric::Negative(a, None) => format!("-{}{}", symbol, a),
            Numeric::Positive(a, None) => format!("{}{}", symbol, a),
            Numeric::Decimal(d) => {
                if d.is_negative() {
                    format!("-{}{}", symbol, d.abs())
                } else {
                    format!("{}{}", symbol, d)
                }
            }
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct CommodityExpression {
    pub quantity: Numeric,
    pub symbol: String,
    pub price: Option<Numeric>,
    pub lot_price: Option<Numeric>,
    pub date: Option<NaiveDate>,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Expression {
    Literal(Numeric),
    Commodity(CommodityExpression),
    Factor((bool, u64)),
    Calculated(BigDecimal),
}

impl Expression {
    pub fn to_decimal(&self) -> Option<BigDecimal> {
        match self {
            Expression::Literal(numeric) => Some(numeric.to_decimal()),
            Expression::Calculated(value) => Some(value.clone()),
            Expression::Commodity(c) => c
                .price
                .as_ref()
                .map(|price| c.quantity.to_decimal() * price.to_decimal()),
            Expression::Factor(_) => None,
        }
    }

    pub fn into_balance(&self) -> Option<Balance> {
        match self {
            Expression::Literal(numeric) => Some(Balance::currency("$", numeric.to_decimal())), // TODO This could be better.
            Expression::Calculated(decimal) => Some(Balance::currency("$", decimal.clone())),
            Expression::Commodity(c) => Some(Balance::commodity(
                &c.symbol,
                c.quantity.to_decimal(),
                c.price.as_ref().map(|p| p.to_decimal()),
            )),
            Expression::Factor(_) => None,
        }
    }
}

impl std::fmt::Display for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expression::Literal(numeric) => f.pad(&numeric.to_text_format("$")),
            Expression::Commodity(c) => match c {
                CommodityExpression {
                    quantity,
                    symbol,
                    price: Some(price),
                    lot_price: _,
                    date: None,
                } => f.pad(&format!(
                    "{} {} @ {}",
                    quantity.to_text_format(""),
                    symbol,
                    price.to_text_format("$")
                )),
                CommodityExpression {
                    quantity,
                    symbol,
                    price: None,
                    lot_price: _,
                    date: None,
                } => f.pad(&format!("{} {}", quantity.to_text_format(""), symbol)),
                CommodityExpression {
                    quantity,
                    symbol,
                    price: Some(price),
                    lot_price: _,
                    date: Some(date),
                } => f.pad(&format!(
                    "{} {} @ {} [{}]",
                    quantity.to_text_format(""),
                    symbol,
                    price.to_text_format("$"),
                    date.format("%Y/%m/%d")
                )),
                CommodityExpression {
                    quantity: _,
                    symbol: _,
                    price: None,
                    lot_price: _,
                    date: Some(_),
                } => unimplemented!(),
            },
            Expression::Factor(n) => match &n {
                (true, i) => f.pad(&format!("(-{})", i)),
                (false, i) => f.pad(&format!("({})", i)),
            },
            Expression::Calculated(_) => f.pad(""),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum ParsedDate {
    Full(NaiveDate),
    MonthDay(u32, u32),
    QualifiedMonthDay(NaiveDate),
}

impl From<NaiveDate> for ParsedDate {
    fn from(value: NaiveDate) -> Self {
        Self::Full(value)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct ParsedTransaction {
    pub date: ParsedDate,
    pub payee: String,
    pub cleared: bool,
    pub postings: Vec<Posting>,
    pub notes: Vec<String>,
    pub mid: Option<String>,
    pub order: Option<u64>, // Torn on this, need a way to maintain "file order"
    pub origin: Option<Origin>,
    pub refs: Vec<String>,
}

impl From<Transaction> for ParsedTransaction {
    fn from(value: Transaction) -> Self {
        Self {
            date: value.date,
            payee: value.payee,
            cleared: value.cleared,
            postings: value.postings,
            notes: value.notes,
            mid: value.mid,
            order: value.order,
            origin: value.origin,
            refs: value.refs,
        }
    }
}

#[derive(Error, Debug)]
pub enum TransactionError {
    #[error("no year for month and day date")]
    NoYear,
    #[error("bad date")]
    BadDate,
}

impl Transaction {
    pub fn qualify_year(self, year: Option<i32>) -> Result<Transaction, TransactionError> {
        Ok(Transaction {
            date: match self.date {
                ParsedDate::Full(date) => ParsedDate::Full(date),
                ParsedDate::QualifiedMonthDay(date) => ParsedDate::QualifiedMonthDay(date),
                ParsedDate::MonthDay(month, day) => match year {
                    Some(year) => ParsedDate::QualifiedMonthDay(
                        NaiveDate::from_ymd_opt(year, month, day)
                            .ok_or(TransactionError::BadDate)?,
                    ),
                    None => Err(TransactionError::NoYear)?,
                },
            },
            payee: self.payee,
            cleared: self.cleared,
            postings: self.postings,
            notes: self.notes,
            mid: self.mid,
            order: self.order,
            origin: self.origin,
            refs: self.refs,
        })
    }

    pub fn short_string(&self) -> String {
        let postings = self
            .postings
            .iter()
            .sorted_by_key(|p| p.has_value())
            .map(|p| p.account.as_str())
            .join(", ");
        format!("Tx('{}' <{}>)", &self.payee, postings)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Transaction {
    pub date: ParsedDate,
    pub payee: String,
    pub cleared: bool,
    pub postings: Vec<Posting>,
    pub notes: Vec<String>,
    pub mid: Option<String>,
    pub order: Option<u64>, // Torn on this, need a way to maintain "file order"
    pub origin: Option<Origin>,
    pub refs: Vec<String>,
}

#[derive(Debug, PartialEq, Eq, Clone, PartialOrd, Ord, Serialize)]
#[serde(rename_all = "camelCase")]
pub enum Origin {
    File,
    Automatic,
    Generated,
}

impl Serialize for Transaction {
    fn serialize<S>(&self, serializer: S) -> std::result::Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        let mut state = serializer.serialize_struct("Transaction", 6)?;
        state.serialize_field("date", &format!("{:?}T00:00:00", self.date()))?;
        state.serialize_field("payee", &self.payee)?;
        state.serialize_field("cleared", &self.cleared)?;
        state.serialize_field("mid", &self.mid)?;
        state.serialize_field("notes", &self.notes)?;
        state.serialize_field("postings", &self.postings)?;
        state.serialize_field("order", &self.order)?;
        state.serialize_field("origin", &self.origin)?;
        state.serialize_field("refs", &self.refs)?;
        state.end()
    }
}

impl Transaction {
    pub fn date(&self) -> &NaiveDate {
        match &self.date {
            ParsedDate::Full(date) => date,
            ParsedDate::QualifiedMonthDay(date) => date,
            ParsedDate::MonthDay(_, _) => todo!(),
        }
    }

    pub fn is_simple(&self) -> bool {
        !self.postings.iter().any(|p| p.has_value().is_none())
    }

    pub fn magnitude(&self) -> Option<BigDecimal> {
        if self.is_simple() {
            self.postings
                .iter()
                .flat_map(|p| p.has_value().map(|d| d.abs()))
                .next()
        } else {
            None
        }
    }

    pub fn is_generated(&self) -> bool {
        self.notes.iter().any(|n| n.contains(":generated:"))
    }

    pub fn specific_order(&self) -> Option<usize> {
        self.notes
            .iter()
            .flat_map(|n| match n.split_once(":order:") {
                Some((_, order)) => order.parse().ok(),
                None => None,
            })
            .next()
    }

    pub fn has_posting_for(&self, name: &str) -> bool {
        self.postings.iter().any(|p| p.account.as_str() == name)
    }

    pub fn has_posting_for_re(&self, re: &regex::Regex) -> bool {
        self.postings
            .iter()
            .any(|p| re.is_match(p.account.as_str()))
    }

    pub fn iter_postings_for<'a>(
        &'a self,
        name: &'a str,
    ) -> impl Iterator<Item = &'a Posting> + 'a {
        self.postings
            .iter()
            .filter(move |p| p.account.as_str() == name)
    }

    pub fn iter_postings_for_re<'a, 'b: 'a>(
        &'a self,
        re: &'b regex::Regex,
    ) -> impl Iterator<Item = &'a Posting> + 'a {
        self.postings
            .iter()
            .filter(move |p| re.is_match(p.account.as_str()))
    }

    pub(crate) fn into_balanced(self) -> Result<Self> {
        if self.postings.iter().any(|p| p.expression.is_none()) {
            let total: BigDecimal = self
                .postings
                .iter()
                .map(|p| p.has_value().unwrap_or(BigDecimal::zero()))
                .collect::<Vec<_>>()
                .into_iter()
                .sum();

            let mut postings = self.postings;

            for posting in postings.iter_mut() {
                if posting.expression.is_none() {
                    posting.expression = Some(Expression::Calculated(-total.clone()))
                }
            }

            Ok(Self {
                date: self.date,
                payee: self.payee,
                cleared: self.cleared,
                mid: self.mid,
                order: self.order,
                origin: self.origin,
                notes: self.notes,
                refs: self.refs,
                postings,
            })
        } else {
            let total: BigDecimal = self
                .postings
                .iter()
                .map(|p| p.has_value().unwrap_or(BigDecimal::zero()))
                .collect::<Vec<_>>()
                .into_iter()
                .sum();

            if !total.is_zero() {
                Err(anyhow!(
                    "Unbalanced transaction: {} '{}' ~ {}!",
                    self.date(),
                    self.payee,
                    total
                ))
            } else {
                Ok(self)
            }
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
            order: self.order,
            origin: self.origin,
            refs: self.refs,
        }
    }

    fn into_with_order(self, order: u64) -> Self {
        Self {
            date: self.date,
            payee: self.payee,
            cleared: self.cleared,
            notes: self.notes,
            postings: self.postings,
            mid: self.mid,
            order: Some(order),
            origin: self.origin,
            refs: self.refs,
        }
    }

    fn into_with_origin(self, origin: Option<Origin>) -> Self {
        Self {
            date: self.date,
            payee: self.payee,
            cleared: self.cleared,
            notes: self.notes,
            postings: self.postings,
            mid: self.mid,
            order: self.order,
            origin: origin.or(self.origin),
            refs: self.refs,
        }
    }

    fn infer_origin_override(self) -> Self {
        if self.is_generated() {
            self.into_with_origin(Some(Origin::Generated))
        } else {
            self
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Posting {
    pub account: AccountPath,
    pub expression: Option<Expression>,
    pub note: Option<String>,
}

impl Posting {
    pub fn has_expression(&self) -> bool {
        self.expression.is_some()
    }

    pub fn has_literal(&self) -> Option<&Numeric> {
        match &self.expression {
            Some(Expression::Literal(numeric)) => Some(numeric),
            _ => None,
        }
    }

    pub fn has_value(&self) -> Option<BigDecimal> {
        self.expression.as_ref().and_then(|e| e.to_decimal())
    }

    pub fn into_balance(&self) -> Option<Balance> {
        self.expression.as_ref().and_then(|e| e.into_balance())
    }

    pub(crate) fn only_positive(&self) -> Option<BigDecimal> {
        self.has_value().filter(|v| v.is_positive())
    }

    pub(crate) fn only_negative(&self) -> Option<BigDecimal> {
        self.has_value().filter(|v| v.is_negative())
    }
}

impl Serialize for Posting {
    fn serialize<S>(&self, serializer: S) -> std::result::Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        let value = self
            .has_value()
            .map(|value| format!("{}", value.with_scale(2)));
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
                    date: tx.date.clone(),
                    payee: tx.payee.clone(),
                    cleared: tx.cleared,
                    notes: self.notes.clone(),
                    refs: vec![tx.mid.clone().unwrap()],
                    postings: self.postings_with_value(value),
                    origin: Some(Origin::Automatic),
                    order: tx.order,
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

pub trait HasNotes {
    fn iter_notes(&self) -> Vec<&String>;
}

impl HasNotes for Transaction {
    fn iter_notes(&self) -> Vec<&String> {
        self.notes
            .iter()
            .chain(self.postings.iter().filter_map(|p| p.note.as_ref()))
            .collect_vec()
    }
}

impl HasNotes for AutomaticTransaction {
    fn iter_notes(&self) -> Vec<&String> {
        self.notes
            .iter()
            .chain(self.postings.iter().filter_map(|p| p.note.as_ref()))
            .collect_vec()
    }
}

#[derive(Debug, PartialEq)]
pub struct DatedPrice {
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
    Included(String, Vec<Node>),
    Generated(Vec<Node>),
    DefaultCommodity(String),
    DatedPrice(DatedPrice),
    CommodityDeclaration(String),
    AutomaticTransaction(AutomaticTransaction),
    Year(i32),
    EmptyLine,
}

fn mid_factory(prefix: &str) -> impl FnMut() -> String + '_ {
    use std::sync::atomic::Ordering::SeqCst;
    let counter = AtomicU64::new(1);
    move || format!("{}-{}", prefix, counter.fetch_add(1, SeqCst))
}

#[derive(Debug)]
pub struct LedgerFile {
    path: PathBuf,
    nodes: Vec<Node>,
}

impl LedgerFile {
    pub fn new(path: PathBuf, nodes: Vec<Node>) -> Self {
        Self { path, nodes }
    }

    pub fn parse(path: &Path) -> Result<Self> {
        info!("parsing {:?}", path);

        let data = std::fs::read_to_string(path)?;

        Self::parse_text(data, path.to_owned())
    }

    pub fn parse_text(text: String, path: PathBuf) -> Result<Self> {
        let nodes = crate::parsing::parse_str(&text)?;

        Ok(LedgerFile { path, nodes })
    }

    pub fn name(&self) -> Option<&str> {
        match self.path.file_name() {
            Some(name) => name.to_str(),
            None => None,
        }
    }

    pub fn declared_accounts(&self) -> HashMap<String, AccountPath> {
        let mut accounts = HashMap::new();
        for node in self.recursive_iter() {
            match node {
                Node::AccountDeclaration(ap) => accounts.insert(ap.as_str().to_owned(), ap.clone()),
                _ => None,
            };
        }
        accounts
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

        let mut new_mid = mid_factory("AUTOMATIC");

        let nodes = self
            .nodes
            .into_iter()
            .chain(vec![Node::Generated(
                generated
                    .into_iter()
                    .map(|tx| Node::Transaction(tx.into_with_mid(new_mid())))
                    .collect_vec(),
            )])
            .collect_vec();

        let elapsed = Instant::now() - started;
        info!("done in {:?}", elapsed);

        Ok(Self {
            path: self.path,
            nodes,
        })
    }

    pub fn preprocess(self, order: Rc<AtomicU64>) -> Result<LedgerFile> {
        debug!("preprocessing {:?}", self.path);

        let relative = self
            .path
            .parent()
            .ok_or(anyhow!("Expected parent directory: {:?}", self.path))?;

        let name = self
            .name()
            .map_or(Err(anyhow!("Unfriendly path")), Ok)?
            .split('.')
            .next()
            .map_or(Err(anyhow!("Expected extension on path")), Ok)?
            .to_ascii_uppercase();

        let mut new_mid = mid_factory(&name);
        let our_order = order.fetch_add(1, std::sync::atomic::Ordering::SeqCst);
        let mut current_year = None;

        let nodes = self
            .nodes
            .into_iter()
            .map(|node| match node {
                Node::Year(year) => {
                    current_year = Some(year);
                    Ok(node)
                }
                Node::Transaction(tx) => Ok(Node::Transaction(
                    tx.qualify_year(current_year)?
                        .into_with_mid(new_mid())
                        .into_with_order(our_order)
                        .infer_origin_override()
                        .into_balanced()?,
                )),
                Node::Include(include_path_or_glob) => Ok(Node::Included(
                    include_path_or_glob.clone(),
                    include_glob(
                        relative
                            .join(&include_path_or_glob)
                            .to_str()
                            .ok_or(anyhow!("Unfriendly path"))?,
                        order.clone(),
                    )?,
                )),
                _ => Ok(node),
            })
            .collect::<Result<Vec<_>>>()?;

        Ok(Self {
            path: self.path,
            nodes,
        })
    }

    pub fn iter_transactions_in_order(&self) -> impl Iterator<Item = &Transaction> {
        let mut txs: Vec<&Transaction> = self.iter_transactions().collect();
        txs.sort_by_key(|i| (i.date(), i.order, i.specific_order(), &i.origin, &i.payee));
        txs.into_iter()
    }

    pub fn iter_transactions(&self) -> impl Iterator<Item = &Transaction> {
        self.recursive_iter().filter_map(|node| match node {
            Node::Transaction(tx) => Some(tx),
            _ => None,
        })
    }

    pub fn iter_automatic_transactions(&self) -> impl Iterator<Item = &AutomaticTransaction> {
        self.recursive_iter().filter_map(|node| match node {
            Node::AutomaticTransaction(tx) => Some(tx),
            _ => None,
        })
    }

    pub fn recursive_iter(&self) -> impl Iterator<Item = &Node> {
        fn recursively_iter(nodes: &[Node]) -> Box<dyn Iterator<Item = &Node> + '_> {
            Box::new(nodes.iter().flat_map(|node| match node {
                Node::Included(_, children) | Node::Generated(children) => {
                    recursively_iter(children)
                }
                _ => Box::new(std::iter::once(node)),
            }))
        }

        recursively_iter(&self.nodes)
    }

    pub fn nodes_iter(&self) -> impl Iterator<Item = &Node> {
        self.nodes.iter()
    }

    pub fn sortable_nodes_iter(&self) -> impl Iterator<Item = SortedNode> {
        sortable_nodes(self.nodes_iter())
    }

    pub fn get_prices(&self) -> Result<Prices> {
        let prices: HashMap<String, Vec<(NaiveDate, BigDecimal)>> = self
            .recursive_iter()
            .filter_map(|n| match n {
                Node::DatedPrice(dp) => Some(dp),
                _ => None,
            })
            .map(|dp| {
                (
                    dp.symbol.clone(),
                    (dp.date, dp.expression.to_decimal().unwrap()),
                )
            })
            .sorted()
            .fold(HashMap::new(), |mut acc, dp| {
                match acc.get_mut(&dp.0) {
                    Some(entry) => entry.push(dp.1),
                    None => {
                        acc.insert(dp.0.clone(), vec![dp.1]);
                    }
                };

                acc
            });

        Ok(Prices { prices })
    }
}

#[derive(Debug, Default)]
pub struct Prices {
    prices: HashMap<String, Vec<(NaiveDate, BigDecimal)>>,
}

impl Prices {
    pub fn get_prices(&self, date: NaiveDate) -> Self {
        Self {
            prices: self
                .prices
                .iter()
                .map(|(k, v)| {
                    (
                        k.clone(),
                        v.clone() // TODO
                            .into_iter()
                            .take_while(|p| p.0 < date)
                            .collect_vec(),
                    )
                })
                .collect(),
        }
    }

    pub fn get_price(&self, symbol: &str) -> Option<&BigDecimal> {
        match self.prices.get(symbol) {
            Some(timeline) => {
                let mut value = None;
                for (_, price) in timeline {
                    value = Some(price);
                }
                value
            }
            None => None,
        }
    }

    pub fn value_of(&self, symbol: &str, quantity: Option<BigDecimal>) -> Option<BigDecimal> {
        match quantity {
            Some(quantity) => self.get_price(symbol).map(|price| quantity * price),
            None => None,
        }
    }
}

pub struct SortedNode<'a>(NaiveDate, usize, &'a Node);

impl<'a> SortedNode<'a> {
    pub fn date(&self) -> &NaiveDate {
        &self.0
    }
}

pub fn sortable_nodes<'a>(
    i: impl Iterator<Item = &'a Node>,
) -> impl Iterator<Item = SortedNode<'a>> {
    i.scan((NaiveDate::MIN, 0), |acc, node| {
        let node_date: Option<NaiveDate> = match node {
            Node::Transaction(tx) => Some(*tx.date()),
            Node::DatedPrice(p) => Some(p.date),
            Node::Year(year) => NaiveDate::from_ymd_opt(*year, 1, 1),
            _ => None,
        };

        *acc = node_date.map_or((acc.0, acc.1 + 1), |d| {
            if d == acc.0 {
                (acc.0, acc.1 + 1)
            } else {
                (d, acc.1 + 1)
            }
        });

        Some(SortedNode(acc.0, acc.1, node))
    })
}

pub fn sort_nodes<'a>(i: impl Iterator<Item = SortedNode<'a>>) -> impl Iterator<Item = &'a Node> {
    let mut sortable: Vec<_> = i.collect();
    sortable.sort_unstable_by_key(|i| (i.0, i.1));
    sortable.into_iter().map(|i| i.2)
}

fn include_glob(path: &str, order: Rc<AtomicU64>) -> Result<Vec<Node>> {
    use glob::glob;

    let files = glob(path)?
        .map(|entry| LedgerFile::parse(&entry?)?.preprocess(order.clone()))
        .collect::<Result<Vec<_>>>()?;

    let nodes = files
        .into_iter()
        .map(|file| Node::Included(path.to_owned(), file.nodes))
        .collect();

    Ok(nodes)
}

#[derive(Debug, Clone)]
pub struct Lot {
    pub date: Option<NaiveDate>,
    pub quantity: BigDecimal,
    pub price: Option<BigDecimal>,
}

impl Lot {
    pub fn new(quantity: BigDecimal, price: Option<BigDecimal>) -> Self {
        Self {
            date: None,
            quantity,
            price,
        }
    }

    pub fn quantity(lots: &[Self]) -> Option<BigDecimal> {
        let quantity: BigDecimal = lots.iter().map(|l| l.quantity.clone()).sum();
        if quantity.is_zero() {
            None
        } else {
            Some(quantity)
        }
    }

    pub fn sum(lots: &[Self]) -> Self {
        if lots
            .iter()
            .any(|l| l.price.is_none() || l.price.as_ref().unwrap().is_zero())
        {
            panic!("Sorry, all lots are required to have prices!");
        }

        let quantity: BigDecimal = lots.iter().map(|l| l.quantity.clone()).sum();
        if quantity.is_zero() {
            Self {
                date: None,
                quantity,
                price: None,
            }
        } else {
            let price = lots
                .iter()
                .filter_map(|l| l.price.as_ref().map(|p| p * &l.quantity))
                .sum::<BigDecimal>()
                / quantity.clone();

            Self {
                date: None,
                quantity,
                price: Some(price.with_scale(2)),
            }
        }
    }

    fn is_zero(&self) -> bool {
        self.quantity.is_zero()
    }
}

#[derive(Debug, Clone)]
pub enum Balance {
    Currency { symbol: String, value: BigDecimal },
    Commodity { symbol: String, lots: Vec<Lot> },
}

impl Balance {
    pub fn currency(symbol: &str, value: BigDecimal) -> Self {
        Self::Currency {
            symbol: symbol.to_owned(),
            value,
        }
    }

    pub fn as_currency(&self) -> Option<BigDecimal> {
        match self {
            Balance::Currency {
                symbol: _symbol,
                value,
            } => Some(value.clone()),
            _ => None,
        }
    }

    pub fn commodity(symbol: &str, quantity: BigDecimal, price: Option<BigDecimal>) -> Self {
        Self::Commodity {
            symbol: symbol.to_owned(),
            lots: vec![Lot::new(quantity, price)],
        }
    }

    pub fn symbol(&self) -> &str {
        match self {
            Balance::Currency { symbol, value: _ } => symbol,
            Balance::Commodity { symbol, lots: _ } => symbol,
        }
    }

    // Cow on balance? Also this is ill-defined for Commodities and perhaps we
    // should error and require a filter step before?
    pub fn abs(&self) -> Balance {
        match self {
            Balance::Currency { symbol, value } => Balance::Currency {
                symbol: symbol.clone(),
                value: value.abs(),
            },
            _ => self.clone(),
        }
    }

    // Cow on balance? Also this is ill-defined for Commodities and perhaps we
    // should error and require a filter step before?
    pub fn neg(&self) -> Balance {
        match self {
            Balance::Currency { symbol, value } => Balance::Currency {
                symbol: symbol.clone(),
                value: value.neg(),
            },
            _ => self.clone(),
        }
    }

    pub fn is_zero(&self) -> bool {
        match self {
            Balance::Currency { symbol: _, value } => value.is_zero(),
            Balance::Commodity { symbol: _, lots } => Lot::sum(lots).is_zero(),
        }
    }

    pub fn value(&self) -> Option<f32> {
        match self {
            Balance::Currency { symbol: _, value } => value.to_f32(),
            Balance::Commodity { symbol: _, lots } => Lot::sum(lots).price.and_then(|p| p.to_f32()),
        }
    }
}

impl Serialize for Balance {
    fn serialize<S>(&self, serializer: S) -> std::result::Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        let mut state = serializer.serialize_struct("Balance", 2)?;
        state.serialize_field("symbol", self.symbol())?;
        state.serialize_field("display", &format!("{}", self))?;
        state.end()
    }
}

// We might be able to get away with creating an Expression and displaying that?
impl std::fmt::Display for Balance {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Balance::Currency { symbol, value } => match symbol.as_str() {
                "$" => f.pad(&format!("${}", value.with_scale(2))),
                _ => unimplemented!("Display for Currency '{}'", symbol),
            },
            Balance::Commodity { symbol, lots } => {
                let sum = Lot::sum(lots);
                match sum.price {
                    Some(price) => f.pad(&format!("{} {} @ ${}", sum.quantity, symbol, price)),
                    None => f.pad(&format!("{} {}", symbol, sum.quantity)),
                }
            }
        }
    }
}

#[derive(Debug, Default, Clone)]
pub struct Balances {
    by_symbol: HashMap<String, Balance>,
}

impl Balances {
    pub fn new_from_balance(balance: Balance) -> Self {
        Self {
            by_symbol: HashMap::from([(balance.symbol().to_owned(), balance)]),
        }
    }

    pub fn abs(&self) -> Self {
        let mut by_symbol = HashMap::new();
        for (key, value) in self.by_symbol.iter() {
            by_symbol.insert(key.clone(), value.abs());
        }
        Self { by_symbol }
    }

    pub fn iter(&self) -> impl Iterator<Item = &Balance> {
        self.by_symbol.values()
    }
}

impl std::ops::AddAssign<Balances> for Balances {
    fn add_assign(&mut self, rhs: Balances) {
        for (key, value) in rhs.by_symbol.iter() {
            if self.by_symbol.contains_key(key) {
                *self.by_symbol.get_mut(key).unwrap() += value;
            } else {
                self.by_symbol.insert(key.to_owned(), value.clone());
            }
        }
    }
}

impl std::ops::AddAssign<&Balance> for Balance {
    fn add_assign(&mut self, rhs: &Balance) {
        assert_eq!(self.symbol(), rhs.symbol());

        match (self, rhs) {
            (
                Balance::Currency { symbol: _, value },
                Balance::Currency {
                    symbol: _,
                    value: rhs,
                },
            ) => {
                *value += rhs;
            }
            (
                Balance::Commodity { symbol: _, lots },
                Balance::Commodity {
                    symbol: _,
                    lots: rhs,
                },
            ) => {
                lots.extend(rhs.clone());
            }
            _ => panic!("Bug: refusing to add Currency and Commodity"),
        }
    }
}
