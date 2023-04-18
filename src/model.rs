use anyhow::anyhow;
use serde::{ser::SerializeStruct, Serialize};
use std::{
    path::{Path, PathBuf},
    str::FromStr,
    sync::atomic::AtomicU64,
    time::Instant,
};
use tracing::{debug, info, span, Level};

pub use anyhow::Result;
pub use bigdecimal::{BigDecimal, Zero};
pub use chrono::NaiveDate;
pub use itertools::Itertools;

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

impl std::fmt::Display for AccountPath {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            AccountPath::Real(name) => f.pad(name),
            AccountPath::Virtual(name) => f.pad(&format!("[{}]", name)),
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum Numeric {
    Negative(String, Option<String>),
    Positive(String, Option<String>),
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
                BigDecimal::from_str(&format!("{}", a)).expect("Malformed Numeric")
            }
        }
    }

    pub fn to_text_format(&self, symbol: &str) -> String {
        match self {
            Numeric::Negative(a, Some(b)) => format!("-{}{}.{:02}", symbol, a, b),
            Numeric::Positive(a, Some(b)) => format!("{}{}.{:02}", symbol, a, b),
            Numeric::Negative(a, None) => format!("-{}{}", symbol, a),
            Numeric::Positive(a, None) => format!("{}{}", symbol, a),
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct CommodityExpression {
    pub quantity: Numeric,
    pub symbol: String,
    pub price: Option<Numeric>,
}

#[derive(Debug, PartialEq)]
pub enum Expression {
    Literal(Numeric),
    Commodity(CommodityExpression),
    Factor((bool, u64)),
    Calculated(BigDecimal),
}

impl std::fmt::Display for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expression::Literal(numeric) => f.pad(&format!("{}", numeric.to_text_format("$"))),
            Expression::Commodity(c) => match c {
                CommodityExpression {
                    quantity,
                    symbol,
                    price: Some(price),
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
                } => f.pad(&format!("{} {}", quantity.to_text_format(""), symbol)),
            },
            Expression::Factor(n) => match &n {
                (true, i) => f.pad(&format!("(-{})", i)),
                (false, i) => f.pad(&format!("({})", i)),
            },
            Expression::Calculated(_) => f.pad(""),
        }
    }
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

    pub fn iter_postings_for<'a>(&'a self, name: &'a str) -> impl Iterator<Item = &Posting> + 'a {
        self.postings
            .iter()
            .filter(move |p| p.account.as_str() == name)
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

    pub(crate) fn into_with_mid(self, mid: String) -> Self {
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
        self.expression.is_some()
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
            Some(Expression::Commodity(c)) => match &c.price {
                Some(price) => Some(c.quantity.to_decimal() * price.to_decimal()),
                None => None,
            },
            _ => None,
        }
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
    Included(String, Vec<Node>),
    Generated(Vec<Node>),
    DefaultCommodity(String),
    CommodityPrice(CommodityPrice),
    CommodityDeclaration(String),
    AutomaticTransaction(AutomaticTransaction),
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
    pub fn parse(path: &Path) -> Result<Self> {
        info!("parsing {:?}", path);

        let data = std::fs::read_to_string(path)?;
        let nodes = crate::parsing::parse_str(&data)?;

        Ok(LedgerFile {
            path: path.to_owned(),
            nodes,
        })
    }

    pub fn name(&self) -> Option<&str> {
        match self.path.file_name() {
            Some(name) => name.to_str(),
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
        info!("done in {:?}ms", elapsed);

        Ok(Self {
            path: self.path,
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
            .map_or(Err(anyhow!("Unfriendly path")), Ok)?
            .split('.')
            .next()
            .map_or(Err(anyhow!("Expected extension on path")), Ok)?
            .to_ascii_uppercase();

        let mut new_mid = mid_factory(&name);

        let nodes = self
            .nodes
            .into_iter()
            .map(|node| match node {
                Node::Transaction(tx) => Ok(Node::Transaction(
                    tx.into_with_mid(new_mid()).into_balanced()?,
                )),
                Node::Include(include_path_or_glob) => Ok(Node::Included(
                    include_path_or_glob.clone(),
                    include_glob(
                        relative
                            .join(&include_path_or_glob)
                            .to_str()
                            .ok_or(anyhow!("Unfriendly path"))?,
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
        txs.sort_unstable_by_key(|i| (i.date, &i.payee));
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

    pub fn sorted_nodes_iter(&self) -> impl Iterator<Item = &Node> {
        let mapped = self.nodes_iter().scan((NaiveDate::MIN, 0), |acc, node| {
            let node_date: Option<NaiveDate> = match node {
                Node::Transaction(tx) => Some(tx.date),
                Node::CommodityPrice(p) => Some(p.date),
                _ => None,
            };

            *acc = node_date.map_or((acc.0, acc.1 + 1), |d| {
                if d == acc.0 {
                    (acc.0, acc.1 + 1)
                } else {
                    (d, acc.1 + 1)
                }
            });

            Some((acc.0, acc.1, node))
        });

        let mut sortable: Vec<_> = mapped.collect();
        sortable.sort_unstable_by_key(|i| (i.0, i.1));
        sortable.into_iter().map(|i| i.2)
    }
}

fn include_glob(path: &str) -> Result<Vec<Node>> {
    use glob::glob;

    let files = glob(path)?
        .map(|entry| LedgerFile::parse(&entry?)?.preprocess())
        .collect::<Result<Vec<_>>>()?;

    let nodes = files
        .into_iter()
        .map(|file| Node::Included(path.to_owned(), file.nodes))
        .collect();

    Ok(nodes)
}
