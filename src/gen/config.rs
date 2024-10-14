use anyhow::Result;
use regex::Regex;
use serde::Deserialize;
use std::{fs::File, path::Path};

use crate::model::Transaction;

#[derive(Debug, Deserialize)]
#[allow(dead_code)]
pub struct Configuration {
    pub names: Names,
    pub income: Vec<IncomeDefinition>,
    pub envelopes: Vec<EnvelopeDefinition>,
    pub overdraft: Vec<OverdraftDefinition>,
    pub spending: Vec<SpendingDefinition>,
    pub emergency: Vec<EmergencyDefinition>,
    pub refund: Vec<RefundDefinition>,
    pub checks: Vec<CheckDefinition>,
}

impl Configuration {
    pub fn load(path: &Path) -> Result<Configuration> {
        let file = File::open(path)?;
        Ok(serde_json::from_reader(file)?)
    }
}

#[derive(Debug, Deserialize, Clone)]
#[serde(rename_all = "camelCase")]
pub enum CheckDefinition {
    Balanced(String, String),
}

#[derive(Debug, Deserialize, Clone)]
#[allow(dead_code)]
pub struct Names {
    pub available: String,
    pub refunded: String,
    pub emergency: String,
    pub taxes: String,
    pub reserved: String,
    pub early: String,
}

impl Names {
    pub fn get_envelope_source(&self, tx: &Transaction) -> Option<String> {
        for p in tx.postings.iter() {
            if p.account.as_str() == "assets:checking" {
                return Some("assets:checking:reserved".to_owned());
            }
            if p.account.as_str().starts_with("liabilities:cards:") {
                return Some(
                    "allocations:checking:".to_owned() + &p.account.as_str().replace("cards:", ""),
                );
            }
        }

        None
    }
}

#[derive(Debug, Deserialize, Clone)]
pub struct IncomeDefinition {
    #[serde(with = "serde_regex")]
    path: regex::Regex,
    handler: IncomeHandler,
}

#[derive(Debug, Deserialize, Clone)]
struct IncomeHandler {
    income: InnerIncome,
    #[allow(dead_code)]
    path: String,
}

#[derive(Debug, Deserialize, Clone)]
struct InnerIncome {
    name: String,
    #[allow(dead_code)]
    epoch: f32,
    factor: String,
}

impl IncomeDefinition {
    pub(crate) fn cycle(&self) -> super::pay::Cycle {
        match self.handler.income.factor.as_str() {
            "1" => super::pay::Cycle::Monthly,
            "2" => super::pay::Cycle::Bimonthly,
            _ => panic!(),
        }
    }

    pub(crate) fn name(&self) -> &str {
        &self.handler.income.name
    }

    pub(crate) fn regex(&self) -> &Regex {
        &self.path
    }
}

#[derive(Debug, Deserialize, Clone)]
pub struct OverdraftDefinition {
    #[allow(dead_code)]
    path: String,
}

#[derive(Debug, Deserialize, Clone)]
pub struct SpendingDefinition {
    #[serde(with = "serde_regex")]
    path: regex::Regex,
}

impl SpendingDefinition {
    pub fn regex(&self) -> &Regex {
        &self.path
    }
}

#[derive(Debug, Deserialize, Clone)]
pub struct EmergencyDefinition {
    #[allow(dead_code)]
    #[serde(with = "serde_regex")]
    path: regex::Regex,
}

#[derive(Debug, Deserialize, Clone)]
pub struct RefundDefinition {
    #[serde(with = "serde_regex")]
    path: regex::Regex,
}

impl RefundDefinition {
    pub fn regex(&self) -> &Regex {
        &self.path
    }
}

#[derive(Debug, Deserialize, Clone)]
pub struct EnvelopeDefinition {
    #[serde(with = "serde_regex")]
    expense: regex::Regex,
    pub name: String,
    pub enabled: bool,
    #[serde(default)]
    pub simple: bool,
    #[serde(default)]
    #[allow(dead_code)]
    pub debug: bool,
}

impl EnvelopeDefinition {
    pub fn regex(&self) -> &Regex {
        &self.expense
    }
}
