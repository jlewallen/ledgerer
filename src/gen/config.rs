use anyhow::Result;
use regex::Regex;
use serde::Deserialize;
use std::fs::File;

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
}

impl Configuration {
    pub fn load(path: &str) -> Result<Configuration> {
        let file = File::open(path)?;
        Ok(serde_json::from_reader(file)?)
    }
}

#[derive(Debug, Deserialize)]
#[allow(dead_code)]
pub struct Names {
    available: String,
    refunded: String,
    emergency: String,
    taxes: String,
    reserved: String,
    early: String,
}

#[derive(Debug, Deserialize, Clone)]
#[allow(dead_code)]
pub struct IncomeDefinition {
    #[serde(with = "serde_regex")]
    path: regex::Regex,
    handler: IncomeHandler,
}

#[derive(Debug, Deserialize, Clone)]
#[allow(dead_code)]
struct IncomeHandler {
    income: InnerIncome,
    path: String,
}

#[derive(Debug, Deserialize, Clone)]
#[allow(dead_code)]
struct InnerIncome {
    name: String,
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
#[allow(dead_code)]
pub struct OverdraftDefinition {
    pub path: String,
}

#[derive(Debug, Deserialize, Clone)]
#[allow(dead_code)]
pub struct SpendingDefinition {
    #[serde(with = "serde_regex")]
    pub path: regex::Regex,
}

impl SpendingDefinition {
    pub fn regex(&self) -> &Regex {
        &self.path
    }
}

#[derive(Debug, Deserialize, Clone)]
#[allow(dead_code)]
pub struct EmergencyDefinition {
    #[serde(with = "serde_regex")]
    pub path: regex::Regex,
}

#[derive(Debug, Deserialize, Clone)]
#[allow(dead_code)]
pub struct RefundDefinition {
    #[serde(with = "serde_regex")]
    pub path: regex::Regex,
}

impl RefundDefinition {
    pub fn regex(&self) -> &Regex {
        &self.path
    }
}

#[derive(Debug, Deserialize, Clone)]
#[allow(dead_code)]
pub struct EnvelopeDefinition {
    pub name: String,
    #[serde(with = "serde_regex")]
    pub expense: regex::Regex,
    pub enabled: bool,
}

impl EnvelopeDefinition {
    pub fn regex(&self) -> &Regex {
        &self.expense
    }
}
