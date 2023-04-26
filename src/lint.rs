use std::collections::HashMap;

use clap::Args;
use itertools::Itertools;
use regex::Regex;
use tracing::info;

use crate::model::{HasNotes, LedgerFile, Node, Posting};

#[derive(Debug, Args)]
pub struct Command {}

trait LintCheck {
    fn check_node(&mut self, node: &Node) -> anyhow::Result<()>;
    fn done(&mut self) -> anyhow::Result<()>;
}

#[derive(Debug, Default)]
struct RequireAccountDeclarationsCheck {
    accounts: HashMap<String, bool>,
}

impl RequireAccountDeclarationsCheck {
    fn check_postings(&mut self, postings: &[Posting]) -> anyhow::Result<()> {
        for ap in postings.iter().map(|p| &p.account) {
            if !self.accounts.contains_key(ap.as_str()) {
                return Err(anyhow::anyhow!("Unknown Account: {}", ap.as_str()));
            }
        }

        Ok(())
    }
}

impl LintCheck for RequireAccountDeclarationsCheck {
    fn check_node(&mut self, node: &Node) -> anyhow::Result<()> {
        match node {
            Node::AccountDeclaration(ap) => {
                self.accounts.insert(ap.as_str().to_owned(), true);

                Ok(())
            }
            Node::Transaction(tx) => self.check_postings(&tx.postings),
            Node::AutomaticTransaction(tx) => self.check_postings(&tx.postings),
            _ => Ok(()),
        }
    }

    fn done(&mut self) -> anyhow::Result<()> {
        Ok(())
    }
}

#[derive(Debug, Default)]
struct RequireTagDeclarationCheck {
    tags: HashMap<String, bool>,
}

impl RequireTagDeclarationCheck {
    fn check_notes<'a, I>(&self, iter: I) -> anyhow::Result<()>
    where
        I: Iterator<Item = &'a String>, // How can we make this &str?
    {
        for note in iter {
            for tag in find_note_tags(note) {
                if !self.tags.contains_key(tag.as_str()) {
                    return Err(anyhow::anyhow!("Unknown Tag: {}", tag));
                }
            }
        }

        Ok(())
    }
}

impl LintCheck for RequireTagDeclarationCheck {
    fn check_node(&mut self, node: &Node) -> anyhow::Result<()> {
        match node {
            Node::TagDeclaration(name) => {
                self.tags.insert(name.as_str().to_owned(), true);

                Ok(())
            }
            Node::Transaction(tx) => Ok(self.check_notes(tx.iter_notes().into_iter())?),
            Node::AutomaticTransaction(tx) => Ok(self.check_notes(tx.iter_notes().into_iter())?),
            _ => Ok(()),
        }
    }

    fn done(&mut self) -> anyhow::Result<()> {
        Ok(())
    }
}

#[derive(Debug, Default)]
struct UnusedAccountDeclaration {
    unused: HashMap<String, bool>,
}

impl UnusedAccountDeclaration {
    fn check_postings(&mut self, postings: &[Posting]) -> Result<(), anyhow::Error> {
        for ap in postings.iter().map(|p| &p.account) {
            if self.unused.contains_key(ap.as_str()) {
                self.unused.remove(ap.as_str());
            }
        }

        Ok(())
    }
}

impl LintCheck for UnusedAccountDeclaration {
    fn check_node(&mut self, node: &Node) -> anyhow::Result<()> {
        match node {
            Node::AccountDeclaration(ap) => {
                self.unused.insert(ap.as_str().to_owned(), true);

                Ok(())
            }
            Node::Transaction(tx) => self.check_postings(&tx.postings),
            Node::AutomaticTransaction(tx) => self.check_postings(&tx.postings),
            _ => Ok(()),
        }
    }

    fn done(&mut self) -> anyhow::Result<()> {
        for key in self.unused.keys().sorted() {
            info!("Unused {}", key);
        }
        Ok(())
    }
}

pub fn execute_command(file: &LedgerFile, _cmd: &Command) -> anyhow::Result<()> {
    let mut checks: Vec<Box<dyn LintCheck>> = vec![
        Box::<RequireAccountDeclarationsCheck>::default(),
        Box::<RequireTagDeclarationCheck>::default(),
        Box::<UnusedAccountDeclaration>::default(),
    ];

    for node in file.recursive_iter() {
        for check in checks.iter_mut() {
            check.check_node(node)?;
        }
    }

    for check in checks.iter_mut() {
        check.done()?;
    }

    Ok(())
}

fn find_note_tags(note: &str) -> Vec<String> {
    use lazy_static::lazy_static;
    lazy_static! {
        static ref PLAIN: Regex = Regex::new(r"(:?:([a-z-]+))").unwrap();
        static ref TAGGED: Regex = Regex::new(r"(:?:([\w-]+)=(:?[^\s:=]+))*$").unwrap();
    }

    PLAIN
        .captures_iter(note)
        .chain(TAGGED.captures_iter(note))
        .filter_map(|c| c.get(2))
        .map(|m| m.as_str().to_owned())
        .unique()
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_find_note_tags_empty() {
        let empty: Vec<String> = Vec::new();
        assert_eq!(find_note_tags(""), empty);
    }

    #[test]
    fn test_find_note_tags_none_regular_sentence() {
        let empty: Vec<String> = Vec::new();
        assert_eq!(find_note_tags("hello, world! no tags here, no way."), empty);
    }

    #[test]
    fn test_find_note_tags_one_simple() {
        assert_eq!(find_note_tags(":tag:"), vec!["tag".to_owned()]);
    }

    #[test]
    fn test_find_note_tags_two_simple() {
        assert_eq!(
            find_note_tags(":something:another"),
            vec!["something".to_owned(), "another".to_owned()]
        );
    }

    #[test]
    fn test_find_note_tags_assigned() {
        assert_eq!(
            find_note_tags(":something=another"),
            vec!["something".to_owned()]
        );
    }
}
