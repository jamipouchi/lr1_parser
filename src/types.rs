use std::collections::HashMap;

use crate::firsts::Firsts;

pub type Terminal = String;
pub fn eof() -> Terminal {
    Terminal::from("eof")
}

pub type NonTerminal = String;

#[derive(Eq, Hash, PartialEq, Debug, Clone, PartialOrd, Ord)]
pub enum Symbol {
    Terminal(Terminal),
    NonTerminal(NonTerminal),
}

#[derive(Debug)]
pub struct Grammar {
    pub productions: HashMap<NonTerminal, Vec<Vec<Symbol>>>,
    pub start: NonTerminal,
    pub firsts: Option<Firsts>,
}

#[derive(PartialEq, Eq)]
pub struct ASTNode {
    pub symbol: Symbol,
    pub children: Vec<ASTNode>,
}

impl std::fmt::Debug for ASTNode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "{:?} ", self.symbol)?;
        if !self.children.is_empty() {
            writeln!(f, "{{")?;
        }
        for child in &self.children {
            write!(f, "{:?}", child)?;
        }
        if !self.children.is_empty() {
            writeln!(f, "}}")?;
        }
        Ok(())
    }
}

pub trait Lexer {
    fn next(&mut self) -> Option<String>;
}

impl Grammar {
    pub fn parse(productions: Vec<(&str, Vec<&str>)>) -> Grammar {
        let grammar = Grammar::parse_productions(productions);
        let firsts = Firsts::new(&grammar);
        Grammar {
            firsts: Some(firsts),
            ..grammar
        }
    }
    // We assume that the first production is the start symbol
    fn parse_productions(productions: Vec<(&str, Vec<&str>)>) -> Grammar {
        let start = productions.first().unwrap().0.to_string();
        let independent_productions: Vec<(NonTerminal, Vec<Symbol>)> = productions
            .into_iter()
            .map(|(lhs, rhs)| {
                let lhs = lhs.to_string();
                let rhs: Vec<Symbol> = rhs
                    .into_iter()
                    .map(|s| {
                        if s.chars().next().unwrap().is_uppercase() {
                            Symbol::NonTerminal(s.to_string())
                        } else {
                            Symbol::Terminal(s.to_string())
                        }
                    })
                    .collect();
                (lhs, rhs)
            })
            .collect();
        let mut productions = HashMap::new();
        for (lhs, rhs) in independent_productions {
            productions.entry(lhs).or_insert(vec![]).push(rhs);
        }
        Grammar {
            productions,
            start,
            firsts: None,
        }
    }

    pub fn firsts(&self) -> &Firsts {
        self.firsts.as_ref().unwrap()
    }

    pub fn non_terminals(&self) -> Vec<NonTerminal> {
        self.productions.keys().cloned().collect()
    }
}
