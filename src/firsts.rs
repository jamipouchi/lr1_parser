use std::collections::{HashMap, HashSet};

use crate::types::*;

#[derive(Debug)]
pub struct Firsts {
    firsts: HashMap<NonTerminal, HashSet<Terminal>>,
}

impl Firsts {
    pub fn new(grammar: &Grammar) -> Firsts {
        let mut firsts: HashMap<NonTerminal, HashSet<Terminal>> = HashMap::new();
        for non_terminal in grammar.productions.keys() {
            firsts.insert(non_terminal.clone(), HashSet::new());
        }
        let mut changed = true;
        while changed {
            changed = false;
            for (non_terminal, productions) in &grammar.productions {
                for production in productions {
                    let mut rhs = Self::first(&firsts, &production[0]);
                    rhs.remove(&eof());
                    let mut i = 0;
                    // If symbols from 0 to k-1 are nullable
                    while Self::first(&firsts, &production[i]).contains(&eof())
                        && i < production.len() - 1
                    {
                        let mut rhs_i = Self::first(&firsts, &production[i]);
                        rhs_i.remove(&eof());
                        rhs.extend(rhs_i);
                        i += 1;
                    }
                    // If the last symbol `k` in the production is nullable.
                    if i == production.len() - 1
                        && Self::first(&firsts, &production[i]).contains(&eof())
                    {
                        rhs.insert(eof());
                    }
                    if rhs.is_subset(&firsts[non_terminal]) {
                        continue;
                    }
                    changed = true;
                    firsts.get_mut(non_terminal).unwrap().extend(rhs);
                }
            }
        }

        Firsts { firsts }
    }

    pub fn get(&self, symbol: &Symbol) -> HashSet<Terminal> {
        Self::first(&self.firsts, symbol)
    }

    fn first(
        firsts: &HashMap<NonTerminal, HashSet<Terminal>>,
        symbol: &Symbol,
    ) -> HashSet<Terminal> {
        match symbol {
            Symbol::Terminal(t) => vec![t.clone()].into_iter().collect(),
            Symbol::NonTerminal(s) => firsts[s].clone(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::Grammar;

    #[test]
    fn test_expressions() {
        let grammar = Grammar::parse(vec![
            ("Goal", vec!["Expr"]),
            ("Expr", vec!["Term", "Expr'"]),
            ("Expr'", vec!["+", "Term", "Expr'"]),
            ("Expr'", vec!["-", "Term", "Expr'"]),
            ("Expr'", vec!["eof"]),
            ("Term", vec!["Factor", "Term'"]),
            ("Term'", vec!["*", "Factor", "Term'"]),
            ("Term'", vec!["/", "Factor", "Term'"]),
            ("Term'", vec!["eof"]),
            ("Factor", vec!["(", "Expr", ")"]),
            ("Factor", vec!["num"]),
            ("Factor", vec!["name"]),
        ]);

        let firsts = Firsts::new(&grammar);
        assert_eq!(
            firsts.get(&Symbol::NonTerminal(NonTerminal::from("Goal"))),
            vec![
                Terminal::from("num"),
                Terminal::from("name"),
                Terminal::from("(")
            ]
            .into_iter()
            .collect()
        );
        assert_eq!(
            firsts.get(&Symbol::NonTerminal(NonTerminal::from("Expr"))),
            vec![
                Terminal::from("num"),
                Terminal::from("name"),
                Terminal::from("(")
            ]
            .into_iter()
            .collect()
        );
        assert_eq!(
            firsts.get(&Symbol::NonTerminal(NonTerminal::from("Expr'"))),
            vec![
                Terminal::from("+"),
                Terminal::from("-"),
                Terminal::from("eof")
            ]
            .into_iter()
            .collect()
        );
        assert_eq!(
            firsts.get(&Symbol::NonTerminal(NonTerminal::from("Term"))),
            vec![
                Terminal::from("num"),
                Terminal::from("name"),
                Terminal::from("(")
            ]
            .into_iter()
            .collect()
        );
        assert_eq!(
            firsts.get(&Symbol::NonTerminal(NonTerminal::from("Term'"))),
            vec![
                Terminal::from("*"),
                Terminal::from("/"),
                Terminal::from("eof")
            ]
            .into_iter()
            .collect()
        );
        assert_eq!(
            firsts.get(&Symbol::NonTerminal(NonTerminal::from("Factor"))),
            vec![
                Terminal::from("("),
                Terminal::from("num"),
                Terminal::from("name")
            ]
            .into_iter()
            .collect()
        );
    }

    #[test]
    fn test_parenthesis() {
        let grammar = Grammar::parse(vec![
            ("Goal", vec!["List"]),
            ("List", vec!["List", "Pair"]),
            ("List", vec!["Pair"]),
            ("Pair", vec!["(", "Pair", ")"]),
            ("Pair", vec!["(", ")"]),
        ]);
        let firsts = Firsts::new(&grammar);
        assert_eq!(
            firsts.get(&Symbol::NonTerminal(NonTerminal::from("Goal"))),
            vec![Terminal::from("(")].into_iter().collect()
        );
        assert_eq!(
            firsts.get(&Symbol::NonTerminal(NonTerminal::from("List"))),
            vec![Terminal::from("(")].into_iter().collect()
        );
        assert_eq!(
            firsts.get(&Symbol::NonTerminal(NonTerminal::from("Pair"))),
            vec![Terminal::from("(")].into_iter().collect()
        );
    }
}
