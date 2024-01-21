mod firsts;
mod types;

use std::{
    collections::{HashMap, HashSet, VecDeque},
    hash::{Hash, Hasher},
    iter::Iterator,
    ops::{Deref, DerefMut},
    vec,
};

use firsts::Firsts;
use types::*;

#[derive(Debug, PartialEq, Eq)]
pub enum Action {
    Shift(usize),
    Reduce(NonTerminal, Vec<Symbol>),
    Accept,
}

#[derive(Debug, Eq, PartialEq, Hash, Clone, PartialOrd, Ord)]

pub struct Item {
    production: (NonTerminal, Vec<Symbol>),
    terminal: Terminal,
    dot: usize,
}

#[derive(Debug, Eq, PartialEq)]
pub struct CcElement(HashSet<Item>);

impl Deref for CcElement {
    type Target = HashSet<Item>;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}
impl DerefMut for CcElement {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl Hash for CcElement {
    fn hash<H: Hasher>(&self, state: &mut H) {
        let mut items: Vec<&Item> = self.0.iter().collect();
        items.sort();
        items.hash(state);
    }
}

pub fn lr1_parse(
    action_table: &HashMap<(usize, Terminal), Action>,
    goto_table: &HashMap<(usize, NonTerminal), usize>,
    lexer: &mut impl Lexer,
) -> ASTNode {
    #[derive(Debug)]
    enum StackItem {
        State(usize),
        ASTNode(ASTNode),
    }
    let mut stack = Vec::<StackItem>::new();
    stack.push(StackItem::State(0));
    let mut word = lexer.next().expect("Empty input");
    loop {
        let state = match stack.last().expect("Stack is empty") {
            StackItem::State(state) => state,
            StackItem::ASTNode(_) => panic!("Expected state, got symbol {:?} on stack", stack),
        };
        let action = action_table.get(&(*state, word.to_string()));
        match action {
            Some(Action::Shift(next_state)) => {
                stack.push(StackItem::ASTNode(ASTNode {
                    symbol: Symbol::Terminal(word.clone()),
                    children: Vec::new(),
                }));
                stack.push(StackItem::State(*next_state));
                word = lexer
                    .next()
                    .expect(&format!("Empty input on shift, current stack: {:?}", stack));
            }
            Some(Action::Reduce(lhs, rhs)) => {
                // I'd like to push the children in reverse order
                let mut children = VecDeque::new();
                for _ in 0..rhs.len() {
                    stack.pop();
                    if let Some(StackItem::ASTNode(node)) = stack.pop() {
                        children.push_front(node);
                    } else {
                        panic!("Expected ASTNode, got {:?}", stack);
                    }
                }
                let state = match stack.last().expect("Stack is empty") {
                    StackItem::State(state) => *state,
                    StackItem::ASTNode(_) => {
                        panic!("Expected state, got symbol {:?} on stack", stack)
                    }
                };
                stack.push(StackItem::ASTNode(ASTNode {
                    symbol: Symbol::NonTerminal(lhs.clone()),
                    children: children.into(),
                }));
                let next_state = goto_table
                    .get(&(state, lhs.to_string()))
                    .expect(&format!("Miss in goto table: {}, {}", state, lhs));
                stack.push(StackItem::State(*next_state));
            }
            Some(Action::Accept) => {
                println!("Accept");
                break;
            }
            None => {
                panic!("Error");
            }
        }
    }
    assert!(stack.len() == 3, "Stack is not empty: {:?}", stack);
    stack.pop();
    return match stack.pop().expect("Stack is empty") {
        StackItem::ASTNode(node) => node,
        StackItem::State(_) => panic!("Expected ASTNode, got {:?}", stack),
    };
}

pub fn generate_tables(
    grammar: &Grammar,
) -> (
    HashMap<(usize, Terminal), Action>,
    HashMap<(usize, NonTerminal), usize>,
) {
    let ccs = generate_cc(grammar);
    let mut action_table = HashMap::new();
    let mut goto_table = HashMap::new();
    for (i, cc) in ccs.iter().enumerate() {
        for item in cc.iter() {
            if item.dot == item.production.1.len() {
                if item.production.0 == grammar.start && item.terminal == eof() {
                    action_table.insert((i, eof()), Action::Accept); // Accept
                } else {
                    // Reduce item.production.0 -> item.production.1
                    action_table.insert(
                        (i, item.terminal.clone()),
                        Action::Reduce(item.production.0.clone(), item.production.1.clone()),
                    );
                }
            } else {
                let next_symbol = &item.production.1[item.dot];
                if let Symbol::Terminal(symbol) = next_symbol {
                    let next_cc = goto(grammar, cc, next_symbol);
                    let next_cc_index = match ccs.iter().position(|cc| cc == &next_cc) {
                        Some(index) => index,
                        None => continue,
                    };
                    // Shift next_cc_index
                    action_table.insert((i, symbol.clone()), Action::Shift(next_cc_index));
                }
            }
        }
        for non_terminal in Grammar::non_terminals(grammar) {
            let next_cc = goto(grammar, cc, &Symbol::NonTerminal(non_terminal.clone()));
            let next_cc_index = match ccs.iter().position(|cc| cc == &next_cc) {
                Some(index) => index,
                None => continue,
            };
            goto_table.insert((i, non_terminal), next_cc_index); // This is actually correct! We simply insert the next_cc_index into the goto table.
        }
    }
    (action_table, goto_table)
}

pub fn generate_cc(grammar: &Grammar) -> Vec<CcElement> {
    let mut first_items = HashSet::new();
    for production in &grammar.productions[&grammar.start] {
        first_items.insert(Item {
            production: (grammar.start.clone(), production.clone()),
            terminal: eof(),
            dot: 0,
        });
    }
    let cc0 = closure(grammar, &first_items);
    let mut processed_ccs = Vec::new();
    let mut curr_ccs = vec![CcElement(cc0)];
    let mut next_cc_items;
    loop {
        next_cc_items = Vec::new();
        for cc in &curr_ccs {
            let next_ccs = next_ccs(grammar, cc);
            for next_cc in next_ccs {
                if !processed_ccs.contains(&next_cc) && !curr_ccs.contains(&next_cc) {
                    next_cc_items.push(next_cc);
                }
            }
        }
        processed_ccs.append(&mut curr_ccs);
        if next_cc_items.is_empty() {
            break;
        }
        curr_ccs = next_cc_items;
    }
    processed_ccs
}

fn next_ccs(grammar: &Grammar, cc: &HashSet<Item>) -> HashSet<CcElement> {
    let mut next_ccs = HashMap::new();
    for item in cc {
        if item.dot == item.production.1.len() {
            continue;
        }
        let next_symbol = &item.production.1[item.dot];
        if next_ccs.get(next_symbol).is_some() {
            continue;
        }
        let next_cc_items = goto(grammar, &CcElement(cc.clone()), next_symbol);
        next_ccs.insert(next_symbol.clone(), next_cc_items);
    }
    next_ccs.into_values().collect()
}

pub fn closure(grammar: &Grammar, first_items: &HashSet<Item>) -> HashSet<Item> {
    let mut processed_items = HashSet::new();
    let mut curr_items = first_items.clone();
    let mut next_items;
    loop {
        next_items = HashSet::new();
        for item in &curr_items {
            if item.dot == item.production.1.len() {
                continue;
            }
            let next_symbol = match &item.production.1[item.dot] {
                Symbol::NonTerminal(s) => s.clone(),
                _ => continue,
            };
            for production in &grammar.productions[&next_symbol] {
                for b in get_from_item(grammar.firsts(), item) {
                    let new_item = Item {
                        production: (next_symbol.clone(), production.clone()),
                        terminal: b,
                        dot: 0,
                    };
                    next_items.insert(new_item);
                }
            }
        }
        processed_items.extend(curr_items);
        if next_items.is_subset(&processed_items) {
            break;
        }
        curr_items = next_items;
    }
    processed_items
}

pub fn goto(grammar: &Grammar, items: &CcElement, symbol: &Symbol) -> CcElement {
    let mut next_items = HashSet::new();
    for item in items.iter() {
        if item.dot == item.production.1.len() || &item.production.1[item.dot] != symbol {
            continue;
        }
        next_items.insert(Item {
            production: (item.production.0.clone(), item.production.1.clone()),
            terminal: item.terminal.clone(),
            dot: item.dot + 1,
        });
    }
    CcElement(closure(grammar, &next_items))
}

fn get_from_item(firsts: &Firsts, item: &Item) -> HashSet<Terminal> {
    let symbol = item.production.1.get(item.dot + 1);
    if let Some(symbol) = symbol {
        let mut firsts = firsts.get(symbol);
        if firsts.contains(&eof()) {
            firsts.insert(item.terminal.clone());
            firsts
        } else {
            firsts
        }
    } else {
        return vec![item.terminal.clone()].into_iter().collect();
    }
}

#[cfg(test)]
mod tests {
    use std::collections::VecDeque;

    use crate::*;

    #[test]
    fn test_get_from_item() {
        let grammar = Grammar::parse(vec![
            ("Goal", vec!["SheepNoise"]),
            ("SheepNoise", vec!["SheepNoise", "baa"]),
            ("SheepNoise", vec!["baa"]),
        ]);
        let firsts = Firsts::new(&grammar);
        let start_item = Item {
            production: (
                NonTerminal::from("Goal"),
                vec![Symbol::NonTerminal(NonTerminal::from("SheepNoise"))],
            ),
            dot: 0,
            terminal: eof(),
        };
        let firsts_from_start_item = get_from_item(&firsts, &start_item);
        assert_eq!(
            firsts_from_start_item,
            vec![Terminal::from(eof())].into_iter().collect()
        );
        let second_item = Item {
            production: (
                NonTerminal::from("SheepNoise"),
                vec![
                    Symbol::NonTerminal(NonTerminal::from("SheepNoise")),
                    Symbol::Terminal(Terminal::from("baa")),
                ],
            ),
            dot: 0,
            terminal: eof(),
        };
        let firsts_from_second_item = get_from_item(&firsts, &second_item);
        assert_eq!(
            firsts_from_second_item,
            vec![Terminal::from("baa")].into_iter().collect()
        );
    }

    #[test]
    fn test_closure() {
        let grammar = Grammar::parse(vec![
            ("Goal", vec!["List"]),
            ("List", vec!["List", "Pair"]),
            ("List", vec!["Pair"]),
            ("Pair", vec!["(", "Pair", ")"]),
            ("Pair", vec!["(", ")"]),
        ]);
        let mut first_items = HashSet::new();
        for production in &grammar.productions[&grammar.start] {
            first_items.insert(Item {
                production: (grammar.start.clone(), production.clone()),
                terminal: eof(),
                dot: 0,
            });
        }
        let cc0 = closure(&grammar, &first_items);
        assert_eq!(cc0.len(), 9);
        assert!(cc0.contains(&Item {
            production: (
                "Goal".to_string(),
                vec![Symbol::NonTerminal("List".to_string())]
            ),
            terminal: eof(),
            dot: 0,
        }));
        assert!(cc0.contains(&Item {
            production: (
                "List".to_string(),
                vec![
                    Symbol::NonTerminal("List".to_string()),
                    Symbol::NonTerminal("Pair".to_string())
                ]
            ),
            terminal: eof(),
            dot: 0,
        }));
        assert!(cc0.contains(&Item {
            production: (
                "List".to_string(),
                vec![Symbol::NonTerminal("Pair".to_string())]
            ),
            terminal: eof(),
            dot: 0,
        }));
        assert!(cc0.contains(&Item {
            production: (
                "Pair".to_string(),
                vec![
                    Symbol::Terminal("(".to_string()),
                    Symbol::NonTerminal("Pair".to_string()),
                    Symbol::Terminal(")".to_string())
                ]
            ),
            terminal: eof(),
            dot: 0,
        }));
        assert!(cc0.contains(&Item {
            production: (
                "Pair".to_string(),
                vec![
                    Symbol::Terminal("(".to_string()),
                    Symbol::Terminal(")".to_string())
                ]
            ),
            terminal: eof(),
            dot: 0,
        }));
        assert!(cc0.contains(&Item {
            production: (
                "List".to_string(),
                vec![
                    Symbol::NonTerminal("List".to_string()),
                    Symbol::NonTerminal("Pair".to_string())
                ]
            ),
            terminal: "(".to_string(),
            dot: 0,
        }));
        assert!(cc0.contains(&Item {
            production: (
                "List".to_string(),
                vec![Symbol::NonTerminal("Pair".to_string())]
            ),
            terminal: "(".to_string(),
            dot: 0,
        }));
        assert!(cc0.contains(&Item {
            production: (
                "Pair".to_string(),
                vec![
                    Symbol::Terminal("(".to_string()),
                    Symbol::NonTerminal("Pair".to_string()),
                    Symbol::Terminal(")".to_string())
                ]
            ),
            terminal: "(".to_string(),
            dot: 0,
        }));
        assert!(cc0.contains(&Item {
            production: (
                "Pair".to_string(),
                vec![
                    Symbol::Terminal("(".to_string()),
                    Symbol::Terminal(")".to_string())
                ]
            ),
            terminal: "(".to_string(),
            dot: 0,
        }));
    }

    #[test]
    fn test_goto() {
        let grammar = Grammar::parse(vec![
            ("Goal", vec!["List"]),
            ("List", vec!["List", "Pair"]),
            ("List", vec!["Pair"]),
            ("Pair", vec!["(", "Pair", ")"]),
            ("Pair", vec!["(", ")"]),
        ]);
        let mut first_items = HashSet::new();
        for production in &grammar.productions[&grammar.start] {
            first_items.insert(Item {
                production: (grammar.start.clone(), production.clone()),
                terminal: eof(),
                dot: 0,
            });
        }
        let cc0 = closure(&grammar, &first_items);
        let after_lparent = goto(
            &grammar,
            &CcElement(cc0.clone()),
            &Symbol::Terminal(Terminal::from("(")),
        );
        assert_eq!(after_lparent.len(), 6);
        assert!(after_lparent.contains(&Item {
            production: (
                "Pair".to_string(),
                vec![
                    Symbol::Terminal("(".to_string()),
                    Symbol::NonTerminal("Pair".to_string()),
                    Symbol::Terminal(")".to_string()),
                ]
            ),
            terminal: eof(),
            dot: 1,
        }));
        assert!(after_lparent.contains(&Item {
            production: (
                "Pair".to_string(),
                vec![
                    Symbol::Terminal("(".to_string()),
                    Symbol::NonTerminal("Pair".to_string()),
                    Symbol::Terminal(")".to_string()),
                ]
            ),
            terminal: Terminal::from("("),
            dot: 1,
        }));
        assert!(after_lparent.contains(&Item {
            production: (
                "Pair".to_string(),
                vec![
                    Symbol::Terminal("(".to_string()),
                    Symbol::Terminal(")".to_string()),
                ]
            ),
            terminal: eof(),
            dot: 1,
        }));
        assert!(after_lparent.contains(&Item {
            production: (
                "Pair".to_string(),
                vec![
                    Symbol::Terminal("(".to_string()),
                    Symbol::Terminal(")".to_string()),
                ]
            ),
            terminal: Terminal::from("("),
            dot: 1,
        }));
        assert!(after_lparent.contains(&Item {
            production: (
                "Pair".to_string(),
                vec![
                    Symbol::Terminal("(".to_string()),
                    Symbol::NonTerminal("Pair".to_string()),
                    Symbol::Terminal(")".to_string()),
                ]
            ),
            terminal: Terminal::from(")"),
            dot: 0,
        }));
        assert!(after_lparent.contains(&Item {
            production: (
                "Pair".to_string(),
                vec![
                    Symbol::Terminal("(".to_string()),
                    Symbol::Terminal(")".to_string()),
                ]
            ),
            terminal: Terminal::from(")"),
            dot: 0,
        }));
    }

    #[test]
    fn test_next_ccs() {
        let grammar = Grammar::parse(vec![
            ("Goal", vec!["List"]),
            ("List", vec!["List", "Pair"]),
            ("List", vec!["Pair"]),
            ("Pair", vec!["(", "Pair", ")"]),
            ("Pair", vec!["(", ")"]),
        ]);
        let mut first_items = HashSet::new();
        for production in &grammar.productions[&grammar.start] {
            first_items.insert(Item {
                production: (grammar.start.clone(), production.clone()),
                terminal: eof(),
                dot: 0,
            });
        }
        let cc0 = closure(&grammar, &first_items);
        let next_ccs = next_ccs(&grammar, &cc0);
        assert_eq!(next_ccs.len(), 3);

        assert!(next_ccs.contains(&goto(
            &grammar,
            &CcElement(cc0.clone()),
            &Symbol::NonTerminal("List".to_string()),
        )),);
        assert!(next_ccs.contains(&goto(
            &grammar,
            &CcElement(cc0.clone()),
            &Symbol::NonTerminal("Pair".to_string())
        )));
        assert!(next_ccs.contains(&goto(
            &grammar,
            &CcElement(cc0.clone()),
            &Symbol::Terminal(Terminal::from("("))
        )));
    }

    #[test]
    fn test_generate_cc() {
        let grammar = Grammar::parse(vec![
            ("Goal", vec!["List"]),
            ("List", vec!["List", "Pair"]),
            ("List", vec!["Pair"]),
            ("Pair", vec!["(", "Pair", ")"]),
            ("Pair", vec!["(", ")"]),
        ]);
        let ccs = generate_cc(&grammar);
        assert_eq!(ccs.len(), 12);
        let mut first_items = HashSet::new();
        for production in &grammar.productions[&grammar.start] {
            first_items.insert(Item {
                production: (grammar.start.clone(), production.clone()),
                terminal: eof(),
                dot: 0,
            });
        }
        let cc0 = CcElement(closure(&grammar, &first_items));
        assert!(ccs.contains(&cc0));
        let cc1 = goto(
            &grammar,
            &cc0,
            &Symbol::NonTerminal(NonTerminal::from("List")),
        );
        assert!(ccs.contains(&cc1));
        let cc2 = goto(
            &grammar,
            &cc0,
            &Symbol::NonTerminal(NonTerminal::from("Pair")),
        );
        assert!(ccs.contains(&cc2));
        let cc3 = goto(&grammar, &cc0, &Symbol::Terminal(Terminal::from("(")));
        assert!(ccs.contains(&cc3));
        let cc4 = goto(
            &grammar,
            &cc1,
            &Symbol::NonTerminal(NonTerminal::from("Pair")),
        );
        assert!(ccs.contains(&cc4));
        let cc5 = goto(
            &grammar,
            &cc3,
            &&Symbol::NonTerminal(NonTerminal::from("Pair")),
        );
        assert!(ccs.contains(&cc5));
        let cc6 = goto(&grammar, &cc3, &Symbol::Terminal(Terminal::from("(")));
        assert!(ccs.contains(&cc6));
        let cc7 = goto(&grammar, &cc3, &Symbol::Terminal(Terminal::from(")")));
        assert!(ccs.contains(&cc7));
        let cc8 = goto(&grammar, &cc5, &Symbol::Terminal(Terminal::from(")")));
        assert!(ccs.contains(&cc8));
        let cc9 = goto(
            &grammar,
            &cc6,
            &Symbol::NonTerminal(NonTerminal::from("Pair")),
        );
        assert!(ccs.contains(&cc9));
        let cc10 = goto(&grammar, &cc6, &Symbol::Terminal(Terminal::from(")")));
        assert!(ccs.contains(&cc10));
        let cc11 = goto(&grammar, &cc9, &Symbol::Terminal(Terminal::from(")")));
        assert!(ccs.contains(&cc11));
    }

    #[test]
    fn test_generate_tables() {
        // Setup
        let grammar = Grammar::parse(vec![
            ("Goal", vec!["List"]),
            ("List", vec!["List", "Pair"]),
            ("List", vec!["Pair"]),
            ("Pair", vec!["(", "Pair", ")"]),
            ("Pair", vec!["(", ")"]),
        ]);
        let (action_table, goto_table) = generate_tables(&grammar);

        let three_state = match action_table.get(&(0, "(".to_owned())) {
            Some(Action::Shift(x)) => *x,
            Some(_) => panic!("Expected Shift"),
            None => panic!("No entry for (first_jump) (, 0"),
        };
        let entry0 = ((0, "(".to_owned()), Action::Shift(three_state));

        let six_state = match action_table.get(&(three_state, "(".to_owned())) {
            Some(Action::Shift(x)) => *x,
            Some(_) => panic!("Expected Shift"),
            None => panic!("No entry for (second_jump) ), {}", three_state),
        };
        let entry1 = ((three_state, "(".to_owned()), Action::Shift(six_state));
        let seven_state = match action_table.get(&(three_state, ")".to_owned())) {
            Some(Action::Shift(x)) => *x,
            Some(_) => panic!("Expected Shift"),
            None => panic!("No entry for (third_jump) ), {}", three_state),
        };
        let entry2 = ((three_state, ")".to_owned()), Action::Shift(seven_state));

        let _ = match action_table.get(&(six_state, "(".to_owned())) {
            Some(Action::Shift(x)) => *x,
            Some(_) => panic!("Expected Shifg"),
            None => panic!("No entry for (fourth_jump) ), {}", six_state),
        };
        let entry3 = ((six_state, "(".to_owned()), Action::Shift(six_state));
        let ten_state = match action_table.get(&(six_state, ")".to_owned())) {
            Some(Action::Shift(x)) => *x,
            Some(_) => panic!("Expected Shift"),
            None => panic!("No entry for (fifth_jump) ), {}", six_state),
        };
        let entry4 = ((six_state, ")".to_owned()), Action::Shift(ten_state));

        let (n_t, i) = match action_table.get(&(seven_state, "eof".to_owned())) {
            Some(Action::Reduce(n_t, i)) => (n_t.clone(), i),
            Some(_) => panic!("Expected Reduce"),
            None => panic!("No entry for (sixth_jump) ), {}", seven_state),
        };
        let entry5 = (
            (seven_state, "eof".to_owned()),
            Action::Reduce(n_t, i.clone()),
        );
        let (n_t, i) = match action_table.get(&(seven_state, "(".to_owned())) {
            Some(Action::Reduce(n_t, i)) => (n_t.clone(), i),
            Some(_) => panic!("Expected Reduce"),
            None => panic!("No entry for (seventh_jump) ), {}", seven_state),
        };
        let entry6 = (
            (seven_state, "(".to_owned()),
            Action::Reduce(n_t, i.clone()),
        );

        let (n_t, i) = match action_table.get(&(ten_state, ")".to_owned())) {
            Some(Action::Reduce(n_t, i)) => (n_t.clone(), i),
            Some(_) => panic!("Expected Reduce"),
            None => panic!("No entry for (eigth_jump) ), {}", ten_state),
        };
        let entry7 = ((ten_state, ")".to_owned()), Action::Reduce(n_t, i.clone()));

        let one_state = *goto_table.get(&(0, "List".to_owned())).unwrap();
        let _ = match action_table.get(&(one_state, "eof".to_owned())) {
            // Acc
            Some(Action::Accept) => (),
            Some(_) => panic!("Expected Accept"),
            None => panic!("No entry for eof, {}", one_state),
        };
        let entry8 = ((one_state, "eof".to_owned()), Action::Accept);
        let _ = match action_table.get(&(one_state, "(".to_owned())) {
            Some(Action::Shift(x)) => *x,
            Some(_) => panic!("Expected Shift"),
            None => panic!("No entry for (ninth_jump) ), {}", one_state),
        };
        let entry9 = ((one_state, "(".to_owned()), Action::Shift(three_state));

        let two_state = *goto_table.get(&(0, "Pair".to_owned())).unwrap();
        let (n_t, i) = match action_table.get(&(two_state, "eof".to_owned())) {
            Some(Action::Reduce(n_t, i)) => (n_t.clone(), i),
            Some(_) => panic!("Expected Reduce"),
            None => panic!("No entry for (eigth_jump) ), {}", ten_state),
        };
        let entry10 = (
            (two_state, "eof".to_owned()),
            Action::Reduce(n_t, i.clone()),
        );
        let (n_t, i) = match action_table.get(&(two_state, "(".to_owned())) {
            Some(Action::Reduce(n_t, i)) => (n_t.clone(), i),
            Some(_) => panic!("Expected Reduce"),
            None => panic!("No entry for (eigth_jump) ), {}", ten_state),
        };
        let entry11 = ((two_state, "(".to_owned()), Action::Reduce(n_t, i.clone()));

        let four_state = *goto_table.get(&(one_state, "Pair".to_owned())).unwrap();
        let (n_t, i) = match action_table.get(&(four_state, "eof".to_owned())) {
            Some(Action::Reduce(n_t, i)) => (n_t.clone(), i),
            Some(_) => panic!("Expected Reduce"),
            None => panic!("No entry for (eigth_jump) ), {}", ten_state),
        };
        let entry12 = (
            (four_state, "eof".to_owned()),
            Action::Reduce(n_t, i.clone()),
        );
        let (n_t, i) = match action_table.get(&(four_state, "(".to_owned())) {
            Some(Action::Reduce(n_t, i)) => (n_t.clone(), i),
            Some(_) => panic!("Expected Reduce"),
            None => panic!("No entry for (eigth_jump) ), {}", ten_state),
        };
        let entry13 = ((four_state, "(".to_owned()), Action::Reduce(n_t, i.clone()));

        let five_state = *goto_table.get(&(three_state, "Pair".to_owned())).unwrap();
        let eigth_state = match action_table.get(&(five_state, ")".to_owned())) {
            // Acc
            Some(Action::Shift(x)) => *x,
            Some(_) => panic!("Expected Accept"),
            None => panic!("No entry for eof, {}", five_state),
        };
        let entry14 = ((five_state, ")".to_owned()), Action::Shift(eigth_state));

        let (n_t, i) = match action_table.get(&(eigth_state, "eof".to_owned())) {
            Some(Action::Reduce(n_t, i)) => (n_t.clone(), i),
            Some(_) => panic!("Expected Reduce"),
            None => panic!("No entry for (eigth_jump) ), {}", ten_state),
        };
        let entry15 = (
            (eigth_state, "eof".to_owned()),
            Action::Reduce(n_t, i.clone()),
        );
        let (n_t, i) = match action_table.get(&(eigth_state, "(".to_owned())) {
            Some(Action::Reduce(n_t, i)) => (n_t.clone(), i),
            Some(_) => panic!("Expected Reduce"),
            None => panic!("No entry for (eigth_jump) ), {}", ten_state),
        };
        let entry16 = (
            (eigth_state, "(".to_owned()),
            Action::Reduce(n_t, i.clone()),
        );

        let nine_state = *goto_table.get(&(six_state, "Pair".to_owned())).unwrap();

        let eleven_state = match action_table.get(&(nine_state, ")".to_owned())) {
            Some(Action::Shift(x)) => *x,
            Some(_) => panic!("Expected Accept"),
            None => panic!("No entry for eof, {}", five_state),
        };
        let entry17 = ((nine_state, ")".to_owned()), Action::Shift(eleven_state));

        let (n_t, i) = match action_table.get(&(eleven_state, ")".to_owned())) {
            Some(Action::Reduce(n_t, i)) => (n_t.clone(), i),
            Some(_) => panic!("Expected Reduce"),
            None => panic!("No entry for (eigth_jump) ), {}", ten_state),
        };
        let entry18 = (
            (eleven_state, ")".to_owned()),
            Action::Reduce(n_t, i.clone()),
        );

        let expected_action_table = HashMap::from([
            entry0, entry1, entry2, entry3, entry4, entry5, entry6, entry7, entry8, entry9,
            entry10, entry11, entry12, entry13, entry14, entry15, entry16, entry17, entry18,
        ]);
        let expected_goto_table = HashMap::from([
            ((0, "List".to_owned()), one_state),
            ((0, "Pair".to_owned()), two_state),
            ((one_state, "Pair".to_owned()), four_state),
            ((three_state, "Pair".to_owned()), five_state),
            ((six_state, "Pair".to_owned()), nine_state),
        ]);

        assert_eq!(action_table, expected_action_table);
        assert_eq!(goto_table, expected_goto_table);
    }

    #[test]
    fn test_lr1_parse() {
        let grammar = Grammar::parse(vec![
            ("Goal", vec!["List"]),
            ("List", vec!["List", "Pair"]),
            ("List", vec!["Pair"]),
            ("Pair", vec!["(", "Pair", ")"]),
            ("Pair", vec!["(", ")"]),
        ]);
        let (action_table, goto_table) = generate_tables(&grammar);

        struct MyLexer {
            words: VecDeque<String>,
        }
        impl MyLexer {
            fn new_from(vec: Vec<&str>) -> Self {
                MyLexer {
                    words: VecDeque::from(
                        vec.iter().map(|s| s.to_string()).collect::<Vec<String>>(),
                    ),
                }
            }
        }
        impl Lexer for MyLexer {
            fn next(&mut self) -> Option<String> {
                self.words.pop_front()
            }
        }

        let mut lexer = MyLexer::new_from(vec!["(", ")", "eof"]);
        let ast_tree = lr1_parse(&action_table, &goto_table, &mut lexer);
        let expected_ast_tree = ASTNode {
            symbol: Symbol::NonTerminal(NonTerminal::from("List")),
            children: vec![ASTNode {
                symbol: Symbol::NonTerminal(NonTerminal::from("Pair")),
                children: vec![
                    ASTNode {
                        symbol: Symbol::Terminal(Terminal::from("(")),
                        children: vec![],
                    },
                    ASTNode {
                        symbol: Symbol::Terminal(Terminal::from(")")),
                        children: vec![],
                    },
                ],
            }],
        };
        assert_eq!(ast_tree, expected_ast_tree);

        let mut lexer = MyLexer::new_from(vec!["(", "(", ")", ")", "eof"]);
        let ast_tree = lr1_parse(&action_table, &goto_table, &mut lexer);
        let expected_ast_tree = ASTNode {
            symbol: Symbol::NonTerminal(NonTerminal::from("List")),
            children: vec![ASTNode {
                symbol: Symbol::NonTerminal(NonTerminal::from("Pair")),
                children: vec![
                    ASTNode {
                        symbol: Symbol::Terminal(Terminal::from("(")),
                        children: vec![],
                    },
                    ASTNode {
                        symbol: Symbol::NonTerminal(NonTerminal::from("Pair")),
                        children: vec![
                            ASTNode {
                                symbol: Symbol::Terminal(Terminal::from("(")),
                                children: vec![],
                            },
                            ASTNode {
                                symbol: Symbol::Terminal(Terminal::from(")")),
                                children: vec![],
                            },
                        ],
                    },
                    ASTNode {
                        symbol: Symbol::Terminal(Terminal::from(")")),
                        children: vec![],
                    },
                ],
            }],
        };
        assert_eq!(ast_tree, expected_ast_tree);

        let mut lexer = MyLexer::new_from(vec!["(", ")", "(", ")", "eof"]);
        let ast_tree = lr1_parse(&action_table, &goto_table, &mut lexer);
        let expected_ast_tree = ASTNode {
            symbol: Symbol::NonTerminal(NonTerminal::from("List")),
            children: vec![
                ASTNode {
                    symbol: Symbol::NonTerminal(NonTerminal::from("List")),
                    children: vec![ASTNode {
                        symbol: Symbol::NonTerminal(NonTerminal::from("Pair")),
                        children: vec![
                            ASTNode {
                                symbol: Symbol::Terminal(Terminal::from("(")),
                                children: vec![],
                            },
                            ASTNode {
                                symbol: Symbol::Terminal(Terminal::from(")")),
                                children: vec![],
                            },
                        ],
                    }],
                },
                ASTNode {
                    symbol: Symbol::NonTerminal(NonTerminal::from("Pair")),
                    children: vec![
                        ASTNode {
                            symbol: Symbol::Terminal(Terminal::from("(")),
                            children: vec![],
                        },
                        ASTNode {
                            symbol: Symbol::Terminal(Terminal::from(")")),
                            children: vec![],
                        },
                    ],
                },
            ],
        };
        assert_eq!(ast_tree, expected_ast_tree);

        let mut lexer = MyLexer::new_from(vec!["(", "(", ")", ")", "(", ")", "eof"]);
        let ast_tree = lr1_parse(&action_table, &goto_table, &mut lexer);
        let expected_ast_tree = ASTNode {
            symbol: Symbol::NonTerminal(NonTerminal::from("List")),
            children: vec![
                ASTNode {
                    symbol: Symbol::NonTerminal(NonTerminal::from("List")),
                    children: vec![ASTNode {
                        symbol: Symbol::NonTerminal(NonTerminal::from("Pair")),
                        children: vec![
                            ASTNode {
                                symbol: Symbol::Terminal(Terminal::from("(")),
                                children: vec![],
                            },
                            ASTNode {
                                symbol: Symbol::NonTerminal(NonTerminal::from("Pair")),
                                children: vec![
                                    ASTNode {
                                        symbol: Symbol::Terminal(Terminal::from("(")),
                                        children: vec![],
                                    },
                                    ASTNode {
                                        symbol: Symbol::Terminal(Terminal::from(")")),
                                        children: vec![],
                                    },
                                ],
                            },
                            ASTNode {
                                symbol: Symbol::Terminal(Terminal::from(")")),
                                children: vec![],
                            },
                        ],
                    }],
                },
                ASTNode {
                    symbol: Symbol::NonTerminal(NonTerminal::from("Pair")),
                    children: vec![
                        ASTNode {
                            symbol: Symbol::Terminal(Terminal::from("(")),
                            children: vec![],
                        },
                        ASTNode {
                            symbol: Symbol::Terminal(Terminal::from(")")),
                            children: vec![],
                        },
                    ],
                },
            ],
        };
        assert_eq!(ast_tree, expected_ast_tree);

        let mut lexer = MyLexer::new_from(vec!["(", ")", ")", "eof"]);
        let result =
            std::panic::catch_unwind(move || lr1_parse(&action_table, &goto_table, &mut lexer));
        assert!(result.is_err());
    }
}
