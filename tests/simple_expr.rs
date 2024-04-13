use std::collections::HashMap;

use triemap::trie_map;

type UsizeMap<V> = HashMap<usize, V>;

#[derive(Debug, Clone, PartialEq, Eq)]
#[trie_map(Expr -> *ExprMap, usize -> UsizeMap)]
pub enum Expr {
    Zero,
    Var(usize),
    App(Box<Expr>, usize, Box<Expr>),
    Def(Box<Expr>),
}

#[test]
fn test_map_type_exists() {
    let tm: ExprMap<char> = ExprMap::new();
}

#[test]
fn test_insert_then_get() {
    let mut tm: ExprMap<char> = ExprMap::new();

    let key = Expr::App(Expr::Var(0).into(), 42, Expr::Var(1).into());
    let value = 'v';
    tm.insert(key.clone(), value);

    assert_eq!(tm.get(&key), Some(&value));
}

#[test]
fn test_insert_multi_then_get() {
    let mut tm: ExprMap<char> = ExprMap::new();

    let key = Expr::App(Expr::Var(0).into(), 42, Expr::Var(1).into());
    let value = 'v';
    tm.insert(key.clone(), value);

    assert_eq!(tm.get(&key), Some(&value));

    let key = Expr::Def(Expr::Var(0).into());
    let value = 'a';
    tm.insert(key.clone(), value);

    assert_eq!(tm.get(&key), Some(&value));
}
