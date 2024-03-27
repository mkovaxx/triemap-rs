use std::collections::HashMap;

use triemap::trie_map;

type UsizeMap<V> = HashMap<usize, V>;

#[derive(Debug, Clone)]
#[trie_map(Expr -> ExprMap, usize -> UsizeMap)]
enum Expr {
    Var(usize),
    App(Box<Expr>, Box<Expr>),
}

#[test]
fn test_map_type_exists() {
    let tm: ExprMap<char> = ExprMap::new();
}

#[test]
fn test_insert_then_get() {
    let mut tm: ExprMap<char> = ExprMap::new();

    let key = Expr::App(Expr::Var(0).into(), Expr::Var(1).into());
    let value = 'v';
    tm.insert(key.clone(), value);

    assert_eq!(tm.get(&key), Some(&value));
}
