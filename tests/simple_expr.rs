use std::collections::HashMap;

use triemap::trie_map;

type UsizeMap<V> = HashMap<usize, V>;

#[trie_map(Expr -> ExprMap, usize -> UsizeMap)]
enum Expr {
    Var(usize),
    App(Box<Expr>, Box<Expr>),
}

#[test]
fn test_map_type_exists() {
    let tm: ExprMap<char> = ExprMap::new();
}
