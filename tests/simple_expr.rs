use std::collections::HashMap;
use std::hash::Hash;

use triemap::{merge_with_trait, trie_map};

type UsizeMap<V> = HashMap<usize, V>;

merge_with_trait!(MergeWith);

#[derive(Debug, Clone, PartialEq, Eq)]
#[trie_map(MergeWith, Expr -> *ExprMap, usize -> UsizeMap)]
pub enum Expr {
    Zero,
    Var(usize),
    App(Box<Expr>, usize, Box<Expr>),
    Def(Box<Expr>),
}

impl<V> MergeWith<Self> for Option<V> {
    type Value = V;

    fn merge_with(&mut self, that: Self, func: &mut dyn FnMut(&mut Self::Value, Self::Value)) {
        match (self, that) {
            (None, None) => {}
            (None, r) => *self = r,
            (l, None) => {}
            (Some(mut l), Some(r)) => func(&mut l, r),
        }
    }
}

impl<K, V> MergeWith<Self> for HashMap<K, V>
where
    K: Eq + Hash,
{
    type Value = V;

    fn merge_with(&mut self, that: Self, func: &mut dyn FnMut(&mut Self::Value, Self::Value)) {
        for (k2, v2) in that {
            match self.entry(k2) {
                std::collections::hash_map::Entry::Occupied(mut entry) => {
                    func(entry.get_mut(), v2);
                }
                std::collections::hash_map::Entry::Vacant(entry) => {
                    entry.insert(v2);
                }
            }
        }
    }
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
