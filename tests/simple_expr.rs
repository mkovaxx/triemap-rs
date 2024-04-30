use std::collections::HashMap;
use std::hash::Hash;

use triemap::{declare_map_trait, trie_map};

type UsizeMap<V> = HashMap<usize, V>;

declare_map_trait!(Map);

#[derive(Debug, Clone, PartialEq, Eq)]
#[trie_map(Map, Expr -> *ExprMap, usize -> UsizeMap)]
pub enum Expr {
    Zero,
    Var(usize),
    App(Box<Expr>, usize, Box<Expr>),
    Def(Box<Expr>),
}

impl<K, V> Map for HashMap<K, V>
where
    K: Eq + Hash,
{
    type K = K;
    type V = V;

    fn empty() -> Self {
        HashMap::new()
    }

    fn one(key: K, value: V) -> Self {
        Self::from_iter([(key, value)])
    }

    fn get(&self, key: &K) -> Option<&V> {
        self.get(key)
    }

    fn remove(&mut self, key: &K) -> Option<V> {
        self.remove(key)
    }

    fn insert_with(
        &mut self,
        key: Self::K,
        value: Self::V,
        func: &mut dyn FnMut(&mut Self::V, Self::V),
    ) {
        match self.entry(key) {
            std::collections::hash_map::Entry::Occupied(mut entry) => {
                func(entry.get_mut(), value);
            }
            std::collections::hash_map::Entry::Vacant(entry) => {
                entry.insert(value);
            }
        }
    }

    fn merge_with(&mut self, that: Self, func: &mut dyn FnMut(&mut V, V)) {
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

impl<V> Map for Option<V> {
    type K = ();
    type V = V;

    fn empty() -> Self {
        None
    }

    fn one(_key: (), value: V) -> Self {
        Some(value)
    }

    fn get(&self, _key: &()) -> Option<&V> {
        self.as_ref()
    }

    fn remove(&mut self, _key: &()) -> Option<V> {
        self.take()
    }

    fn insert_with(&mut self, _key: (), value: V, func: &mut dyn FnMut(&mut V, V)) {
        match self {
            Some(old_value) => func(old_value, value),
            None => *self = Some(value),
        }
    }

    fn merge_with(&mut self, that: Self, func: &mut dyn FnMut(&mut V, V)) {
        // a humble offering to the Borrow Checker, Keeper of Lifetimes
        let mut old_self = Self::empty();
        std::mem::swap(self, &mut old_self);

        match (old_self, that) {
            (None, None) => {}
            (None, Some(v))|
            (Some(v), None) => *self = Some(v),
            (Some(mut v), Some(w)) => {
                func(&mut v, w);
                *self = Some(v);
            }
        }
    }
}

#[test]
fn test_map_type_exists() {
    let tm: ExprMap<char> = ExprMap::empty();
}

#[test]
fn test_insert_then_get() {
    let mut tm: ExprMap<char> = ExprMap::empty();

    let key = Expr::App(Expr::Var(0).into(), 42, Expr::Var(1).into());
    let value = 'v';
    tm.insert(key.clone(), value);

    assert_eq!(tm.get(&key), Some(&value));
}

#[test]
fn test_insert_multi_then_get() {
    let mut tm: ExprMap<char> = ExprMap::empty();

    let key = Expr::App(Expr::Var(0).into(), 42, Expr::Var(1).into());
    let value = 'v';
    tm.insert(key.clone(), value);

    assert_eq!(tm.get(&key), Some(&value));

    let key = Expr::Def(Expr::Var(0).into());
    let value = 'a';
    tm.insert(key.clone(), value);

    assert_eq!(tm.get(&key), Some(&value));
}

#[test]
fn test_insert_two_apps() {
    let mut tm: ExprMap<&str> = ExprMap::empty();

    let key = Expr::App(Expr::Var(0).into(), 42, Expr::Var(1).into());
    tm.insert(key.clone(), "test");
    assert_eq!(tm.get(&key), Some(&"test"));

    let key = Expr::App(Expr::Var(2).into(), 42, Expr::Var(3).into());
    tm.insert(key.clone(), "another_test");
    assert_eq!(tm.get(&key), Some(&"another_test"));
}
