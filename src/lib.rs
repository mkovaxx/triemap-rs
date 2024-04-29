use quote::quote;
use syn::{parse_macro_input, DeriveInput, Ident};

mod trie_map;

/// Makes a fresh trait that unifies map-like things
#[proc_macro]
pub fn declare_map_trait(args: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let name = parse_macro_input!(args as Ident);
    quote! {
        trait #name {
            type K;
            type V;

            fn empty() -> Self;
            fn one(key: Self::K, value: Self::V) -> Self;

            fn get(&self, key: &Self::K) -> Option<&Self::V>;
            fn remove(&mut self, key: &Self::K) -> Option<Self::V>;
            fn insert_with(
                &mut self,
                key: Self::K,
                value: Self::V,
                func: &mut dyn FnMut(&mut Self::V, Self::V),
            );
            fn merge_with(&mut self, that: Self, func: &mut dyn FnMut(&mut Self::V, Self::V));

            fn insert(&mut self, key: Self::K, value: Self::V) {
                self.insert_with(key, value, &mut |v, w| *v = w);
            }
        }

        /// Provide an impl for a map M<K, V> "factorized" as (M<K, DefaultKey>, SlotMap<DefaultKey, V>)
        /// "Factorizing" maps such as ExprMap<ExprMap<V>> avoids "recursive type overflow" in rustc
        impl<M, V> #name for (M, slotmap::SlotMap<slotmap::DefaultKey, V>)
        where
            M: #name<V = slotmap::DefaultKey>,
        {
            type K = M::K;
            type V = V;

            fn empty() -> Self {
                (M::empty(), slotmap::SlotMap::new())
            }

            fn one(key: M::K, value: V) -> Self {
                let mut slotmap = slotmap::SlotMap::new();
                let slot_k = slotmap.insert(value);
                (M::one(key, slot_k), slotmap)
            }

            fn get(&self, key: &M::K) -> Option<&V> {
                let slot_k = self.0.get(key)?;
                self.1.get(*slot_k)
            }

            fn remove(&mut self, key: &M::K) -> Option<V> {
                let slot_k = self.0.remove(key)?;
                let value = self.1.remove(slot_k).unwrap();
                Some(value)
            }

            fn merge_with(&mut self, mut that: Self, func: &mut dyn FnMut(&mut V, V)) {
                self.0.merge_with(that.0, &mut |slot_k1, slot_k2| {
                    let value1 = &mut self.1[*slot_k1];
                    let value2 = that.1.remove(slot_k2).unwrap();
                    func(value1, value2);
                });
            }

            fn insert_with(
                &mut self,
                key: Self::K,
                value: Self::V,
                func: &mut dyn FnMut(&mut Self::V, Self::V),
            ) {
                self.merge_with(Self::one(key, value), func);
            }
        }
    }
    .into()
}

#[proc_macro_attribute]
pub fn trie_map(
    attr: proc_macro::TokenStream,
    item: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    let args = parse_macro_input!(attr as crate::trie_map::TrieMapArgs);
    let key = parse_macro_input!(item as DeriveInput);
    let result = crate::trie_map::trie_map_impl(args, key);
    result.unwrap_or_else(|err| err.to_compile_error().into())
}
