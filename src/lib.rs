use quote::quote;
use syn::{parse_macro_input, DeriveInput, Ident};

mod trie_map;

#[proc_macro]
pub fn merge_with_trait(attr: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let name = parse_macro_input!(attr as Ident);
    quote! {
        trait #name<M> {
            type Value;
            fn merge_with(&mut self, that: M, func: &mut dyn FnMut(&mut Self::Value, Self::Value));
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
