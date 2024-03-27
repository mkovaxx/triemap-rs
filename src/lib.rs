use std::collections::BTreeMap;

use proc_macro::TokenStream;
use quote::quote;
use syn::{
    parse::{Parse, ParseStream},
    parse_macro_input,
    punctuated::Punctuated,
    DeriveInput, Ident, Result, Token,
};

#[proc_macro_attribute]
pub fn trie_map(attr: TokenStream, item: TokenStream) -> TokenStream {
    let args = parse_macro_input!(attr as Args);
    let input = parse_macro_input!(item as DeriveInput);

    let key_name = &input.ident;
    let tm_name = args.type_map.get(key_name).expect(&format!(
        "the type mapping must contain an entry for {}",
        key_name,
    ));

    quote! {
        #input

        enum #tm_name<V> {
            Empty,
            Single {
                key: #key_name,
                value: V,
            },
            Multi {
                // TODO
            },
        }

        impl<V> #tm_name<V> {
            pub fn new() -> Self {
                Self::Empty
            }

            pub fn single(key: #key_name, value: V) -> Self {
                todo!()
            }

            pub fn get(&self, key: &#key_name) -> Option<&V> {
                todo!()
            }

            pub fn insert(&mut self, key: #key_name, value: V) {
                todo!()
            }
        }
    }
    .into()
}

struct Args {
    type_map: BTreeMap<Ident, Ident>,
}

impl Parse for Args {
    fn parse(input: ParseStream) -> Result<Self> {
        let entries = Punctuated::<TypeMapEntry, Token![,]>::parse_terminated(input)?;
        let type_map = entries
            .into_iter()
            .map(|entry| (entry.key, entry.value))
            .collect();
        Ok(Args { type_map })
    }
}

struct TypeMapEntry {
    key: Ident,
    value: Ident,
}

impl Parse for TypeMapEntry {
    fn parse(input: ParseStream) -> Result<Self> {
        let key = input.parse::<Ident>()?;
        let _ = input.parse::<Token![->]>()?;
        let value = input.parse::<Ident>()?;
        Ok(TypeMapEntry { key, value })
    }
}
