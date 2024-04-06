use std::collections::BTreeMap;

use quote::{format_ident, quote};
use syn::{
    parse::{Parse, ParseStream},
    parse_macro_input, parse_quote,
    punctuated::Punctuated,
    DeriveInput, Ident, Result, Token,
};

#[proc_macro_attribute]
pub fn trie_map(
    attr: proc_macro::TokenStream,
    item: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    let args = parse_macro_input!(attr as Args);
    let key = parse_macro_input!(item as DeriveInput);

    let key_name = &key.ident;
    let wrapper_name = args.type_map.get(key_name).expect(&format!(
        "the type mapping must contain an entry for {}",
        key_name,
    ));
    let inner_name = format_ident!("Many_{}", wrapper_name);

    let wrapper = generate_wrapper(wrapper_name, &inner_name, key_name);
    let inner = generate_inner(&inner_name, wrapper_name);

    quote! {
        #key

        #wrapper

        #inner
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

fn generate_wrapper(
    wrapper_name: &Ident,
    inner_name: &Ident,
    key_name: &Ident,
) -> proc_macro2::TokenStream {
    parse_quote! {
        pub enum #wrapper_name<V> {
            Empty,
            One(#key_name, V),
            Many(Box<#inner_name>),
        }

        impl<V> #wrapper_name<V> {
            pub fn new() -> Self {
                Self::Empty
            }

            pub fn get(&self, key: &#key_name) -> Option<&V> {
                match self {
                    Self::Empty => None,
                    Self::One(k, value) => {
                        if k == key {
                            Some(value)
                        } else {
                            None
                        }
                    }
                    Self::Many(em) => em.get(key),
                }
            }

            pub fn insert(&mut self, key: #key_name, value: V) {
                // an offering to the Borrow Checker
                let mut old_self = #wrapper_name::Empty;
                std::mem::swap(self, &mut old_self);

                match old_self {
                    Self::Empty => {
                        *self = Self::One(key, value);
                    }
                    Self::One(k, v) => {
                        let mut em = Box::new(#inner_name::new());
                        em.insert(k, v);
                        em.insert(key, value);
                        *self = Self::Many(em);
                    }
                    Self::Many(mut em) => {
                        em.insert(key, value);
                        *self = Self::Many(em);
                    }
                }
            }

            pub fn remove(&mut self, key: &#key_name) -> Option<V> {
                // an offering to the Borrow Checker
                let mut old_self = Self::Empty;
                std::mem::swap(self, &mut old_self);

                match old_self {
                    Self::Empty => {
                        *self = Self::Empty;
                        None
                    }
                    Self::One(k, value) => {
                        if k == *key {
                            *self = Self::Empty;
                            Some(value)
                        } else {
                            *self = Self::One(k, value);
                            None
                        }
                    }
                    Self::Many(mut em) => {
                        let value = em.remove(key);
                        // TODO: collapse into One when possible
                        *self = Self::Many(em);
                        value
                    }
                }
            }

            pub fn merge_with<F>(&mut self, that: Self, func: &mut F)
            where
                F: FnMut(&mut V, V),
            {
                // an offering to the Borrow Checker
                let mut old_self = Self::Empty;
                std::mem::swap(self, &mut old_self);

                match old_self {
                    Self::Empty => {
                        *self = that;
                    }
                    Self::One(key, value) => {
                        *self = that;
                        self.insert(key, value);
                    }
                    Self::Many(mut em) => match that {
                        Self::Empty => {}
                        Self::One(key, value) => {
                            em.insert(key, value);
                            *self = Self::Many(em);
                        }
                        Self::Many(em_that) => {
                            em.merge_with(*em_that, func);
                            *self = Self::Many(em);
                        }
                    },
                }
            }
        }
    }
}

fn generate_inner(inner_name: &Ident, wrapper_name: &Ident) -> proc_macro2::TokenStream {
    parse_quote! {
        #[allow(non_camel_case_types)]
        struct #inner_name<V> {
            // TODO
        }
    }
}
