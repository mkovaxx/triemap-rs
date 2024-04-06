use std::collections::BTreeMap;

use quote::{format_ident, quote};
use syn::{
    parse::{Parse, ParseStream},
    parse_macro_input, parse_quote,
    punctuated::Punctuated,
    Data, DeriveInput, Fields, Ident, Result, Token, Type,
};

#[proc_macro_attribute]
pub fn trie_map(
    attr: proc_macro::TokenStream,
    item: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    let args = parse_macro_input!(attr as Args);
    let key = parse_macro_input!(item as DeriveInput);

    let key_name = &key.ident;
    let wrapper_entry = args.type_map.get(key_name).expect(&format!(
        "the type mapping must contain an entry for {}",
        key_name,
    ));
    let wrapper_name = &wrapper_entry.name;
    let inner_name = format_ident!("Many_{}", wrapper_name);

    let key_variants = get_key_variants(&key);
    let inner_fields: Vec<InnerField> = vec![];

    let wrapper = generate_wrapper(wrapper_name, &inner_name, key_name);
    let inner = generate_inner(&inner_name, wrapper_name, &inner_fields);

    quote! {
        #key

        #wrapper

        #inner
    }
    .into()
}

fn get_key_variants(key: &DeriveInput) -> Vec<KeyVariant> {
    match &key.data {
        Data::Struct(struct_data) => vec![
            // treat a struct as if it was the single variant of an enum
            KeyVariant {
                name: key.ident.to_string(),
                fields: struct_data.fields.clone(),
            },
        ],
        Data::Enum(enum_data) => enum_data
            .variants
            .iter()
            .map(|variant| KeyVariant {
                name: format!("{}::{}", key.ident, variant.ident),
                fields: variant.fields.clone(),
            })
            .collect(),
        Data::Union(_) => panic!("union types are not supported"),
    }
}

struct Args {
    type_map: TypeMap,
}

type TypeMap = BTreeMap<Ident, TypeMapEntry>;

struct TypeMapEntry {
    name: Ident,
    is_indirect: bool,
}

impl Parse for Args {
    fn parse(input: ParseStream) -> Result<Self> {
        let entries = Punctuated::<TypeMapArg, Token![,]>::parse_terminated(input)?;
        let type_map = entries
            .into_iter()
            .map(|arg| {
                (
                    arg.key,
                    TypeMapEntry {
                        name: arg.value,
                        is_indirect: arg.is_indirect,
                    },
                )
            })
            .collect();
        Ok(Args { type_map })
    }
}

struct TypeMapArg {
    key: Ident,
    value: Ident,
    is_indirect: bool,
}

impl Parse for TypeMapArg {
    fn parse(input: ParseStream) -> Result<Self> {
        let key = input.parse::<Ident>()?;
        let _ = input.parse::<Token![->]>()?;
        let is_indirect = input.parse::<Token![*]>().is_ok();
        let value = input.parse::<Ident>()?;
        Ok(TypeMapArg {
            key,
            value,
            is_indirect,
        })
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

struct KeyVariant {
    name: String,
    fields: Fields,
}

struct InnerField {
    name: String,
    map_ty: Type,
    store_ty: Option<Type>,
}

fn generate_inner(
    inner_name: &Ident,
    wrapper_name: &Ident,
    inner_fields: &Vec<InnerField>,
) -> proc_macro2::TokenStream {
    let typed_fields: Vec<proc_macro2::TokenStream> = inner_fields
        .iter()
        .map(
            |InnerField {
                 name,
                 map_ty,
                 store_ty,
             }| {
                let map_name = format_ident!("map_{}", name);
                let store_name = format_ident!("store_{}", name);
                if let Some(store_ty) = store_ty {
                    parse_quote! {
                        #map_name: #map_ty,
                        #store_name: #store_ty,
                    }
                } else {
                    parse_quote!(#map_name: #map_ty,)
                }
            },
        )
        .collect();

    parse_quote! {
        #[allow(non_camel_case_types)]
        struct #inner_name<V> {
            #(#typed_fields)*
        }
    }
}

fn generate_inner_field(type_map: &TypeMap, variant: KeyVariant) -> InnerField {
    let fields: Vec<_> = variant.fields.iter().collect();
    if fields.is_empty() {
        // unit variant
        InnerField {
            name: variant.name.to_string(),
            map_ty: parse_quote!(Option<V>),
            store_ty: None,
        }
    } else {
        // variant has at least one field
        let first_field = fields[0];
        let field_ty_name = get_field_type_name(&first_field.ty);

        let entry = &type_map[&field_ty_name];
        let map_name = &entry.name;

        let value_ty: proc_macro2::TokenStream = if fields.len() == 1 {
            parse_quote!(V)
        } else {
            parse_quote!(#map_name<V>)
        };

        if !entry.is_indirect {
            InnerField {
                name: variant.name,
                map_ty: parse_quote!(#map_name<#value_ty>),
                store_ty: None,
            }
        } else {
            InnerField {
                name: variant.name,
                map_ty: parse_quote!(#map_name<DefaultKey>),
                store_ty: Some(parse_quote!(SlotMap<DefaultKey, #value_ty>)),
            }
        }
    }
}

fn get_field_type_name(ty: &Type) -> Ident {
    match ty {
        Type::Array(_) => todo!(),
        Type::BareFn(_) => todo!(),
        Type::Group(_) => todo!(),
        Type::ImplTrait(_) => todo!(),
        Type::Infer(_) => todo!(),
        Type::Macro(_) => todo!(),
        Type::Never(_) => todo!(),
        Type::Paren(_) => todo!(),
        Type::Path(_) => todo!(),
        Type::Ptr(_) => todo!(),
        Type::Reference(_) => todo!(),
        Type::Slice(_) => todo!(),
        Type::TraitObject(_) => todo!(),
        Type::Tuple(_) => todo!(),
        Type::Verbatim(_) => todo!(),
        _ => todo!(),
    }
}
