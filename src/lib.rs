use std::collections::BTreeMap;

use proc_macro2::Span;
use quote::{format_ident, quote, ToTokens};
use syn::{
    parse::{Parse, ParseStream},
    parse_macro_input, parse_quote,
    punctuated::Punctuated,
    spanned::Spanned,
    Data, DeriveInput, Error, Fields, GenericArgument, Ident, PathArguments, Result, Token, Type,
};

#[proc_macro_attribute]
pub fn trie_map(
    attr: proc_macro::TokenStream,
    item: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    let args = parse_macro_input!(attr as Args);
    let key = parse_macro_input!(item as DeriveInput);
    let result = trie_map_impl(args, key);
    result.unwrap_or_else(|err| err.to_compile_error().into())
}

fn trie_map_impl(args: Args, key: DeriveInput) -> Result<proc_macro::TokenStream> {
    let key_name = &key.ident;
    let wrapper_entry = match args.type_map.get(key_name) {
        Some(entry) => entry,
        None => {
            return Err(Error::new(
                args.span,
                format!("the type mapping must contain an entry for {}", key_name,),
            ))
        }
    };
    let wrapper_name = &wrapper_entry.name;
    let inner_name = format_ident!("Many_{}", wrapper_name);

    let key_variants = get_key_variants(&key)?;

    let mut inner_fields: Vec<InnerField> = vec![];
    for key_variant in key_variants {
        let inner_field = generate_inner_field(key_name, &key_variant, &args.type_map)?;
        inner_fields.push(inner_field);
    }

    let wrapper = generate_wrapper(key_name, wrapper_name, &inner_name);
    let inner = generate_inner(key_name, wrapper_name, &inner_name, &inner_fields);

    Ok(quote! {
        #key

        #wrapper

        #inner
    }
    .into())
}

fn get_key_variants(key: &DeriveInput) -> Result<Vec<KeyVariant>> {
    match &key.data {
        Data::Struct(_) => Err(Error::new(key.span(), "struct types are not yet supported")),
        Data::Enum(enum_data) => Ok(enum_data
            .variants
            .iter()
            .map(|variant| KeyVariant {
                name: Some(variant.ident.clone()),
                fields: variant.fields.clone(),
            })
            .collect()),
        Data::Union(_) => Err(Error::new(key.span(), "union types are not supported")),
    }
}

struct Args {
    type_map: TypeMap,
    span: Span,
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
        Ok(Args {
            type_map,
            span: input.span(),
        })
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
    key_name: &Ident,
    wrapper_name: &Ident,
    inner_name: &Ident,
) -> proc_macro2::TokenStream {
    parse_quote! {
        pub enum #wrapper_name<V> {
            Empty,
            One(#key_name, V),
            Many(Box<#inner_name<V>>),
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
    name: Option<Ident>,
    fields: Fields,
}

struct InnerField {
    name: Ident,
    map_ty: proc_macro2::TokenStream,
    store_ty: Option<proc_macro2::TokenStream>,
}

fn generate_inner(
    key_name: &Ident,
    wrapper_name: &Ident,
    inner_name: &Ident,
    inner_fields: &Vec<InnerField>,
) -> proc_macro2::TokenStream {
    let typed_fields: Vec<proc_macro2::TokenStream> = inner_fields
        .iter()
        .map(|field| {
            let map_ty = &field.map_ty;
            let map_name = format_ident!("map_{}", field.name);
            let store_name = format_ident!("store_{}", field.name);
            if let Some(store_ty) = &field.store_ty {
                parse_quote! {
                    #map_name: #map_ty,
                    #store_name: #store_ty,
                }
            } else {
                parse_quote!(#map_name: #map_ty,)
            }
        })
        .collect();

    parse_quote! {
        #[allow(non_camel_case_types)]
        struct #inner_name<V> {
            #(#typed_fields)*
        }

        impl<V> #inner_name<V> {
            pub fn new() -> Self {
                todo!()
            }

            pub fn get(&self, key: &#key_name) -> Option<&V> {
                todo!()
            }

            pub fn insert(&mut self, key: #key_name, value: V) {
                todo!()
            }

            pub fn remove(&mut self, key: &#key_name) -> Option<V> {
                todo!()
            }

            pub fn merge_with<F>(&mut self, that: Self, func: &mut F)
            where
                F: FnMut(&mut V, V),
            {
                todo!()
            }
        }
    }
}

fn generate_inner_field(
    key_name: &Ident,
    variant: &KeyVariant,
    type_map: &TypeMap,
) -> Result<InnerField> {
    let fields: Vec<_> = variant.fields.iter().collect();
    if fields.is_empty() {
        // unit variant
        Ok(InnerField {
            name: variant.name.clone().unwrap_or(key_name.clone()),
            map_ty: parse_quote!(Option<V>),
            store_ty: None,
        })
    } else {
        // TODO: revisit this part because it is wrong :)

        // variant has at least one field
        let first_field = fields[0];
        let field_ty_name = get_type_name(&first_field.ty)?;

        let entry = match type_map.get(&field_ty_name) {
            Some(entry) => entry,
            None => {
                return Err(Error::new(
                    first_field.span(),
                    format!("No entry for {:#?} in the type mapping", first_field.ident),
                ))
            }
        };
        let map_name = &entry.name;

        let value_ty: proc_macro2::TokenStream = if fields.len() == 1 {
            parse_quote!(V)
        } else {
            parse_quote!(#map_name<V>)
        };

        if !entry.is_indirect {
            Ok(InnerField {
                name: variant.name.clone().unwrap_or(key_name.clone()),
                map_ty: parse_quote!(#map_name<#value_ty>),
                store_ty: None,
            })
        } else {
            Ok(InnerField {
                name: variant.name.clone().unwrap_or(key_name.clone()),
                map_ty: parse_quote!(#map_name<slotmap::DefaultKey>),
                store_ty: Some(parse_quote!(slotmap::SlotMap<slotmap::DefaultKey, #value_ty>)),
            })
        }
    }
}

fn get_type_name(ty: &Type) -> Result<Ident> {
    match ty {
        Type::Path(path) => {
            let last_segment = path.path.segments.last().unwrap();
            if last_segment.ident == format_ident!("Box") {
                // special case: spoof Box<T> to be just T
                if let PathArguments::AngleBracketed(ref args) = last_segment.arguments {
                    let args: Vec<_> = args.args.iter().collect();
                    if let [GenericArgument::Type(ty)] = &args[..] {
                        get_type_name(ty)
                    } else {
                        Err(Error::new(
                            ty.span(),
                            format!(
                                "Expected a type, got {}",
                                last_segment.arguments.to_token_stream()
                            ),
                        ))
                    }
                } else {
                    Err(Error::new(
                        ty.span(),
                        format!(
                            "Expected <GENERIC_ARGS>, got {}",
                            last_segment.arguments.to_token_stream()
                        ),
                    ))
                }
            } else {
                Ok(last_segment.ident.clone())
            }
        }
        _ => Err(Error::new(
            ty.span(),
            format!("Not supported here: {}", ty.into_token_stream()),
        )),
    }
}
