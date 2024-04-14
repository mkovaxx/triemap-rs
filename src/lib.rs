use std::collections::BTreeMap;

use proc_macro2::Span;
use quote::{format_ident, quote, ToTokens};
use syn::{
    parse::{Parse, ParseStream},
    parse_macro_input,
    punctuated::Punctuated,
    spanned::Spanned,
    Data, DeriveInput, Error, GenericArgument, Ident, PathArguments, Result, Token, Type,
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
                format!("The type mapping must contain an entry for `{}`", key_name,),
            ))
        }
    };
    let wrapper_name = &wrapper_entry.name;
    let inner_name = format_ident!("Many_{}", wrapper_name);

    let key_variants = get_key_variants(&key, &args.type_map)?;

    let wrapper = generate_wrapper(key_name, wrapper_name, &inner_name);
    let inner = generate_inner(key_name, key_variants, &inner_name);

    Ok(quote! {
        #key

        #wrapper

        #inner
    }
    .into())
}

fn get_key_variants(key: &DeriveInput, type_map: &TypeMap) -> Result<Vec<KeyVariant>> {
    match &key.data {
        Data::Struct(_) => Err(Error::new(key.span(), "struct types are not yet supported")),
        Data::Enum(enum_data) => {
            let mut key_variants = vec![];
            for variant in &enum_data.variants {
                let mut field_types = vec![];

                for field in &variant.fields {
                    let field_ty_name = get_type_name(&field.ty)?;
                    let entry = match type_map.get(&field_ty_name) {
                        Some(entry) => entry,
                        None => {
                            return Err(Error::new(
                                field.ty.span(),
                                format!(
                                    "No entry for `{}` in the type mapping",
                                    field.ty.to_token_stream(),
                                ),
                            ))
                        }
                    };
                    field_types.push(KeyVariantFieldType {
                        map_ty: entry.name.clone(),
                        is_map_ty_indirect: entry.is_indirect,
                    });
                }

                // infer field indirection requirements
                // a field is indirect if
                //     - its own type is indirect, and
                //     - it has any field after it whose type is indirect
                let mut has_nested_indirect = false;
                for field in field_types.iter_mut().rev() {
                    let tmp = field.is_map_ty_indirect;
                    field.is_map_ty_indirect &= has_nested_indirect;
                    has_nested_indirect |= tmp;
                }

                key_variants.push(KeyVariant {
                    name: Some(variant.ident.clone()),
                    fields: variant.fields.clone(),
                    field_types,
                });
            }
            Ok(key_variants)
        }
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
    quote! {
        pub enum #wrapper_name<V> {
            Empty,
            One(#key_name, V),
            Many(Box<#inner_name<V>>),
        }

        impl<V> std::default::Default for #wrapper_name<V> {
            fn default() -> Self {
                Self::new()
            }
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
    // We treat a struct as an enum with a single variant that has no name
    name: Option<Ident>,
    fields: syn::Fields,
    field_types: Vec<KeyVariantFieldType>,
}

#[derive(Clone)]
struct KeyVariantFieldType {
    map_ty: Ident,
    is_map_ty_indirect: bool,
}

type TypedField = (Ident, KeyVariantFieldType);

fn generate_inner(
    key_name: &Ident,
    variants: Vec<KeyVariant>,
    inner_name: &Ident,
) -> proc_macro2::TokenStream {
    let typed_fields: Vec<proc_macro2::TokenStream> = variants
        .iter()
        .map(|variant| {
            let field_name = generate_variant_field_name(key_name, &variant.name);
            generate_variant_map_field(&field_name, &variant.field_types)
        })
        .collect();

    let field_inits: Vec<proc_macro2::TokenStream> = variants
        .iter()
        .map(|variant| {
            let field_name = generate_variant_field_name(key_name, &variant.name);
            quote!(#field_name: std::default::Default::default(),)
        })
        .collect();

    let variant_getters: Vec<proc_macro2::TokenStream> = variants
        .iter()
        .map(|variant| {
            let (variant_pattern, typed_fields) = generate_variant_pattern(key_name, variant);
            let field_name = generate_variant_field_name(key_name, &variant.name);
            let variant_getter = generate_variant_getter(&field_name, &typed_fields);
            quote!(#variant_pattern => #variant_getter)
        })
        .collect();

    let variant_removers: Vec<proc_macro2::TokenStream> = variants
        .iter()
        .map(|variant| {
            let (variant_pattern, typed_fields) = generate_variant_pattern(key_name, variant);
            let field_name = generate_variant_field_name(key_name, &variant.name);
            let variant_remover = generate_variant_remover(&field_name, &typed_fields);
            quote!(#variant_pattern => #variant_remover)
        })
        .collect();

    quote! {
        #[allow(non_camel_case_types, non_snake_case)]
        struct #inner_name<V> {
            #(#typed_fields)*
        }

        impl<V> std::default::Default for #inner_name<V> {
            fn default() -> Self {
                Self::new()
            }
        }

        impl<V> #inner_name<V> {
            pub fn new() -> Self {
                Self {
                    #(#field_inits)*
                }
            }

            pub fn get(&self, key: &#key_name) -> Option<&V> {
                match key {
                    #(#variant_getters)*
                }
            }

            pub fn insert(&mut self, key: #key_name, value: V) {
                todo!()
            }

            pub fn remove(&mut self, key: &#key_name) -> Option<V> {
                match key {
                    #(#variant_removers)*
                }
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

fn generate_variant_field_name(key_name: &Ident, variant_name: &Option<Ident>) -> Ident {
    format_ident!("map_{}", variant_name.as_ref().unwrap_or(key_name))
}

fn generate_variant_pattern(
    key_name: &Ident,
    variant: &KeyVariant,
) -> (proc_macro2::TokenStream, Vec<TypedField>) {
    let variant_name = match &variant.name {
        Some(name) => quote!(#key_name::#name),
        None => quote!(#key_name),
    };

    match &variant.fields {
        syn::Fields::Named(fields) => {
            let field_names: Vec<_> = fields
                .named
                .iter()
                .map(|field| format_ident!("f_{}", field.ident.as_ref().unwrap()))
                .collect();
            let pattern = quote! {
                #variant_name { #(#field_names,)* }
            };
            let typed_fields = field_names
                .into_iter()
                .zip(variant.field_types.iter().cloned())
                .collect();
            (pattern, typed_fields)
        }
        syn::Fields::Unnamed(fields) => {
            let field_names: Vec<_> = (0..fields.unnamed.len())
                .map(|i| format_ident!("f_{}", i))
                .collect();
            let pattern = quote! {
                #variant_name( #(#field_names,)* )
            };
            let typed_fields = field_names
                .into_iter()
                .zip(variant.field_types.iter().cloned())
                .collect();
            (pattern, typed_fields)
        }
        syn::Fields::Unit => {
            let pattern = quote! {
                #variant_name
            };
            (pattern, vec![])
        }
    }
}

fn generate_variant_getter(
    map_name: &Ident,
    typed_fields: &Vec<TypedField>,
) -> proc_macro2::TokenStream {
    if typed_fields.is_empty() {
        quote! {
            {
                self.#map_name.as_ref()
            }
        }
    } else {
        let mut steps: Vec<proc_macro2::TokenStream> = vec![quote! {
            let v_0 = &self.#map_name;
        }];

        for (i, (field_name, field_ty)) in typed_fields.iter().enumerate() {
            let v_prev = format_ident!("v_{}", i);
            let v_curr = format_ident!("v_{}", i + 1);
            if field_ty.is_map_ty_indirect {
                let k_curr = format_ident!("k_{}", i + 1);
                steps.push(quote! {
                    let #k_curr = #v_prev.0.get(#field_name)?;
                    let #v_curr = &#v_prev.1[*#k_curr];
                });
            } else {
                steps.push(quote! {
                    let #v_curr = #v_prev.get(#field_name)?;
                });
            }
        }

        let v_last = format_ident!("v_{}", typed_fields.len());

        quote! {
            {
                #(#steps)*
                Some(#v_last)
            }
        }
    }
}

fn generate_variant_remover(
    map_name: &Ident,
    typed_fields: &Vec<TypedField>,
) -> proc_macro2::TokenStream {
    if typed_fields.is_empty() {
        quote! {
            {
                self.#map_name.take()
            }
        }
    } else {
        let mut steps: Vec<proc_macro2::TokenStream> = vec![quote! {
            let mut v_0 = &mut self.#map_name;
        }];

        for (i, (field_name, field_ty)) in typed_fields.iter().enumerate() {
            let v_prev = format_ident!("v_{}", i);
            let v_curr = format_ident!("v_{}", i + 1);
            if field_ty.is_map_ty_indirect {
                let k_curr = format_ident!("k_{}", i + 1);
                steps.push(quote! {
                    let mut #k_curr = #v_prev.0.remove(&#field_name)?;
                    let mut #v_curr = #v_prev.1.remove(#k_curr).unwrap();
                });
            } else {
                steps.push(quote! {
                    let mut #v_curr = #v_prev.remove(&#field_name)?;
                });
            }
        }

        let v_last = format_ident!("v_{}", typed_fields.len());

        quote! {
            {
                #(#steps)*
                Some(#v_last)
            }
        }
    }
}

fn generate_variant_map_field(
    map_name: &Ident,
    field_types: &Vec<KeyVariantFieldType>,
) -> proc_macro2::TokenStream {
    let map_ty = if field_types.is_empty() {
        // unit variant
        quote!(Option<V>)
    } else {
        // variant has at least one payload field
        let mut ty = quote!(V);
        for field_ty in field_types.iter().rev() {
            let map_ty = &field_ty.map_ty;
            ty = if field_ty.is_map_ty_indirect {
                // when indirection is required, we use this other pattern
                quote! {
                    (
                        #map_ty<slotmap::DefaultKey>,
                        slotmap::SlotMap<slotmap::DefaultKey, #ty>,
                    )
                }
            } else {
                // this is the simply nested case
                quote!(#map_ty<#ty>)
            };
        }
        ty
    };
    quote! {
        #map_name: #map_ty,
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
                                "Expected a type, got `{}`",
                                last_segment.arguments.to_token_stream()
                            ),
                        ))
                    }
                } else {
                    Err(Error::new(
                        ty.span(),
                        format!(
                            "Expected <GENERIC_ARGS>, got `{}`",
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
            format!("Not supported here: `{}`", ty.into_token_stream()),
        )),
    }
}
