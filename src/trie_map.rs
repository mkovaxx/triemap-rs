use std::collections::BTreeMap;

use proc_macro2::Span;
use quote::{format_ident, quote, ToTokens};
use syn::{
    parse::{Parse, ParseStream},
    punctuated::Punctuated,
    spanned::Spanned,
    Data, DeriveInput, Error, GenericArgument, Ident, PathArguments, Result, Token, Type,
};

pub fn trie_map_impl(args: TrieMapArgs, key: DeriveInput) -> Result<proc_macro::TokenStream> {
    let key_name = &key.ident;
    let wrapper_entry = match args.type_map.get(key_name) {
        Some(entry) => entry,
        None => {
            return Err(Error::new(
                args.span,
                format!("The type mapping must contain an entry for `{}`", key_name),
            ))
        }
    };
    let wrapper_name = &wrapper_entry.name;
    let inner_name = format_ident!("Many_{}", wrapper_name);

    let key_variants = get_key_variants(&key, &args.type_map)?;

    let wrapper = generate_wrapper(&args.map_trait_name, key_name, wrapper_name, &inner_name);
    let inner = generate_inner(&args.map_trait_name, key_name, &key_variants, &inner_name);

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
                    let (field_ty_name, is_boxed) = get_type_name(&field.ty)?;
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
                        is_boxed,
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

pub struct TrieMapArgs {
    map_trait_name: Ident,
    type_map: TypeMap,
    span: Span,
}

type TypeMap = BTreeMap<Ident, TypeMapEntry>;

struct TypeMapEntry {
    name: Ident,
    is_indirect: bool,
}

impl Parse for TrieMapArgs {
    fn parse(input: ParseStream) -> Result<Self> {
        let map_trait_name = input.parse::<Ident>()?;
        let _ = input.parse::<Token![,]>()?;
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
        Ok(TrieMapArgs {
            map_trait_name,
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
    map_trait_name: &Ident,
    key_name: &Ident,
    wrapper_name: &Ident,
    inner_name: &Ident,
) -> proc_macro2::TokenStream {
    quote! {
        #[derive(Debug)]
        pub enum #wrapper_name<V> {
            Empty,
            One(#key_name, V),
            Many(Box<#inner_name<V>>),
        }

        impl<V> #map_trait_name for #wrapper_name<V> {
            type K = #key_name;
            type V = V;

            fn empty() -> Self {
                Self::Empty
            }

            fn one(key: #key_name, value: V) -> Self {
                Self::One(key, value)
            }

            fn get(&self, key: &#key_name) -> Option<&V> {
                match self {
                    Self::Empty => None,
                    Self::One(k, value) => {
                        if k == key {
                            Some(value)
                        } else {
                            None
                        }
                    }
                    Self::Many(m) => m.get(key),
                }
            }

            fn remove(&mut self, key: &#key_name) -> Option<V> {
                // a humble offering to the Borrow Checker, Keeper of Lifetimes
                let mut old_self = Self::empty();
                std::mem::swap(self, &mut old_self);

                match old_self {
                    Self::Empty => None,
                    Self::One(k, value) => {
                        if k == *key {
                            Some(value)
                        } else {
                            *self = Self::One(k, value);
                            None
                        }
                    }
                    Self::Many(mut m) => {
                        let value = m.remove(key);
                        // TODO: collapse into Self::One when possible
                        *self = Self::Many(m);
                        value
                    }
                }
            }

            fn for_each(&mut self, func: &mut dyn FnMut(&mut V)) {
                match self {
                    Self::Empty => {}
                    Self::One(_, value) => func(value),
                    Self::Many(m) => m.for_each(func),
                }
            }

            fn insert_with(&mut self, key: #key_name, value: V, func: &mut dyn FnMut(&mut V, V)) {
                // a humble offering to the Borrow Checker, Keeper of Lifetimes
                let mut old_self = Self::empty();
                std::mem::swap(self, &mut old_self);

                match old_self {
                    Self::Empty => {
                        *self = Self::One(key, value);
                    }
                    Self::One(k, v) => {
                        let mut m = Box::new(#inner_name::one(k, v));
                        m.insert_with(key, value, func);
                        *self = Self::Many(m);
                    }
                    Self::Many(mut m) => {
                        m.insert_with(key, value, func);
                        *self = Self::Many(m);
                    }
                }
            }

            fn merge_with(&mut self, that: Self, func: &mut dyn FnMut(&mut V, V)) {
                // a humble offering to the Borrow Checker, Keeper of Lifetimes
                let mut old_self = Self::empty();
                std::mem::swap(self, &mut old_self);

                match (old_self, that) {
                    (Self::Empty, that) => {
                        *self = that;
                    }
                    (old_self, Self::Empty) => {
                        *self = old_self;
                    }
                    (Self::One(k1, v1), Self::One(k2, v2)) => {
                        let mut m1 = Box::new(#inner_name::one(k1, v1));
                        m1.insert_with(k2, v2, func);
                        *self = Self::Many(m1);
                    }
                    (Self::Many(mut m1), Self::One(k2, v2)) => {
                        m1.insert_with(k2, v2, func);
                        *self = Self::Many(m1);
                    }
                    (Self::One(k1, v1), Self::Many(m2)) => {
                        let mut m1 = Box::new(#inner_name::one(k1, v1));
                        m1.merge_with(*m2, func);
                        *self = Self::Many(m1);
                    }
                    (Self::Many(mut m1), Self::Many(m2)) => {
                        m1.merge_with(*m2, func);
                        *self = Self::Many(m1);
                    }
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
    is_boxed: bool,
    is_map_ty_indirect: bool,
}

type TypedField = (Ident, KeyVariantFieldType);

fn generate_inner(
    map_trait_name: &Ident,
    key_name: &Ident,
    variants: &[KeyVariant],
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
            quote!(#field_name: #map_trait_name::empty(),)
        })
        .collect();

    let variant_field_ones: Vec<proc_macro2::TokenStream> = variants
        .iter()
        .map(|variant| {
            let (variant_pattern, typed_fields) = generate_variant_pattern(key_name, variant);
            let field_name = generate_variant_field_name(key_name, &variant.name);
            let field_one = generate_variant_field_one(&map_trait_name, &typed_fields);
            quote!(#variant_pattern => {
                m.#field_name = #field_one;
            })
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

    let field_for_eaches = variants.iter().map(|variant| {
        let (_, typed_fields) = generate_variant_pattern(key_name, variant);
        let field_name = generate_variant_field_name(key_name, &variant.name);
        let variant_for_each = generate_variant_for_each_body(&typed_fields);
        quote!(self.#field_name.for_each(#variant_for_each);)
    });

    let field_merges = variants.iter().map(|variant| {
        let (_, typed_fields) = generate_variant_pattern(key_name, variant);
        let field_name = generate_variant_field_name(key_name, &variant.name);
        let variant_merger = generate_variant_field_merger(&typed_fields);
        quote!(self.#field_name.merge_with(that.#field_name, #variant_merger);)
    });

    quote! {
        #[derive(Debug)]
        #[allow(non_camel_case_types, non_snake_case)]
        struct #inner_name<V> {
            #(#typed_fields)*
        }

        impl<V> #map_trait_name for #inner_name<V> {
            type K = #key_name;
            type V = V;

            fn empty() -> Self {
                Self {
                    #(#field_inits)*
                }
            }

            fn one(key: #key_name, value: V) -> Self {
                let mut m = Self::empty();

                match key {
                    #(#variant_field_ones)*
                }

                m
            }

            fn get(&self, key: &#key_name) -> Option<&V> {
                match key {
                    #(#variant_getters)*
                }
            }

            fn remove(&mut self, key: &#key_name) -> Option<V> {
                match key {
                    #(#variant_removers)*
                }
            }

            fn for_each(&mut self, func: &mut dyn FnMut(&mut V)) {
                #(#field_for_eaches)*
            }

            fn insert_with(&mut self, key: #key_name, value: V, func: &mut dyn FnMut(&mut V, V)) {
                // NOTE(mkovaxx): Could be specialized for some performance gain?
                self.merge_with(Self::one(key, value), func);
            }

            fn merge_with(&mut self, that: Self, func: &mut dyn FnMut(&mut V, V)) {
                #(#field_merges)*
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

fn generate_variant_field_one(
    map_trait_name: &Ident,
    typed_fields: &Vec<TypedField>,
) -> proc_macro2::TokenStream {
    if typed_fields.is_empty() {
        quote!(#map_trait_name::one((), value))
    } else {
        let mut one = quote!(value);

        for (field_name, field_ty) in typed_fields.iter().rev() {
            let field_by_val = if field_ty.is_boxed {
                quote!(*#field_name)
            } else {
                quote!(#field_name)
            };
            one = quote!(#map_trait_name::one(#field_by_val, #one));
        }

        one
    }
}

fn generate_variant_getter(
    map_name: &Ident,
    typed_fields: &Vec<TypedField>,
) -> proc_macro2::TokenStream {
    if typed_fields.is_empty() {
        quote!(self.#map_name.get(&()),)
    } else {
        let mut steps: Vec<proc_macro2::TokenStream> = vec![quote! {
            let v_0 = &self.#map_name;
        }];

        for (i, (field_name, _)) in typed_fields.iter().enumerate() {
            let v_prev = format_ident!("v_{}", i);
            let v_curr = format_ident!("v_{}", i + 1);
            steps.push(quote! {
                let #v_curr = #v_prev.get(#field_name)?;
            });
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
        quote!(self.#map_name.remove(&()),)
    } else {
        let mut steps: Vec<proc_macro2::TokenStream> = vec![quote! {
            let mut v_0 = &mut self.#map_name;
        }];

        for (i, (field_name, _)) in typed_fields.iter().enumerate() {
            let v_prev = format_ident!("v_{}", i);
            let v_curr = format_ident!("v_{}", i + 1);
            steps.push(quote! {
                let mut #v_curr = #v_prev.remove(&#field_name)?;
            });
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

fn generate_variant_for_each_body(typed_fields: &[TypedField]) -> proc_macro2::TokenStream {
    let mut body = quote!(func);

    for _ in typed_fields.iter().skip(1) {
        body = quote!(&mut |m| m.for_each(#body));
    }

    body
}

fn generate_variant_field_merger(typed_fields: &[TypedField]) -> proc_macro2::TokenStream {
    let mut merger = quote!(func);

    for _ in typed_fields.iter().skip(1) {
        merger = quote!(&mut |m1, m2| m1.merge_with(m2, #merger));
    }

    merger
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

fn get_type_name(ty: &Type) -> Result<(Ident, bool)> {
    match ty {
        Type::Path(path) => {
            let last_segment = path.path.segments.last().unwrap();
            if last_segment.ident == format_ident!("Box") {
                // special case: spoof Box<T> to be just T
                if let PathArguments::AngleBracketed(ref args) = last_segment.arguments {
                    let args: Vec<_> = args.args.iter().collect();
                    if let [GenericArgument::Type(ty)] = &args[..] {
                        let (name, _) = get_type_name(ty)?;
                        // NOTE: true = type is boxed
                        Ok((name, true))
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
                // NOTE: false = type is unboxed
                Ok((last_segment.ident.clone(), false))
            }
        }
        _ => Err(Error::new(
            ty.span(),
            format!("Not supported here: `{}`", ty.into_token_stream()),
        )),
    }
}
