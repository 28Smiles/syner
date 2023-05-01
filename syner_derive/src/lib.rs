extern crate proc_macro;
use proc_macro::TokenStream;
use proc_macro2::Span;

use quote::quote;
use syn::{parse_macro_input, DeriveInput};

fn extract_generic_arg_from_path(generic: &syn::PathArguments) -> &syn::Type {
    if let syn::PathArguments::AngleBracketed(syn::AngleBracketedGenericArguments {
        args, ..
    }) = generic
    {
        if let Some(syn::GenericArgument::Type(ty)) = args.first() {
            return ty;
        }
    }
    panic!("Expected a generic argument");
}

fn parse_default_or_default_function(attrs: &[syn::Attribute]) -> (bool, Option<syn::Expr>) {
    for attr in attrs {
        if let Ok(syn::Meta::List(syn::MetaList { path, nested, .. })) = attr.parse_meta() {
            if path.is_ident("syner") {
                for nested in nested {
                    match nested {
                        syn::NestedMeta::Meta(syn::Meta::NameValue(syn::MetaNameValue {
                            path,
                            lit: syn::Lit::Str(lit),
                            ..
                        })) => {
                            if path.is_ident("default") {
                                return (true, Some(syn::parse_str(&lit.value()).unwrap()));
                            }
                        }
                        syn::NestedMeta::Meta(syn::Meta::Path(path)) => {
                            if path.is_ident("default") {
                                return (true, None);
                            }
                        }
                        _ => {}
                    }
                }
            }
        }
    }

    (false, None)
}

fn parse_name_from_args(args: &[syn::Attribute]) -> Option<String> {
    for arg in args {
        if let Ok(syn::Meta::List(syn::MetaList { path, nested, .. })) = arg.parse_meta() {
            if path.is_ident("syner") {
                for nested in nested {
                    match nested {
                        syn::NestedMeta::Meta(syn::Meta::NameValue(syn::MetaNameValue {
                            path,
                            lit: syn::Lit::Str(lit),
                            ..
                        })) => {
                            if path.is_ident("name") {
                                return Some(lit.value());
                            }
                        }
                        _ => {}
                    }
                }
            }
        }
    }

    None
}

struct SynerArg<'a> {
    name: String,
    ident: &'a syn::Ident,
    default: (bool, Option<syn::Expr>),
    ty: &'a syn::Type,
    inner_ty: &'a syn::Type,
    optional: bool,
    repeatable: bool,
}

impl<'a> SynerArg<'a> {
    fn from_field(field: &'a syn::Field) -> Result<Self, syn::Error> {
        // either use the field name or the name specified in the attribute
        let name = if let Some(name) = field.ident.as_ref().map(|ident| ident.to_string()) {
            name
        } else {
            return Err(syn::Error::new_spanned(field, "Expected a named field"));
        };
        let ident = if let Some(ident) = field.ident.as_ref() {
            ident
        } else {
            return Err(syn::Error::new_spanned(field, "Expected a named field"));
        };
        let ty = &field.ty;
        let default = parse_default_or_default_function(&field.attrs);
        let mut inner_ty = ty;
        let optional = if let syn::Type::Path(syn::TypePath { path, .. }) = inner_ty {
            if let Some(syn::PathSegment { ident, arguments }) = path.segments.last() {
                if ident == "Option" {
                    inner_ty = extract_generic_arg_from_path(arguments);
                    true
                } else {
                    false
                }
            } else {
                false
            }
        } else {
            false
        };
        let repeatable = if let syn::Type::Path(syn::TypePath { path, .. }) = inner_ty {
            if let Some(syn::PathSegment { ident, arguments }) = path.segments.last() {
                if ident == "Vec" {
                    inner_ty = extract_generic_arg_from_path(arguments);
                    true
                } else {
                    false
                }
            } else {
                false
            }
        } else {
            false
        };

        Ok(Self {
            name,
            ident,
            default,
            optional,
            repeatable,
            ty,
            inner_ty,
        })
    }

    /// Returns the tokens for the field of the parser struct.
    fn parser_field(&self) -> proc_macro2::TokenStream {
        let ident = self.ident;
        let ty = self.ty;
        let optional = self.optional;

        if optional {
            // Type is already an Option
            quote! {
                #ident: #ty
            }
        } else {
            quote! {
                #ident: Option<#ty>
            }
        }
    }

    /// Returns the tokens for the initial value of the parser struct.
    fn parser_init(&self) -> proc_macro2::TokenStream {
        let ident = self.ident;

        // We set all fields to None, also the ones with a default value.
        // This is because we want to check if the user has specified the field multiple times.
        // Since this is not allowed, we can just check if the field is None.
        quote! {
            #ident: None
        }
    }

    /// Returns the expected value of the parser struct.
    fn parser_expected(&self) -> Result<proc_macro2::TokenStream, syn::Error> {
        let name = &self.name;
        let inner_ty = self.inner_ty;
        let optional = self.optional;
        let repeatable = self.repeatable;
        let type_ident = if let syn::Type::Path(syn::TypePath { path, .. }) = inner_ty {
            if let Some(syn::PathSegment { ident, .. }) = path.segments.last() {
                Some(ident.to_string())
            } else {
                None
            }
        } else {
            None
        };

        let primitive_value = if let Some(type_ident) = type_ident {
            match type_ident.as_str() {
                "bool" => Some(format!("true|false OR {}", name)),
                "i8" | "i16" | "i32" | "i64" | "i128" | "isize" | "u8" | "u16" | "u32" | "u64"
                | "u128" | "usize" | "f32" | "f64" => Some(format!("<{}>", type_ident.as_str())),
                "char" => Some(format!("'<char>'")),
                "String" => Some(format!("\"...\"")),
                "str" => {
                    // This is not supported, since we cant borrow the string
                    return Err(syn::Error::new_spanned(
                        inner_ty,
                        "Expected an allocated String not a &str",
                    ));
                }
                _ => None,
            }
        } else {
            None
        };

        let value = if let Some(primitive_value) = primitive_value {
            // The expected inner value.
            if optional {
                if repeatable {
                    let msg = format!(
                        "{}({} /* <-- Expected 0..n times */) /* <-- Expected 0..1 times */",
                        name, primitive_value
                    );
                    quote! { #msg }
                } else {
                    let msg = format!(
                        "{} = {} /* <-- Expected 0..1 times */",
                        name, primitive_value
                    );
                    quote! { #msg }
                }
            } else {
                if repeatable {
                    let msg = format!(
                        "{}({} /* <-- Expected 0..n times */)",
                        name, primitive_value
                    );
                    quote! { #msg }
                } else {
                    let msg = format!("{} = {}", name, primitive_value);
                    quote! { #msg }
                }
            }
        } else {
            // The expected inner value.
            if optional {
                if repeatable {
                    let msg = format!("{}({{}}({{}}) /* <-- Expected 0..n times */) /* <-- Expected 0..1 times */", name);
                    quote! { &std::format!(#msg, <#inner_ty as syner::Syner>::name(), <#inner_ty as syner::Syner>::expected_meta()) }
                } else {
                    let msg = format!("{}({{}}) /* <-- Expected 0..1 times */", name);
                    quote! { &std::format!(#msg, <#inner_ty as syner::Syner>::expected_meta()) }
                }
            } else {
                if repeatable {
                    let msg = format!("{}({{}}({{}}) /* <-- Expected 0..n times */)", name);
                    quote! { &std::format!(#msg, <#inner_ty as syner::Syner>::name(), <#inner_ty as syner::Syner>::expected_meta()) }
                } else {
                    let msg = format!("{}({{}})", name);
                    quote! { &std::format!(#msg, <#inner_ty as syner::Syner>::expected_meta()) }
                }
            }
        };

        Ok(value)
    }

    /// Returns the tokens for the check if all required fields are set.
    fn parser_check(&self) -> Result<proc_macro2::TokenStream, syn::Error> {
        let ident = self.ident;
        let optional = self.optional;
        let (use_default, _) = self.default;

        Ok(if optional || use_default {
            quote! {}
        } else {
            let expected = self.parser_expected()?;
            quote! {
                if self.#ident.is_none() {
                    errors.push(syner::__private::syn::Error::new_spanned(
                        parent,
                        std::format!("Missing required argument {}", #expected)
                    ));
                    encountered_error = true;
                }
            }
        })
    }

    /// Returns the tokens for converting the parser struct into the final struct.
    fn parser_convert(&self) -> Result<proc_macro2::TokenStream, syn::Error> {
        let ident = &self.ident;
        let optional = self.optional;
        let (use_default, ref default) = self.default;

        Ok(if use_default {
            if let Some(default) = default {
                quote! {
                    #ident: self.#ident.unwrap_or_else(|| #default)
                }
            } else {
                quote! {
                    #ident: self.#ident.unwrap_or_default()
                }
            }
        } else if optional {
            quote! {
                #ident: self.#ident
            }
        } else {
            // We checked the optional case above, so this is safe.
            quote! {
                #ident: self.#ident.unwrap()
            }
        })
    }

    /// Returns the tokens for the parsing of the field.
    fn parser_match_arm(&self) -> Result<proc_macro2::TokenStream, syn::Error> {
        let name = &self.name;
        let ident = &self.ident;
        let inner_ty = self.inner_ty;
        let repeatable = self.repeatable;
        let type_ident = if let syn::Type::Path(syn::TypePath { path, .. }) = inner_ty {
            if let Some(syn::PathSegment { ident, .. }) = path.segments.last() {
                Some(ident.to_string())
            } else {
                None
            }
        } else {
            None
        };

        let primitive_value_parser = if let Some(type_ident) = type_ident {
            match type_ident.as_str() {
                "bool" => Some(quote! {
                    if let syner::__private::syn::Lit::Bool(syner::__private::syn::LitBool { value, .. }) = lit {
                        *value
                    } else {
                        errors.push(syner::__private::syn::Error::new_spanned(
                            meta,
                            "Expected a boolean value"
                        ));
                        return;
                    }
                }),
                "i8" | "i16" | "i32" | "i64" | "i128" | "isize" | "u8" | "u16" | "u32" | "u64"
                | "u128" | "usize" => {
                    let error_msg = format!("Expected a number of type {}", type_ident.as_str());
                    Some(quote! {
                        if let syner::__private::syn::Lit::Int(lit) = lit {
                            value.base10_parse()
                        } else {
                            errors.push(syner::__private::syn::Error::new_spanned(
                                meta,
                                #error_msg
                            ));
                            return;
                        }
                    })
                }
                "f32" | "f64" => {
                    let error_msg = format!("Expected a number of type {}", type_ident.as_str());
                    Some(quote! {
                        if let syner::__private::syn::Lit::Float(lit) = lit {
                            value.base10_parse()
                        } else {
                            errors.push(syner::__private::syn::Error::new_spanned(
                                meta,
                                #error_msg
                            ));
                            return;
                        }
                    })
                }
                "char" => Some(quote! {
                    if let syner::__private::syn::Lit::Char(lit) = lit {
                        lit.value()
                    } else {
                        errors.push(syner::__private::syn::Error::new_spanned(
                            meta,
                            "Expected a char value"
                        ));
                        return;
                    }
                }),
                "String" => Some(quote! {
                    if let syner::__private::syn::Lit::Str(lit) = lit {
                        lit.value()
                    } else {
                        errors.push(syner::__private::syn::Error::new_spanned(
                            meta,
                            "Expected a string value"
                        ));
                        return;
                    }
                }),
                "str" => {
                    // This is not supported, since we cant borrow the string
                    return Err(syn::Error::new_spanned(
                        inner_ty,
                        "Expected an allocated String not a &str",
                    ));
                }
                _ => None,
            }
        } else {
            None
        };

        let value_parser = if let Some(primitive_value_parser) = &primitive_value_parser {
            let expected = self.parser_expected()?;
            quote! {
                if let syner::__private::syn::Meta::NameValue(syner::__private::syn::MetaNameValue { lit, .. }) = meta {
                    #primitive_value_parser
                } else {
                    errors.push(syner::__private::syn::Error::new_spanned(
                        meta,
                        &std::format!("Expected Named Value {}", #expected),
                    ));
                    return;
                }
            }
        } else {
            quote! {
                match <#inner_ty as syner::Syner>::parse_meta(meta) {
                    Ok(value) => value,
                    Err(err) => {
                        errors.extend(err);
                        return;
                    }
                }
            }
        };

        // Generate Repeat Guard
        let error_message_duplicate = format!("Duplicate field with name: {}", name);
        let repeat_guard = quote! {
            if self.#ident.is_some() {
                errors.push(syner::__private::syn::Error::new_spanned(meta, #error_message_duplicate));
                return;
            }
        };

        // Generate Set Value
        let value_parser = if repeatable {
            if let Some(primitive_value_parser) = primitive_value_parser {
                quote! {
                    {
                        let mut value = Vec::new();
                        if let syner::__private::syn::Meta::List(syner::__private::syn::MetaList { nested, .. }) = meta {
                            for meta in nested.iter() {
                                if let syner::__private::syn::NestedMeta::Lit(lit) = meta {
                                    value.push(#primitive_value_parser);
                                } else {
                                    errors.push(syner::__private::syn::Error::new_spanned(
                                        meta,
                                        "Expected a literal value"
                                    ));
                                    return;
                                }
                            }
                        } else {
                            errors.push(syner::__private::syn::Error::new_spanned(
                                meta,
                                "Expected a list of literals"
                            ));
                            return;
                        }
                        value
                    }
                }
            } else {
                quote! {
                    {
                        let mut value = Vec::new();
                        if let syner::__private::syn::Meta::List(syner::__private::syn::MetaList { nested, .. }) = meta {
                            for meta in nested.iter() {
                                if let syner::__private::syn::NestedMeta::Meta(meta) = meta {
                                    value.push(#value_parser);
                                } else {
                                    errors.push(syner::__private::syn::Error::new_spanned(
                                        meta,
                                        "Expected a meta value"
                                    ));
                                    return;
                                }
                            }
                        } else {
                            errors.push(syner::__private::syn::Error::new_spanned(
                                meta,
                                "Expected a list value"
                            ));
                            return;
                        }
                        value
                    }
                }
            }
        } else {
            value_parser
        };

        Ok(quote! {
            #name =>
            {
                #repeat_guard
                self.#ident = Some(#value_parser)
            }
        })
    }
}

fn collect_errors<'a, I, O>(
    errors: &'a mut Vec<syn::Error>,
    mut f: impl FnMut(I) -> Result<O, syn::Error> + 'a,
) -> impl FnMut(I) -> Option<O> + 'a {
    move |i| match f(i) {
        Ok(o) => Some(o),
        Err(err) => {
            errors.push(err);
            None
        }
    }
}

#[proc_macro_derive(Syner, attributes(syner))]
pub fn derive_attribute_parser(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    if let syn::Data::Struct(data) = input.data {
        let name = &input.ident;
        let attribute_name_str = parse_name_from_args(&input.attrs)
            .unwrap_or_else(|| name.to_string().to_ascii_lowercase());
        let attribute_name = &attribute_name_str;
        let parser = syn::Ident::new(&format!("__{}Syner", name), Span::call_site());
        let mut errors = Vec::new();
        let args = data
            .fields
            .iter()
            .filter_map(|field| match SynerArg::from_field(field) {
                Ok(arg) => Some(arg),
                Err(err) => {
                    errors.push(err);
                    None
                }
            })
            .collect::<Vec<_>>();

        if !errors.is_empty() {
            return errors
                .into_iter()
                .map(|err| err.to_compile_error())
                .collect::<proc_macro2::TokenStream>()
                .into();
        }

        let parser_fields = args.iter().map(|arg| arg.parser_field());
        let parser_init = args.iter().map(|arg| arg.parser_init());
        let parser_match_arms = args
            .iter()
            .filter_map(collect_errors(&mut errors, |arg: &SynerArg| {
                arg.parser_match_arm()
            }))
            .collect::<Vec<_>>();
        let parser_check = args
            .iter()
            .filter_map(collect_errors(&mut errors, |arg: &SynerArg| {
                arg.parser_check()
            }))
            .collect::<Vec<_>>();
        let parser_convert = args
            .iter()
            .filter_map(collect_errors(&mut errors, |arg: &SynerArg| {
                arg.parser_convert()
            }))
            .collect::<Vec<_>>();
        let parser_expected = args
            .iter()
            .filter_map(collect_errors(&mut errors, |arg: &SynerArg| {
                arg.parser_expected()
            }))
            .collect::<Vec<_>>();

        if !errors.is_empty() {
            return errors
                .into_iter()
                .map(|err| err.to_compile_error())
                .collect::<proc_macro2::TokenStream>()
                .into();
        }

        let tokens = quote! {
            struct #parser { #(#parser_fields,)* }
            impl #parser {
                fn new() -> Self { Self { #(#parser_init,)* } }

                fn consume(&mut self, errors: &mut Vec<syner::__private::syn::Error>, path: &str, meta: &syner::__private::syn::Meta) {
                    if let Some(ident) = meta.path().get_ident().map(|ident| ident.to_string()) {
                        match ident.as_str() {
                            #(#parser_match_arms)*
                            _ => {
                                errors.push(syner::__private::syn::Error::new_spanned(
                                    meta,
                                    "Unknown attribute"
                                ));
                            }
                        }
                    } else {
                        errors.push(syner::__private::syn::Error::new_spanned(meta, "Unsupported attribute type"));
                    }
                }

                fn complete(self, errors: &mut Vec<syner::__private::syn::Error>, parent: &syner::__private::syn::Meta) -> ::std::result::Result<#name, ()> {
                    let mut encountered_error = false;
                    #(#parser_check)*

                    if encountered_error {
                        Err(())
                    } else {
                        Ok(#name {
                            #(#parser_convert,)*
                        })
                    }
                }
            }

            impl Syner for #name {
                fn parse_attrs<'a>(
                    attrs: impl IntoIterator<Item = &'a syner::__private::syn::Attribute> + 'a,
                ) -> ::std::result::Result<Self, Vec<syner::__private::syn::Error>> {
                    let mut parser = #parser::new();
                    let mut errors = Vec::new();
                    let attrs = attrs.into_iter()
                        .filter(|attr| attr.path.is_ident(#attribute_name))
                        .filter_map(|attr| {
                            let meta = attr.parse_meta().ok()?;
                            if let syner::__private::syn::Meta::List(syner::__private::syn::MetaList { nested, .. }) = meta {
                                Some(nested)
                            } else {
                                None
                            }
                        })
                        .flatten()
                        .collect::<Vec<_>>();
                    for attr in attrs.iter() {
                        match &attr {
                            syner::__private::syn::NestedMeta::Meta(meta) => {
                                parser.consume(&mut errors, #attribute_name, meta)
                            },
                            syner::__private::syn::NestedMeta::Lit(lit) => {
                                errors.push(syner::__private::syn::Error::new_spanned(
                                    lit,
                                    "Unexpected literal"
                                ));
                            },
                        }
                    }

                    if attrs.is_empty() || !errors.is_empty() {
                        Err(errors)
                    } else {
                        let first = attrs.first().unwrap();
                        match &first {
                            syner::__private::syn::NestedMeta::Meta(meta) => {
                                let result = parser.complete(&mut errors, meta);

                                result.map_err(|_| errors)
                            },
                            syner::__private::syn::NestedMeta::Lit(lit) => {
                                Err(errors)
                            },
                        }
                    }
                }

                fn parse_meta(
                    input: &syner::__private::syn::Meta
                ) -> ::std::result::Result<Self, Vec<syner::__private::syn::Error>> {
                    match input {
                        syner::__private::syn::Meta::List(list) => {
                            let mut parser = #parser::new();
                            let mut errors = Vec::new();
                            for meta in &list.nested {
                                match meta {
                                    syner::__private::syn::NestedMeta::Meta(meta) => {
                                        parser.consume(&mut errors, #attribute_name, meta);
                                    }
                                    syner::__private::syn::NestedMeta::Lit(lit) => {
                                        errors.push(syner::__private::syn::Error::new_spanned(lit, "Expected meta"));
                                    }
                                }
                            }

                            let result = parser.complete(&mut errors, input);

                            result.map_err(|_| errors)
                        }
                        _ => {
                            Err(vec![syner::__private::syn::Error::new_spanned(input, "Expected list")])
                        }
                    }
                }

                fn expected() -> String {
                    std::format!("{}{}", #attribute_name, <Self as syner::Syner>::expected_meta())
                }

                fn expected_meta() -> String {
                    let mut expected = String::new();
                    expected.push_str("(");
                    #(
                        expected.push_str(#parser_expected);
                        expected.push_str(", ");
                    )*
                    expected.push_str(")");
                    expected
                }

                fn name() -> &'static str {
                    #attribute_name
                }
            }
        };

        tokens.into()
    } else {
        syn::Error::new(
            Span::call_site(),
            "The `Syner` attribute can only be used on structs.",
        )
        .into_compile_error()
        .into()
    }
}
