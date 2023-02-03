pub use syner_derive::Syner;

pub mod __private {
    pub use proc_macro2;
    pub use syn;
}

pub trait Syner {
    /// Parse this struct from an iterator of attributes.
    fn parse_attrs<'a>(
        attrs: impl IntoIterator<Item = &'a syn::Attribute> + 'a,
    ) -> Result<Self, Vec<syn::Error>>
    where
        Self: Sized;

    /// Parse this struct from a [syn::Meta].
    fn parse_meta(input: &syn::Meta) -> Result<Self, Vec<syn::Error>>
    where
        Self: Sized;

    /// The expected attributes of this struct.
    fn expected() -> String;

    /// The expected inner attributes of this struct.
    fn expected_meta() -> String;

    /// The name of the attribute that this struct is expecting.
    fn name() -> &'static str;
}

#[cfg(test)]
mod tests {
    use syn::{parse_quote, DeriveInput};

    use super::*;

    // Definitions for the tests
    mod syner {
        pub use crate::Syner;
        pub use crate::__private;
    }

    #[derive(Syner)]
    #[syner(name = "test_attribute")]
    struct Test {
        pub some: String,
        pub maybe: Option<String>,
        #[syner(default)]
        pub is_default: bool,
        #[syner(default = "String::from(\"default\")")]
        pub default: String,
        pub inner: Inner,
        pub inner_list: Vec<Inner>,
        pub inner_bools: Vec<bool>,
        pub inner_maybe: Option<Inner>,
    }

    #[derive(Syner, PartialEq, Debug)]
    struct Inner {
        pub some: String,
        pub is_default: bool,
    }

    #[test]
    fn test_correct() {
        let input: DeriveInput = parse_quote! {
            #[test_attribute(
                some = "hello",
                inner(
                    some = "inner",
                    is_default = true
                ),
                inner_list(
                    inner(
                        some = "inner_list0",
                        is_default = true
                    ),
                    inner(
                        some = "inner_list1",
                        is_default = false
                    ),
                    inner(
                        some = "inner_list2",
                        is_default = true
                    )
                ),
                inner_bools(true, false, true)
            )]
            struct Test;
        };
        let test = Test::parse_attrs(&input.attrs).unwrap();
        assert_eq!(test.some, "hello");
        assert_eq!(test.maybe, None);
        assert_eq!(test.is_default, false);
        assert_eq!(test.default, "default");
        assert_eq!(test.inner.some, "inner");
        assert_eq!(test.inner.is_default, true);
        assert_eq!(test.inner_list.len(), 3);
        assert_eq!(test.inner_list[0].some, "inner_list0");
        assert_eq!(test.inner_list[0].is_default, true);
        assert_eq!(test.inner_list[1].some, "inner_list1");
        assert_eq!(test.inner_list[1].is_default, false);
        assert_eq!(test.inner_list[2].some, "inner_list2");
        assert_eq!(test.inner_list[2].is_default, true);
        assert_eq!(test.inner_bools.len(), 3);
        assert_eq!(test.inner_bools[0], true);
        assert_eq!(test.inner_bools[1], false);
        assert_eq!(test.inner_bools[2], true);
        assert_eq!(test.inner_maybe, None);
    }

    #[test]
    fn test_duplicate() {
        let input: DeriveInput = parse_quote! {
            #[test_attribute(
                some = "hello",
                some = "hello",
                inner(
                    some = "inner",
                    is_default = true
                ),
                inner(
                    some = "inner",
                    is_default = true
                )
            )]
            struct Test;
        };
        let test = Test::parse_attrs(&input.attrs);
        assert!(test.is_err());
        assert_eq!(test.err().unwrap().len(), 2);
    }

    #[test]
    fn test_missing() {
        let input: DeriveInput = parse_quote! {
            #[test_attribute(
                inner(
                    some = "inner",
                    is_default = true
                ),
                inner_list(
                    inner(
                        some = "inner_list0",
                        is_default = true
                    ),
                    inner(
                        some = "inner_list1",
                        is_default = false
                    ),
                    inner(
                        some = "inner_list2",
                        is_default = true
                    )
                ),
                inner_bools(true, false, true)
            )]
            struct Test;
        };
        let test = Test::parse_attrs(&input.attrs);
        assert!(test.is_err());
        assert_eq!(test.err().unwrap().len(), 1);
    }

    #[test]
    fn test_wrong_type() {
        let input: DeriveInput = parse_quote! {
            #[test_attribute(
                some = "hello",
                inner(
                    some = "inner",
                    is_default = true
                ),
                inner_list(
                    inner(
                        some = "inner_list0",
                        is_default = true
                    ),
                    inner(
                        some = "inner_list1",
                        is_default = false
                    ),
                    inner(
                        some = "inner_list2",
                        is_default = true
                    )
                ),
                inner_bools(true, false, true),
                inner_maybe = "hello"
            )]
            struct Test;
        };
        let test = Test::parse_attrs(&input.attrs);
        assert!(test.is_err());
        assert_eq!(test.err().unwrap().len(), 1);
    }

    #[test]
    fn test_wrong_type_meta() {
        let input: DeriveInput = parse_quote! {
            #[test_attribute(
                some = "hello",
                inner(
                    some = "inner",
                    is_default = true
                ),
                inner_list(
                    inner(
                        some = "inner_list0",
                        is_default = true
                    ),
                    inner(
                        some = "inner_list1",
                        is_default = false
                    ),
                    inner(
                        some = "inner_list2",
                        is_default = true
                    )
                ),
                inner_bools(meta(true), false, true),
            )]
            struct Test;
        };
        let test = Test::parse_attrs(&input.attrs);
        assert!(test.is_err());
        assert_eq!(test.err().unwrap().len(), 1);
    }
}
