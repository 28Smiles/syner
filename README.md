[![Crates](https://badgen.net/crates/v/syner)](https://crates.io/crates/syner)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![CI](https://github.com/28Smiles/syner/actions/workflows/ci.yml/badge.svg)](https://github.com/28Smiles/syner/actions/workflows/build.yml)
[![Coverage Status](https://coveralls.io/repos/github/28Smiles/syner/badge.svg)](https://coveralls.io/github/28Smiles/syner)
[![Latest Stable](https://img.shields.io/github/v/release/28Smiles/syner?label=latest%20stable)](https://github.com/28Smiles/syner/releases/latest)
[![Latest Release](https://img.shields.io/github/v/release/28Smiles/syner?include_prereleases&label=latest%20release)](https://github.com/28Smiles/syner/releases)

# Syner - A simple, fast and safe way to parse attributes from syn

Syner is a simple, fast and safe way to parse attributes from syn. It is designed to be used with the [syn](https://crates.io/crates/syn) crate.
Definition of your attributes is done using a procedural macro. This allows you to define your attributes in a type safe way.
You create a struct that represents your attribute and then use the `#[derive(Syner)]` macro to generate the parsing code.

## Example

```rust
    #[derive(Syner)]
    struct Test {
        pub some: String,
        pub maybe: Option<String>,
        #[syner(default)]
        pub is_default: bool,
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
```

This will parse the following attribute:

```rust
#[test(
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
struct TestStruct {}
```

You can parse the attribute using the `parse_attrs` function.
It takes an iterator of `syn::Attribute` and returns a `Result` with the parsed attribute.

```rust
    let attrs = Test::parse_attrs(&item.attrs)?;
```

## Supported types

Syner supports the following types:
 - `String` - Parses the value as a string
 - `bool` - Parses the value as a boolean
 - `i8`, `i16`, `i32`, `i64`, `i128`, `isize` - Parses the value as a signed integer
 - `u8`, `u16`, `u32`, `u64`, `u128`, `usize` - Parses the value as an unsigned integer
 - `f32`, `f64` - Parses the value as a float
 - `T` - Parses the value as `<name>(T)` if `T` is a struct that implements `Syner`
 - `Option<T>` - Parses the value as `T` if it is present
 - `Vec<T>` - Parses the value as `<name>(T...)`

Annotating a field with `#[syner(default)]` will make it optional and use the default value if it is not present.
You can also use `#[syner(default = "<expr>")]` to specify a default value.
The name of the field is used as the name of the attribute except if the field is of type `Vec<T>`, 
in which case the name of the struct (`lowercase`) is used. 
For the top level attribute the lowercase name of the struct is used as a default name.
If you want to use a different name you can annotate the struct with `#[syner(name = "<name>")]`.

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details