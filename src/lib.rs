//! A macro and a trait for implementing a bitset/flag type that
//! serializes to and deserializes from a sequence of strings.
//!
//! # Usage
//!
//! ```
//! #[macro_use]
//! extern crate option_set;
//! #[macro_use]
//! extern crate bitflags;
//! extern crate serde;
//! extern crate serde_json;
//!
//! option_set! {
//!     struct FooOptions: UpperCamel + u16 {
//!         const BAR_FIRST        = 0x0001;
//!         const QUX_SECOND_THING = 0x0080;
//!         const LOL_3RD          = 0x4000;
//!     }
//! }
//!
//! fn main() {
//!     let opts_in = FooOptions::BAR_FIRST | FooOptions::LOL_3RD;
//!     let json = serde_json::to_string_pretty(&opts_in).unwrap();
//!
//!     println!("{}", json);
//!
//!     let opts_out: FooOptions = serde_json::from_str(&json).unwrap();
//!
//!     println!("{:?}", opts_out);
//!     assert!(opts_out == opts_in);
//! }
//! ```

#![deny(missing_debug_implementations, missing_copy_implementations,
        trivial_casts, trivial_numeric_casts,
        unsafe_code,
        unstable_features,
        unused_import_braces, unused_qualifications, missing_docs)]
#![cfg_attr(feature = "cargo-clippy",
            allow(match_same_arms, clone_on_ref_ptr, needless_pass_by_value))]
#![cfg_attr(feature = "cargo-clippy",
            deny(wrong_pub_self_convention, used_underscore_binding,
                 stutter, similar_names, pub_enum_variant_names,
                 missing_docs_in_private_items,
                 non_ascii_literal, unicode_not_nfc,
                 result_unwrap_used, option_unwrap_used,
                 option_map_unwrap_or_else, option_map_unwrap_or, filter_map,
                 shadow_unrelated, shadow_reuse, shadow_same,
                 int_plus_one, string_add_assign, if_not_else,
                 invalid_upcast_comparisons,
                 cast_precision_loss,
                 cast_possible_wrap, cast_possible_truncation,
                 mutex_integer, mut_mut, items_after_statements,
                 print_stdout, mem_forget, maybe_infinite_iter))]

extern crate serde;
extern crate heck;

use std::ops::{ BitAnd, BitOrAssign };
use std::fmt::{ self, Formatter };
use std::borrow::Cow;
use std::marker::PhantomData;
use serde::{ Serializer, Deserializer };
use serde::ser::SerializeSeq;
use serde::de::{ Visitor, SeqAccess };
use heck::{ SnakeCase, ShoutySnakeCase, MixedCase, CamelCase, KebabCase, TitleCase };

/// Defines an option set type.
#[macro_export]
macro_rules! option_set {
    ($(#[$outer:meta])* pub struct $name:ident: $case:ident + $repr:ty {
        $($(#[$inner:ident $($args:tt)*])* const $variant:ident = $value:expr;)*
    }) => {
        bitflags! {
            $(#[$outer])*
            #[derive(Default)]
            pub struct $name: $repr {
                $(
                    $(#[$inner $($args)*])*
                    const $variant = $value;
                )*
            }
        }

        impl $crate::OptionSet for $name {
            const VARIANTS: &'static [$name] = &[$($name::$variant,)*];
            const NAMES: &'static [&'static str] = &[$(stringify!($variant),)*];
        }

        impl ::serde::ser::Serialize for $name {
            fn serialize<S: ::serde::ser::Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
                $crate::serialize(self, serializer, $crate::CaseTransform::$case)
            }
        }

        impl<'de> ::serde::de::Deserialize<'de> for $name {
            fn deserialize<D: ::serde::de::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
                $crate::deserialize(deserializer, $crate::CaseTransform::$case)
            }
        }
    };
    ($(#[$outer:meta])* struct $name:ident: $case:ident + $repr:ty {
        $($(#[$inner:ident $($args:tt)*])* const $variant:ident = $value:expr;)*
    }) => {
        bitflags! {
            $(#[$outer])*
            #[derive(Default)]
            struct $name: $repr {
                $(
                    $(#[$inner $($args)*])*
                    const $variant = $value;
                )*
            }
        }

        impl $crate::OptionSet for $name {
            const VARIANTS: &'static [$name] = &[$($name::$variant,)*];
            const NAMES: &'static [&'static str] = &[$(stringify!($variant),)*];
        }

        impl ::serde::ser::Serialize for $name {
            fn serialize<S: ::serde::ser::Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
                $crate::serialize(self, serializer, $crate::CaseTransform::$case)
            }
        }

        impl<'de> ::serde::de::Deserialize<'de> for $name {
            fn deserialize<D: ::serde::de::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
                $crate::deserialize(deserializer, $crate::CaseTransform::$case)
            }
        }
    };
}

/// Trait for bit flags that forwards to std traits for useful bit operators.
pub trait OptionSet: Copy + Default + Eq + BitAnd<Output=Self> + BitOrAssign + 'static {
    /// The basis flags (in the algebraic sense): one for each independent option.
    const VARIANTS: &'static [Self];
    /// The corresponding names. `VARIANTS.len() == NAMES.len()` must always hold.
    const NAMES: &'static [&'static str];
}

/// Type that knows how to transform the case of individual option flag names.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum CaseTransform {
    /// Do not transform the name.
    Identity,
    /// `lower_snake_case`
    LowerSnake,
    /// `UPPER_SNAKE_CASE`
    UpperSnake,
    /// `lowerCamelCase`
    LowerCamel,
    /// `UpperCamelCase`
    UpperCamel,
    /// `kebab-case`
    Kebab,
    /// `Title Case`
    Title,
}

impl CaseTransform {
    /// Converts the name of an option flag to the specified case
    fn apply(self, s: &str) -> Cow<str> {
        use CaseTransform::*;

        match self {
            Identity   => Cow::Borrowed(s),
            LowerSnake => Cow::Owned(s.to_snake_case()),
            UpperSnake => Cow::Owned(s.to_shouty_snake_case()),
            LowerCamel => Cow::Owned(s.to_mixed_case()),
            UpperCamel => Cow::Owned(s.to_camel_case()),
            Kebab      => Cow::Owned(s.to_kebab_case()),
            Title      => Cow::Owned(s.to_title_case()),
        }
    }
}

impl Default for CaseTransform {
    fn default() -> Self {
        CaseTransform::Identity
    }
}

/// Serialize an OptionSet's set bits as a sequence of strings.
pub fn serialize<T, S>(options: &T, serializer: S, transform: CaseTransform) -> Result<S::Ok, S::Error>
    where T: OptionSet, S: Serializer {

    assert!(T::VARIANTS.len() == T::NAMES.len());

    let mut seq = serializer.serialize_seq(T::NAMES.len().into())?;

    for (&variant, &name) in T::VARIANTS.iter().zip(T::NAMES) {
        if *options & variant == variant {
            seq.serialize_element(&transform.apply(name))?;
        }
    }

    seq.end()
}

/// Deserialize set bits from a sequence of name strings.
pub fn deserialize<'de, T, D>(deserializer: D, transform: CaseTransform) -> Result<T, D::Error>
    where T: OptionSet, D: Deserializer<'de> {

    deserializer.deserialize_seq(OptionSetVisitor(transform, PhantomData))
}

/// This visitor tries to associate bitflag values with their name.
#[derive(Debug, Clone, Copy)]
struct OptionSetVisitor<T: OptionSet>(CaseTransform, PhantomData<T>);

impl<'de, T: OptionSet> Visitor<'de> for OptionSetVisitor<T> {
    type Value = T;

    fn expecting(&self, f: &mut Formatter) -> fmt::Result {
        f.write_str("set of option strings")
    }

    fn visit_seq<A: SeqAccess<'de>>(self, seq: A) -> Result<Self::Value, A::Error> {
        assert!(T::VARIANTS.len() == T::NAMES.len());

        match self.0 {
            CaseTransform::Identity => extract_bits(seq, T::NAMES),
            _ => {
                let names: Vec<_> = T::NAMES.iter().map(|name| self.0.apply(name)).collect();
                extract_bits(seq, &names)
            }
        }
    }
}

/// Actually performs the sequence processing and flag extraction.
fn extract_bits<'de, A, T, S>(mut seq: A, names: &[S]) -> Result<T, A::Error>
    where A: SeqAccess<'de>, T: OptionSet, S: AsRef<str> {

    use serde::de::Error;

    let mut flags = T::default();

    while let Some(elem) = seq.next_element::<Cow<'de, str>>()? {
        let mut iter = T::VARIANTS.iter().zip(names);

        match iter.find(|&(_, name)| name.as_ref() == elem) {
            Some((&flag, _)) => flags |= flag,
            None => Err(A::Error::unknown_variant(&elem, T::NAMES))?,
        }
    }

    Ok(flags)
}
