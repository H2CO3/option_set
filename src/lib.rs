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
#![allow(clippy::match_same_arms,
         clippy::clone_on_ref_ptr,
         clippy::needless_pass_by_value)]
#![deny(clippy::wrong_pub_self_convention, clippy::used_underscore_binding,
        clippy::module_name_repetitions, clippy::similar_names,
        clippy::pub_enum_variant_names,
        clippy::missing_docs_in_private_items,
        clippy::non_ascii_literal, clippy::unicode_not_nfc,
        clippy::result_unwrap_used, clippy::option_unwrap_used,
        clippy::option_map_unwrap_or_else, clippy::option_map_unwrap_or,
        clippy::filter_map,
        clippy::shadow_unrelated, clippy::shadow_reuse, clippy::shadow_same,
        clippy::int_plus_one, clippy::string_add_assign, clippy::if_not_else,
        clippy::invalid_upcast_comparisons,
        clippy::cast_precision_loss,
        clippy::cast_possible_wrap, clippy::cast_possible_truncation,
        clippy::mutex_integer, clippy::mut_mut, clippy::items_after_statements,
        clippy::print_stdout, clippy::mem_forget, clippy::maybe_infinite_iter)]

extern crate serde;
extern crate heck;

use std::ops::{ BitAnd, BitOrAssign, Deref };
use std::fmt::{ self, Formatter };
use std::borrow::Cow;
use std::marker::PhantomData;
use serde::{ Serializer, Deserializer };
use serde::ser::SerializeSeq;
use serde::de::{ Visitor, SeqAccess, Deserialize };
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
    where A: SeqAccess<'de>, T: OptionSet, S: Deref<Target=str> {

    use serde::de::Error;

    let mut flags = T::default();

    while let Some(elem) = seq.next_element::<Str<'de>>()? {
        let mut iter = T::VARIANTS.iter().zip(names);

        match iter.find(|&(_, name)| **name == *elem) {
            Some((&flag, _)) => flags |= flag,
            None => Err(A::Error::unknown_variant(&elem, T::NAMES))?,
        }
    }

    Ok(flags)
}

/// Equivalent of `Cow<'a, str>` except that this
/// type can deserialize from a borrowed string.
#[derive(Debug)]
enum Str<'a> {
    /// Owned.
    String(String),
    /// Borrowed.
    Str(&'a str),
}

impl<'a> Deref for Str<'a> {
    type Target = str;

    fn deref(&self) -> &Self::Target {
        match *self {
            Str::Str(s) => s,
            Str::String(ref s) => s,
        }
    }
}

/// Visitor for deserializing a `Str<'a>`.
struct StrVisitor;

impl<'a> Visitor<'a> for StrVisitor {
    type Value = Str<'a>;

    fn expecting(&self, formatter: &mut Formatter) -> fmt::Result {
        formatter.write_str("a string")
    }

    fn visit_str<E>(self, v: &str) -> Result<Self::Value, E>
    where
        E: serde::de::Error,
    {
        self.visit_string(v.to_owned())
    }

    fn visit_string<E>(self, v: String) -> Result<Self::Value, E>
    where
        E: serde::de::Error,
    {
        Ok(Str::String(v))
    }

    fn visit_borrowed_str<E>(self, v: &'a str) -> Result<Self::Value, E>
    where
        E: serde::de::Error,
    {
        Ok(Str::Str(v))
    }
}

impl<'de> Deserialize<'de> for Str<'de> {
    #[inline]
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        deserializer.deserialize_str(StrVisitor)
    }
}
