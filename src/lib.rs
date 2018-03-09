//! A macro and a trait for implementing a bitset/flag type that
//! serializes to and deserializes from a sequence of strings.

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

/// A Type that knows how to transform the case of individual option flag names.
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

impl Default for CaseTransform {
    fn default() -> Self {
        CaseTransform::Identity
    }
}

impl CaseTransform {
    /// Converts the name of an option flag to the case intrinsic to the implementing type.
    fn transform(self, name: &str) -> Cow<str> {
        use CaseTransform::*;

        match self {
            Identity => Cow::Borrowed(name),
            LowerSnake => Cow::Owned(name.to_snake_case()),
            UpperSnake => Cow::Owned(name.to_shouty_snake_case()),
            LowerCamel => Cow::Owned(name.to_mixed_case()),
            UpperCamel => Cow::Owned(name.to_camel_case()),
            Kebab => Cow::Owned(name.to_kebab_case()),
            Title => Cow::Owned(name.to_title_case()),
        }
    }
}

/// Serialize an OptionSet's set bits as a sequence of strings.
pub fn serialize<T, S>(options: &T, serializer: S, transform: CaseTransform) -> Result<S::Ok, S::Error>
    where T: OptionSet, S: Serializer {

    assert!(T::VARIANTS.len() == T::NAMES.len());

    let mut seq = serializer.serialize_seq(T::NAMES.len().into())?;

    for (&variant, &name) in T::VARIANTS.iter().zip(T::NAMES) {
        if *options & variant == variant {
            seq.serialize_element(transform.transform(name).as_ref())?;
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

    fn visit_seq<A: SeqAccess<'de>>(self, mut seq: A) -> Result<Self::Value, A::Error> {
        use serde::de::Error;

        assert!(T::VARIANTS.len() == T::NAMES.len());

        let names: Vec<_> = T::NAMES.iter().map(|name| self.0.transform(name)).collect();
        let mut flags = T::default();

        while let Some(elem) = seq.next_element()? {
            let mut iter = T::VARIANTS.iter().zip(&names);

            match iter.find(|&(_, &ref name)| name == elem) {
                Some((&flag, _)) => flags |= flag,
                None => Err(A::Error::unknown_variant(elem, T::NAMES))?,
            }
        }

        Ok(flags)
    }
}
