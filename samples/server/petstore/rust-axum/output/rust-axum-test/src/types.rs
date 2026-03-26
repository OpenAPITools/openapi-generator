use std::{mem, str::FromStr};

use base64::{Engine, engine::general_purpose};
use serde::{Deserialize, Deserializer, Serialize, Serializer};

#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize)]
#[allow(dead_code)]
pub struct Object(pub serde_json::Value);

impl validator::Validate for Object {
    fn validate(&self) -> Result<(), validator::ValidationErrors> {
        Ok(())
    }
}

impl FromStr for Object {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(Self(serde_json::Value::String(s.to_owned())))
    }
}

/// Serde helper function to create a default `Option<Nullable<T>>` while
/// deserializing
pub fn default_optional_nullable<T>() -> Option<Nullable<T>> {
    None
}

/// Serde helper function to deserialize into an `Option<Nullable<T>>`
pub fn deserialize_optional_nullable<'de, D, T>(
    deserializer: D,
) -> Result<Option<Nullable<T>>, D::Error>
where
    D: Deserializer<'de>,
    T: Deserialize<'de>,
{
    Option::<T>::deserialize(deserializer).map(|val| match val {
        Some(inner) => Some(Nullable::Present(inner)),
        None => Some(Nullable::Null),
    })
}

/// The Nullable type. Represents a value which may be specified as null on an API.
/// Note that this is distinct from a value that is optional and not present!
///
/// Nullable implements many of the same methods as the Option type (map, unwrap, etc).
#[derive(Debug, Clone, Copy, PartialEq, PartialOrd)]
pub enum Nullable<T> {
    /// Null value
    Null,
    /// Value is present
    Present(T),
}

impl<T> Nullable<T> {
    /////////////////////////////////////////////////////////////////////////
    // Querying the contained values
    /////////////////////////////////////////////////////////////////////////

    /// Returns `true` if the Nullable is a `Present` value.
    ///
    /// # Examples
    ///
    /// ```
    /// # use rust_server_test::types::Nullable;
    ///
    /// let x: Nullable<u32> = Nullable::Present(2);
    /// assert_eq!(x.is_present(), true);
    ///
    /// let x: Nullable<u32> = Nullable::Null;
    /// assert_eq!(x.is_present(), false);
    /// ```
    #[inline]
    pub fn is_present(&self) -> bool {
        match *self {
            Nullable::Present(_) => true,
            Nullable::Null => false,
        }
    }

    /// Returns `true` if the Nullable is a `Null` value.
    ///
    /// # Examples
    ///
    /// ```
    /// # use rust_server_test::types::Nullable;
    ///
    /// let x: Nullable<u32> = Nullable::Present(2);
    /// assert_eq!(x.is_null(), false);
    ///
    /// let x: Nullable<u32> = Nullable::Null;
    /// assert_eq!(x.is_null(), true);
    /// ```
    #[inline]
    pub fn is_null(&self) -> bool {
        !self.is_present()
    }

    /////////////////////////////////////////////////////////////////////////
    // Adapter for working with references
    /////////////////////////////////////////////////////////////////////////

    /// Converts from `Nullable<T>` to `Nullable<&T>`.
    ///
    /// # Examples
    ///
    /// Convert an `Nullable<`[`String`]`>` into a `Nullable<`[`usize`]`>`, preserving the original.
    /// The [`map`] method takes the `self` argument by value, consuming the original,
    /// so this technique uses `as_ref` to first take a `Nullable` to a reference
    /// to the value inside the original.
    ///
    /// [`map`]: enum.Nullable.html#method.map
    /// [`String`]: ../../std/string/struct.String.html
    /// [`usize`]: ../../std/primitive.usize.html
    ///
    /// ```
    /// # use rust_server_test::types::Nullable;
    ///
    /// let num_as_str: Nullable<String> = Nullable::Present("10".to_string());
    /// // First, cast `Nullable<String>` to `Nullable<&String>` with `as_ref`,
    /// // then consume *that* with `map`, leaving `num_as_str` on the stack.
    /// let num_as_int: Nullable<usize> = num_as_str.as_ref().map(|n| n.len());
    /// println!("still can print num_as_str: {:?}", num_as_str);
    /// ```
    #[inline]
    pub fn as_ref(&self) -> Nullable<&T> {
        match *self {
            Nullable::Present(ref x) => Nullable::Present(x),
            Nullable::Null => Nullable::Null,
        }
    }

    /// Converts from `Nullable<T>` to `Nullable<&mut T>`.
    ///
    /// # Examples
    ///
    /// ```
    /// # use rust_server_test::types::Nullable;
    ///
    /// let mut x = Nullable::Present(2);
    /// match x.as_mut() {
    ///     Nullable::Present(v) => *v = 42,
    ///     Nullable::Null => {},
    /// }
    /// assert_eq!(x, Nullable::Present(42));
    /// ```
    #[inline]
    pub fn as_mut(&mut self) -> Nullable<&mut T> {
        match *self {
            Nullable::Present(ref mut x) => Nullable::Present(x),
            Nullable::Null => Nullable::Null,
        }
    }

    /////////////////////////////////////////////////////////////////////////
    // Getting to contained values
    /////////////////////////////////////////////////////////////////////////

    /// Unwraps a Nullable, yielding the content of a `Nullable::Present`.
    ///
    /// # Panics
    ///
    /// Panics if the value is a [`Nullable::Null`] with a custom panic message provided by
    /// `msg`.
    ///
    /// [`Nullable::Null`]: #variant.Null
    ///
    /// # Examples
    ///
    /// ```
    /// # use rust_server_test::types::Nullable;
    ///
    /// let x = Nullable::Present("value");
    /// assert_eq!(x.expect("the world is ending"), "value");
    /// ```
    ///
    /// ```should_panic
    /// # use rust_server_test::types::Nullable;
    ///
    /// let x: Nullable<&str> = Nullable::Null;
    /// x.expect("the world is ending"); // panics with `the world is ending`
    /// ```
    #[inline]
    pub fn expect(self, msg: &str) -> T {
        match self {
            Nullable::Present(val) => val,
            Nullable::Null => expect_failed(msg),
        }
    }

    /// Moves the value `v` out of the `Nullable<T>` if it is `Nullable::Present(v)`.
    ///
    /// In general, because this function may panic, its use is discouraged.
    /// Instead, prefer to use pattern matching and handle the `Nullable::Null`
    /// case explicitly.
    ///
    /// # Panics
    ///
    /// Panics if the self value equals [`Nullable::Null`].
    ///
    /// [`Nullable::Null`]: #variant.Null
    ///
    /// # Examples
    ///
    /// ```
    /// # use rust_server_test::types::Nullable;
    ///
    /// let x = Nullable::Present("air");
    /// assert_eq!(x.unwrap(), "air");
    /// ```
    ///
    /// ```should_panic
    /// # use rust_server_test::types::Nullable;
    ///
    /// let x: Nullable<&str> = Nullable::Null;
    /// assert_eq!(x.unwrap(), "air"); // fails
    /// ```
    #[inline]
    pub fn unwrap(self) -> T {
        match self {
            Nullable::Present(val) => val,
            Nullable::Null => panic!("called `Nullable::unwrap()` on a `Nullable::Null` value"),
        }
    }

    /// Returns the contained value or a default.
    ///
    /// # Examples
    ///
    /// ```
    /// # use rust_server_test::types::Nullable;
    ///
    /// assert_eq!(Nullable::Present("car").unwrap_or("bike"), "car");
    /// assert_eq!(Nullable::Null.unwrap_or("bike"), "bike");
    /// ```
    #[inline]
    pub fn unwrap_or(self, def: T) -> T {
        match self {
            Nullable::Present(x) => x,
            Nullable::Null => def,
        }
    }

    /// Returns the contained value or computes it from a closure.
    ///
    /// # Examples
    ///
    /// ```
    /// # use rust_server_test::types::Nullable;
    ///
    /// let k = 10;
    /// assert_eq!(Nullable::Present(4).unwrap_or_else(|| 2 * k), 4);
    /// assert_eq!(Nullable::Null.unwrap_or_else(|| 2 * k), 20);
    /// ```
    #[inline]
    pub fn unwrap_or_else<F: FnOnce() -> T>(self, f: F) -> T {
        match self {
            Nullable::Present(x) => x,
            Nullable::Null => f(),
        }
    }

    /////////////////////////////////////////////////////////////////////////
    // Transforming contained values
    /////////////////////////////////////////////////////////////////////////

    /// Maps a `Nullable<T>` to `Nullable<U>` by applying a function to a contained value.
    ///
    /// # Examples
    ///
    /// Convert a `Nullable<`[`String`]`>` into a `Nullable<`[`usize`]`>`, consuming the original:
    ///
    /// [`String`]: ../../std/string/struct.String.html
    /// [`usize`]: ../../std/primitive.usize.html
    ///
    /// ```
    /// # use rust_server_test::types::Nullable;
    ///
    /// let maybe_some_string = Nullable::Present(String::from("Hello, World!"));
    /// // `Nullable::map` takes self *by value*, consuming `maybe_some_string`
    /// let maybe_some_len = maybe_some_string.map(|s| s.len());
    ///
    /// assert_eq!(maybe_some_len, Nullable::Present(13));
    /// ```
    #[inline]
    pub fn map<U, F: FnOnce(T) -> U>(self, f: F) -> Nullable<U> {
        match self {
            Nullable::Present(x) => Nullable::Present(f(x)),
            Nullable::Null => Nullable::Null,
        }
    }

    /// Applies a function to the contained value (if any),
    /// or returns a `default` (if not).
    ///
    /// # Examples
    ///
    /// ```
    /// # use rust_server_test::types::Nullable;
    ///
    /// let x = Nullable::Present("foo");
    /// assert_eq!(x.map_or(42, |v| v.len()), 3);
    ///
    /// let x: Nullable<&str> = Nullable::Null;
    /// assert_eq!(x.map_or(42, |v| v.len()), 42);
    /// ```
    #[inline]
    pub fn map_or<U, F: FnOnce(T) -> U>(self, default: U, f: F) -> U {
        match self {
            Nullable::Present(t) => f(t),
            Nullable::Null => default,
        }
    }

    /// Applies a function to the contained value (if any),
    /// or computes a `default` (if not).
    ///
    /// # Examples
    ///
    /// ```
    /// # use rust_server_test::types::Nullable;
    ///
    /// let k = 21;
    ///
    /// let x = Nullable::Present("foo");
    /// assert_eq!(x.map_or_else(|| 2 * k, |v| v.len()), 3);
    ///
    /// let x: Nullable<&str> = Nullable::Null;
    /// assert_eq!(x.map_or_else(|| 2 * k, |v| v.len()), 42);
    /// ```
    #[inline]
    pub fn map_or_else<U, D: FnOnce() -> U, F: FnOnce(T) -> U>(self, default: D, f: F) -> U {
        match self {
            Nullable::Present(t) => f(t),
            Nullable::Null => default(),
        }
    }

    /// Transforms the `Nullable<T>` into a [`Result<T, E>`], mapping `Nullable::Present(v)` to
    /// [`Ok(v)`] and `Nullable::Null` to [`Err(err)`][Err].
    ///
    /// [`Result<T, E>`]: ../../std/result/enum.Result.html
    /// [`Ok(v)`]: ../../std/result/enum.Result.html#variant.Ok
    /// [Err]: ../../std/result/enum.Result.html#variant.Err
    ///
    /// # Examples
    ///
    /// ```
    /// # use rust_server_test::types::Nullable;
    ///
    /// let x = Nullable::Present("foo");
    /// assert_eq!(x.ok_or(0), Ok("foo"));
    ///
    /// let x: Nullable<&str> = Nullable::Null;
    /// assert_eq!(x.ok_or(0), Err(0));
    /// ```
    #[inline]
    pub fn ok_or<E>(self, err: E) -> Result<T, E> {
        match self {
            Nullable::Present(v) => Ok(v),
            Nullable::Null => Err(err),
        }
    }

    /// Transforms the `Nullable<T>` into a [`Result<T, E>`], mapping `Nullable::Present(v)` to
    /// [`Ok(v)`] and `Nullable::Null` to [`Err(err())`][Err].
    ///
    /// [`Result<T, E>`]: ../../std/result/enum.Result.html
    /// [`Ok(v)`]: ../../std/result/enum.Result.html#variant.Ok
    /// [Err]: ../../std/result/enum.Result.html#variant.Err
    ///
    /// # Examples
    ///
    /// ```
    /// # use rust_server_test::types::Nullable;
    ///
    /// let x = Nullable::Present("foo");
    /// assert_eq!(x.ok_or_else(|| 0), Ok("foo"));
    ///
    /// let x: Nullable<&str> = Nullable::Null;
    /// assert_eq!(x.ok_or_else(|| 0), Err(0));
    /// ```
    #[inline]
    pub fn ok_or_else<E, F: FnOnce() -> E>(self, err: F) -> Result<T, E> {
        match self {
            Nullable::Present(v) => Ok(v),
            Nullable::Null => Err(err()),
        }
    }

    /////////////////////////////////////////////////////////////////////////
    // Boolean operations on the values, eager and lazy
    /////////////////////////////////////////////////////////////////////////

    /// Returns `Nullable::Null` if the Nullable is `Nullable::Null`, otherwise returns `optb`.
    ///
    /// # Examples
    ///
    /// ```
    /// # use rust_server_test::types::Nullable;
    ///
    /// let x = Nullable::Present(2);
    /// let y: Nullable<&str> = Nullable::Null;
    /// assert_eq!(x.and(y), Nullable::Null);
    ///
    /// let x: Nullable<u32> = Nullable::Null;
    /// let y = Nullable::Present("foo");
    /// assert_eq!(x.and(y), Nullable::Null);
    ///
    /// let x = Nullable::Present(2);
    /// let y = Nullable::Present("foo");
    /// assert_eq!(x.and(y), Nullable::Present("foo"));
    ///
    /// let x: Nullable<u32> = Nullable::Null;
    /// let y: Nullable<&str> = Nullable::Null;
    /// assert_eq!(x.and(y), Nullable::Null);
    /// ```
    #[inline]
    pub fn and<U>(self, optb: Nullable<U>) -> Nullable<U> {
        match self {
            Nullable::Present(_) => optb,
            Nullable::Null => Nullable::Null,
        }
    }

    /// Returns `Nullable::Null` if the Nullable is `Nullable::Null`, otherwise calls `f` with the
    /// wrapped value and returns the result.
    ///
    /// Some languages call this operation flatmap.
    ///
    /// # Examples
    ///
    /// ```
    /// # use rust_server_test::types::Nullable;
    ///
    /// fn sq(x: u32) -> Nullable<u32> { Nullable::Present(x * x) }
    /// fn nope(_: u32) -> Nullable<u32> { Nullable::Null }
    ///
    /// assert_eq!(Nullable::Present(2).and_then(sq).and_then(sq), Nullable::Present(16));
    /// assert_eq!(Nullable::Present(2).and_then(sq).and_then(nope), Nullable::Null);
    /// assert_eq!(Nullable::Present(2).and_then(nope).and_then(sq), Nullable::Null);
    /// assert_eq!(Nullable::Null.and_then(sq).and_then(sq), Nullable::Null);
    /// ```
    #[inline]
    pub fn and_then<U, F: FnOnce(T) -> Nullable<U>>(self, f: F) -> Nullable<U> {
        match self {
            Nullable::Present(x) => f(x),
            Nullable::Null => Nullable::Null,
        }
    }

    /// Returns the Nullable if it contains a value, otherwise returns `optb`.
    ///
    /// # Examples
    ///
    /// ```
    /// # use rust_server_test::types::Nullable;
    ///
    /// let x = Nullable::Present(2);
    /// let y = Nullable::Null;
    /// assert_eq!(x.or(y), Nullable::Present(2));
    ///
    /// let x = Nullable::Null;
    /// let y = Nullable::Present(100);
    /// assert_eq!(x.or(y), Nullable::Present(100));
    ///
    /// let x = Nullable::Present(2);
    /// let y = Nullable::Present(100);
    /// assert_eq!(x.or(y), Nullable::Present(2));
    ///
    /// let x: Nullable<u32> = Nullable::Null;
    /// let y = Nullable::Null;
    /// assert_eq!(x.or(y), Nullable::Null);
    /// ```
    #[inline]
    pub fn or(self, optb: Nullable<T>) -> Nullable<T> {
        match self {
            Nullable::Present(_) => self,
            Nullable::Null => optb,
        }
    }

    /// Returns the Nullable if it contains a value, otherwise calls `f` and
    /// returns the result.
    ///
    /// # Examples
    ///
    /// ```
    /// # use rust_server_test::types::Nullable;
    ///
    /// fn nobody() -> Nullable<&'static str> { Nullable::Null }
    /// fn vikings() -> Nullable<&'static str> { Nullable::Present("vikings") }
    ///
    /// assert_eq!(Nullable::Present("barbarians").or_else(vikings),
    ///            Nullable::Present("barbarians"));
    /// assert_eq!(Nullable::Null.or_else(vikings), Nullable::Present("vikings"));
    /// assert_eq!(Nullable::Null.or_else(nobody), Nullable::Null);
    /// ```
    #[inline]
    pub fn or_else<F: FnOnce() -> Nullable<T>>(self, f: F) -> Nullable<T> {
        match self {
            Nullable::Present(_) => self,
            Nullable::Null => f(),
        }
    }

    /////////////////////////////////////////////////////////////////////////
    // Misc
    /////////////////////////////////////////////////////////////////////////

    /// Takes the value out of the Nullable, leaving a `Nullable::Null` in its place.
    ///
    /// # Examples
    ///
    /// ```
    /// # use rust_server_test::types::Nullable;
    ///
    /// let mut x = Nullable::Present(2);
    /// x.take();
    /// assert_eq!(x, Nullable::Null);
    ///
    /// let mut x: Nullable<u32> = Nullable::Null;
    /// x.take();
    /// assert_eq!(x, Nullable::Null);
    /// ```
    #[inline]
    pub fn take(&mut self) -> Nullable<T> {
        mem::replace(self, Nullable::Null)
    }
}

impl<T: Clone> Nullable<&T> {
    /// Maps an `Nullable<&T>` to an `Nullable<T>` by cloning the contents of the
    /// Nullable.
    ///
    /// # Examples
    ///
    /// ```
    /// # use rust_server_test::types::Nullable;
    ///
    /// let x = 12;
    /// let opt_x = Nullable::Present(&x);
    /// assert_eq!(opt_x, Nullable::Present(&12));
    /// let cloned = opt_x.cloned();
    /// assert_eq!(cloned, Nullable::Present(12));
    /// ```
    pub fn cloned(self) -> Nullable<T> {
        self.map(Clone::clone)
    }
}

impl<T: Default> Nullable<T> {
    /// Returns the contained value or a default
    ///
    /// Consumes the `self` argument then, if `Nullable::Present`, returns the contained
    /// value, otherwise if `Nullable::Null`, returns the default value for that
    /// type.
    ///
    /// # Examples
    ///
    /// ```
    /// # use rust_server_test::types::Nullable;
    ///
    /// let x = Nullable::Present(42);
    /// assert_eq!(42, x.unwrap_or_default());
    ///
    /// let y: Nullable<i32> = Nullable::Null;
    /// assert_eq!(0, y.unwrap_or_default());
    /// ```
    #[inline]
    pub fn unwrap_or_default(self) -> T {
        match self {
            Nullable::Present(x) => x,
            Nullable::Null => Default::default(),
        }
    }
}

impl<T> Default for Nullable<T> {
    /// Returns None.
    #[inline]
    fn default() -> Nullable<T> {
        Nullable::Null
    }
}

impl<T> From<T> for Nullable<T> {
    fn from(val: T) -> Nullable<T> {
        Nullable::Present(val)
    }
}

impl<T> Serialize for Nullable<T>
where
    T: Serialize,
{
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        match *self {
            Nullable::Present(ref inner) => serializer.serialize_some(&inner),
            Nullable::Null => serializer.serialize_none(),
        }
    }
}

impl<'de, T> Deserialize<'de> for Nullable<T>
where
    T: serde::de::DeserializeOwned,
{
    fn deserialize<D>(deserializer: D) -> Result<Nullable<T>, D::Error>
    where
        D: Deserializer<'de>,
    {
        // In order to deserialize a required, but nullable, value, we first have to check whether
        // the value is present at all. To do this, we deserialize to a serde_json::Value, which
        // fails if the value is missing, or gives serde_json::Value::Null if the value is present.
        // If that succeeds as null, we can easily return a Null.
        // If that succeeds as some value, we deserialize that value and return a Present.
        // If that errors, we return the error.
        let presence: Result<::serde_json::Value, _> =
            serde::Deserialize::deserialize(deserializer);
        match presence {
            Ok(serde_json::Value::Null) => Ok(Nullable::Null),
            Ok(some_value) => serde_json::from_value(some_value)
                .map(Nullable::Present)
                .map_err(serde::de::Error::custom),
            Err(x) => Err(x),
        }
    }
}

impl<T> validator::Validate for Nullable<T>
where
    T: validator::Validate,
{
    fn validate(&self) -> Result<(), validator::ValidationErrors> {
        match self {
            Self::Present(x) => x.validate(),
            Self::Null => Ok(()),
        }
    }
}

impl<'a, T> validator::ValidateArgs<'a> for Nullable<T>
where
    T: validator::ValidateArgs<'a>,
{
    type Args = T::Args;
    fn validate_with_args(&self, args: Self::Args) -> Result<(), validator::ValidationErrors> {
        match self {
            Self::Present(x) => x.validate_with_args(args),
            Self::Null => Ok(()),
        }
    }
}

impl<T> validator::ValidateEmail for Nullable<T>
where
    T: validator::ValidateEmail,
{
    fn as_email_string(&'_ self) -> Option<std::borrow::Cow<'_, str>> {
        match self {
            Self::Present(x) => x.as_email_string(),
            Self::Null => None,
        }
    }
}

impl<T> validator::ValidateUrl for Nullable<T>
where
    T: validator::ValidateUrl,
{
    fn as_url_string(&'_ self) -> Option<std::borrow::Cow<'_, str>> {
        match self {
            Self::Present(x) => x.as_url_string(),
            Self::Null => None,
        }
    }
}

impl<T> validator::ValidateContains for Nullable<T>
where
    T: validator::ValidateContains,
{
    fn validate_contains(&self, needle: &str) -> bool {
        match self {
            Self::Present(x) => x.validate_contains(needle),
            Self::Null => true,
        }
    }
}

impl<T> validator::ValidateRequired for Nullable<T>
where
    T: validator::ValidateRequired,
{
    fn is_some(&self) -> bool {
        self.is_present()
    }
}

impl<T> validator::ValidateRegex for Nullable<T>
where
    T: validator::ValidateRegex,
{
    fn validate_regex(&self, regex: impl validator::AsRegex) -> bool {
        match self {
            Self::Present(x) => x.validate_regex(regex),
            Self::Null => true,
        }
    }
}

impl<T, I> validator::ValidateRange<I> for Nullable<T>
where
    T: validator::ValidateRange<I>,
{
    fn greater_than(&self, max: I) -> Option<bool> {
        use validator::ValidateRange;
        match self {
            Self::Present(x) => x.greater_than(max),
            Self::Null => None,
        }
    }
    fn less_than(&self, min: I) -> Option<bool> {
        use validator::ValidateRange;
        match self {
            Self::Present(x) => x.less_than(min),
            Self::Null => None,
        }
    }
}

impl<T, I> validator::ValidateLength<I> for Nullable<T>
where
    T: validator::ValidateLength<I>,
    I: PartialEq + PartialOrd,
{
    fn length(&self) -> Option<I> {
        use validator::ValidateLength;
        match self {
            Self::Present(x) => x.length(),
            Self::Null => None,
        }
    }
}

impl<T> From<Nullable<T>> for Option<T> {
    fn from(value: Nullable<T>) -> Option<T> {
        match value {
            Nullable::Present(x) => Some(x),
            Nullable::Null => None,
        }
    }
}

#[inline(never)]
#[cold]
fn expect_failed(msg: &str) -> ! {
    panic!("{}", msg)
}

#[derive(Debug, Clone, PartialEq, PartialOrd)]
/// Base64-encoded byte array
pub struct ByteArray(pub Vec<u8>);

impl Serialize for ByteArray {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        serializer.serialize_str(&general_purpose::STANDARD.encode(&self.0))
    }
}

impl<'de> Deserialize<'de> for ByteArray {
    fn deserialize<D>(deserializer: D) -> Result<ByteArray, D::Error>
    where
        D: Deserializer<'de>,
    {
        let s = String::deserialize(deserializer)?;
        match general_purpose::STANDARD.decode(s) {
            Ok(bin) => Ok(ByteArray(bin)),
            _ => Err(serde::de::Error::custom("invalid base64")),
        }
    }
}
