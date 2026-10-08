#![allow(unused_qualifications)]

#[cfg(feature = "server")]
use http::HeaderValue;
use validator::Validate;

#[cfg(feature = "server")]
use crate::header;
use crate::{models, types::*};

#[cfg(feature = "server")]
#[allow(dead_code)]
pub type SSE = std::pin::Pin<
    std::boxed::Box<
        dyn futures_util::Stream<
                Item = std::result::Result<axum::response::sse::Event, std::convert::Infallible>,
            > + std::marker::Send
            + std::marker::Sync,
    >,
>;

#[allow(dead_code)]
fn from_validation_error(e: validator::ValidationError) -> validator::ValidationErrors {
    let mut errs = validator::ValidationErrors::new();
    errs.add("na", e);
    errs
}

/// XSS detection relies on `ammonia`, which is only pulled in by the `server` feature.
/// Without it (e.g. models-only WASM builds), values are never flagged as HTML.
#[cfg(feature = "server")]
fn contains_html(v: &str) -> bool {
    ammonia::is_html(v)
}

#[cfg(not(feature = "server"))]
fn contains_html(_v: &str) -> bool {
    false
}

#[allow(dead_code)]
pub fn check_xss_string(v: &str) -> std::result::Result<(), validator::ValidationError> {
    if contains_html(v) {
        std::result::Result::Err(validator::ValidationError::new("xss detected"))
    } else {
        std::result::Result::Ok(())
    }
}

#[allow(dead_code)]
pub fn check_xss_vec_string(v: &[String]) -> std::result::Result<(), validator::ValidationError> {
    if v.iter().any(|i| contains_html(i)) {
        std::result::Result::Err(validator::ValidationError::new("xss detected"))
    } else {
        std::result::Result::Ok(())
    }
}

#[allow(dead_code)]
pub fn check_xss_map_string(
    v: &std::collections::HashMap<String, String>,
) -> std::result::Result<(), validator::ValidationError> {
    if v.keys().any(|k| contains_html(k)) || v.values().any(|v| contains_html(v)) {
        std::result::Result::Err(validator::ValidationError::new("xss detected"))
    } else {
        std::result::Result::Ok(())
    }
}

#[allow(dead_code)]
pub fn check_xss_map_nested<T>(
    v: &std::collections::HashMap<String, T>,
) -> std::result::Result<(), validator::ValidationError>
where
    T: validator::Validate,
{
    if v.keys().any(|k| contains_html(k)) || v.values().any(|v| v.validate().is_err()) {
        std::result::Result::Err(validator::ValidationError::new("xss detected"))
    } else {
        std::result::Result::Ok(())
    }
}

#[allow(dead_code)]
pub fn check_xss_map<T>(
    v: &std::collections::HashMap<String, T>,
) -> std::result::Result<(), validator::ValidationError> {
    if v.keys().any(|k| contains_html(k)) {
        std::result::Result::Err(validator::ValidationError::new("xss detected"))
    } else {
        std::result::Result::Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize, validator::Validate)]
#[cfg_attr(feature = "conversion", derive(frunk::LabelledGeneric))]
pub struct Apple {
    #[serde(rename = "fruitType")]
    #[validate(nested)]
    pub fruit_type: models::FruitType,

    #[serde(rename = "cultivar")]
    #[validate(custom(function = "check_xss_string"))]
    #[serde(skip_serializing_if = "Option::is_none")]
    pub cultivar: Option<String>,
}

impl Apple {
    #[allow(clippy::new_without_default, clippy::too_many_arguments)]
    pub fn new(fruit_type: models::FruitType) -> Apple {
        Apple {
            fruit_type,
            cultivar: None,
        }
    }
}

/// Converts the Apple value to the Query Parameters representation (style=form, explode=false)
/// specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde serializer
impl std::fmt::Display for Apple {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let params: Vec<Option<String>> = vec![
            // Skipping fruitType in query parameter serialization
            self.cultivar
                .as_ref()
                .map(|cultivar| ["cultivar".to_string(), cultivar.to_string()].join(",")),
        ];

        write!(
            f,
            "{}",
            params.into_iter().flatten().collect::<Vec<_>>().join(",")
        )
    }
}

/// Converts Query Parameters representation (style=form, explode=false) to a Apple value
/// as specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde deserializer
impl std::str::FromStr for Apple {
    type Err = String;

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        /// An intermediate representation of the struct to use for parsing.
        #[derive(Default)]
        #[allow(dead_code)]
        struct IntermediateRep {
            pub fruit_type: Vec<models::FruitType>,
            pub cultivar: Vec<String>,
        }

        let mut intermediate_rep = IntermediateRep::default();

        // Parse into intermediate representation
        let mut string_iter = s.split(',');
        let mut key_result = string_iter.next();

        while key_result.is_some() {
            let val = match string_iter.next() {
                Some(x) => x,
                None => {
                    return std::result::Result::Err(
                        "Missing value while parsing Apple".to_string(),
                    )
                }
            };

            if let Some(key) = key_result {
                #[allow(clippy::match_single_binding)]
                match key {
                    #[allow(clippy::redundant_clone)]
                    "fruitType" => intermediate_rep.fruit_type.push(
                        <models::FruitType as std::str::FromStr>::from_str(val)
                            .map_err(|x| x.to_string())?,
                    ),
                    #[allow(clippy::redundant_clone)]
                    "cultivar" => intermediate_rep.cultivar.push(
                        <String as std::str::FromStr>::from_str(val).map_err(|x| x.to_string())?,
                    ),
                    _ => {
                        return std::result::Result::Err(
                            "Unexpected key while parsing Apple".to_string(),
                        )
                    }
                }
            }

            // Get the next key
            key_result = string_iter.next();
        }

        // Use the intermediate representation to return the struct
        std::result::Result::Ok(Apple {
            fruit_type: intermediate_rep
                .fruit_type
                .into_iter()
                .next()
                .ok_or_else(|| "fruitType missing in Apple".to_string())?,
            cultivar: intermediate_rep.cultivar.into_iter().next(),
        })
    }
}

// Methods for converting between header::IntoHeaderValue<Apple> and HeaderValue

#[cfg(feature = "server")]
impl std::convert::TryFrom<header::IntoHeaderValue<Apple>> for HeaderValue {
    type Error = String;

    fn try_from(
        hdr_value: header::IntoHeaderValue<Apple>,
    ) -> std::result::Result<Self, Self::Error> {
        let hdr_value = hdr_value.to_string();
        match HeaderValue::from_str(&hdr_value) {
            std::result::Result::Ok(value) => std::result::Result::Ok(value),
            std::result::Result::Err(e) => std::result::Result::Err(format!(
                r#"Invalid header value for Apple - value: {hdr_value} is invalid {e}"#
            )),
        }
    }
}

#[cfg(feature = "server")]
impl std::convert::TryFrom<HeaderValue> for header::IntoHeaderValue<Apple> {
    type Error = String;

    fn try_from(hdr_value: HeaderValue) -> std::result::Result<Self, Self::Error> {
        match hdr_value.to_str() {
            std::result::Result::Ok(value) => match <Apple as std::str::FromStr>::from_str(value) {
                std::result::Result::Ok(value) => {
                    std::result::Result::Ok(header::IntoHeaderValue(value))
                }
                std::result::Result::Err(err) => std::result::Result::Err(format!(
                    r#"Unable to convert header value '{value}' into Apple - {err}"#
                )),
            },
            std::result::Result::Err(e) => std::result::Result::Err(format!(
                r#"Unable to convert header: {hdr_value:?} to string: {e}"#
            )),
        }
    }
}

#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize, validator::Validate)]
#[cfg_attr(feature = "conversion", derive(frunk::LabelledGeneric))]
pub struct Banana {
    #[serde(default = "Banana::_name_for_fruit_type")]
    #[serde(serialize_with = "Banana::_serialize_fruit_type")]
    #[serde(rename = "fruitType")]
    #[validate(nested)]
    pub fruit_type: models::FruitType,

    #[serde(rename = "lengthCm")]
    #[serde(skip_serializing_if = "Option::is_none")]
    pub length_cm: Option<f64>,
}

impl Banana {
    fn _name_for_fruit_type() -> models::FruitType {
        models::FruitType::Banana
    }

    fn _serialize_fruit_type<S>(_: &models::FruitType, s: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        s.serialize_str("banana")
    }
}

impl Banana {
    #[allow(clippy::new_without_default, clippy::too_many_arguments)]
    pub fn new() -> Banana {
        Banana {
            fruit_type: Self::_name_for_fruit_type(),
            length_cm: None,
        }
    }
}

/// Converts the Banana value to the Query Parameters representation (style=form, explode=false)
/// specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde serializer
impl std::fmt::Display for Banana {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let params: Vec<Option<String>> = vec![
            // Skipping fruitType in query parameter serialization
            self.length_cm
                .as_ref()
                .map(|length_cm| ["lengthCm".to_string(), length_cm.to_string()].join(",")),
        ];

        write!(
            f,
            "{}",
            params.into_iter().flatten().collect::<Vec<_>>().join(",")
        )
    }
}

/// Converts Query Parameters representation (style=form, explode=false) to a Banana value
/// as specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde deserializer
impl std::str::FromStr for Banana {
    type Err = String;

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        /// An intermediate representation of the struct to use for parsing.
        #[derive(Default)]
        #[allow(dead_code)]
        struct IntermediateRep {
            pub fruit_type: Vec<models::FruitType>,
            pub length_cm: Vec<f64>,
        }

        let mut intermediate_rep = IntermediateRep::default();

        // Parse into intermediate representation
        let mut string_iter = s.split(',');
        let mut key_result = string_iter.next();

        while key_result.is_some() {
            let val = match string_iter.next() {
                Some(x) => x,
                None => {
                    return std::result::Result::Err(
                        "Missing value while parsing Banana".to_string(),
                    )
                }
            };

            if let Some(key) = key_result {
                #[allow(clippy::match_single_binding)]
                match key {
                    #[allow(clippy::redundant_clone)]
                    "fruitType" => intermediate_rep.fruit_type.push(
                        <models::FruitType as std::str::FromStr>::from_str(val)
                            .map_err(|x| x.to_string())?,
                    ),
                    #[allow(clippy::redundant_clone)]
                    "lengthCm" => intermediate_rep.length_cm.push(
                        <f64 as std::str::FromStr>::from_str(val).map_err(|x| x.to_string())?,
                    ),
                    _ => {
                        return std::result::Result::Err(
                            "Unexpected key while parsing Banana".to_string(),
                        )
                    }
                }
            }

            // Get the next key
            key_result = string_iter.next();
        }

        // Use the intermediate representation to return the struct
        std::result::Result::Ok(Banana {
            fruit_type: intermediate_rep
                .fruit_type
                .into_iter()
                .next()
                .ok_or_else(|| "fruitType missing in Banana".to_string())?,
            length_cm: intermediate_rep.length_cm.into_iter().next(),
        })
    }
}

// Methods for converting between header::IntoHeaderValue<Banana> and HeaderValue

#[cfg(feature = "server")]
impl std::convert::TryFrom<header::IntoHeaderValue<Banana>> for HeaderValue {
    type Error = String;

    fn try_from(
        hdr_value: header::IntoHeaderValue<Banana>,
    ) -> std::result::Result<Self, Self::Error> {
        let hdr_value = hdr_value.to_string();
        match HeaderValue::from_str(&hdr_value) {
            std::result::Result::Ok(value) => std::result::Result::Ok(value),
            std::result::Result::Err(e) => std::result::Result::Err(format!(
                r#"Invalid header value for Banana - value: {hdr_value} is invalid {e}"#
            )),
        }
    }
}

#[cfg(feature = "server")]
impl std::convert::TryFrom<HeaderValue> for header::IntoHeaderValue<Banana> {
    type Error = String;

    fn try_from(hdr_value: HeaderValue) -> std::result::Result<Self, Self::Error> {
        match hdr_value.to_str() {
            std::result::Result::Ok(value) => {
                match <Banana as std::str::FromStr>::from_str(value) {
                    std::result::Result::Ok(value) => {
                        std::result::Result::Ok(header::IntoHeaderValue(value))
                    }
                    std::result::Result::Err(err) => std::result::Result::Err(format!(
                        r#"Unable to convert header value '{value}' into Banana - {err}"#
                    )),
                }
            }
            std::result::Result::Err(e) => std::result::Result::Err(format!(
                r#"Unable to convert header: {hdr_value:?} to string: {e}"#
            )),
        }
    }
}

#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize, validator::Validate)]
#[cfg_attr(feature = "conversion", derive(frunk::LabelledGeneric))]
pub struct Cat {
    #[serde(default = "Cat::_name_for_kind")]
    #[serde(serialize_with = "Cat::_serialize_kind")]
    #[serde(rename = "kind")]
    #[validate(nested)]
    pub kind: models::PetKind,

    #[serde(rename = "meow")]
    #[validate(custom(function = "check_xss_string"))]
    #[serde(skip_serializing_if = "Option::is_none")]
    pub meow: Option<String>,
}

impl Cat {
    fn _name_for_kind() -> models::PetKind {
        models::PetKind::Cat
    }

    fn _serialize_kind<S>(_: &models::PetKind, s: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        s.serialize_str("cat")
    }
}

impl Cat {
    #[allow(clippy::new_without_default, clippy::too_many_arguments)]
    pub fn new() -> Cat {
        Cat {
            kind: Self::_name_for_kind(),
            meow: None,
        }
    }
}

/// Converts the Cat value to the Query Parameters representation (style=form, explode=false)
/// specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde serializer
impl std::fmt::Display for Cat {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let params: Vec<Option<String>> = vec![
            // Skipping kind in query parameter serialization
            self.meow
                .as_ref()
                .map(|meow| ["meow".to_string(), meow.to_string()].join(",")),
        ];

        write!(
            f,
            "{}",
            params.into_iter().flatten().collect::<Vec<_>>().join(",")
        )
    }
}

/// Converts Query Parameters representation (style=form, explode=false) to a Cat value
/// as specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde deserializer
impl std::str::FromStr for Cat {
    type Err = String;

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        /// An intermediate representation of the struct to use for parsing.
        #[derive(Default)]
        #[allow(dead_code)]
        struct IntermediateRep {
            pub kind: Vec<models::PetKind>,
            pub meow: Vec<String>,
        }

        let mut intermediate_rep = IntermediateRep::default();

        // Parse into intermediate representation
        let mut string_iter = s.split(',');
        let mut key_result = string_iter.next();

        while key_result.is_some() {
            let val = match string_iter.next() {
                Some(x) => x,
                None => {
                    return std::result::Result::Err("Missing value while parsing Cat".to_string())
                }
            };

            if let Some(key) = key_result {
                #[allow(clippy::match_single_binding)]
                match key {
                    #[allow(clippy::redundant_clone)]
                    "kind" => intermediate_rep.kind.push(
                        <models::PetKind as std::str::FromStr>::from_str(val)
                            .map_err(|x| x.to_string())?,
                    ),
                    #[allow(clippy::redundant_clone)]
                    "meow" => intermediate_rep.meow.push(
                        <String as std::str::FromStr>::from_str(val).map_err(|x| x.to_string())?,
                    ),
                    _ => {
                        return std::result::Result::Err(
                            "Unexpected key while parsing Cat".to_string(),
                        )
                    }
                }
            }

            // Get the next key
            key_result = string_iter.next();
        }

        // Use the intermediate representation to return the struct
        std::result::Result::Ok(Cat {
            kind: intermediate_rep
                .kind
                .into_iter()
                .next()
                .ok_or_else(|| "kind missing in Cat".to_string())?,
            meow: intermediate_rep.meow.into_iter().next(),
        })
    }
}

// Methods for converting between header::IntoHeaderValue<Cat> and HeaderValue

#[cfg(feature = "server")]
impl std::convert::TryFrom<header::IntoHeaderValue<Cat>> for HeaderValue {
    type Error = String;

    fn try_from(hdr_value: header::IntoHeaderValue<Cat>) -> std::result::Result<Self, Self::Error> {
        let hdr_value = hdr_value.to_string();
        match HeaderValue::from_str(&hdr_value) {
            std::result::Result::Ok(value) => std::result::Result::Ok(value),
            std::result::Result::Err(e) => std::result::Result::Err(format!(
                r#"Invalid header value for Cat - value: {hdr_value} is invalid {e}"#
            )),
        }
    }
}

#[cfg(feature = "server")]
impl std::convert::TryFrom<HeaderValue> for header::IntoHeaderValue<Cat> {
    type Error = String;

    fn try_from(hdr_value: HeaderValue) -> std::result::Result<Self, Self::Error> {
        match hdr_value.to_str() {
            std::result::Result::Ok(value) => match <Cat as std::str::FromStr>::from_str(value) {
                std::result::Result::Ok(value) => {
                    std::result::Result::Ok(header::IntoHeaderValue(value))
                }
                std::result::Result::Err(err) => std::result::Result::Err(format!(
                    r#"Unable to convert header value '{value}' into Cat - {err}"#
                )),
            },
            std::result::Result::Err(e) => std::result::Result::Err(format!(
                r#"Unable to convert header: {hdr_value:?} to string: {e}"#
            )),
        }
    }
}

#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize, validator::Validate)]
#[cfg_attr(feature = "conversion", derive(frunk::LabelledGeneric))]
pub struct Circle {
    #[serde(rename = "kind")]
    #[validate(nested)]
    pub kind: models::ShapeKind,
}

impl Circle {
    #[allow(clippy::new_without_default, clippy::too_many_arguments)]
    pub fn new(kind: models::ShapeKind) -> Circle {
        Circle { kind }
    }
}

/// Converts the Circle value to the Query Parameters representation (style=form, explode=false)
/// specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde serializer
impl std::fmt::Display for Circle {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let params: Vec<Option<String>> = vec![
            // Skipping kind in query parameter serialization

        ];

        write!(
            f,
            "{}",
            params.into_iter().flatten().collect::<Vec<_>>().join(",")
        )
    }
}

/// Converts Query Parameters representation (style=form, explode=false) to a Circle value
/// as specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde deserializer
impl std::str::FromStr for Circle {
    type Err = String;

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        /// An intermediate representation of the struct to use for parsing.
        #[derive(Default)]
        #[allow(dead_code)]
        struct IntermediateRep {
            pub kind: Vec<models::ShapeKind>,
        }

        let mut intermediate_rep = IntermediateRep::default();

        // Parse into intermediate representation
        let mut string_iter = s.split(',');
        let mut key_result = string_iter.next();

        while key_result.is_some() {
            let val = match string_iter.next() {
                Some(x) => x,
                None => {
                    return std::result::Result::Err(
                        "Missing value while parsing Circle".to_string(),
                    )
                }
            };

            if let Some(key) = key_result {
                #[allow(clippy::match_single_binding)]
                match key {
                    #[allow(clippy::redundant_clone)]
                    "kind" => intermediate_rep.kind.push(
                        <models::ShapeKind as std::str::FromStr>::from_str(val)
                            .map_err(|x| x.to_string())?,
                    ),
                    _ => {
                        return std::result::Result::Err(
                            "Unexpected key while parsing Circle".to_string(),
                        )
                    }
                }
            }

            // Get the next key
            key_result = string_iter.next();
        }

        // Use the intermediate representation to return the struct
        std::result::Result::Ok(Circle {
            kind: intermediate_rep
                .kind
                .into_iter()
                .next()
                .ok_or_else(|| "kind missing in Circle".to_string())?,
        })
    }
}

// Methods for converting between header::IntoHeaderValue<Circle> and HeaderValue

#[cfg(feature = "server")]
impl std::convert::TryFrom<header::IntoHeaderValue<Circle>> for HeaderValue {
    type Error = String;

    fn try_from(
        hdr_value: header::IntoHeaderValue<Circle>,
    ) -> std::result::Result<Self, Self::Error> {
        let hdr_value = hdr_value.to_string();
        match HeaderValue::from_str(&hdr_value) {
            std::result::Result::Ok(value) => std::result::Result::Ok(value),
            std::result::Result::Err(e) => std::result::Result::Err(format!(
                r#"Invalid header value for Circle - value: {hdr_value} is invalid {e}"#
            )),
        }
    }
}

#[cfg(feature = "server")]
impl std::convert::TryFrom<HeaderValue> for header::IntoHeaderValue<Circle> {
    type Error = String;

    fn try_from(hdr_value: HeaderValue) -> std::result::Result<Self, Self::Error> {
        match hdr_value.to_str() {
            std::result::Result::Ok(value) => {
                match <Circle as std::str::FromStr>::from_str(value) {
                    std::result::Result::Ok(value) => {
                        std::result::Result::Ok(header::IntoHeaderValue(value))
                    }
                    std::result::Result::Err(err) => std::result::Result::Err(format!(
                        r#"Unable to convert header value '{value}' into Circle - {err}"#
                    )),
                }
            }
            std::result::Result::Err(e) => std::result::Result::Err(format!(
                r#"Unable to convert header: {hdr_value:?} to string: {e}"#
            )),
        }
    }
}

#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize, validator::Validate)]
#[cfg_attr(feature = "conversion", derive(frunk::LabelledGeneric))]
pub struct Dog {
    #[serde(default = "Dog::_name_for_kind")]
    #[serde(serialize_with = "Dog::_serialize_kind")]
    #[serde(rename = "kind")]
    #[validate(nested)]
    pub kind: models::PetKind,

    #[serde(rename = "bark")]
    #[validate(custom(function = "check_xss_string"))]
    #[serde(skip_serializing_if = "Option::is_none")]
    pub bark: Option<String>,
}

impl Dog {
    fn _name_for_kind() -> models::PetKind {
        models::PetKind::Dog
    }

    fn _serialize_kind<S>(_: &models::PetKind, s: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        s.serialize_str("dog")
    }
}

impl Dog {
    #[allow(clippy::new_without_default, clippy::too_many_arguments)]
    pub fn new() -> Dog {
        Dog {
            kind: Self::_name_for_kind(),
            bark: None,
        }
    }
}

/// Converts the Dog value to the Query Parameters representation (style=form, explode=false)
/// specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde serializer
impl std::fmt::Display for Dog {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let params: Vec<Option<String>> = vec![
            // Skipping kind in query parameter serialization
            self.bark
                .as_ref()
                .map(|bark| ["bark".to_string(), bark.to_string()].join(",")),
        ];

        write!(
            f,
            "{}",
            params.into_iter().flatten().collect::<Vec<_>>().join(",")
        )
    }
}

/// Converts Query Parameters representation (style=form, explode=false) to a Dog value
/// as specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde deserializer
impl std::str::FromStr for Dog {
    type Err = String;

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        /// An intermediate representation of the struct to use for parsing.
        #[derive(Default)]
        #[allow(dead_code)]
        struct IntermediateRep {
            pub kind: Vec<models::PetKind>,
            pub bark: Vec<String>,
        }

        let mut intermediate_rep = IntermediateRep::default();

        // Parse into intermediate representation
        let mut string_iter = s.split(',');
        let mut key_result = string_iter.next();

        while key_result.is_some() {
            let val = match string_iter.next() {
                Some(x) => x,
                None => {
                    return std::result::Result::Err("Missing value while parsing Dog".to_string())
                }
            };

            if let Some(key) = key_result {
                #[allow(clippy::match_single_binding)]
                match key {
                    #[allow(clippy::redundant_clone)]
                    "kind" => intermediate_rep.kind.push(
                        <models::PetKind as std::str::FromStr>::from_str(val)
                            .map_err(|x| x.to_string())?,
                    ),
                    #[allow(clippy::redundant_clone)]
                    "bark" => intermediate_rep.bark.push(
                        <String as std::str::FromStr>::from_str(val).map_err(|x| x.to_string())?,
                    ),
                    _ => {
                        return std::result::Result::Err(
                            "Unexpected key while parsing Dog".to_string(),
                        )
                    }
                }
            }

            // Get the next key
            key_result = string_iter.next();
        }

        // Use the intermediate representation to return the struct
        std::result::Result::Ok(Dog {
            kind: intermediate_rep
                .kind
                .into_iter()
                .next()
                .ok_or_else(|| "kind missing in Dog".to_string())?,
            bark: intermediate_rep.bark.into_iter().next(),
        })
    }
}

// Methods for converting between header::IntoHeaderValue<Dog> and HeaderValue

#[cfg(feature = "server")]
impl std::convert::TryFrom<header::IntoHeaderValue<Dog>> for HeaderValue {
    type Error = String;

    fn try_from(hdr_value: header::IntoHeaderValue<Dog>) -> std::result::Result<Self, Self::Error> {
        let hdr_value = hdr_value.to_string();
        match HeaderValue::from_str(&hdr_value) {
            std::result::Result::Ok(value) => std::result::Result::Ok(value),
            std::result::Result::Err(e) => std::result::Result::Err(format!(
                r#"Invalid header value for Dog - value: {hdr_value} is invalid {e}"#
            )),
        }
    }
}

#[cfg(feature = "server")]
impl std::convert::TryFrom<HeaderValue> for header::IntoHeaderValue<Dog> {
    type Error = String;

    fn try_from(hdr_value: HeaderValue) -> std::result::Result<Self, Self::Error> {
        match hdr_value.to_str() {
            std::result::Result::Ok(value) => match <Dog as std::str::FromStr>::from_str(value) {
                std::result::Result::Ok(value) => {
                    std::result::Result::Ok(header::IntoHeaderValue(value))
                }
                std::result::Result::Err(err) => std::result::Result::Err(format!(
                    r#"Unable to convert header value '{value}' into Dog - {err}"#
                )),
            },
            std::result::Result::Err(e) => std::result::Result::Err(format!(
                r#"Unable to convert header: {hdr_value:?} to string: {e}"#
            )),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
#[allow(non_camel_case_types, clippy::large_enum_variant)]
pub enum Fruit {
    Apple(models::Apple),
    Banana(models::Banana),
}

impl validator::Validate for Fruit {
    fn validate(&self) -> std::result::Result<(), validator::ValidationErrors> {
        match self {
            Self::Apple(v) => v.validate(),
            Self::Banana(v) => v.validate(),
        }
    }
}

/// Converts Query Parameters representation (style=form, explode=false) to a Fruit value
/// as specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde deserializer
impl std::str::FromStr for Fruit {
    type Err = serde_json::Error;

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        serde_json::from_str(s)
    }
}

impl serde::Serialize for Fruit {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        match self {
            Self::Apple(x) => x.serialize(serializer),
            Self::Banana(x) => x.serialize(serializer),
        }
    }
}

crate::impl_deserialize_tagged!(Fruit, "fruitType", {
    "green_apple" | "red_apple" => Apple,
    "banana" => Banana,
});

impl From<models::Apple> for Fruit {
    fn from(value: models::Apple) -> Self {
        Self::Apple(value)
    }
}
impl From<models::Banana> for Fruit {
    fn from(value: models::Banana) -> Self {
        Self::Banana(value)
    }
}

/// Enumeration of values.
/// Since this enum's variants do not hold data, we can easily define them as `#[repr(C)]`
/// which helps with FFI.
#[allow(non_camel_case_types, clippy::large_enum_variant)]
#[repr(C)]
#[derive(
    Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, serde::Serialize, serde::Deserialize,
)]
#[cfg_attr(feature = "conversion", derive(frunk_enum_derive::LabelledGenericEnum))]
pub enum FruitType {
    #[serde(rename = "green_apple")]
    GreenApple,
    #[serde(rename = "red_apple")]
    RedApple,
    #[serde(rename = "banana")]
    Banana,
}

impl validator::Validate for FruitType {
    fn validate(&self) -> std::result::Result<(), validator::ValidationErrors> {
        std::result::Result::Ok(())
    }
}

impl std::fmt::Display for FruitType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match *self {
            FruitType::GreenApple => write!(f, "green_apple"),
            FruitType::RedApple => write!(f, "red_apple"),
            FruitType::Banana => write!(f, "banana"),
        }
    }
}

impl std::str::FromStr for FruitType {
    type Err = String;

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        match s {
            "green_apple" => std::result::Result::Ok(FruitType::GreenApple),
            "red_apple" => std::result::Result::Ok(FruitType::RedApple),
            "banana" => std::result::Result::Ok(FruitType::Banana),
            _ => std::result::Result::Err(format!(r#"Value not valid: {s}"#)),
        }
    }
}

#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize, validator::Validate)]
#[cfg_attr(feature = "conversion", derive(frunk::LabelledGeneric))]
pub struct Hamster {
    #[serde(default = "Hamster::_name_for_kind")]
    #[serde(serialize_with = "Hamster::_serialize_kind")]
    #[serde(rename = "kind")]
    #[validate(nested)]
    pub kind: models::RodentKind,
}

impl Hamster {
    fn _name_for_kind() -> models::RodentKind {
        models::RodentKind::Hamster
    }

    fn _serialize_kind<S>(_: &models::RodentKind, s: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        s.serialize_str("hamster")
    }
}

impl Hamster {
    #[allow(clippy::new_without_default, clippy::too_many_arguments)]
    pub fn new() -> Hamster {
        Hamster {
            kind: Self::_name_for_kind(),
        }
    }
}

/// Converts the Hamster value to the Query Parameters representation (style=form, explode=false)
/// specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde serializer
impl std::fmt::Display for Hamster {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let params: Vec<Option<String>> = vec![
            // Skipping kind in query parameter serialization

        ];

        write!(
            f,
            "{}",
            params.into_iter().flatten().collect::<Vec<_>>().join(",")
        )
    }
}

/// Converts Query Parameters representation (style=form, explode=false) to a Hamster value
/// as specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde deserializer
impl std::str::FromStr for Hamster {
    type Err = String;

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        /// An intermediate representation of the struct to use for parsing.
        #[derive(Default)]
        #[allow(dead_code)]
        struct IntermediateRep {
            pub kind: Vec<models::RodentKind>,
        }

        let mut intermediate_rep = IntermediateRep::default();

        // Parse into intermediate representation
        let mut string_iter = s.split(',');
        let mut key_result = string_iter.next();

        while key_result.is_some() {
            let val = match string_iter.next() {
                Some(x) => x,
                None => {
                    return std::result::Result::Err(
                        "Missing value while parsing Hamster".to_string(),
                    )
                }
            };

            if let Some(key) = key_result {
                #[allow(clippy::match_single_binding)]
                match key {
                    #[allow(clippy::redundant_clone)]
                    "kind" => intermediate_rep.kind.push(
                        <models::RodentKind as std::str::FromStr>::from_str(val)
                            .map_err(|x| x.to_string())?,
                    ),
                    _ => {
                        return std::result::Result::Err(
                            "Unexpected key while parsing Hamster".to_string(),
                        )
                    }
                }
            }

            // Get the next key
            key_result = string_iter.next();
        }

        // Use the intermediate representation to return the struct
        std::result::Result::Ok(Hamster {
            kind: intermediate_rep
                .kind
                .into_iter()
                .next()
                .ok_or_else(|| "kind missing in Hamster".to_string())?,
        })
    }
}

// Methods for converting between header::IntoHeaderValue<Hamster> and HeaderValue

#[cfg(feature = "server")]
impl std::convert::TryFrom<header::IntoHeaderValue<Hamster>> for HeaderValue {
    type Error = String;

    fn try_from(
        hdr_value: header::IntoHeaderValue<Hamster>,
    ) -> std::result::Result<Self, Self::Error> {
        let hdr_value = hdr_value.to_string();
        match HeaderValue::from_str(&hdr_value) {
            std::result::Result::Ok(value) => std::result::Result::Ok(value),
            std::result::Result::Err(e) => std::result::Result::Err(format!(
                r#"Invalid header value for Hamster - value: {hdr_value} is invalid {e}"#
            )),
        }
    }
}

#[cfg(feature = "server")]
impl std::convert::TryFrom<HeaderValue> for header::IntoHeaderValue<Hamster> {
    type Error = String;

    fn try_from(hdr_value: HeaderValue) -> std::result::Result<Self, Self::Error> {
        match hdr_value.to_str() {
            std::result::Result::Ok(value) => {
                match <Hamster as std::str::FromStr>::from_str(value) {
                    std::result::Result::Ok(value) => {
                        std::result::Result::Ok(header::IntoHeaderValue(value))
                    }
                    std::result::Result::Err(err) => std::result::Result::Err(format!(
                        r#"Unable to convert header value '{value}' into Hamster - {err}"#
                    )),
                }
            }
            std::result::Result::Err(e) => std::result::Result::Err(format!(
                r#"Unable to convert header: {hdr_value:?} to string: {e}"#
            )),
        }
    }
}

#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize, validator::Validate)]
#[cfg_attr(feature = "conversion", derive(frunk::LabelledGeneric))]
pub struct Mouse {
    #[serde(rename = "kind")]
    #[validate(nested)]
    #[serde(skip_serializing_if = "Option::is_none")]
    pub kind: Option<models::RodentKind>,
}

impl Mouse {
    #[allow(clippy::new_without_default, clippy::too_many_arguments)]
    pub fn new() -> Mouse {
        Mouse { kind: None }
    }
}

/// Converts the Mouse value to the Query Parameters representation (style=form, explode=false)
/// specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde serializer
impl std::fmt::Display for Mouse {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let params: Vec<Option<String>> = vec![
            // Skipping kind in query parameter serialization

        ];

        write!(
            f,
            "{}",
            params.into_iter().flatten().collect::<Vec<_>>().join(",")
        )
    }
}

/// Converts Query Parameters representation (style=form, explode=false) to a Mouse value
/// as specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde deserializer
impl std::str::FromStr for Mouse {
    type Err = String;

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        /// An intermediate representation of the struct to use for parsing.
        #[derive(Default)]
        #[allow(dead_code)]
        struct IntermediateRep {
            pub kind: Vec<models::RodentKind>,
        }

        let mut intermediate_rep = IntermediateRep::default();

        // Parse into intermediate representation
        let mut string_iter = s.split(',');
        let mut key_result = string_iter.next();

        while key_result.is_some() {
            let val = match string_iter.next() {
                Some(x) => x,
                None => {
                    return std::result::Result::Err(
                        "Missing value while parsing Mouse".to_string(),
                    )
                }
            };

            if let Some(key) = key_result {
                #[allow(clippy::match_single_binding)]
                match key {
                    #[allow(clippy::redundant_clone)]
                    "kind" => intermediate_rep.kind.push(
                        <models::RodentKind as std::str::FromStr>::from_str(val)
                            .map_err(|x| x.to_string())?,
                    ),
                    _ => {
                        return std::result::Result::Err(
                            "Unexpected key while parsing Mouse".to_string(),
                        )
                    }
                }
            }

            // Get the next key
            key_result = string_iter.next();
        }

        // Use the intermediate representation to return the struct
        std::result::Result::Ok(Mouse {
            kind: intermediate_rep.kind.into_iter().next(),
        })
    }
}

// Methods for converting between header::IntoHeaderValue<Mouse> and HeaderValue

#[cfg(feature = "server")]
impl std::convert::TryFrom<header::IntoHeaderValue<Mouse>> for HeaderValue {
    type Error = String;

    fn try_from(
        hdr_value: header::IntoHeaderValue<Mouse>,
    ) -> std::result::Result<Self, Self::Error> {
        let hdr_value = hdr_value.to_string();
        match HeaderValue::from_str(&hdr_value) {
            std::result::Result::Ok(value) => std::result::Result::Ok(value),
            std::result::Result::Err(e) => std::result::Result::Err(format!(
                r#"Invalid header value for Mouse - value: {hdr_value} is invalid {e}"#
            )),
        }
    }
}

#[cfg(feature = "server")]
impl std::convert::TryFrom<HeaderValue> for header::IntoHeaderValue<Mouse> {
    type Error = String;

    fn try_from(hdr_value: HeaderValue) -> std::result::Result<Self, Self::Error> {
        match hdr_value.to_str() {
            std::result::Result::Ok(value) => match <Mouse as std::str::FromStr>::from_str(value) {
                std::result::Result::Ok(value) => {
                    std::result::Result::Ok(header::IntoHeaderValue(value))
                }
                std::result::Result::Err(err) => std::result::Result::Err(format!(
                    r#"Unable to convert header value '{value}' into Mouse - {err}"#
                )),
            },
            std::result::Result::Err(e) => std::result::Result::Err(format!(
                r#"Unable to convert header: {hdr_value:?} to string: {e}"#
            )),
        }
    }
}

#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize, validator::Validate)]
#[cfg_attr(feature = "conversion", derive(frunk::LabelledGeneric))]
pub struct Person {
    #[serde(rename = "type")]
    #[validate(custom(function = "check_xss_string"))]
    pub r_type: String,

    #[serde(rename = "name")]
    #[validate(custom(function = "check_xss_string"))]
    pub name: String,

    #[serde(rename = "objectType")]
    pub object_type: String,
}

impl Person {
    #[allow(clippy::new_without_default, clippy::too_many_arguments)]
    pub fn new(r_type: String, name: String, object_type: String) -> Person {
        Person {
            r_type,
            name,
            object_type,
        }
    }
}

/// Converts the Person value to the Query Parameters representation (style=form, explode=false)
/// specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde serializer
impl std::fmt::Display for Person {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let params: Vec<Option<String>> = vec![
            Some("type".to_string()),
            Some(self.r_type.to_string()),
            Some("name".to_string()),
            Some(self.name.to_string()),
            Some("objectType".to_string()),
            Some(self.object_type.to_string()),
        ];

        write!(
            f,
            "{}",
            params.into_iter().flatten().collect::<Vec<_>>().join(",")
        )
    }
}

/// Converts Query Parameters representation (style=form, explode=false) to a Person value
/// as specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde deserializer
impl std::str::FromStr for Person {
    type Err = String;

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        /// An intermediate representation of the struct to use for parsing.
        #[derive(Default)]
        #[allow(dead_code)]
        struct IntermediateRep {
            pub r_type: Vec<String>,
            pub name: Vec<String>,
            pub object_type: Vec<String>,
        }

        let mut intermediate_rep = IntermediateRep::default();

        // Parse into intermediate representation
        let mut string_iter = s.split(',');
        let mut key_result = string_iter.next();

        while key_result.is_some() {
            let val = match string_iter.next() {
                Some(x) => x,
                None => {
                    return std::result::Result::Err(
                        "Missing value while parsing Person".to_string(),
                    )
                }
            };

            if let Some(key) = key_result {
                #[allow(clippy::match_single_binding)]
                match key {
                    #[allow(clippy::redundant_clone)]
                    "type" => intermediate_rep.r_type.push(
                        <String as std::str::FromStr>::from_str(val).map_err(|x| x.to_string())?,
                    ),
                    #[allow(clippy::redundant_clone)]
                    "name" => intermediate_rep.name.push(
                        <String as std::str::FromStr>::from_str(val).map_err(|x| x.to_string())?,
                    ),
                    #[allow(clippy::redundant_clone)]
                    "objectType" => intermediate_rep.object_type.push(
                        <String as std::str::FromStr>::from_str(val).map_err(|x| x.to_string())?,
                    ),
                    _ => {
                        return std::result::Result::Err(
                            "Unexpected key while parsing Person".to_string(),
                        )
                    }
                }
            }

            // Get the next key
            key_result = string_iter.next();
        }

        // Use the intermediate representation to return the struct
        std::result::Result::Ok(Person {
            r_type: intermediate_rep
                .r_type
                .into_iter()
                .next()
                .ok_or_else(|| "type missing in Person".to_string())?,
            name: intermediate_rep
                .name
                .into_iter()
                .next()
                .ok_or_else(|| "name missing in Person".to_string())?,
            object_type: intermediate_rep
                .object_type
                .into_iter()
                .next()
                .ok_or_else(|| "objectType missing in Person".to_string())?,
        })
    }
}

// Methods for converting between header::IntoHeaderValue<Person> and HeaderValue

#[cfg(feature = "server")]
impl std::convert::TryFrom<header::IntoHeaderValue<Person>> for HeaderValue {
    type Error = String;

    fn try_from(
        hdr_value: header::IntoHeaderValue<Person>,
    ) -> std::result::Result<Self, Self::Error> {
        let hdr_value = hdr_value.to_string();
        match HeaderValue::from_str(&hdr_value) {
            std::result::Result::Ok(value) => std::result::Result::Ok(value),
            std::result::Result::Err(e) => std::result::Result::Err(format!(
                r#"Invalid header value for Person - value: {hdr_value} is invalid {e}"#
            )),
        }
    }
}

#[cfg(feature = "server")]
impl std::convert::TryFrom<HeaderValue> for header::IntoHeaderValue<Person> {
    type Error = String;

    fn try_from(hdr_value: HeaderValue) -> std::result::Result<Self, Self::Error> {
        match hdr_value.to_str() {
            std::result::Result::Ok(value) => {
                match <Person as std::str::FromStr>::from_str(value) {
                    std::result::Result::Ok(value) => {
                        std::result::Result::Ok(header::IntoHeaderValue(value))
                    }
                    std::result::Result::Err(err) => std::result::Result::Err(format!(
                        r#"Unable to convert header value '{value}' into Person - {err}"#
                    )),
                }
            }
            std::result::Result::Err(e) => std::result::Result::Err(format!(
                r#"Unable to convert header: {hdr_value:?} to string: {e}"#
            )),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
#[allow(non_camel_case_types, clippy::large_enum_variant)]
pub enum PersonOrVehicle {
    Person(models::Person),
    Vehicle(models::Vehicle),
}

impl validator::Validate for PersonOrVehicle {
    fn validate(&self) -> std::result::Result<(), validator::ValidationErrors> {
        match self {
            Self::Person(v) => v.validate(),
            Self::Vehicle(v) => v.validate(),
        }
    }
}

/// Converts Query Parameters representation (style=form, explode=false) to a PersonOrVehicle value
/// as specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde deserializer
impl std::str::FromStr for PersonOrVehicle {
    type Err = serde_json::Error;

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        serde_json::from_str(s)
    }
}

impl serde::Serialize for PersonOrVehicle {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        match self {
            Self::Person(x) => x.serialize(serializer),
            Self::Vehicle(x) => x.serialize(serializer),
        }
    }
}

crate::impl_deserialize_tagged!(PersonOrVehicle, "objectType", {
    "student" | "teacher" => Person,
    "car" => Vehicle,
});

impl From<models::Person> for PersonOrVehicle {
    fn from(value: models::Person) -> Self {
        Self::Person(value)
    }
}
impl From<models::Vehicle> for PersonOrVehicle {
    fn from(value: models::Vehicle) -> Self {
        Self::Vehicle(value)
    }
}

#[derive(Debug, Clone, PartialEq, serde::Deserialize)]
#[serde(tag = "kind")]
#[allow(non_camel_case_types, clippy::large_enum_variant)]
pub enum Pet {
    #[serde(alias = "dog")]
    Dog(models::Dog),
    #[serde(alias = "cat")]
    Cat(models::Cat),
}

impl validator::Validate for Pet {
    fn validate(&self) -> std::result::Result<(), validator::ValidationErrors> {
        match self {
            Self::Dog(v) => v.validate(),
            Self::Cat(v) => v.validate(),
        }
    }
}

/// Converts Query Parameters representation (style=form, explode=false) to a Pet value
/// as specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde deserializer
impl std::str::FromStr for Pet {
    type Err = serde_json::Error;

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        serde_json::from_str(s)
    }
}

impl serde::Serialize for Pet {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        match self {
            Self::Dog(x) => x.serialize(serializer),
            Self::Cat(x) => x.serialize(serializer),
        }
    }
}

impl From<models::Dog> for Pet {
    fn from(value: models::Dog) -> Self {
        Self::Dog(value)
    }
}
impl From<models::Cat> for Pet {
    fn from(value: models::Cat) -> Self {
        Self::Cat(value)
    }
}

/// Enumeration of values.
/// Since this enum's variants do not hold data, we can easily define them as `#[repr(C)]`
/// which helps with FFI.
#[allow(non_camel_case_types, clippy::large_enum_variant)]
#[repr(C)]
#[derive(
    Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, serde::Serialize, serde::Deserialize,
)]
#[cfg_attr(feature = "conversion", derive(frunk_enum_derive::LabelledGenericEnum))]
pub enum PetKind {
    #[serde(rename = "dog")]
    Dog,
    #[serde(rename = "cat")]
    Cat,
}

impl validator::Validate for PetKind {
    fn validate(&self) -> std::result::Result<(), validator::ValidationErrors> {
        std::result::Result::Ok(())
    }
}

impl std::fmt::Display for PetKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match *self {
            PetKind::Dog => write!(f, "dog"),
            PetKind::Cat => write!(f, "cat"),
        }
    }
}

impl std::str::FromStr for PetKind {
    type Err = String;

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        match s {
            "dog" => std::result::Result::Ok(PetKind::Dog),
            "cat" => std::result::Result::Ok(PetKind::Cat),
            _ => std::result::Result::Err(format!(r#"Value not valid: {s}"#)),
        }
    }
}

#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize)]
#[serde(untagged)]
#[allow(non_camel_case_types, clippy::large_enum_variant)]
pub enum Rodent {
    Hamster(models::Hamster),
    Mouse(models::Mouse),
}

impl validator::Validate for Rodent {
    fn validate(&self) -> std::result::Result<(), validator::ValidationErrors> {
        match self {
            Self::Hamster(v) => v.validate(),
            Self::Mouse(v) => v.validate(),
        }
    }
}

/// Converts Query Parameters representation (style=form, explode=false) to a Rodent value
/// as specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde deserializer
impl std::str::FromStr for Rodent {
    type Err = serde_json::Error;

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        serde_json::from_str(s)
    }
}

impl From<models::Hamster> for Rodent {
    fn from(value: models::Hamster) -> Self {
        Self::Hamster(value)
    }
}
impl From<models::Mouse> for Rodent {
    fn from(value: models::Mouse) -> Self {
        Self::Mouse(value)
    }
}

/// Enumeration of values.
/// Since this enum's variants do not hold data, we can easily define them as `#[repr(C)]`
/// which helps with FFI.
#[allow(non_camel_case_types, clippy::large_enum_variant)]
#[repr(C)]
#[derive(
    Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, serde::Serialize, serde::Deserialize,
)]
#[cfg_attr(feature = "conversion", derive(frunk_enum_derive::LabelledGenericEnum))]
pub enum RodentKind {
    #[serde(rename = "hamster")]
    Hamster,
    #[serde(rename = "mouse")]
    Mouse,
}

impl validator::Validate for RodentKind {
    fn validate(&self) -> std::result::Result<(), validator::ValidationErrors> {
        std::result::Result::Ok(())
    }
}

impl std::fmt::Display for RodentKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match *self {
            RodentKind::Hamster => write!(f, "hamster"),
            RodentKind::Mouse => write!(f, "mouse"),
        }
    }
}

impl std::str::FromStr for RodentKind {
    type Err = String;

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        match s {
            "hamster" => std::result::Result::Ok(RodentKind::Hamster),
            "mouse" => std::result::Result::Ok(RodentKind::Mouse),
            _ => std::result::Result::Err(format!(r#"Value not valid: {s}"#)),
        }
    }
}

#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize)]
#[serde(untagged)]
#[allow(non_camel_case_types, clippy::large_enum_variant)]
pub enum Shape {
    Circle(models::Circle),
    Square(models::Square),
}

impl validator::Validate for Shape {
    fn validate(&self) -> std::result::Result<(), validator::ValidationErrors> {
        match self {
            Self::Circle(v) => v.validate(),
            Self::Square(v) => v.validate(),
        }
    }
}

/// Converts Query Parameters representation (style=form, explode=false) to a Shape value
/// as specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde deserializer
impl std::str::FromStr for Shape {
    type Err = serde_json::Error;

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        serde_json::from_str(s)
    }
}

impl From<models::Circle> for Shape {
    fn from(value: models::Circle) -> Self {
        Self::Circle(value)
    }
}
impl From<models::Square> for Shape {
    fn from(value: models::Square) -> Self {
        Self::Square(value)
    }
}

/// Enumeration of values.
/// Since this enum's variants do not hold data, we can easily define them as `#[repr(C)]`
/// which helps with FFI.
#[allow(non_camel_case_types, clippy::large_enum_variant)]
#[repr(C)]
#[derive(
    Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, serde::Serialize, serde::Deserialize,
)]
#[cfg_attr(feature = "conversion", derive(frunk_enum_derive::LabelledGenericEnum))]
pub enum ShapeKind {
    #[serde(rename = "circle")]
    Circle,
}

impl validator::Validate for ShapeKind {
    fn validate(&self) -> std::result::Result<(), validator::ValidationErrors> {
        std::result::Result::Ok(())
    }
}

impl std::fmt::Display for ShapeKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match *self {
            ShapeKind::Circle => write!(f, "circle"),
        }
    }
}

impl std::str::FromStr for ShapeKind {
    type Err = String;

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        match s {
            "circle" => std::result::Result::Ok(ShapeKind::Circle),
            _ => std::result::Result::Err(format!(r#"Value not valid: {s}"#)),
        }
    }
}

#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize, validator::Validate)]
#[cfg_attr(feature = "conversion", derive(frunk::LabelledGeneric))]
pub struct Square {
    #[serde(rename = "kind")]
    #[validate(nested)]
    pub kind: models::ShapeKind,
}

impl Square {
    #[allow(clippy::new_without_default, clippy::too_many_arguments)]
    pub fn new(kind: models::ShapeKind) -> Square {
        Square { kind }
    }
}

/// Converts the Square value to the Query Parameters representation (style=form, explode=false)
/// specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde serializer
impl std::fmt::Display for Square {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let params: Vec<Option<String>> = vec![
            // Skipping kind in query parameter serialization

        ];

        write!(
            f,
            "{}",
            params.into_iter().flatten().collect::<Vec<_>>().join(",")
        )
    }
}

/// Converts Query Parameters representation (style=form, explode=false) to a Square value
/// as specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde deserializer
impl std::str::FromStr for Square {
    type Err = String;

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        /// An intermediate representation of the struct to use for parsing.
        #[derive(Default)]
        #[allow(dead_code)]
        struct IntermediateRep {
            pub kind: Vec<models::ShapeKind>,
        }

        let mut intermediate_rep = IntermediateRep::default();

        // Parse into intermediate representation
        let mut string_iter = s.split(',');
        let mut key_result = string_iter.next();

        while key_result.is_some() {
            let val = match string_iter.next() {
                Some(x) => x,
                None => {
                    return std::result::Result::Err(
                        "Missing value while parsing Square".to_string(),
                    )
                }
            };

            if let Some(key) = key_result {
                #[allow(clippy::match_single_binding)]
                match key {
                    #[allow(clippy::redundant_clone)]
                    "kind" => intermediate_rep.kind.push(
                        <models::ShapeKind as std::str::FromStr>::from_str(val)
                            .map_err(|x| x.to_string())?,
                    ),
                    _ => {
                        return std::result::Result::Err(
                            "Unexpected key while parsing Square".to_string(),
                        )
                    }
                }
            }

            // Get the next key
            key_result = string_iter.next();
        }

        // Use the intermediate representation to return the struct
        std::result::Result::Ok(Square {
            kind: intermediate_rep
                .kind
                .into_iter()
                .next()
                .ok_or_else(|| "kind missing in Square".to_string())?,
        })
    }
}

// Methods for converting between header::IntoHeaderValue<Square> and HeaderValue

#[cfg(feature = "server")]
impl std::convert::TryFrom<header::IntoHeaderValue<Square>> for HeaderValue {
    type Error = String;

    fn try_from(
        hdr_value: header::IntoHeaderValue<Square>,
    ) -> std::result::Result<Self, Self::Error> {
        let hdr_value = hdr_value.to_string();
        match HeaderValue::from_str(&hdr_value) {
            std::result::Result::Ok(value) => std::result::Result::Ok(value),
            std::result::Result::Err(e) => std::result::Result::Err(format!(
                r#"Invalid header value for Square - value: {hdr_value} is invalid {e}"#
            )),
        }
    }
}

#[cfg(feature = "server")]
impl std::convert::TryFrom<HeaderValue> for header::IntoHeaderValue<Square> {
    type Error = String;

    fn try_from(hdr_value: HeaderValue) -> std::result::Result<Self, Self::Error> {
        match hdr_value.to_str() {
            std::result::Result::Ok(value) => {
                match <Square as std::str::FromStr>::from_str(value) {
                    std::result::Result::Ok(value) => {
                        std::result::Result::Ok(header::IntoHeaderValue(value))
                    }
                    std::result::Result::Err(err) => std::result::Result::Err(format!(
                        r#"Unable to convert header value '{value}' into Square - {err}"#
                    )),
                }
            }
            std::result::Result::Err(e) => std::result::Result::Err(format!(
                r#"Unable to convert header: {hdr_value:?} to string: {e}"#
            )),
        }
    }
}

#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize, validator::Validate)]
#[cfg_attr(feature = "conversion", derive(frunk::LabelledGeneric))]
pub struct Vehicle {
    #[serde(rename = "type")]
    #[validate(custom(function = "check_xss_string"))]
    pub r_type: String,

    #[serde(rename = "speed")]
    pub speed: f64,

    #[serde(default = "Vehicle::_name_for_object_type")]
    #[serde(serialize_with = "Vehicle::_serialize_object_type")]
    #[serde(rename = "objectType")]
    pub object_type: String,
}

impl Vehicle {
    fn _name_for_object_type() -> String {
        String::from("car")
    }

    fn _serialize_object_type<S>(_: &String, s: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        s.serialize_str(&Self::_name_for_object_type())
    }
}

impl Vehicle {
    #[allow(clippy::new_without_default, clippy::too_many_arguments)]
    pub fn new(r_type: String, speed: f64) -> Vehicle {
        Vehicle {
            r_type,
            speed,
            object_type: Self::_name_for_object_type(),
        }
    }
}

/// Converts the Vehicle value to the Query Parameters representation (style=form, explode=false)
/// specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde serializer
impl std::fmt::Display for Vehicle {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let params: Vec<Option<String>> = vec![
            Some("type".to_string()),
            Some(self.r_type.to_string()),
            Some("speed".to_string()),
            Some(self.speed.to_string()),
            Some("objectType".to_string()),
            Some(self.object_type.to_string()),
        ];

        write!(
            f,
            "{}",
            params.into_iter().flatten().collect::<Vec<_>>().join(",")
        )
    }
}

/// Converts Query Parameters representation (style=form, explode=false) to a Vehicle value
/// as specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde deserializer
impl std::str::FromStr for Vehicle {
    type Err = String;

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        /// An intermediate representation of the struct to use for parsing.
        #[derive(Default)]
        #[allow(dead_code)]
        struct IntermediateRep {
            pub r_type: Vec<String>,
            pub speed: Vec<f64>,
            pub object_type: Vec<String>,
        }

        let mut intermediate_rep = IntermediateRep::default();

        // Parse into intermediate representation
        let mut string_iter = s.split(',');
        let mut key_result = string_iter.next();

        while key_result.is_some() {
            let val = match string_iter.next() {
                Some(x) => x,
                None => {
                    return std::result::Result::Err(
                        "Missing value while parsing Vehicle".to_string(),
                    )
                }
            };

            if let Some(key) = key_result {
                #[allow(clippy::match_single_binding)]
                match key {
                    #[allow(clippy::redundant_clone)]
                    "type" => intermediate_rep.r_type.push(
                        <String as std::str::FromStr>::from_str(val).map_err(|x| x.to_string())?,
                    ),
                    #[allow(clippy::redundant_clone)]
                    "speed" => intermediate_rep.speed.push(
                        <f64 as std::str::FromStr>::from_str(val).map_err(|x| x.to_string())?,
                    ),
                    #[allow(clippy::redundant_clone)]
                    "objectType" => intermediate_rep.object_type.push(
                        <String as std::str::FromStr>::from_str(val).map_err(|x| x.to_string())?,
                    ),
                    _ => {
                        return std::result::Result::Err(
                            "Unexpected key while parsing Vehicle".to_string(),
                        )
                    }
                }
            }

            // Get the next key
            key_result = string_iter.next();
        }

        // Use the intermediate representation to return the struct
        std::result::Result::Ok(Vehicle {
            r_type: intermediate_rep
                .r_type
                .into_iter()
                .next()
                .ok_or_else(|| "type missing in Vehicle".to_string())?,
            speed: intermediate_rep
                .speed
                .into_iter()
                .next()
                .ok_or_else(|| "speed missing in Vehicle".to_string())?,
            object_type: intermediate_rep
                .object_type
                .into_iter()
                .next()
                .ok_or_else(|| "objectType missing in Vehicle".to_string())?,
        })
    }
}

// Methods for converting between header::IntoHeaderValue<Vehicle> and HeaderValue

#[cfg(feature = "server")]
impl std::convert::TryFrom<header::IntoHeaderValue<Vehicle>> for HeaderValue {
    type Error = String;

    fn try_from(
        hdr_value: header::IntoHeaderValue<Vehicle>,
    ) -> std::result::Result<Self, Self::Error> {
        let hdr_value = hdr_value.to_string();
        match HeaderValue::from_str(&hdr_value) {
            std::result::Result::Ok(value) => std::result::Result::Ok(value),
            std::result::Result::Err(e) => std::result::Result::Err(format!(
                r#"Invalid header value for Vehicle - value: {hdr_value} is invalid {e}"#
            )),
        }
    }
}

#[cfg(feature = "server")]
impl std::convert::TryFrom<HeaderValue> for header::IntoHeaderValue<Vehicle> {
    type Error = String;

    fn try_from(hdr_value: HeaderValue) -> std::result::Result<Self, Self::Error> {
        match hdr_value.to_str() {
            std::result::Result::Ok(value) => {
                match <Vehicle as std::str::FromStr>::from_str(value) {
                    std::result::Result::Ok(value) => {
                        std::result::Result::Ok(header::IntoHeaderValue(value))
                    }
                    std::result::Result::Err(err) => std::result::Result::Err(format!(
                        r#"Unable to convert header value '{value}' into Vehicle - {err}"#
                    )),
                }
            }
            std::result::Result::Err(e) => std::result::Result::Err(format!(
                r#"Unable to convert header: {hdr_value:?} to string: {e}"#
            )),
        }
    }
}
