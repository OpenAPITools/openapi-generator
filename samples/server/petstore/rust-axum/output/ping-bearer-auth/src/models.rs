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
