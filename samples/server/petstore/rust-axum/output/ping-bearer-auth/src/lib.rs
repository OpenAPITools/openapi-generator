#![allow(
    missing_docs,
    trivial_casts,
    unused_variables,
    unused_mut,
    unused_extern_crates,
    non_camel_case_types,
    unused_imports,
    unused_attributes
)]
#![allow(clippy::derive_partial_eq_without_eq, clippy::disallowed_names)]

use async_trait::async_trait;
use axum::extract::*;
use axum_extra::extract::{CookieJar, Multipart};
use bytes::Bytes;
use http::Method;
use serde::{Deserialize, Serialize};

use types::*;

pub const BASE_PATH: &str = "";
pub const API_VERSION: &str = "1.0";

#[derive(Debug, PartialEq, Serialize, Deserialize)]
#[must_use]
#[allow(clippy::large_enum_variant)]
pub enum PingGetResponse {
    /// OK
    Status201_OK,
}

/// API
#[async_trait]
#[allow(clippy::ptr_arg)]
pub trait Api {
    /// PingGet - GET /ping
    async fn ping_get(
        &self,
        method: Method,
        host: Host,
        cookies: CookieJar,
    ) -> Result<PingGetResponse, String>;
}

#[cfg(feature = "server")]
pub mod server;

pub mod models;
pub mod types;

#[cfg(feature = "server")]
pub(crate) mod header;
