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
pub const API_VERSION: &str = "0.1.9";

        #[derive(Debug, PartialEq, Serialize, Deserialize)]
#[must_use]
#[allow(clippy::large_enum_variant)]
pub enum UsersPostResponse {
    /// Added row to table!
    Status201_AddedRowToTable
    (String)
}


/// API
#[async_trait]
#[allow(clippy::ptr_arg)]
pub trait Api {

                /// UsersPost - POST /users
                async fn users_post(
                &self,
                method: Method,
                host: Host,
                cookies: CookieJar,
                  header_params: models::UsersPostHeaderParams,
                ) -> Result<UsersPostResponse, String>;

}

#[cfg(feature = "server")]
pub mod server;

pub mod models;
pub mod types;

#[cfg(feature = "server")]
pub(crate) mod header;
