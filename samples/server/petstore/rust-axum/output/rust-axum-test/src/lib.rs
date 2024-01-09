#![allow(
    missing_docs,
    trivial_casts,
    unused_variables,
    unused_mut,
    unused_imports,
    unused_extern_crates,
    non_camel_case_types
)]
#![allow(unused_imports, unused_attributes)]
#![allow(clippy::derive_partial_eq_without_eq, clippy::disallowed_names)]

use async_trait::async_trait;
use axum::extract::*;
use axum_extra::extract::{CookieJar, Multipart};
use bytes::Bytes;
use http::Method;
use serde::{Deserialize, Serialize};

use types::*;

pub const BASE_PATH: &str = "";
pub const API_VERSION: &str = "2.3.4";

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub enum AllOfGetResponse {
    /// OK
    OK(models::AllOfObject),
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub enum DummyGetResponse {
    /// Success
    Success,
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub enum DummyPutResponse {
    /// Success
    Success,
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub enum FileResponseGetResponse {
    /// Success
    Success(ByteArray),
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub enum GetStructuredYamlResponse {
    /// OK
    OK(String),
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub enum HtmlPostResponse {
    /// Success
    Success(String),
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub enum PostYamlResponse {
    /// OK
    OK,
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub enum RawJsonGetResponse {
    /// Success
    Success(crate::types::Object),
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub enum SoloObjectPostResponse {
    /// OK
    OK,
}

/// API
#[async_trait]
#[allow(clippy::ptr_arg)]
pub trait Api {
    /// AllOfGet - GET /allOf
    async fn all_of_get(
        &self,
        method: Method,
        host: Host,
        cookies: CookieJar,
    ) -> Result<AllOfGetResponse, String>;

    /// A dummy endpoint to make the spec valid..
    ///
    /// DummyGet - GET /dummy
    async fn dummy_get(
        &self,
        method: Method,
        host: Host,
        cookies: CookieJar,
    ) -> Result<DummyGetResponse, String>;

    /// DummyPut - PUT /dummy
    async fn dummy_put(
        &self,
        method: Method,
        host: Host,
        cookies: CookieJar,
        body: models::DummyPutRequest,
    ) -> Result<DummyPutResponse, String>;

    /// Get a file.
    ///
    /// FileResponseGet - GET /file_response
    async fn file_response_get(
        &self,
        method: Method,
        host: Host,
        cookies: CookieJar,
    ) -> Result<FileResponseGetResponse, String>;

    /// GetStructuredYaml - GET /get-structured-yaml
    async fn get_structured_yaml(
        &self,
        method: Method,
        host: Host,
        cookies: CookieJar,
    ) -> Result<GetStructuredYamlResponse, String>;

    /// Test HTML handling.
    ///
    /// HtmlPost - POST /html
    async fn html_post(
        &self,
        method: Method,
        host: Host,
        cookies: CookieJar,
        body: String,
    ) -> Result<HtmlPostResponse, String>;

    /// PostYaml - POST /post-yaml
    async fn post_yaml(
        &self,
        method: Method,
        host: Host,
        cookies: CookieJar,
        body: String,
    ) -> Result<PostYamlResponse, String>;

    /// Get an arbitrary JSON blob..
    ///
    /// RawJsonGet - GET /raw_json
    async fn raw_json_get(
        &self,
        method: Method,
        host: Host,
        cookies: CookieJar,
    ) -> Result<RawJsonGetResponse, String>;

    /// Send an arbitrary JSON blob.
    ///
    /// SoloObjectPost - POST /solo-object
    async fn solo_object_post(
        &self,
        method: Method,
        host: Host,
        cookies: CookieJar,
        body: crate::types::Object,
    ) -> Result<SoloObjectPostResponse, String>;
}

#[cfg(feature = "server")]
pub mod server;

pub mod models;
pub mod types;

#[cfg(feature = "server")]
pub(crate) mod header;
