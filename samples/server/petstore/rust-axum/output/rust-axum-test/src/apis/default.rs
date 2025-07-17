use async_trait::async_trait;
use axum::extract::*;
use axum_extra::extract::{CookieJar, Host};
use bytes::Bytes;
use http::Method;
use serde::{Deserialize, Serialize};

use crate::{models, types::*};

#[derive(Debug, PartialEq, Serialize, Deserialize)]
#[must_use]
#[allow(clippy::large_enum_variant)]
pub enum AllOfGetResponse {
    /// OK
    Status200_OK(models::FooAllOfObject),
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
#[must_use]
#[allow(clippy::large_enum_variant)]
pub enum DummyGetResponse {
    /// Success
    Status200_Success,
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
#[must_use]
#[allow(clippy::large_enum_variant)]
pub enum DummyPutResponse {
    /// Success
    Status200_Success,
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
#[must_use]
#[allow(clippy::large_enum_variant)]
pub enum FileResponseGetResponse {
    /// Success
    Status200_Success(ByteArray),
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
#[must_use]
#[allow(clippy::large_enum_variant)]
pub enum GetStructuredYamlResponse {
    /// OK
    Status200_OK(String),
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
#[must_use]
#[allow(clippy::large_enum_variant)]
pub enum HtmlPostResponse {
    /// Success
    Status200_Success(String),
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
#[must_use]
#[allow(clippy::large_enum_variant)]
pub enum PostYamlResponse {
    /// OK
    Status204_OK,
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
#[must_use]
#[allow(clippy::large_enum_variant)]
pub enum RawJsonGetResponse {
    /// Success
    Status200_Success(crate::types::Object),
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
#[must_use]
#[allow(clippy::large_enum_variant)]
pub enum SoloObjectPostResponse {
    /// OK
    Status204_OK,
}

/// Default
#[async_trait]
#[allow(clippy::ptr_arg)]
pub trait Default<E: std::fmt::Debug + Send + Sync + 'static = ()>: super::ErrorHandler<E> {
    /// AllOfGet - GET /allOf
    async fn all_of_get(
        &self,

        method: &Method,
        host: &Host,
        cookies: &CookieJar,
    ) -> Result<AllOfGetResponse, E>;

    /// A dummy endpoint to make the spec valid..
    ///
    /// DummyGet - GET /dummy
    async fn dummy_get(
        &self,

        method: &Method,
        host: &Host,
        cookies: &CookieJar,
    ) -> Result<DummyGetResponse, E>;

    /// DummyPut - PUT /dummy
    async fn dummy_put(
        &self,

        method: &Method,
        host: &Host,
        cookies: &CookieJar,
        body: &models::FooDummyPutRequest,
    ) -> Result<DummyPutResponse, E>;

    /// Get a file.
    ///
    /// FileResponseGet - GET /file_response
    async fn file_response_get(
        &self,

        method: &Method,
        host: &Host,
        cookies: &CookieJar,
    ) -> Result<FileResponseGetResponse, E>;

    /// GetStructuredYaml - GET /get-structured-yaml
    async fn get_structured_yaml(
        &self,

        method: &Method,
        host: &Host,
        cookies: &CookieJar,
    ) -> Result<GetStructuredYamlResponse, E>;

    /// Test HTML handling.
    ///
    /// HtmlPost - POST /html
    async fn html_post(
        &self,

        method: &Method,
        host: &Host,
        cookies: &CookieJar,
        body: &String,
    ) -> Result<HtmlPostResponse, E>;

    /// PostYaml - POST /post-yaml
    async fn post_yaml(
        &self,

        method: &Method,
        host: &Host,
        cookies: &CookieJar,
        body: &String,
    ) -> Result<PostYamlResponse, E>;

    /// Get an arbitrary JSON blob..
    ///
    /// RawJsonGet - GET /raw_json
    async fn raw_json_get(
        &self,

        method: &Method,
        host: &Host,
        cookies: &CookieJar,
    ) -> Result<RawJsonGetResponse, E>;

    /// Send an arbitrary JSON blob.
    ///
    /// SoloObjectPost - POST /solo-object
    async fn solo_object_post(
        &self,

        method: &Method,
        host: &Host,
        cookies: &CookieJar,
        body: &crate::types::Object,
    ) -> Result<SoloObjectPostResponse, E>;
}
