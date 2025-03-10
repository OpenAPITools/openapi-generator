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
pub enum MultipartRelatedRequestPostResponse {
    /// OK
    Status201_OK,
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
#[must_use]
#[allow(clippy::large_enum_variant)]
pub enum MultipartRequestPostResponse {
    /// OK
    Status201_OK,
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
#[must_use]
#[allow(clippy::large_enum_variant)]
pub enum MultipleIdenticalMimeTypesPostResponse {
    /// OK
    Status200_OK,
}

/// Default
#[async_trait]
#[allow(clippy::ptr_arg)]
pub trait Default<E: std::fmt::Debug + Send + Sync + 'static = ()>: super::ErrorHandler<E> {
    /// MultipartRelatedRequestPost - POST /multipart_related_request
    async fn multipart_related_request_post(
        &self,
        method: &Method,
        host: &Host,
        cookies: &CookieJar,
        body: &axum::body::Body,
    ) -> Result<MultipartRelatedRequestPostResponse, E>;

    /// MultipartRequestPost - POST /multipart_request
    async fn multipart_request_post(
        &self,
        method: &Method,
        host: &Host,
        cookies: &CookieJar,
        body: &Multipart,
    ) -> Result<MultipartRequestPostResponse, E>;

    /// MultipleIdenticalMimeTypesPost - POST /multiple-identical-mime-types
    async fn multiple_identical_mime_types_post(
        &self,
        method: &Method,
        host: &Host,
        cookies: &CookieJar,
        body: &axum::body::Body,
    ) -> Result<MultipleIdenticalMimeTypesPostResponse, E>;
}
