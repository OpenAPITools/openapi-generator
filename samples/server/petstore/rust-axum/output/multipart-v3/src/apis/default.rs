use async_trait::async_trait;
use axum::extract::*;
<<<<<<< HEAD
use axum_extra::extract::{CookieJar, Host};
=======
use axum_extra::extract::{CookieJar, Host, Multipart};
>>>>>>> fb7dae12a7d (Update axum to 0.8)
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
<<<<<<< HEAD
<<<<<<< HEAD
=======
>>>>>>> 3d833fd5ff9 (Make API methods take references instead of ownership)
        method: &Method,
        host: &Host,
        cookies: &CookieJar,
        body: &axum::body::Body,
<<<<<<< HEAD
=======
        method: Method,
        host: Host,
        cookies: CookieJar,
        body: axum::body::Body,
>>>>>>> 9841fa4dc2c (Implement a custom error handler for unhandled or generic endpoint errors)
=======
>>>>>>> 3d833fd5ff9 (Make API methods take references instead of ownership)
    ) -> Result<MultipartRelatedRequestPostResponse, E>;

    /// MultipartRequestPost - POST /multipart_request
    async fn multipart_request_post(
        &self,
<<<<<<< HEAD
<<<<<<< HEAD
=======
>>>>>>> 3d833fd5ff9 (Make API methods take references instead of ownership)
        method: &Method,
        host: &Host,
        cookies: &CookieJar,
        body: &Multipart,
<<<<<<< HEAD
=======
        method: Method,
        host: Host,
        cookies: CookieJar,
        body: Multipart,
>>>>>>> 9841fa4dc2c (Implement a custom error handler for unhandled or generic endpoint errors)
=======
>>>>>>> 3d833fd5ff9 (Make API methods take references instead of ownership)
    ) -> Result<MultipartRequestPostResponse, E>;

    /// MultipleIdenticalMimeTypesPost - POST /multiple-identical-mime-types
    async fn multiple_identical_mime_types_post(
        &self,
<<<<<<< HEAD
<<<<<<< HEAD
=======
>>>>>>> 3d833fd5ff9 (Make API methods take references instead of ownership)
        method: &Method,
        host: &Host,
        cookies: &CookieJar,
        body: &axum::body::Body,
<<<<<<< HEAD
=======
        method: Method,
        host: Host,
        cookies: CookieJar,
        body: axum::body::Body,
>>>>>>> 9841fa4dc2c (Implement a custom error handler for unhandled or generic endpoint errors)
=======
>>>>>>> 3d833fd5ff9 (Make API methods take references instead of ownership)
    ) -> Result<MultipleIdenticalMimeTypesPostResponse, E>;
}
