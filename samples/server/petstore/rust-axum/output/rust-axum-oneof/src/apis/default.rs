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
pub enum FooResponse {
    /// Re-serialize and echo the request data
    Status200_Re(models::Message),
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
#[must_use]
#[allow(clippy::large_enum_variant)]
pub enum I211431Response {
    /// Re-serialize and echo the request data
    Status200_Re(models::Message),
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
#[must_use]
#[allow(clippy::large_enum_variant)]
pub enum I211432Response {
    /// Re-serialize and echo the request data
    Status200_Re(models::Message),
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
#[must_use]
#[allow(clippy::large_enum_variant)]
pub enum I211433Response {
    /// Re-serialize and echo the request data
    Status200_Re(models::Message),
}

/// Default
#[async_trait]
#[allow(clippy::ptr_arg)]
pub trait Default<E: std::fmt::Debug + Send + Sync + 'static = ()>: super::ErrorHandler<E> {
    /// Foo - POST /
    async fn foo(
        &self,

        method: &Method,
        host: &Host,
        cookies: &CookieJar,
        body: &models::Message,
    ) -> Result<FooResponse, E>;

    /// I211431 - POST /issue21143_1
    async fn i211431(
        &self,

        method: &Method,
        host: &Host,
        cookies: &CookieJar,
        body: &Vec<i32>,
    ) -> Result<I211431Response, E>;

    /// I211432 - POST /issue21143_2
    async fn i211432(
        &self,

        method: &Method,
        host: &Host,
        cookies: &CookieJar,
        body: &String,
    ) -> Result<I211432Response, E>;

    /// I211433 - POST /issue21143_3
    async fn i211433(
        &self,

        method: &Method,
        host: &Host,
        cookies: &CookieJar,
        body: &i32,
    ) -> Result<I211433Response, E>;
}
