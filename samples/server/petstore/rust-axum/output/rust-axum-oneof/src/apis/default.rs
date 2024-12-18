use async_trait::async_trait;
use axum::extract::*;
use axum_extra::extract::{CookieJar, Multipart};
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

/// Default
#[async_trait]
#[allow(clippy::ptr_arg)]
pub trait Default {
    /// Foo - POST /
    async fn foo(
        &self,
        method: Method,
        host: Host,
        cookies: CookieJar,
        body: models::Message,
    ) -> Result<FooResponse, ()>;
}
