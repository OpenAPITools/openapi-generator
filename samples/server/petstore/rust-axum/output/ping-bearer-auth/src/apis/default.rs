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
pub enum PingGetResponse {
    /// OK
    Status201_OK,
}

/// Default
#[async_trait]
#[allow(clippy::ptr_arg)]
pub trait Default {
    /// PingGet - GET /ping
    async fn ping_get(
        &self,
        method: Method,
        host: Host,
        cookies: CookieJar,
    ) -> Result<PingGetResponse, String>;
}
