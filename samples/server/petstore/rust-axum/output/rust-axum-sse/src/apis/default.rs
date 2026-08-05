use async_trait::async_trait;
use axum::extract::*;
use axum_extra::extract::CookieJar;
use bytes::Bytes;
use headers::Host;
use http::Method;
use serde::{Deserialize, Serialize};

use crate::{models, types::*};

#[must_use]
#[allow(clippy::large_enum_variant)]
pub enum LiveUpdatesGetResponse {
    /// Response
    Status200_Response(models::SSE),
    /// Other Status
    Status201_OtherStatus(models::LiveUpdatesGet201Response),
}

/// Default
#[async_trait]
#[allow(clippy::ptr_arg)]
pub trait Default<E: std::fmt::Debug + Send + Sync + 'static = ()>: super::ErrorHandler<E> {
    /// LiveUpdatesGet - GET /live-updates
    async fn live_updates_get(
        &self,

        method: &Method,
        host: &Host,
        cookies: &CookieJar,
    ) -> Result<LiveUpdatesGetResponse, E>;
}
