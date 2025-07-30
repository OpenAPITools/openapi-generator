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
pub enum EndpointGetResponse {
    /// OK.
    Status200_OK,
}

/// Default
#[async_trait]
#[allow(clippy::ptr_arg)]
pub trait Default<E: std::fmt::Debug + Send + Sync + 'static = ()>: super::ErrorHandler<E> {
    /// EndpointGet - GET /endpoint
    async fn endpoint_get(
        &self,

        method: &Method,
        host: &Host,
        cookies: &CookieJar,
        query_params: &models::EndpointGetQueryParams,
    ) -> Result<EndpointGetResponse, E>;
}
