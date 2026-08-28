use async_trait::async_trait;
use axum::extract::*;
use axum_extra::extract::CookieJar;
use bytes::Bytes;
use headers::Host;
use http::Method;
use serde::{Deserialize, Serialize};

use crate::{models, types::*};

#[derive(Debug, PartialEq, Serialize, Deserialize)]
#[must_use]
#[allow(clippy::large_enum_variant)]
pub enum GetIntegersResponse {
    /// OK
    Status200_OK(models::IntegerTypes),
}

/// Default
#[async_trait]
#[allow(clippy::ptr_arg)]
pub trait Default<E: std::fmt::Debug + Send + Sync + 'static = ()>: super::ErrorHandler<E> {
    /// GetIntegers - GET /integers
    async fn get_integers(
        &self,

        method: &Method,
        host: &Host,
        cookies: &CookieJar,
        query_params: &models::GetIntegersQueryParams,
    ) -> Result<GetIntegersResponse, E>;
}
