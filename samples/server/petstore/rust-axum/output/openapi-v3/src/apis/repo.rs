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
pub enum CreateRepoResponse {
    /// Success
    Status200_Success,
}

/// Repo
#[async_trait]
#[allow(clippy::ptr_arg)]
pub trait Repo<E: std::fmt::Debug + Send + Sync + 'static = ()>: super::ErrorHandler<E> {
    /// CreateRepo - POST /repos
    async fn create_repo(
        &self,

        method: &Method,
        host: &Host,
        cookies: &CookieJar,
        body: &models::ObjectParam,
    ) -> Result<CreateRepoResponse, E>;
}
