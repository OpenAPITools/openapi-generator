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
pub enum GetRepoInfoResponse {
    /// OK
    Status200_OK(String),
}

/// InfoRepo
#[async_trait]
#[allow(clippy::ptr_arg)]
pub trait InfoRepo<E: std::fmt::Debug + Send + Sync + 'static = ()>:
    super::ErrorHandler<E>
{
    /// GetRepoInfo - GET /repos/{repoId}
    async fn get_repo_info(
        &self,
        method: &Method,
        host: &Host,
        cookies: &CookieJar,
        path_params: &models::GetRepoInfoPathParams,
    ) -> Result<GetRepoInfoResponse, E>;
}
