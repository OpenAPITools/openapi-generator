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
pub enum CreateRepoResponse {
    /// Success
    Status200_Success,
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
#[must_use]
#[allow(clippy::large_enum_variant)]
pub enum GetRepoInfoResponse {
    /// OK
    Status200_OK(String),
}

/// Repo
#[async_trait]
#[allow(clippy::ptr_arg)]
pub trait Repo {
    /// CreateRepo - POST /repos
    async fn create_repo(
        &self,
        method: Method,
        host: Host,
        cookies: CookieJar,
        body: models::ObjectParam,
    ) -> Result<CreateRepoResponse, String>;
    /// GetRepoInfo - GET /repos/{repoId}
    async fn get_repo_info(
        &self,
        method: Method,
        host: Host,
        cookies: CookieJar,
        path_params: models::GetRepoInfoPathParams,
    ) -> Result<GetRepoInfoResponse, String>;
}
