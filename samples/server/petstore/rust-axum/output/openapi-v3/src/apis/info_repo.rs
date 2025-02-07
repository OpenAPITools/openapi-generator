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
<<<<<<< HEAD
<<<<<<< HEAD
=======
>>>>>>> 3d833fd5ff9 (Make API methods take references instead of ownership)
        method: &Method,
        host: &Host,
        cookies: &CookieJar,
        path_params: &models::GetRepoInfoPathParams,
<<<<<<< HEAD
=======
        method: Method,
        host: Host,
        cookies: CookieJar,
        path_params: models::GetRepoInfoPathParams,
>>>>>>> 9841fa4dc2c (Implement a custom error handler for unhandled or generic endpoint errors)
=======
>>>>>>> 3d833fd5ff9 (Make API methods take references instead of ownership)
    ) -> Result<GetRepoInfoResponse, E>;
}
