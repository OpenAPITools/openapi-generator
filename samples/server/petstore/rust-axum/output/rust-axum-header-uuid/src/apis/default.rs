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
pub enum UsersPostResponse {
    /// Added row to table!
    Status201_AddedRowToTable(String),
}

/// Default
#[async_trait]
#[allow(clippy::ptr_arg)]
pub trait Default<E: std::fmt::Debug + Send + Sync + 'static = ()>: super::ErrorHandler<E> {
    /// UsersPost - POST /users
    async fn users_post(
        &self,
<<<<<<< HEAD
        method: &Method,
        host: &Host,
        cookies: &CookieJar,
        header_params: &models::UsersPostHeaderParams,
=======
        method: Method,
        host: Host,
        cookies: CookieJar,
        header_params: models::UsersPostHeaderParams,
>>>>>>> 9841fa4dc2c (Implement a custom error handler for unhandled or generic endpoint errors)
    ) -> Result<UsersPostResponse, E>;
}
