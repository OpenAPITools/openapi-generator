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
pub enum PingGetResponse {
    /// OK
    Status201_OK,
}

/// Default
#[async_trait]
#[allow(clippy::ptr_arg)]
pub trait Default<E: std::fmt::Debug + Send + Sync + 'static = ()>: super::ErrorHandler<E> {
    type Claims;

    /// PingGet - GET /ping
    async fn ping_get(
        &self,
<<<<<<< HEAD
<<<<<<< HEAD
<<<<<<< HEAD
<<<<<<< HEAD
=======
>>>>>>> a297ccec6f8 (Rebase error handler)
        method: &Method,
        host: &Host,
        cookies: &CookieJar,
=======
        method: Method,
        host: Host,
        cookies: CookieJar,
<<<<<<< HEAD
>>>>>>> 9841fa4dc2c (Implement a custom error handler for unhandled or generic endpoint errors)
=======
        method: &Method,
        host: &Host,
        cookies: &CookieJar,
>>>>>>> 3d833fd5ff9 (Make API methods take references instead of ownership)
=======
        claims: Self::Claims,
>>>>>>> ba70bfea1e1 (Implement basic and bearer auth handling)
>>>>>>> a297ccec6f8 (Rebase error handler)
=======
        method: &Method,
        host: &Host,
        cookies: &CookieJar,
        claims: &Self::Claims,
>>>>>>> 73be82180e8 (Rebase rust-axum-error-handling)
    ) -> Result<PingGetResponse, E>;
}
