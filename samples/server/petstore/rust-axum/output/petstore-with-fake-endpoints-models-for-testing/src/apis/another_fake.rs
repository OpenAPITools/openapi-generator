use async_trait::async_trait;
use axum::extract::*;
<<<<<<< HEAD
<<<<<<< HEAD
use axum_extra::extract::{CookieJar, Host};
=======
use axum_extra::extract::{CookieJar, Host, Multipart};
>>>>>>> fb7dae12a7d (Update axum to 0.8)
=======
use axum_extra::extract::{CookieJar, Host};
>>>>>>> 47c0a58c968 (Multipart is also part of the axum update)
use bytes::Bytes;
use http::Method;
use serde::{Deserialize, Serialize};

use crate::{models, types::*};

#[derive(Debug, PartialEq, Serialize, Deserialize)]
#[must_use]
#[allow(clippy::large_enum_variant)]
pub enum TestSpecialTagsResponse {
    /// successful operation
    Status200_SuccessfulOperation(models::Client),
}

/// AnotherFake
#[async_trait]
#[allow(clippy::ptr_arg)]
pub trait AnotherFake<E: std::fmt::Debug + Send + Sync + 'static = ()>:
    super::ErrorHandler<E>
{
    /// To test special tags.
    ///
    /// TestSpecialTags - PATCH /v2/another-fake/dummy
    async fn test_special_tags(
        &self,
<<<<<<< HEAD
<<<<<<< HEAD
=======
>>>>>>> 3d833fd5ff9 (Make API methods take references instead of ownership)
        method: &Method,
        host: &Host,
        cookies: &CookieJar,
        body: &models::Client,
<<<<<<< HEAD
=======
        method: Method,
        host: Host,
        cookies: CookieJar,
        body: models::Client,
>>>>>>> 9841fa4dc2c (Implement a custom error handler for unhandled or generic endpoint errors)
=======
>>>>>>> 3d833fd5ff9 (Make API methods take references instead of ownership)
    ) -> Result<TestSpecialTagsResponse, E>;
}
