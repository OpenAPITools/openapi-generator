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

        method: &Method,
        host: &Host,
        cookies: &CookieJar,
        body: &models::Client,
    ) -> Result<TestSpecialTagsResponse, E>;
}
