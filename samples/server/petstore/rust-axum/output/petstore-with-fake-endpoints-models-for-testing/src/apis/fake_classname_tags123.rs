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
pub enum TestClassnameResponse {
    /// successful operation
    Status200_SuccessfulOperation(models::Client),
}

/// FakeClassnameTags123
#[async_trait]
#[allow(clippy::ptr_arg)]
pub trait FakeClassnameTags123<E: std::fmt::Debug + Send + Sync + 'static = ()>:
    super::ErrorHandler<E>
{
    /// To test class name in snake case.
    ///
    /// TestClassname - PATCH /v2/fake_classname_test
    async fn test_classname(
        &self,

        method: &Method,
        host: &Host,
        cookies: &CookieJar,
        body: &models::Client,
    ) -> Result<TestClassnameResponse, E>;
}
