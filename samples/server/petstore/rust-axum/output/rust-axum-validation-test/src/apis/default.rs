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
pub enum MailPutResponse {
    /// OK.
    Status204_OK,
}

/// Default
#[async_trait]
#[allow(clippy::ptr_arg)]
pub trait Default {
    /// MailPut - PUT /mail
    async fn mail_put(
        &self,
        method: Method,
        host: Host,
        cookies: CookieJar,
        body: models::Email,
    ) -> Result<MailPutResponse, String>;
}
