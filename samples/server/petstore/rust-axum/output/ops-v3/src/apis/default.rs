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
pub enum Op10GetResponse {
    /// OK
    Status200_OK,
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
#[must_use]
#[allow(clippy::large_enum_variant)]
pub enum Op11GetResponse {
    /// OK
    Status200_OK,
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
#[must_use]
#[allow(clippy::large_enum_variant)]
pub enum Op12GetResponse {
    /// OK
    Status200_OK,
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
#[must_use]
#[allow(clippy::large_enum_variant)]
pub enum Op13GetResponse {
    /// OK
    Status200_OK,
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
#[must_use]
#[allow(clippy::large_enum_variant)]
pub enum Op14GetResponse {
    /// OK
    Status200_OK,
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
#[must_use]
#[allow(clippy::large_enum_variant)]
pub enum Op15GetResponse {
    /// OK
    Status200_OK,
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
#[must_use]
#[allow(clippy::large_enum_variant)]
pub enum Op16GetResponse {
    /// OK
    Status200_OK,
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
#[must_use]
#[allow(clippy::large_enum_variant)]
pub enum Op17GetResponse {
    /// OK
    Status200_OK,
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
#[must_use]
#[allow(clippy::large_enum_variant)]
pub enum Op18GetResponse {
    /// OK
    Status200_OK,
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
#[must_use]
#[allow(clippy::large_enum_variant)]
pub enum Op19GetResponse {
    /// OK
    Status200_OK,
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
#[must_use]
#[allow(clippy::large_enum_variant)]
pub enum Op1GetResponse {
    /// OK
    Status200_OK,
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
#[must_use]
#[allow(clippy::large_enum_variant)]
pub enum Op20GetResponse {
    /// OK
    Status200_OK,
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
#[must_use]
#[allow(clippy::large_enum_variant)]
pub enum Op21GetResponse {
    /// OK
    Status200_OK,
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
#[must_use]
#[allow(clippy::large_enum_variant)]
pub enum Op22GetResponse {
    /// OK
    Status200_OK,
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
#[must_use]
#[allow(clippy::large_enum_variant)]
pub enum Op23GetResponse {
    /// OK
    Status200_OK,
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
#[must_use]
#[allow(clippy::large_enum_variant)]
pub enum Op24GetResponse {
    /// OK
    Status200_OK,
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
#[must_use]
#[allow(clippy::large_enum_variant)]
pub enum Op25GetResponse {
    /// OK
    Status200_OK,
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
#[must_use]
#[allow(clippy::large_enum_variant)]
pub enum Op26GetResponse {
    /// OK
    Status200_OK,
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
#[must_use]
#[allow(clippy::large_enum_variant)]
pub enum Op27GetResponse {
    /// OK
    Status200_OK,
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
#[must_use]
#[allow(clippy::large_enum_variant)]
pub enum Op28GetResponse {
    /// OK
    Status200_OK,
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
#[must_use]
#[allow(clippy::large_enum_variant)]
pub enum Op29GetResponse {
    /// OK
    Status200_OK,
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
#[must_use]
#[allow(clippy::large_enum_variant)]
pub enum Op2GetResponse {
    /// OK
    Status200_OK,
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
#[must_use]
#[allow(clippy::large_enum_variant)]
pub enum Op30GetResponse {
    /// OK
    Status200_OK,
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
#[must_use]
#[allow(clippy::large_enum_variant)]
pub enum Op31GetResponse {
    /// OK
    Status200_OK,
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
#[must_use]
#[allow(clippy::large_enum_variant)]
pub enum Op32GetResponse {
    /// OK
    Status200_OK,
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
#[must_use]
#[allow(clippy::large_enum_variant)]
pub enum Op33GetResponse {
    /// OK
    Status200_OK,
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
#[must_use]
#[allow(clippy::large_enum_variant)]
pub enum Op34GetResponse {
    /// OK
    Status200_OK,
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
#[must_use]
#[allow(clippy::large_enum_variant)]
pub enum Op35GetResponse {
    /// OK
    Status200_OK,
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
#[must_use]
#[allow(clippy::large_enum_variant)]
pub enum Op36GetResponse {
    /// OK
    Status200_OK,
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
#[must_use]
#[allow(clippy::large_enum_variant)]
pub enum Op37GetResponse {
    /// OK
    Status200_OK,
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
#[must_use]
#[allow(clippy::large_enum_variant)]
pub enum Op3GetResponse {
    /// OK
    Status200_OK,
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
#[must_use]
#[allow(clippy::large_enum_variant)]
pub enum Op4GetResponse {
    /// OK
    Status200_OK,
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
#[must_use]
#[allow(clippy::large_enum_variant)]
pub enum Op5GetResponse {
    /// OK
    Status200_OK,
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
#[must_use]
#[allow(clippy::large_enum_variant)]
pub enum Op6GetResponse {
    /// OK
    Status200_OK,
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
#[must_use]
#[allow(clippy::large_enum_variant)]
pub enum Op7GetResponse {
    /// OK
    Status200_OK,
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
#[must_use]
#[allow(clippy::large_enum_variant)]
pub enum Op8GetResponse {
    /// OK
    Status200_OK,
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
#[must_use]
#[allow(clippy::large_enum_variant)]
pub enum Op9GetResponse {
    /// OK
    Status200_OK,
}

/// Default
#[async_trait]
#[allow(clippy::ptr_arg)]
pub trait Default {
    /// Op10Get - GET /op10
    async fn op10_get(
        &self,
        method: Method,
        host: Host,
        cookies: CookieJar,
    ) -> Result<Op10GetResponse, String>;

    /// Op11Get - GET /op11
    async fn op11_get(
        &self,
        method: Method,
        host: Host,
        cookies: CookieJar,
    ) -> Result<Op11GetResponse, String>;

    /// Op12Get - GET /op12
    async fn op12_get(
        &self,
        method: Method,
        host: Host,
        cookies: CookieJar,
    ) -> Result<Op12GetResponse, String>;

    /// Op13Get - GET /op13
    async fn op13_get(
        &self,
        method: Method,
        host: Host,
        cookies: CookieJar,
    ) -> Result<Op13GetResponse, String>;

    /// Op14Get - GET /op14
    async fn op14_get(
        &self,
        method: Method,
        host: Host,
        cookies: CookieJar,
    ) -> Result<Op14GetResponse, String>;

    /// Op15Get - GET /op15
    async fn op15_get(
        &self,
        method: Method,
        host: Host,
        cookies: CookieJar,
    ) -> Result<Op15GetResponse, String>;

    /// Op16Get - GET /op16
    async fn op16_get(
        &self,
        method: Method,
        host: Host,
        cookies: CookieJar,
    ) -> Result<Op16GetResponse, String>;

    /// Op17Get - GET /op17
    async fn op17_get(
        &self,
        method: Method,
        host: Host,
        cookies: CookieJar,
    ) -> Result<Op17GetResponse, String>;

    /// Op18Get - GET /op18
    async fn op18_get(
        &self,
        method: Method,
        host: Host,
        cookies: CookieJar,
    ) -> Result<Op18GetResponse, String>;

    /// Op19Get - GET /op19
    async fn op19_get(
        &self,
        method: Method,
        host: Host,
        cookies: CookieJar,
    ) -> Result<Op19GetResponse, String>;

    /// Op1Get - GET /op1
    async fn op1_get(
        &self,
        method: Method,
        host: Host,
        cookies: CookieJar,
    ) -> Result<Op1GetResponse, String>;

    /// Op20Get - GET /op20
    async fn op20_get(
        &self,
        method: Method,
        host: Host,
        cookies: CookieJar,
    ) -> Result<Op20GetResponse, String>;

    /// Op21Get - GET /op21
    async fn op21_get(
        &self,
        method: Method,
        host: Host,
        cookies: CookieJar,
    ) -> Result<Op21GetResponse, String>;

    /// Op22Get - GET /op22
    async fn op22_get(
        &self,
        method: Method,
        host: Host,
        cookies: CookieJar,
    ) -> Result<Op22GetResponse, String>;

    /// Op23Get - GET /op23
    async fn op23_get(
        &self,
        method: Method,
        host: Host,
        cookies: CookieJar,
    ) -> Result<Op23GetResponse, String>;

    /// Op24Get - GET /op24
    async fn op24_get(
        &self,
        method: Method,
        host: Host,
        cookies: CookieJar,
    ) -> Result<Op24GetResponse, String>;

    /// Op25Get - GET /op25
    async fn op25_get(
        &self,
        method: Method,
        host: Host,
        cookies: CookieJar,
    ) -> Result<Op25GetResponse, String>;

    /// Op26Get - GET /op26
    async fn op26_get(
        &self,
        method: Method,
        host: Host,
        cookies: CookieJar,
    ) -> Result<Op26GetResponse, String>;

    /// Op27Get - GET /op27
    async fn op27_get(
        &self,
        method: Method,
        host: Host,
        cookies: CookieJar,
    ) -> Result<Op27GetResponse, String>;

    /// Op28Get - GET /op28
    async fn op28_get(
        &self,
        method: Method,
        host: Host,
        cookies: CookieJar,
    ) -> Result<Op28GetResponse, String>;

    /// Op29Get - GET /op29
    async fn op29_get(
        &self,
        method: Method,
        host: Host,
        cookies: CookieJar,
    ) -> Result<Op29GetResponse, String>;

    /// Op2Get - GET /op2
    async fn op2_get(
        &self,
        method: Method,
        host: Host,
        cookies: CookieJar,
    ) -> Result<Op2GetResponse, String>;

    /// Op30Get - GET /op30
    async fn op30_get(
        &self,
        method: Method,
        host: Host,
        cookies: CookieJar,
    ) -> Result<Op30GetResponse, String>;

    /// Op31Get - GET /op31
    async fn op31_get(
        &self,
        method: Method,
        host: Host,
        cookies: CookieJar,
    ) -> Result<Op31GetResponse, String>;

    /// Op32Get - GET /op32
    async fn op32_get(
        &self,
        method: Method,
        host: Host,
        cookies: CookieJar,
    ) -> Result<Op32GetResponse, String>;

    /// Op33Get - GET /op33
    async fn op33_get(
        &self,
        method: Method,
        host: Host,
        cookies: CookieJar,
    ) -> Result<Op33GetResponse, String>;

    /// Op34Get - GET /op34
    async fn op34_get(
        &self,
        method: Method,
        host: Host,
        cookies: CookieJar,
    ) -> Result<Op34GetResponse, String>;

    /// Op35Get - GET /op35
    async fn op35_get(
        &self,
        method: Method,
        host: Host,
        cookies: CookieJar,
    ) -> Result<Op35GetResponse, String>;

    /// Op36Get - GET /op36
    async fn op36_get(
        &self,
        method: Method,
        host: Host,
        cookies: CookieJar,
    ) -> Result<Op36GetResponse, String>;

    /// Op37Get - GET /op37
    async fn op37_get(
        &self,
        method: Method,
        host: Host,
        cookies: CookieJar,
    ) -> Result<Op37GetResponse, String>;

    /// Op3Get - GET /op3
    async fn op3_get(
        &self,
        method: Method,
        host: Host,
        cookies: CookieJar,
    ) -> Result<Op3GetResponse, String>;

    /// Op4Get - GET /op4
    async fn op4_get(
        &self,
        method: Method,
        host: Host,
        cookies: CookieJar,
    ) -> Result<Op4GetResponse, String>;

    /// Op5Get - GET /op5
    async fn op5_get(
        &self,
        method: Method,
        host: Host,
        cookies: CookieJar,
    ) -> Result<Op5GetResponse, String>;

    /// Op6Get - GET /op6
    async fn op6_get(
        &self,
        method: Method,
        host: Host,
        cookies: CookieJar,
    ) -> Result<Op6GetResponse, String>;

    /// Op7Get - GET /op7
    async fn op7_get(
        &self,
        method: Method,
        host: Host,
        cookies: CookieJar,
    ) -> Result<Op7GetResponse, String>;

    /// Op8Get - GET /op8
    async fn op8_get(
        &self,
        method: Method,
        host: Host,
        cookies: CookieJar,
    ) -> Result<Op8GetResponse, String>;

    /// Op9Get - GET /op9
    async fn op9_get(
        &self,
        method: Method,
        host: Host,
        cookies: CookieJar,
    ) -> Result<Op9GetResponse, String>;
}
