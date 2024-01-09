#![allow(
    missing_docs,
    trivial_casts,
    unused_variables,
    unused_mut,
    unused_imports,
    unused_extern_crates,
    non_camel_case_types
)]
#![allow(unused_imports, unused_attributes)]
#![allow(clippy::derive_partial_eq_without_eq, clippy::disallowed_names)]

use async_trait::async_trait;
use axum::extract::*;
use axum_extra::extract::{CookieJar, Multipart};
use bytes::Bytes;
use http::Method;
use serde::{Deserialize, Serialize};

use types::*;

pub const BASE_PATH: &str = "";
pub const API_VERSION: &str = "1.0.7";

#[derive(Debug, PartialEq, Serialize, Deserialize)]
#[must_use]
#[allow(clippy::large_enum_variant)]
pub enum AnyOfGetResponse {
    /// Success
    Success(models::AnyOfObject),
    /// AlternateSuccess
    AlternateSuccess(models::Model12345AnyOfObject),
    /// AnyOfSuccess
    AnyOfSuccess(models::AnyOfGet202Response),
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub enum CallbackWithHeaderPostResponse {
    /// OK
    OK,
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub enum ComplexQueryParamGetResponse {
    /// Success
    Success,
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub enum EnumInPathPathParamGetResponse {
    /// Success
    Success,
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub enum JsonComplexQueryParamGetResponse {
    /// Success
    Success,
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub enum MandatoryRequestHeaderGetResponse {
    /// Success
    Success,
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub enum MergePatchJsonGetResponse {
    /// merge-patch+json-encoded response
    Merge(models::AnotherXmlObject),
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
#[must_use]
#[allow(clippy::large_enum_variant)]
pub enum MultigetGetResponse {
    /// JSON rsp
    JSONRsp(models::AnotherXmlObject),
    /// XML rsp
    XMLRsp(String),
    /// octet rsp
    OctetRsp(ByteArray),
    /// string rsp
    StringRsp(String),
    /// Duplicate Response long text. One.
    DuplicateResponseLongText(models::AnotherXmlObject),
    /// Duplicate Response long text. Two.
    DuplicateResponseLongText_2(models::AnotherXmlObject),
    /// Duplicate Response long text. Three.
    DuplicateResponseLongText_3(models::AnotherXmlObject),
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub enum MultipleAuthSchemeGetResponse {
    /// Check that limiting to multiple required auth schemes works
    CheckThatLimitingToMultipleRequiredAuthSchemesWorks,
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub enum OneOfGetResponse {
    /// Success
    Success(models::OneOfGet200Response),
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub enum OverrideServerGetResponse {
    /// Success.
    Success,
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub enum ParamgetGetResponse {
    /// JSON rsp
    JSONRsp(models::AnotherXmlObject),
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub enum ReadonlyAuthSchemeGetResponse {
    /// Check that limiting to a single required auth scheme works
    CheckThatLimitingToASingleRequiredAuthSchemeWorks,
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub enum RegisterCallbackPostResponse {
    /// OK
    OK,
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub enum RequiredOctetStreamPutResponse {
    /// OK
    OK,
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
#[must_use]
#[allow(clippy::large_enum_variant)]
pub enum ResponsesWithHeadersGetResponse {
    /// Success
    Success {
        body: String,
        success_info: String,
        bool_header: Option<bool>,
        object_header: Option<models::ObjectHeader>,
    },
    /// Precondition Failed
    PreconditionFailed {
        further_info: Option<String>,
        failure_info: Option<String>,
    },
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
#[must_use]
#[allow(clippy::large_enum_variant)]
pub enum Rfc7807GetResponse {
    /// OK
    OK(models::ObjectWithArrayOfObjects),
    /// NotFound
    NotFound(models::ObjectWithArrayOfObjects),
    /// NotAcceptable
    NotAcceptable(String),
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub enum UntypedPropertyGetResponse {
    /// Check that untyped properties works
    CheckThatUntypedPropertiesWorks,
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub enum UuidGetResponse {
    /// Duplicate Response long text. One.
    DuplicateResponseLongText(uuid::Uuid),
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
#[must_use]
#[allow(clippy::large_enum_variant)]
pub enum XmlExtraPostResponse {
    /// OK
    OK,
    /// Bad Request
    BadRequest,
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
#[must_use]
#[allow(clippy::large_enum_variant)]
pub enum XmlOtherPostResponse {
    /// OK
    OK(String),
    /// Bad Request
    BadRequest,
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
#[must_use]
#[allow(clippy::large_enum_variant)]
pub enum XmlOtherPutResponse {
    /// OK
    OK,
    /// Bad Request
    BadRequest,
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
#[must_use]
#[allow(clippy::large_enum_variant)]
pub enum XmlPostResponse {
    /// OK
    OK,
    /// Bad Request
    BadRequest,
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
#[must_use]
#[allow(clippy::large_enum_variant)]
pub enum XmlPutResponse {
    /// OK
    OK,
    /// Bad Request
    BadRequest,
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub enum CreateRepoResponse {
    /// Success
    Success,
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub enum GetRepoInfoResponse {
    /// OK
    OK(String),
}

/// API
#[async_trait]
#[allow(clippy::ptr_arg)]
pub trait Api {
    /// AnyOfGet - GET /any-of
    async fn any_of_get(
        &self,
        method: Method,
        host: Host,
        cookies: CookieJar,
        query_params: models::AnyOfGetQueryParams,
    ) -> Result<AnyOfGetResponse, String>;

    /// CallbackWithHeaderPost - POST /callback-with-header
    async fn callback_with_header_post(
        &self,
        method: Method,
        host: Host,
        cookies: CookieJar,
        query_params: models::CallbackWithHeaderPostQueryParams,
    ) -> Result<CallbackWithHeaderPostResponse, String>;

    /// ComplexQueryParamGet - GET /complex-query-param
    async fn complex_query_param_get(
        &self,
        method: Method,
        host: Host,
        cookies: CookieJar,
        query_params: models::ComplexQueryParamGetQueryParams,
    ) -> Result<ComplexQueryParamGetResponse, String>;

    /// EnumInPathPathParamGet - GET /enum_in_path/{path_param}
    async fn enum_in_path_path_param_get(
        &self,
        method: Method,
        host: Host,
        cookies: CookieJar,
        path_params: models::EnumInPathPathParamGetPathParams,
    ) -> Result<EnumInPathPathParamGetResponse, String>;

    /// JsonComplexQueryParamGet - GET /json-complex-query-param
    async fn json_complex_query_param_get(
        &self,
        method: Method,
        host: Host,
        cookies: CookieJar,
        query_params: models::JsonComplexQueryParamGetQueryParams,
    ) -> Result<JsonComplexQueryParamGetResponse, String>;

    /// MandatoryRequestHeaderGet - GET /mandatory-request-header
    async fn mandatory_request_header_get(
        &self,
        method: Method,
        host: Host,
        cookies: CookieJar,
        header_params: models::MandatoryRequestHeaderGetHeaderParams,
    ) -> Result<MandatoryRequestHeaderGetResponse, String>;

    /// MergePatchJsonGet - GET /merge-patch-json
    async fn merge_patch_json_get(
        &self,
        method: Method,
        host: Host,
        cookies: CookieJar,
    ) -> Result<MergePatchJsonGetResponse, String>;

    /// Get some stuff..
    ///
    /// MultigetGet - GET /multiget
    async fn multiget_get(
        &self,
        method: Method,
        host: Host,
        cookies: CookieJar,
    ) -> Result<MultigetGetResponse, String>;

    /// MultipleAuthSchemeGet - GET /multiple_auth_scheme
    async fn multiple_auth_scheme_get(
        &self,
        method: Method,
        host: Host,
        cookies: CookieJar,
    ) -> Result<MultipleAuthSchemeGetResponse, String>;

    /// OneOfGet - GET /one-of
    async fn one_of_get(
        &self,
        method: Method,
        host: Host,
        cookies: CookieJar,
    ) -> Result<OneOfGetResponse, String>;

    /// OverrideServerGet - GET /override-server
    async fn override_server_get(
        &self,
        method: Method,
        host: Host,
        cookies: CookieJar,
    ) -> Result<OverrideServerGetResponse, String>;

    /// Get some stuff with parameters..
    ///
    /// ParamgetGet - GET /paramget
    async fn paramget_get(
        &self,
        method: Method,
        host: Host,
        cookies: CookieJar,
        query_params: models::ParamgetGetQueryParams,
    ) -> Result<ParamgetGetResponse, String>;

    /// ReadonlyAuthSchemeGet - GET /readonly_auth_scheme
    async fn readonly_auth_scheme_get(
        &self,
        method: Method,
        host: Host,
        cookies: CookieJar,
    ) -> Result<ReadonlyAuthSchemeGetResponse, String>;

    /// RegisterCallbackPost - POST /register-callback
    async fn register_callback_post(
        &self,
        method: Method,
        host: Host,
        cookies: CookieJar,
        query_params: models::RegisterCallbackPostQueryParams,
    ) -> Result<RegisterCallbackPostResponse, String>;

    /// RequiredOctetStreamPut - PUT /required_octet_stream
    async fn required_octet_stream_put(
        &self,
        method: Method,
        host: Host,
        cookies: CookieJar,
        body: Bytes,
    ) -> Result<RequiredOctetStreamPutResponse, String>;

    /// ResponsesWithHeadersGet - GET /responses_with_headers
    async fn responses_with_headers_get(
        &self,
        method: Method,
        host: Host,
        cookies: CookieJar,
    ) -> Result<ResponsesWithHeadersGetResponse, String>;

    /// Rfc7807Get - GET /rfc7807
    async fn rfc7807_get(
        &self,
        method: Method,
        host: Host,
        cookies: CookieJar,
    ) -> Result<Rfc7807GetResponse, String>;

    /// UntypedPropertyGet - GET /untyped_property
    async fn untyped_property_get(
        &self,
        method: Method,
        host: Host,
        cookies: CookieJar,
        body: Option<models::ObjectUntypedProps>,
    ) -> Result<UntypedPropertyGetResponse, String>;

    /// UuidGet - GET /uuid
    async fn uuid_get(
        &self,
        method: Method,
        host: Host,
        cookies: CookieJar,
    ) -> Result<UuidGetResponse, String>;

    /// XmlExtraPost - POST /xml_extra
    async fn xml_extra_post(
        &self,
        method: Method,
        host: Host,
        cookies: CookieJar,
        body: Bytes,
    ) -> Result<XmlExtraPostResponse, String>;

    /// XmlOtherPost - POST /xml_other
    async fn xml_other_post(
        &self,
        method: Method,
        host: Host,
        cookies: CookieJar,
        body: Bytes,
    ) -> Result<XmlOtherPostResponse, String>;

    /// XmlOtherPut - PUT /xml_other
    async fn xml_other_put(
        &self,
        method: Method,
        host: Host,
        cookies: CookieJar,
        body: Bytes,
    ) -> Result<XmlOtherPutResponse, String>;

    /// Post an array.
    ///
    /// XmlPost - POST /xml
    async fn xml_post(
        &self,
        method: Method,
        host: Host,
        cookies: CookieJar,
        body: Bytes,
    ) -> Result<XmlPostResponse, String>;

    /// XmlPut - PUT /xml
    async fn xml_put(
        &self,
        method: Method,
        host: Host,
        cookies: CookieJar,
        body: Bytes,
    ) -> Result<XmlPutResponse, String>;

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

#[cfg(feature = "server")]
pub mod server;

pub mod models;
pub mod types;

#[cfg(feature = "server")]
pub(crate) mod header;
