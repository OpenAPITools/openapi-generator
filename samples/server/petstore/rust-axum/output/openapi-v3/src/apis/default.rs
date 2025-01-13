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
pub enum AnyOfGetResponse {
    /// Success
    Status200_Success(models::AnyOfObject),
    /// AlternateSuccess
    Status201_AlternateSuccess(models::Model12345AnyOfObject),
    /// AnyOfSuccess
    Status202_AnyOfSuccess(models::AnyOfGet202Response),
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
#[must_use]
#[allow(clippy::large_enum_variant)]
pub enum CallbackWithHeaderPostResponse {
    /// OK
    Status204_OK,
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
#[must_use]
#[allow(clippy::large_enum_variant)]
pub enum ComplexQueryParamGetResponse {
    /// Success
    Status200_Success,
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
#[must_use]
#[allow(clippy::large_enum_variant)]
pub enum EnumInPathPathParamGetResponse {
    /// Success
    Status200_Success,
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
#[must_use]
#[allow(clippy::large_enum_variant)]
pub enum FormTestResponse {
    /// OK
    Status200_OK,
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
#[must_use]
#[allow(clippy::large_enum_variant)]
pub enum GetWithBooleanParameterResponse {
    /// OK
    Status200_OK,
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
#[must_use]
#[allow(clippy::large_enum_variant)]
pub enum JsonComplexQueryParamGetResponse {
    /// Success
    Status200_Success,
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
#[must_use]
#[allow(clippy::large_enum_variant)]
pub enum MandatoryRequestHeaderGetResponse {
    /// Success
    Status200_Success,
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
#[must_use]
#[allow(clippy::large_enum_variant)]
pub enum MergePatchJsonGetResponse {
    /// merge-patch+json-encoded response
    Status200_Merge(models::AnotherXmlObject),
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
#[must_use]
#[allow(clippy::large_enum_variant)]
pub enum MultigetGetResponse {
    /// JSON rsp
    Status200_JSONRsp(models::AnotherXmlObject),
    /// XML rsp
    Status201_XMLRsp(String),
    /// octet rsp
    Status202_OctetRsp(ByteArray),
    /// string rsp
    Status203_StringRsp(String),
    /// Duplicate Response long text. One.
    Status204_DuplicateResponseLongText(models::AnotherXmlObject),
    /// Duplicate Response long text. Two.
    Status205_DuplicateResponseLongText(models::AnotherXmlObject),
    /// Duplicate Response long text. Three.
    Status206_DuplicateResponseLongText(models::AnotherXmlObject),
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
#[must_use]
#[allow(clippy::large_enum_variant)]
pub enum MultipleAuthSchemeGetResponse {
    /// Check that limiting to multiple required auth schemes works
    Status200_CheckThatLimitingToMultipleRequiredAuthSchemesWorks,
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
#[must_use]
#[allow(clippy::large_enum_variant)]
pub enum MultiplePathParamsWithVeryLongPathToTestFormattingPathParamAPathParamBGetResponse {
    /// Success
    Status200_Success,
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
#[must_use]
#[allow(clippy::large_enum_variant)]
pub enum OneOfGetResponse {
    /// Success
    Status200_Success(models::OneOfGet200Response),
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
#[must_use]
#[allow(clippy::large_enum_variant)]
pub enum OverrideServerGetResponse {
    /// Success.
    Status204_Success,
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
#[must_use]
#[allow(clippy::large_enum_variant)]
pub enum ParamgetGetResponse {
    /// JSON rsp
    Status200_JSONRsp(models::AnotherXmlObject),
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
#[must_use]
#[allow(clippy::large_enum_variant)]
pub enum ReadonlyAuthSchemeGetResponse {
    /// Check that limiting to a single required auth scheme works
    Status200_CheckThatLimitingToASingleRequiredAuthSchemeWorks,
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
#[must_use]
#[allow(clippy::large_enum_variant)]
pub enum RegisterCallbackPostResponse {
    /// OK
    Status204_OK,
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
#[must_use]
#[allow(clippy::large_enum_variant)]
pub enum RequiredOctetStreamPutResponse {
    /// OK
    Status200_OK,
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
#[must_use]
#[allow(clippy::large_enum_variant)]
pub enum ResponsesWithHeadersGetResponse {
    /// Success
    Status200_Success {
        body: String,
        success_info: String,
        bool_header: Option<bool>,
        object_header: Option<models::ObjectHeader>,
    },
    /// Precondition Failed
    Status412_PreconditionFailed {
        further_info: Option<String>,
        failure_info: Option<String>,
    },
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
#[must_use]
#[allow(clippy::large_enum_variant)]
pub enum Rfc7807GetResponse {
    /// OK
    Status204_OK(models::ObjectWithArrayOfObjects),
    /// NotFound
    Status404_NotFound(models::ObjectWithArrayOfObjects),
    /// NotAcceptable
    Status406_NotAcceptable(String),
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
#[must_use]
#[allow(clippy::large_enum_variant)]
pub enum TwoFirstLetterHeadersResponse {
    /// OK
    Status200_OK,
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
#[must_use]
#[allow(clippy::large_enum_variant)]
pub enum UntypedPropertyGetResponse {
    /// Check that untyped properties works
    Status200_CheckThatUntypedPropertiesWorks,
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
#[must_use]
#[allow(clippy::large_enum_variant)]
pub enum UuidGetResponse {
    /// Duplicate Response long text. One.
    Status200_DuplicateResponseLongText(uuid::Uuid),
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
#[must_use]
#[allow(clippy::large_enum_variant)]
pub enum XmlExtraPostResponse {
    /// OK
    Status201_OK,
    /// Bad Request
    Status400_BadRequest,
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
#[must_use]
#[allow(clippy::large_enum_variant)]
pub enum XmlOtherPostResponse {
    /// OK
    Status201_OK(String),
    /// Bad Request
    Status400_BadRequest,
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
#[must_use]
#[allow(clippy::large_enum_variant)]
pub enum XmlOtherPutResponse {
    /// OK
    Status201_OK,
    /// Bad Request
    Status400_BadRequest,
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
#[must_use]
#[allow(clippy::large_enum_variant)]
pub enum XmlPostResponse {
    /// OK
    Status201_OK,
    /// Bad Request
    Status400_BadRequest,
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
#[must_use]
#[allow(clippy::large_enum_variant)]
pub enum XmlPutResponse {
    /// OK
    Status201_OK,
    /// Bad Request
    Status400_BadRequest,
}

/// Default
#[async_trait]
#[allow(clippy::ptr_arg)]
pub trait Default {
    /// AnyOfGet - GET /any-of
    async fn any_of_get(
        &self,
        method: Method,
        host: Host,
        cookies: CookieJar,
        query_params: models::AnyOfGetQueryParams,
    ) -> Result<AnyOfGetResponse, ()>;

    /// CallbackWithHeaderPost - POST /callback-with-header
    async fn callback_with_header_post(
        &self,
        method: Method,
        host: Host,
        cookies: CookieJar,
        query_params: models::CallbackWithHeaderPostQueryParams,
    ) -> Result<CallbackWithHeaderPostResponse, ()>;

    /// ComplexQueryParamGet - GET /complex-query-param
    async fn complex_query_param_get(
        &self,
        method: Method,
        host: Host,
        cookies: CookieJar,
        query_params: models::ComplexQueryParamGetQueryParams,
    ) -> Result<ComplexQueryParamGetResponse, ()>;

    /// EnumInPathPathParamGet - GET /enum_in_path/{path_param}
    async fn enum_in_path_path_param_get(
        &self,
        method: Method,
        host: Host,
        cookies: CookieJar,
        path_params: models::EnumInPathPathParamGetPathParams,
    ) -> Result<EnumInPathPathParamGetResponse, ()>;

    /// Test a Form Post.
    ///
    /// FormTest - POST /form-test
    async fn form_test(
        &self,
        method: Method,
        host: Host,
        cookies: CookieJar,
        body: models::FormTestRequest,
    ) -> Result<FormTestResponse, ()>;

    /// GetWithBooleanParameter - GET /get-with-bool
    async fn get_with_boolean_parameter(
        &self,
        method: Method,
        host: Host,
        cookies: CookieJar,
        query_params: models::GetWithBooleanParameterQueryParams,
    ) -> Result<GetWithBooleanParameterResponse, ()>;

    /// JsonComplexQueryParamGet - GET /json-complex-query-param
    async fn json_complex_query_param_get(
        &self,
        method: Method,
        host: Host,
        cookies: CookieJar,
        query_params: models::JsonComplexQueryParamGetQueryParams,
    ) -> Result<JsonComplexQueryParamGetResponse, ()>;

    /// MandatoryRequestHeaderGet - GET /mandatory-request-header
    async fn mandatory_request_header_get(
        &self,
        method: Method,
        host: Host,
        cookies: CookieJar,
        header_params: models::MandatoryRequestHeaderGetHeaderParams,
    ) -> Result<MandatoryRequestHeaderGetResponse, ()>;

    /// MergePatchJsonGet - GET /merge-patch-json
    async fn merge_patch_json_get(
        &self,
        method: Method,
        host: Host,
        cookies: CookieJar,
    ) -> Result<MergePatchJsonGetResponse, ()>;

    /// Get some stuff..
    ///
    /// MultigetGet - GET /multiget
    async fn multiget_get(
        &self,
        method: Method,
        host: Host,
        cookies: CookieJar,
    ) -> Result<MultigetGetResponse, ()>;

    /// MultipleAuthSchemeGet - GET /multiple_auth_scheme
    async fn multiple_auth_scheme_get(
        &self,
        method: Method,
        host: Host,
        cookies: CookieJar,
    ) -> Result<MultipleAuthSchemeGetResponse, ()>;

    /// MultiplePathParamsWithVeryLongPathToTestFormattingPathParamAPathParamBGet - GET /multiple-path-params-with-very-long-path-to-test-formatting/{path_param_a}/{path_param_b}
    async fn multiple_path_params_with_very_long_path_to_test_formatting_path_param_a_path_param_b_get(
        &self,
        method: Method,
        host: Host,
        cookies: CookieJar,
        path_params: models::MultiplePathParamsWithVeryLongPathToTestFormattingPathParamAPathParamBGetPathParams,
    ) -> Result<MultiplePathParamsWithVeryLongPathToTestFormattingPathParamAPathParamBGetResponse, ()>;

    /// OneOfGet - GET /one-of
    async fn one_of_get(
        &self,
        method: Method,
        host: Host,
        cookies: CookieJar,
    ) -> Result<OneOfGetResponse, ()>;

    /// OverrideServerGet - GET /override-server
    async fn override_server_get(
        &self,
        method: Method,
        host: Host,
        cookies: CookieJar,
    ) -> Result<OverrideServerGetResponse, ()>;

    /// Get some stuff with parameters..
    ///
    /// ParamgetGet - GET /paramget
    async fn paramget_get(
        &self,
        method: Method,
        host: Host,
        cookies: CookieJar,
        query_params: models::ParamgetGetQueryParams,
    ) -> Result<ParamgetGetResponse, ()>;

    /// ReadonlyAuthSchemeGet - GET /readonly_auth_scheme
    async fn readonly_auth_scheme_get(
        &self,
        method: Method,
        host: Host,
        cookies: CookieJar,
    ) -> Result<ReadonlyAuthSchemeGetResponse, ()>;

    /// RegisterCallbackPost - POST /register-callback
    async fn register_callback_post(
        &self,
        method: Method,
        host: Host,
        cookies: CookieJar,
        query_params: models::RegisterCallbackPostQueryParams,
    ) -> Result<RegisterCallbackPostResponse, ()>;

    /// RequiredOctetStreamPut - PUT /required_octet_stream
    async fn required_octet_stream_put(
        &self,
        method: Method,
        host: Host,
        cookies: CookieJar,
        body: Bytes,
    ) -> Result<RequiredOctetStreamPutResponse, ()>;

    /// ResponsesWithHeadersGet - GET /responses_with_headers
    async fn responses_with_headers_get(
        &self,
        method: Method,
        host: Host,
        cookies: CookieJar,
    ) -> Result<ResponsesWithHeadersGetResponse, ()>;

    /// Rfc7807Get - GET /rfc7807
    async fn rfc7807_get(
        &self,
        method: Method,
        host: Host,
        cookies: CookieJar,
    ) -> Result<Rfc7807GetResponse, ()>;

    /// TwoFirstLetterHeaders - POST /operation-two-first-letter-headers
    async fn two_first_letter_headers(
        &self,
        method: Method,
        host: Host,
        cookies: CookieJar,
        header_params: models::TwoFirstLetterHeadersHeaderParams,
    ) -> Result<TwoFirstLetterHeadersResponse, ()>;

    /// UntypedPropertyGet - GET /untyped_property
    async fn untyped_property_get(
        &self,
        method: Method,
        host: Host,
        cookies: CookieJar,
        body: Option<models::ObjectUntypedProps>,
    ) -> Result<UntypedPropertyGetResponse, ()>;

    /// UuidGet - GET /uuid
    async fn uuid_get(
        &self,
        method: Method,
        host: Host,
        cookies: CookieJar,
    ) -> Result<UuidGetResponse, ()>;

    /// XmlExtraPost - POST /xml_extra
    async fn xml_extra_post(
        &self,
        method: Method,
        host: Host,
        cookies: CookieJar,
        body: Bytes,
    ) -> Result<XmlExtraPostResponse, ()>;

    /// XmlOtherPost - POST /xml_other
    async fn xml_other_post(
        &self,
        method: Method,
        host: Host,
        cookies: CookieJar,
        body: Bytes,
    ) -> Result<XmlOtherPostResponse, ()>;

    /// XmlOtherPut - PUT /xml_other
    async fn xml_other_put(
        &self,
        method: Method,
        host: Host,
        cookies: CookieJar,
        body: Bytes,
    ) -> Result<XmlOtherPutResponse, ()>;

    /// Post an array.  It's important we test apostrophes, so include one here..
    ///
    /// XmlPost - POST /xml
    async fn xml_post(
        &self,
        method: Method,
        host: Host,
        cookies: CookieJar,
        body: Bytes,
    ) -> Result<XmlPostResponse, ()>;

    /// XmlPut - PUT /xml
    async fn xml_put(
        &self,
        method: Method,
        host: Host,
        cookies: CookieJar,
        body: Bytes,
    ) -> Result<XmlPutResponse, ()>;
}
