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
pub enum Call123exampleResponse {
    /// success
    Status200_Success,
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
#[must_use]
#[allow(clippy::large_enum_variant)]
pub enum FakeOuterBooleanSerializeResponse {
    /// Output boolean
    Status200_OutputBoolean(bool),
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
#[must_use]
#[allow(clippy::large_enum_variant)]
pub enum FakeOuterCompositeSerializeResponse {
    /// Output composite
    Status200_OutputComposite(models::OuterComposite),
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
#[must_use]
#[allow(clippy::large_enum_variant)]
pub enum FakeOuterNumberSerializeResponse {
    /// Output number
    Status200_OutputNumber(f64),
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
#[must_use]
#[allow(clippy::large_enum_variant)]
pub enum FakeOuterStringSerializeResponse {
    /// Output string
    Status200_OutputString(String),
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
#[must_use]
#[allow(clippy::large_enum_variant)]
pub enum FakeResponseWithNumericalDescriptionResponse {
    /// 1234
    Status200,
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
#[must_use]
#[allow(clippy::large_enum_variant)]
pub enum HyphenParamResponse {
    /// Success
    Status200_Success,
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
#[must_use]
#[allow(clippy::large_enum_variant)]
pub enum TestBodyWithQueryParamsResponse {
    /// Success
    Status200_Success,
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
#[must_use]
#[allow(clippy::large_enum_variant)]
pub enum TestClientModelResponse {
    /// successful operation
    Status200_SuccessfulOperation(models::Client),
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
#[must_use]
#[allow(clippy::large_enum_variant)]
pub enum TestEndpointParametersResponse {
    /// Invalid username supplied
    Status400_InvalidUsernameSupplied,
    /// User not found
    Status404_UserNotFound,
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
#[must_use]
#[allow(clippy::large_enum_variant)]
pub enum TestEnumParametersResponse {
    /// Invalid request
    Status400_InvalidRequest,
    /// Not found
    Status404_NotFound,
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
#[must_use]
#[allow(clippy::large_enum_variant)]
pub enum TestInlineAdditionalPropertiesResponse {
    /// successful operation
    Status200_SuccessfulOperation,
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
#[must_use]
#[allow(clippy::large_enum_variant)]
pub enum TestJsonFormDataResponse {
    /// successful operation
    Status200_SuccessfulOperation,
}

/// Fake
#[async_trait]
#[allow(clippy::ptr_arg)]
pub trait Fake<E: std::fmt::Debug + Send + Sync + 'static = ()>: super::ErrorHandler<E> {
    type Claims;

    /// Call123example - GET /v2/fake/operation-with-numeric-id
    async fn call123example(
        &self,

        method: &Method,
        host: &Host,
        cookies: &CookieJar,
    ) -> Result<Call123exampleResponse, E>;

    /// FakeOuterBooleanSerialize - POST /v2/fake/outer/boolean
    async fn fake_outer_boolean_serialize(
        &self,

        method: &Method,
        host: &Host,
        cookies: &CookieJar,
        body: &Option<models::OuterBoolean>,
    ) -> Result<FakeOuterBooleanSerializeResponse, E>;

    /// FakeOuterCompositeSerialize - POST /v2/fake/outer/composite
    async fn fake_outer_composite_serialize(
        &self,

        method: &Method,
        host: &Host,
        cookies: &CookieJar,
        body: &Option<models::OuterComposite>,
    ) -> Result<FakeOuterCompositeSerializeResponse, E>;

    /// FakeOuterNumberSerialize - POST /v2/fake/outer/number
    async fn fake_outer_number_serialize(
        &self,

        method: &Method,
        host: &Host,
        cookies: &CookieJar,
        body: &Option<models::OuterNumber>,
    ) -> Result<FakeOuterNumberSerializeResponse, E>;

    /// FakeOuterStringSerialize - POST /v2/fake/outer/string
    async fn fake_outer_string_serialize(
        &self,

        method: &Method,
        host: &Host,
        cookies: &CookieJar,
        body: &Option<models::OuterString>,
    ) -> Result<FakeOuterStringSerializeResponse, E>;

    /// FakeResponseWithNumericalDescription - GET /v2/fake/response-with-numerical-description
    async fn fake_response_with_numerical_description(
        &self,

        method: &Method,
        host: &Host,
        cookies: &CookieJar,
    ) -> Result<FakeResponseWithNumericalDescriptionResponse, E>;

    /// HyphenParam - GET /v2/fake/hyphenParam/{hyphen-param}
    async fn hyphen_param(
        &self,

        method: &Method,
        host: &Host,
        cookies: &CookieJar,
        path_params: &models::HyphenParamPathParams,
    ) -> Result<HyphenParamResponse, E>;

    /// TestBodyWithQueryParams - PUT /v2/fake/body-with-query-params
    async fn test_body_with_query_params(
        &self,

        method: &Method,
        host: &Host,
        cookies: &CookieJar,
        query_params: &models::TestBodyWithQueryParamsQueryParams,
        body: &models::User,
    ) -> Result<TestBodyWithQueryParamsResponse, E>;

    /// To test \"client\" model.
    ///
    /// TestClientModel - PATCH /v2/fake
    async fn test_client_model(
        &self,

        method: &Method,
        host: &Host,
        cookies: &CookieJar,
        body: &models::Client,
    ) -> Result<TestClientModelResponse, E>;

    /// Fake endpoint for testing various parameters  假端點  偽のエンドポイント  가짜 엔드 포인트.
    ///
    /// TestEndpointParameters - POST /v2/fake
    async fn test_endpoint_parameters(
        &self,

        method: &Method,
        host: &Host,
        cookies: &CookieJar,
        claims: &Self::Claims,
        body: &models::TestEndpointParametersRequest,
    ) -> Result<TestEndpointParametersResponse, E>;

    /// To test enum parameters.
    ///
    /// TestEnumParameters - GET /v2/fake
    async fn test_enum_parameters(
        &self,

        method: &Method,
        host: &Host,
        cookies: &CookieJar,
        header_params: &models::TestEnumParametersHeaderParams,
        query_params: &models::TestEnumParametersQueryParams,
        body: &Option<models::TestEnumParametersRequest>,
    ) -> Result<TestEnumParametersResponse, E>;

    /// test inline additionalProperties.
    ///
    /// TestInlineAdditionalProperties - POST /v2/fake/inline-additionalProperties
    async fn test_inline_additional_properties(
        &self,

        method: &Method,
        host: &Host,
        cookies: &CookieJar,
        body: &std::collections::HashMap<String, String>,
    ) -> Result<TestInlineAdditionalPropertiesResponse, E>;

    /// test json serialization of form data.
    ///
    /// TestJsonFormData - GET /v2/fake/jsonFormData
    async fn test_json_form_data(
        &self,

        method: &Method,
        host: &Host,
        cookies: &CookieJar,
        body: &models::TestJsonFormDataRequest,
    ) -> Result<TestJsonFormDataResponse, E>;
}
