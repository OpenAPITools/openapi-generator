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
    /// Call123example - GET /v2/fake/operation-with-numeric-id
    async fn call123example(
        &self,
<<<<<<< HEAD
        method: &Method,
        host: &Host,
        cookies: &CookieJar,
=======
        method: Method,
        host: Host,
        cookies: CookieJar,
>>>>>>> 9841fa4dc2c (Implement a custom error handler for unhandled or generic endpoint errors)
    ) -> Result<Call123exampleResponse, E>;

    /// FakeOuterBooleanSerialize - POST /v2/fake/outer/boolean
    async fn fake_outer_boolean_serialize(
        &self,
<<<<<<< HEAD
        method: &Method,
        host: &Host,
        cookies: &CookieJar,
        body: &Option<models::OuterBoolean>,
=======
        method: Method,
        host: Host,
        cookies: CookieJar,
        body: Option<models::OuterBoolean>,
>>>>>>> 9841fa4dc2c (Implement a custom error handler for unhandled or generic endpoint errors)
    ) -> Result<FakeOuterBooleanSerializeResponse, E>;

    /// FakeOuterCompositeSerialize - POST /v2/fake/outer/composite
    async fn fake_outer_composite_serialize(
        &self,
<<<<<<< HEAD
        method: &Method,
        host: &Host,
        cookies: &CookieJar,
        body: &Option<models::OuterComposite>,
=======
        method: Method,
        host: Host,
        cookies: CookieJar,
        body: Option<models::OuterComposite>,
>>>>>>> 9841fa4dc2c (Implement a custom error handler for unhandled or generic endpoint errors)
    ) -> Result<FakeOuterCompositeSerializeResponse, E>;

    /// FakeOuterNumberSerialize - POST /v2/fake/outer/number
    async fn fake_outer_number_serialize(
        &self,
<<<<<<< HEAD
        method: &Method,
        host: &Host,
        cookies: &CookieJar,
        body: &Option<models::OuterNumber>,
=======
        method: Method,
        host: Host,
        cookies: CookieJar,
        body: Option<models::OuterNumber>,
>>>>>>> 9841fa4dc2c (Implement a custom error handler for unhandled or generic endpoint errors)
    ) -> Result<FakeOuterNumberSerializeResponse, E>;

    /// FakeOuterStringSerialize - POST /v2/fake/outer/string
    async fn fake_outer_string_serialize(
        &self,
<<<<<<< HEAD
        method: &Method,
        host: &Host,
        cookies: &CookieJar,
        body: &Option<models::OuterString>,
=======
        method: Method,
        host: Host,
        cookies: CookieJar,
        body: Option<models::OuterString>,
>>>>>>> 9841fa4dc2c (Implement a custom error handler for unhandled or generic endpoint errors)
    ) -> Result<FakeOuterStringSerializeResponse, E>;

    /// FakeResponseWithNumericalDescription - GET /v2/fake/response-with-numerical-description
    async fn fake_response_with_numerical_description(
        &self,
<<<<<<< HEAD
        method: &Method,
        host: &Host,
        cookies: &CookieJar,
=======
        method: Method,
        host: Host,
        cookies: CookieJar,
>>>>>>> 9841fa4dc2c (Implement a custom error handler for unhandled or generic endpoint errors)
    ) -> Result<FakeResponseWithNumericalDescriptionResponse, E>;

    /// HyphenParam - GET /v2/fake/hyphenParam/{hyphen-param}
    async fn hyphen_param(
        &self,
<<<<<<< HEAD
        method: &Method,
        host: &Host,
        cookies: &CookieJar,
        path_params: &models::HyphenParamPathParams,
=======
        method: Method,
        host: Host,
        cookies: CookieJar,
        path_params: models::HyphenParamPathParams,
>>>>>>> 9841fa4dc2c (Implement a custom error handler for unhandled or generic endpoint errors)
    ) -> Result<HyphenParamResponse, E>;

    /// TestBodyWithQueryParams - PUT /v2/fake/body-with-query-params
    async fn test_body_with_query_params(
        &self,
<<<<<<< HEAD
        method: &Method,
        host: &Host,
        cookies: &CookieJar,
        query_params: &models::TestBodyWithQueryParamsQueryParams,
        body: &models::User,
=======
        method: Method,
        host: Host,
        cookies: CookieJar,
        query_params: models::TestBodyWithQueryParamsQueryParams,
        body: models::User,
>>>>>>> 9841fa4dc2c (Implement a custom error handler for unhandled or generic endpoint errors)
    ) -> Result<TestBodyWithQueryParamsResponse, E>;

    /// To test \"client\" model.
    ///
    /// TestClientModel - PATCH /v2/fake
    async fn test_client_model(
        &self,
<<<<<<< HEAD
        method: &Method,
        host: &Host,
        cookies: &CookieJar,
        body: &models::Client,
=======
        method: Method,
        host: Host,
        cookies: CookieJar,
        body: models::Client,
>>>>>>> 9841fa4dc2c (Implement a custom error handler for unhandled or generic endpoint errors)
    ) -> Result<TestClientModelResponse, E>;

    /// Fake endpoint for testing various parameters  假端點  偽のエンドポイント  가짜 엔드 포인트.
    ///
    /// TestEndpointParameters - POST /v2/fake
    async fn test_endpoint_parameters(
        &self,
<<<<<<< HEAD
        method: &Method,
        host: &Host,
        cookies: &CookieJar,
        body: &models::TestEndpointParametersRequest,
=======
        method: Method,
        host: Host,
        cookies: CookieJar,
        body: models::TestEndpointParametersRequest,
>>>>>>> 9841fa4dc2c (Implement a custom error handler for unhandled or generic endpoint errors)
    ) -> Result<TestEndpointParametersResponse, E>;

    /// To test enum parameters.
    ///
    /// TestEnumParameters - GET /v2/fake
    async fn test_enum_parameters(
        &self,
<<<<<<< HEAD
        method: &Method,
        host: &Host,
        cookies: &CookieJar,
        header_params: &models::TestEnumParametersHeaderParams,
        query_params: &models::TestEnumParametersQueryParams,
        body: &Option<models::TestEnumParametersRequest>,
=======
        method: Method,
        host: Host,
        cookies: CookieJar,
        header_params: models::TestEnumParametersHeaderParams,
        query_params: models::TestEnumParametersQueryParams,
        body: Option<models::TestEnumParametersRequest>,
>>>>>>> 9841fa4dc2c (Implement a custom error handler for unhandled or generic endpoint errors)
    ) -> Result<TestEnumParametersResponse, E>;

    /// test inline additionalProperties.
    ///
    /// TestInlineAdditionalProperties - POST /v2/fake/inline-additionalProperties
    async fn test_inline_additional_properties(
        &self,
<<<<<<< HEAD
        method: &Method,
        host: &Host,
        cookies: &CookieJar,
        body: &std::collections::HashMap<String, String>,
=======
        method: Method,
        host: Host,
        cookies: CookieJar,
        body: std::collections::HashMap<String, String>,
>>>>>>> 9841fa4dc2c (Implement a custom error handler for unhandled or generic endpoint errors)
    ) -> Result<TestInlineAdditionalPropertiesResponse, E>;

    /// test json serialization of form data.
    ///
    /// TestJsonFormData - GET /v2/fake/jsonFormData
    async fn test_json_form_data(
        &self,
<<<<<<< HEAD
        method: &Method,
        host: &Host,
        cookies: &CookieJar,
        body: &models::TestJsonFormDataRequest,
=======
        method: Method,
        host: Host,
        cookies: CookieJar,
        body: models::TestJsonFormDataRequest,
>>>>>>> 9841fa4dc2c (Implement a custom error handler for unhandled or generic endpoint errors)
    ) -> Result<TestJsonFormDataResponse, E>;
}
