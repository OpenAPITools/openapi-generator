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
<<<<<<< HEAD
<<<<<<< HEAD
        method: &Method,
        host: &Host,
        cookies: &CookieJar,
=======
        method: Method,
        host: Host,
        cookies: CookieJar,
>>>>>>> 9841fa4dc2c (Implement a custom error handler for unhandled or generic endpoint errors)
=======
        method: &Method,
        host: &Host,
        cookies: &CookieJar,
>>>>>>> 3d833fd5ff9 (Make API methods take references instead of ownership)
    ) -> Result<Call123exampleResponse, E>;

    /// FakeOuterBooleanSerialize - POST /v2/fake/outer/boolean
    async fn fake_outer_boolean_serialize(
        &self,
<<<<<<< HEAD
<<<<<<< HEAD
=======
>>>>>>> 3d833fd5ff9 (Make API methods take references instead of ownership)
        method: &Method,
        host: &Host,
        cookies: &CookieJar,
        body: &Option<models::OuterBoolean>,
<<<<<<< HEAD
=======
        method: Method,
        host: Host,
        cookies: CookieJar,
        body: Option<models::OuterBoolean>,
>>>>>>> 9841fa4dc2c (Implement a custom error handler for unhandled or generic endpoint errors)
=======
>>>>>>> 3d833fd5ff9 (Make API methods take references instead of ownership)
    ) -> Result<FakeOuterBooleanSerializeResponse, E>;

    /// FakeOuterCompositeSerialize - POST /v2/fake/outer/composite
    async fn fake_outer_composite_serialize(
        &self,
<<<<<<< HEAD
<<<<<<< HEAD
=======
>>>>>>> 3d833fd5ff9 (Make API methods take references instead of ownership)
        method: &Method,
        host: &Host,
        cookies: &CookieJar,
        body: &Option<models::OuterComposite>,
<<<<<<< HEAD
=======
        method: Method,
        host: Host,
        cookies: CookieJar,
        body: Option<models::OuterComposite>,
>>>>>>> 9841fa4dc2c (Implement a custom error handler for unhandled or generic endpoint errors)
=======
>>>>>>> 3d833fd5ff9 (Make API methods take references instead of ownership)
    ) -> Result<FakeOuterCompositeSerializeResponse, E>;

    /// FakeOuterNumberSerialize - POST /v2/fake/outer/number
    async fn fake_outer_number_serialize(
        &self,
<<<<<<< HEAD
<<<<<<< HEAD
=======
>>>>>>> 3d833fd5ff9 (Make API methods take references instead of ownership)
        method: &Method,
        host: &Host,
        cookies: &CookieJar,
        body: &Option<models::OuterNumber>,
<<<<<<< HEAD
=======
        method: Method,
        host: Host,
        cookies: CookieJar,
        body: Option<models::OuterNumber>,
>>>>>>> 9841fa4dc2c (Implement a custom error handler for unhandled or generic endpoint errors)
=======
>>>>>>> 3d833fd5ff9 (Make API methods take references instead of ownership)
    ) -> Result<FakeOuterNumberSerializeResponse, E>;

    /// FakeOuterStringSerialize - POST /v2/fake/outer/string
    async fn fake_outer_string_serialize(
        &self,
<<<<<<< HEAD
<<<<<<< HEAD
=======
>>>>>>> 3d833fd5ff9 (Make API methods take references instead of ownership)
        method: &Method,
        host: &Host,
        cookies: &CookieJar,
        body: &Option<models::OuterString>,
<<<<<<< HEAD
=======
        method: Method,
        host: Host,
        cookies: CookieJar,
        body: Option<models::OuterString>,
>>>>>>> 9841fa4dc2c (Implement a custom error handler for unhandled or generic endpoint errors)
=======
>>>>>>> 3d833fd5ff9 (Make API methods take references instead of ownership)
    ) -> Result<FakeOuterStringSerializeResponse, E>;

    /// FakeResponseWithNumericalDescription - GET /v2/fake/response-with-numerical-description
    async fn fake_response_with_numerical_description(
        &self,
<<<<<<< HEAD
<<<<<<< HEAD
        method: &Method,
        host: &Host,
        cookies: &CookieJar,
=======
        method: Method,
        host: Host,
        cookies: CookieJar,
>>>>>>> 9841fa4dc2c (Implement a custom error handler for unhandled or generic endpoint errors)
=======
        method: &Method,
        host: &Host,
        cookies: &CookieJar,
>>>>>>> 3d833fd5ff9 (Make API methods take references instead of ownership)
    ) -> Result<FakeResponseWithNumericalDescriptionResponse, E>;

    /// HyphenParam - GET /v2/fake/hyphenParam/{hyphen-param}
    async fn hyphen_param(
        &self,
<<<<<<< HEAD
<<<<<<< HEAD
=======
>>>>>>> 3d833fd5ff9 (Make API methods take references instead of ownership)
        method: &Method,
        host: &Host,
        cookies: &CookieJar,
        path_params: &models::HyphenParamPathParams,
<<<<<<< HEAD
=======
        method: Method,
        host: Host,
        cookies: CookieJar,
        path_params: models::HyphenParamPathParams,
>>>>>>> 9841fa4dc2c (Implement a custom error handler for unhandled or generic endpoint errors)
=======
>>>>>>> 3d833fd5ff9 (Make API methods take references instead of ownership)
    ) -> Result<HyphenParamResponse, E>;

    /// TestBodyWithQueryParams - PUT /v2/fake/body-with-query-params
    async fn test_body_with_query_params(
        &self,
<<<<<<< HEAD
<<<<<<< HEAD
=======
>>>>>>> 3d833fd5ff9 (Make API methods take references instead of ownership)
        method: &Method,
        host: &Host,
        cookies: &CookieJar,
        query_params: &models::TestBodyWithQueryParamsQueryParams,
        body: &models::User,
<<<<<<< HEAD
=======
        method: Method,
        host: Host,
        cookies: CookieJar,
        query_params: models::TestBodyWithQueryParamsQueryParams,
        body: models::User,
>>>>>>> 9841fa4dc2c (Implement a custom error handler for unhandled or generic endpoint errors)
=======
>>>>>>> 3d833fd5ff9 (Make API methods take references instead of ownership)
    ) -> Result<TestBodyWithQueryParamsResponse, E>;

    /// To test \"client\" model.
    ///
    /// TestClientModel - PATCH /v2/fake
    async fn test_client_model(
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
    ) -> Result<TestClientModelResponse, E>;

    /// Fake endpoint for testing various parameters  假端點  偽のエンドポイント  가짜 엔드 포인트.
    ///
    /// TestEndpointParameters - POST /v2/fake
    async fn test_endpoint_parameters(
        &self,
<<<<<<< HEAD
<<<<<<< HEAD
<<<<<<< HEAD
<<<<<<< HEAD
=======
>>>>>>> 3d833fd5ff9 (Make API methods take references instead of ownership)
=======
>>>>>>> a297ccec6f8 (Rebase error handler)
=======
>>>>>>> 73be82180e8 (Rebase rust-axum-error-handling)
        method: &Method,
        host: &Host,
        cookies: &CookieJar,
        claims: &Self::Claims,
        body: &models::TestEndpointParametersRequest,
<<<<<<< HEAD
<<<<<<< HEAD
<<<<<<< HEAD
=======
>>>>>>> a297ccec6f8 (Rebase error handler)
=======
        method: Method,
        host: Host,
        cookies: CookieJar,
<<<<<<< HEAD
        body: models::TestEndpointParametersRequest,
>>>>>>> 9841fa4dc2c (Implement a custom error handler for unhandled or generic endpoint errors)
=======
>>>>>>> 3d833fd5ff9 (Make API methods take references instead of ownership)
=======
        claims: Self::Claims,
        body: models::TestEndpointParametersRequest,
>>>>>>> ba70bfea1e1 (Implement basic and bearer auth handling)
>>>>>>> a297ccec6f8 (Rebase error handler)
=======
>>>>>>> 73be82180e8 (Rebase rust-axum-error-handling)
    ) -> Result<TestEndpointParametersResponse, E>;

    /// To test enum parameters.
    ///
    /// TestEnumParameters - GET /v2/fake
    async fn test_enum_parameters(
        &self,
<<<<<<< HEAD
<<<<<<< HEAD
=======
>>>>>>> 3d833fd5ff9 (Make API methods take references instead of ownership)
        method: &Method,
        host: &Host,
        cookies: &CookieJar,
        header_params: &models::TestEnumParametersHeaderParams,
        query_params: &models::TestEnumParametersQueryParams,
        body: &Option<models::TestEnumParametersRequest>,
<<<<<<< HEAD
=======
        method: Method,
        host: Host,
        cookies: CookieJar,
        header_params: models::TestEnumParametersHeaderParams,
        query_params: models::TestEnumParametersQueryParams,
        body: Option<models::TestEnumParametersRequest>,
>>>>>>> 9841fa4dc2c (Implement a custom error handler for unhandled or generic endpoint errors)
=======
>>>>>>> 3d833fd5ff9 (Make API methods take references instead of ownership)
    ) -> Result<TestEnumParametersResponse, E>;

    /// test inline additionalProperties.
    ///
    /// TestInlineAdditionalProperties - POST /v2/fake/inline-additionalProperties
    async fn test_inline_additional_properties(
        &self,
<<<<<<< HEAD
<<<<<<< HEAD
=======
>>>>>>> 3d833fd5ff9 (Make API methods take references instead of ownership)
        method: &Method,
        host: &Host,
        cookies: &CookieJar,
        body: &std::collections::HashMap<String, String>,
<<<<<<< HEAD
=======
        method: Method,
        host: Host,
        cookies: CookieJar,
        body: std::collections::HashMap<String, String>,
>>>>>>> 9841fa4dc2c (Implement a custom error handler for unhandled or generic endpoint errors)
=======
>>>>>>> 3d833fd5ff9 (Make API methods take references instead of ownership)
    ) -> Result<TestInlineAdditionalPropertiesResponse, E>;

    /// test json serialization of form data.
    ///
    /// TestJsonFormData - GET /v2/fake/jsonFormData
    async fn test_json_form_data(
        &self,
<<<<<<< HEAD
<<<<<<< HEAD
=======
>>>>>>> 3d833fd5ff9 (Make API methods take references instead of ownership)
        method: &Method,
        host: &Host,
        cookies: &CookieJar,
        body: &models::TestJsonFormDataRequest,
<<<<<<< HEAD
=======
        method: Method,
        host: Host,
        cookies: CookieJar,
        body: models::TestJsonFormDataRequest,
>>>>>>> 9841fa4dc2c (Implement a custom error handler for unhandled or generic endpoint errors)
=======
>>>>>>> 3d833fd5ff9 (Make API methods take references instead of ownership)
    ) -> Result<TestJsonFormDataResponse, E>;
}
