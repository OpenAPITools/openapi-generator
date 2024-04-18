#![allow(
    missing_docs,
    trivial_casts,
    unused_variables,
    unused_mut,
    unused_extern_crates,
    non_camel_case_types,
    unused_imports,
    unused_attributes
)]
#![allow(clippy::derive_partial_eq_without_eq, clippy::disallowed_names)]

use async_trait::async_trait;
use axum::extract::*;
use axum_extra::extract::{CookieJar, Multipart};
use bytes::Bytes;
use http::Method;
use serde::{Deserialize, Serialize};

use types::*;

pub const BASE_PATH: &str = "/v2";
pub const API_VERSION: &str = "1.0.0";

#[derive(Debug, PartialEq, Serialize, Deserialize)]
#[must_use]
#[allow(clippy::large_enum_variant)]
pub enum TestSpecialTagsResponse {
    /// successful operation
    Status200_SuccessfulOperation(models::Client),
}

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

#[derive(Debug, PartialEq, Serialize, Deserialize)]
#[must_use]
#[allow(clippy::large_enum_variant)]
pub enum TestClassnameResponse {
    /// successful operation
    Status200_SuccessfulOperation(models::Client),
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
#[must_use]
#[allow(clippy::large_enum_variant)]
pub enum AddPetResponse {
    /// Invalid input
    Status405_InvalidInput,
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
#[must_use]
#[allow(clippy::large_enum_variant)]
pub enum DeletePetResponse {
    /// Invalid pet value
    Status400_InvalidPetValue,
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
#[must_use]
#[allow(clippy::large_enum_variant)]
pub enum FindPetsByStatusResponse {
    /// successful operation
    Status200_SuccessfulOperation(String),
    /// Invalid status value
    Status400_InvalidStatusValue,
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
#[must_use]
#[allow(clippy::large_enum_variant)]
pub enum FindPetsByTagsResponse {
    /// successful operation
    Status200_SuccessfulOperation(String),
    /// Invalid tag value
    Status400_InvalidTagValue,
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
#[must_use]
#[allow(clippy::large_enum_variant)]
pub enum GetPetByIdResponse {
    /// successful operation
    Status200_SuccessfulOperation(String),
    /// Invalid ID supplied
    Status400_InvalidIDSupplied,
    /// Pet not found
    Status404_PetNotFound,
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
#[must_use]
#[allow(clippy::large_enum_variant)]
pub enum UpdatePetResponse {
    /// Invalid ID supplied
    Status400_InvalidIDSupplied,
    /// Pet not found
    Status404_PetNotFound,
    /// Validation exception
    Status405_ValidationException,
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
#[must_use]
#[allow(clippy::large_enum_variant)]
pub enum UpdatePetWithFormResponse {
    /// Invalid input
    Status405_InvalidInput,
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
#[must_use]
#[allow(clippy::large_enum_variant)]
pub enum UploadFileResponse {
    /// successful operation
    Status200_SuccessfulOperation(models::ApiResponse),
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
#[must_use]
#[allow(clippy::large_enum_variant)]
pub enum DeleteOrderResponse {
    /// Invalid ID supplied
    Status400_InvalidIDSupplied,
    /// Order not found
    Status404_OrderNotFound,
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
#[must_use]
#[allow(clippy::large_enum_variant)]
pub enum GetInventoryResponse {
    /// successful operation
    Status200_SuccessfulOperation(std::collections::HashMap<String, i32>),
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
#[must_use]
#[allow(clippy::large_enum_variant)]
pub enum GetOrderByIdResponse {
    /// successful operation
    Status200_SuccessfulOperation(String),
    /// Invalid ID supplied
    Status400_InvalidIDSupplied,
    /// Order not found
    Status404_OrderNotFound,
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
#[must_use]
#[allow(clippy::large_enum_variant)]
pub enum PlaceOrderResponse {
    /// successful operation
    Status200_SuccessfulOperation(String),
    /// Invalid Order
    Status400_InvalidOrder,
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
#[must_use]
#[allow(clippy::large_enum_variant)]
pub enum CreateUserResponse {
    /// successful operation
    Status0_SuccessfulOperation,
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
#[must_use]
#[allow(clippy::large_enum_variant)]
pub enum CreateUsersWithArrayInputResponse {
    /// successful operation
    Status0_SuccessfulOperation,
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
#[must_use]
#[allow(clippy::large_enum_variant)]
pub enum CreateUsersWithListInputResponse {
    /// successful operation
    Status0_SuccessfulOperation,
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
#[must_use]
#[allow(clippy::large_enum_variant)]
pub enum DeleteUserResponse {
    /// Invalid username supplied
    Status400_InvalidUsernameSupplied,
    /// User not found
    Status404_UserNotFound,
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
#[must_use]
#[allow(clippy::large_enum_variant)]
pub enum GetUserByNameResponse {
    /// successful operation
    Status200_SuccessfulOperation(String),
    /// Invalid username supplied
    Status400_InvalidUsernameSupplied,
    /// User not found
    Status404_UserNotFound,
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
#[must_use]
#[allow(clippy::large_enum_variant)]
pub enum LoginUserResponse {
    /// successful operation
    Status200_SuccessfulOperation {
        body: String,
        x_rate_limit: Option<i32>,
        x_expires_after: Option<chrono::DateTime<chrono::Utc>>,
    },
    /// Invalid username/password supplied
    Status400_InvalidUsername,
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
#[must_use]
#[allow(clippy::large_enum_variant)]
pub enum LogoutUserResponse {
    /// successful operation
    Status0_SuccessfulOperation,
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
#[must_use]
#[allow(clippy::large_enum_variant)]
pub enum UpdateUserResponse {
    /// Invalid user supplied
    Status400_InvalidUserSupplied,
    /// User not found
    Status404_UserNotFound,
}

/// API
#[async_trait]
#[allow(clippy::ptr_arg)]
pub trait Api {
    /// To test special tags.
    ///
    /// TestSpecialTags - PATCH /v2/another-fake/dummy
    async fn test_special_tags(
        &self,
        method: Method,
        host: Host,
        cookies: CookieJar,
        body: models::Client,
    ) -> Result<TestSpecialTagsResponse, String>;

    /// Call123example - GET /v2/fake/operation-with-numeric-id
    async fn call123example(
        &self,
        method: Method,
        host: Host,
        cookies: CookieJar,
    ) -> Result<Call123exampleResponse, String>;

    /// FakeOuterBooleanSerialize - POST /v2/fake/outer/boolean
    async fn fake_outer_boolean_serialize(
        &self,
        method: Method,
        host: Host,
        cookies: CookieJar,
        body: Option<models::OuterBoolean>,
    ) -> Result<FakeOuterBooleanSerializeResponse, String>;

    /// FakeOuterCompositeSerialize - POST /v2/fake/outer/composite
    async fn fake_outer_composite_serialize(
        &self,
        method: Method,
        host: Host,
        cookies: CookieJar,
        body: Option<models::OuterComposite>,
    ) -> Result<FakeOuterCompositeSerializeResponse, String>;

    /// FakeOuterNumberSerialize - POST /v2/fake/outer/number
    async fn fake_outer_number_serialize(
        &self,
        method: Method,
        host: Host,
        cookies: CookieJar,
        body: Option<models::OuterNumber>,
    ) -> Result<FakeOuterNumberSerializeResponse, String>;

    /// FakeOuterStringSerialize - POST /v2/fake/outer/string
    async fn fake_outer_string_serialize(
        &self,
        method: Method,
        host: Host,
        cookies: CookieJar,
        body: Option<models::OuterString>,
    ) -> Result<FakeOuterStringSerializeResponse, String>;

    /// FakeResponseWithNumericalDescription - GET /v2/fake/response-with-numerical-description
    async fn fake_response_with_numerical_description(
        &self,
        method: Method,
        host: Host,
        cookies: CookieJar,
    ) -> Result<FakeResponseWithNumericalDescriptionResponse, String>;

    /// HyphenParam - GET /v2/fake/hyphenParam/{hyphen-param}
    async fn hyphen_param(
        &self,
        method: Method,
        host: Host,
        cookies: CookieJar,
        path_params: models::HyphenParamPathParams,
    ) -> Result<HyphenParamResponse, String>;

    /// TestBodyWithQueryParams - PUT /v2/fake/body-with-query-params
    async fn test_body_with_query_params(
        &self,
        method: Method,
        host: Host,
        cookies: CookieJar,
        query_params: models::TestBodyWithQueryParamsQueryParams,
        body: models::User,
    ) -> Result<TestBodyWithQueryParamsResponse, String>;

    /// To test \"client\" model.
    ///
    /// TestClientModel - PATCH /v2/fake
    async fn test_client_model(
        &self,
        method: Method,
        host: Host,
        cookies: CookieJar,
        body: models::Client,
    ) -> Result<TestClientModelResponse, String>;

    /// Fake endpoint for testing various parameters  假端點  偽のエンドポイント  가짜 엔드 포인트.
    ///
    /// TestEndpointParameters - POST /v2/fake
    async fn test_endpoint_parameters(
        &self,
        method: Method,
        host: Host,
        cookies: CookieJar,
    ) -> Result<TestEndpointParametersResponse, String>;

    /// To test enum parameters.
    ///
    /// TestEnumParameters - GET /v2/fake
    async fn test_enum_parameters(
        &self,
        method: Method,
        host: Host,
        cookies: CookieJar,
        header_params: models::TestEnumParametersHeaderParams,
        query_params: models::TestEnumParametersQueryParams,
    ) -> Result<TestEnumParametersResponse, String>;

    /// test inline additionalProperties.
    ///
    /// TestInlineAdditionalProperties - POST /v2/fake/inline-additionalProperties
    async fn test_inline_additional_properties(
        &self,
        method: Method,
        host: Host,
        cookies: CookieJar,
        body: std::collections::HashMap<String, String>,
    ) -> Result<TestInlineAdditionalPropertiesResponse, String>;

    /// test json serialization of form data.
    ///
    /// TestJsonFormData - GET /v2/fake/jsonFormData
    async fn test_json_form_data(
        &self,
        method: Method,
        host: Host,
        cookies: CookieJar,
    ) -> Result<TestJsonFormDataResponse, String>;

    /// To test class name in snake case.
    ///
    /// TestClassname - PATCH /v2/fake_classname_test
    async fn test_classname(
        &self,
        method: Method,
        host: Host,
        cookies: CookieJar,
        body: models::Client,
    ) -> Result<TestClassnameResponse, String>;

    /// Add a new pet to the store.
    ///
    /// AddPet - POST /v2/pet
    async fn add_pet(
        &self,
        method: Method,
        host: Host,
        cookies: CookieJar,
        body: models::Pet,
    ) -> Result<AddPetResponse, String>;

    /// Deletes a pet.
    ///
    /// DeletePet - DELETE /v2/pet/{petId}
    async fn delete_pet(
        &self,
        method: Method,
        host: Host,
        cookies: CookieJar,
        header_params: models::DeletePetHeaderParams,
        path_params: models::DeletePetPathParams,
    ) -> Result<DeletePetResponse, String>;

    /// Finds Pets by status.
    ///
    /// FindPetsByStatus - GET /v2/pet/findByStatus
    async fn find_pets_by_status(
        &self,
        method: Method,
        host: Host,
        cookies: CookieJar,
        query_params: models::FindPetsByStatusQueryParams,
    ) -> Result<FindPetsByStatusResponse, String>;

    /// Finds Pets by tags.
    ///
    /// FindPetsByTags - GET /v2/pet/findByTags
    async fn find_pets_by_tags(
        &self,
        method: Method,
        host: Host,
        cookies: CookieJar,
        query_params: models::FindPetsByTagsQueryParams,
    ) -> Result<FindPetsByTagsResponse, String>;

    /// Find pet by ID.
    ///
    /// GetPetById - GET /v2/pet/{petId}
    async fn get_pet_by_id(
        &self,
        method: Method,
        host: Host,
        cookies: CookieJar,
        path_params: models::GetPetByIdPathParams,
    ) -> Result<GetPetByIdResponse, String>;

    /// Update an existing pet.
    ///
    /// UpdatePet - PUT /v2/pet
    async fn update_pet(
        &self,
        method: Method,
        host: Host,
        cookies: CookieJar,
        body: models::Pet,
    ) -> Result<UpdatePetResponse, String>;

    /// Updates a pet in the store with form data.
    ///
    /// UpdatePetWithForm - POST /v2/pet/{petId}
    async fn update_pet_with_form(
        &self,
        method: Method,
        host: Host,
        cookies: CookieJar,
        path_params: models::UpdatePetWithFormPathParams,
    ) -> Result<UpdatePetWithFormResponse, String>;

    /// uploads an image.
    ///
    /// UploadFile - POST /v2/pet/{petId}/uploadImage
    async fn upload_file(
        &self,
        method: Method,
        host: Host,
        cookies: CookieJar,
        path_params: models::UploadFilePathParams,
        body: Multipart,
    ) -> Result<UploadFileResponse, String>;

    /// Delete purchase order by ID.
    ///
    /// DeleteOrder - DELETE /v2/store/order/{order_id}
    async fn delete_order(
        &self,
        method: Method,
        host: Host,
        cookies: CookieJar,
        path_params: models::DeleteOrderPathParams,
    ) -> Result<DeleteOrderResponse, String>;

    /// Returns pet inventories by status.
    ///
    /// GetInventory - GET /v2/store/inventory
    async fn get_inventory(
        &self,
        method: Method,
        host: Host,
        cookies: CookieJar,
    ) -> Result<GetInventoryResponse, String>;

    /// Find purchase order by ID.
    ///
    /// GetOrderById - GET /v2/store/order/{order_id}
    async fn get_order_by_id(
        &self,
        method: Method,
        host: Host,
        cookies: CookieJar,
        path_params: models::GetOrderByIdPathParams,
    ) -> Result<GetOrderByIdResponse, String>;

    /// Place an order for a pet.
    ///
    /// PlaceOrder - POST /v2/store/order
    async fn place_order(
        &self,
        method: Method,
        host: Host,
        cookies: CookieJar,
        body: models::Order,
    ) -> Result<PlaceOrderResponse, String>;

    /// Create user.
    ///
    /// CreateUser - POST /v2/user
    async fn create_user(
        &self,
        method: Method,
        host: Host,
        cookies: CookieJar,
        body: models::User,
    ) -> Result<CreateUserResponse, String>;

    /// Creates list of users with given input array.
    ///
    /// CreateUsersWithArrayInput - POST /v2/user/createWithArray
    async fn create_users_with_array_input(
        &self,
        method: Method,
        host: Host,
        cookies: CookieJar,
        body: Vec<models::User>,
    ) -> Result<CreateUsersWithArrayInputResponse, String>;

    /// Creates list of users with given input array.
    ///
    /// CreateUsersWithListInput - POST /v2/user/createWithList
    async fn create_users_with_list_input(
        &self,
        method: Method,
        host: Host,
        cookies: CookieJar,
        body: Vec<models::User>,
    ) -> Result<CreateUsersWithListInputResponse, String>;

    /// Delete user.
    ///
    /// DeleteUser - DELETE /v2/user/{username}
    async fn delete_user(
        &self,
        method: Method,
        host: Host,
        cookies: CookieJar,
        path_params: models::DeleteUserPathParams,
    ) -> Result<DeleteUserResponse, String>;

    /// Get user by user name.
    ///
    /// GetUserByName - GET /v2/user/{username}
    async fn get_user_by_name(
        &self,
        method: Method,
        host: Host,
        cookies: CookieJar,
        path_params: models::GetUserByNamePathParams,
    ) -> Result<GetUserByNameResponse, String>;

    /// Logs user into the system.
    ///
    /// LoginUser - GET /v2/user/login
    async fn login_user(
        &self,
        method: Method,
        host: Host,
        cookies: CookieJar,
        query_params: models::LoginUserQueryParams,
    ) -> Result<LoginUserResponse, String>;

    /// Logs out current logged in user session.
    ///
    /// LogoutUser - GET /v2/user/logout
    async fn logout_user(
        &self,
        method: Method,
        host: Host,
        cookies: CookieJar,
    ) -> Result<LogoutUserResponse, String>;

    /// Updated user.
    ///
    /// UpdateUser - PUT /v2/user/{username}
    async fn update_user(
        &self,
        method: Method,
        host: Host,
        cookies: CookieJar,
        path_params: models::UpdateUserPathParams,
        body: models::User,
    ) -> Result<UpdateUserResponse, String>;
}

#[cfg(feature = "server")]
pub mod server;

pub mod models;
pub mod types;

#[cfg(feature = "server")]
pub(crate) mod header;
