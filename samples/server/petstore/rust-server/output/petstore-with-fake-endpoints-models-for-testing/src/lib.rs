#![allow(missing_docs, trivial_casts, unused_variables, unused_mut, unused_imports, unused_extern_crates, non_camel_case_types)]

use async_trait::async_trait;
use futures::Stream;
use std::error::Error;
use std::task::{Poll, Context};
use swagger::{ApiError, ContextWrapper};
use serde::{Serialize, Deserialize};

type ServiceError = Box<dyn Error + Send + Sync + 'static>;

pub const BASE_PATH: &'static str = "/v2";
pub const API_VERSION: &'static str = "1.0.0";

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub enum TestSpecialTagsResponse {
    /// successful operation
    SuccessfulOperation
    (models::Client)
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub enum Call123exampleResponse {
    /// success
    Success
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub enum FakeOuterBooleanSerializeResponse {
    /// Output boolean
    OutputBoolean
    (bool)
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub enum FakeOuterCompositeSerializeResponse {
    /// Output composite
    OutputComposite
    (models::OuterComposite)
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub enum FakeOuterNumberSerializeResponse {
    /// Output number
    OutputNumber
    (f64)
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub enum FakeOuterStringSerializeResponse {
    /// Output string
    OutputString
    (String)
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub enum FakeResponseWithNumericalDescriptionResponse {
    /// 1234
    Status200
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub enum HyphenParamResponse {
    /// Success
    Success
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub enum TestBodyWithQueryParamsResponse {
    /// Success
    Success
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub enum TestClientModelResponse {
    /// successful operation
    SuccessfulOperation
    (models::Client)
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
#[must_use]
pub enum TestEndpointParametersResponse {
    /// Invalid username supplied
    InvalidUsernameSupplied
    ,
    /// User not found
    UserNotFound
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
#[must_use]
pub enum TestEnumParametersResponse {
    /// Invalid request
    InvalidRequest
    ,
    /// Not found
    NotFound
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub enum TestInlineAdditionalPropertiesResponse {
    /// successful operation
    SuccessfulOperation
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub enum TestJsonFormDataResponse {
    /// successful operation
    SuccessfulOperation
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub enum TestClassnameResponse {
    /// successful operation
    SuccessfulOperation
    (models::Client)
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub enum AddPetResponse {
    /// Invalid input
    InvalidInput
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub enum DeletePetResponse {
    /// Invalid pet value
    InvalidPetValue
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
#[must_use]
pub enum FindPetsByStatusResponse {
    /// successful operation
    SuccessfulOperation
    (Vec<models::Pet>)
    ,
    /// Invalid status value
    InvalidStatusValue
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
#[must_use]
pub enum FindPetsByTagsResponse {
    /// successful operation
    SuccessfulOperation
    (Vec<models::Pet>)
    ,
    /// Invalid tag value
    InvalidTagValue
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
#[must_use]
pub enum GetPetByIdResponse {
    /// successful operation
    SuccessfulOperation
    (models::Pet)
    ,
    /// Invalid ID supplied
    InvalidIDSupplied
    ,
    /// Pet not found
    PetNotFound
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
#[must_use]
pub enum UpdatePetResponse {
    /// Invalid ID supplied
    InvalidIDSupplied
    ,
    /// Pet not found
    PetNotFound
    ,
    /// Validation exception
    ValidationException
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub enum UpdatePetWithFormResponse {
    /// Invalid input
    InvalidInput
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub enum UploadFileResponse {
    /// successful operation
    SuccessfulOperation
    (models::ApiResponse)
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
#[must_use]
pub enum DeleteOrderResponse {
    /// Invalid ID supplied
    InvalidIDSupplied
    ,
    /// Order not found
    OrderNotFound
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub enum GetInventoryResponse {
    /// successful operation
    SuccessfulOperation
    (std::collections::HashMap<String, i32>)
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
#[must_use]
pub enum GetOrderByIdResponse {
    /// successful operation
    SuccessfulOperation
    (models::Order)
    ,
    /// Invalid ID supplied
    InvalidIDSupplied
    ,
    /// Order not found
    OrderNotFound
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
#[must_use]
pub enum PlaceOrderResponse {
    /// successful operation
    SuccessfulOperation
    (models::Order)
    ,
    /// Invalid Order
    InvalidOrder
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub enum CreateUserResponse {
    /// successful operation
    SuccessfulOperation
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub enum CreateUsersWithArrayInputResponse {
    /// successful operation
    SuccessfulOperation
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub enum CreateUsersWithListInputResponse {
    /// successful operation
    SuccessfulOperation
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
#[must_use]
pub enum DeleteUserResponse {
    /// Invalid username supplied
    InvalidUsernameSupplied
    ,
    /// User not found
    UserNotFound
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
#[must_use]
pub enum GetUserByNameResponse {
    /// successful operation
    SuccessfulOperation
    (models::User)
    ,
    /// Invalid username supplied
    InvalidUsernameSupplied
    ,
    /// User not found
    UserNotFound
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
#[must_use]
pub enum LoginUserResponse {
    /// successful operation
    SuccessfulOperation
    {
        body: String,
        x_rate_limit:
        Option<
        i32
        >
        ,
        x_expires_after:
        Option<
        chrono::DateTime::<chrono::Utc>
        >
    }
    ,
    /// Invalid username/password supplied
    InvalidUsername
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub enum LogoutUserResponse {
    /// successful operation
    SuccessfulOperation
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
#[must_use]
pub enum UpdateUserResponse {
    /// Invalid user supplied
    InvalidUserSupplied
    ,
    /// User not found
    UserNotFound
}

/// API
#[async_trait]
pub trait Api<C: Send + Sync> {
    fn poll_ready(&self, _cx: &mut Context) -> Poll<Result<(), Box<dyn Error + Send + Sync + 'static>>> {
        Poll::Ready(Ok(()))
    }

    /// To test special tags
    async fn test_special_tags(
        &self,
        body: models::Client,
        context: &C) -> Result<TestSpecialTagsResponse, ApiError>;

    async fn call123example(
        &self,
        context: &C) -> Result<Call123exampleResponse, ApiError>;

    async fn fake_outer_boolean_serialize(
        &self,
        body: Option<models::OuterBoolean>,
        context: &C) -> Result<FakeOuterBooleanSerializeResponse, ApiError>;

    async fn fake_outer_composite_serialize(
        &self,
        body: Option<models::OuterComposite>,
        context: &C) -> Result<FakeOuterCompositeSerializeResponse, ApiError>;

    async fn fake_outer_number_serialize(
        &self,
        body: Option<models::OuterNumber>,
        context: &C) -> Result<FakeOuterNumberSerializeResponse, ApiError>;

    async fn fake_outer_string_serialize(
        &self,
        body: Option<models::OuterString>,
        context: &C) -> Result<FakeOuterStringSerializeResponse, ApiError>;

    async fn fake_response_with_numerical_description(
        &self,
        context: &C) -> Result<FakeResponseWithNumericalDescriptionResponse, ApiError>;

    async fn hyphen_param(
        &self,
        hyphen_param: String,
        context: &C) -> Result<HyphenParamResponse, ApiError>;

    async fn test_body_with_query_params(
        &self,
        query: String,
        body: models::User,
        context: &C) -> Result<TestBodyWithQueryParamsResponse, ApiError>;

    /// To test \"client\" model
    async fn test_client_model(
        &self,
        body: models::Client,
        context: &C) -> Result<TestClientModelResponse, ApiError>;

    /// Fake endpoint for testing various parameters  假端點  偽のエンドポイント  가짜 엔드 포인트
    async fn test_endpoint_parameters(
        &self,
        number: f64,
        double: f64,
        pattern_without_delimiter: String,
        byte: swagger::ByteArray,
        integer: Option<i32>,
        int32: Option<i32>,
        int64: Option<i64>,
        float: Option<f32>,
        string: Option<String>,
        binary: Option<swagger::ByteArray>,
        date: Option<chrono::DateTime::<chrono::Utc>>,
        date_time: Option<chrono::DateTime::<chrono::Utc>>,
        password: Option<String>,
        callback: Option<String>,
        context: &C) -> Result<TestEndpointParametersResponse, ApiError>;

    /// To test enum parameters
    async fn test_enum_parameters(
        &self,
        enum_header_string_array: Option<&Vec<String>>,
        enum_header_string: Option<String>,
        enum_query_string_array: Option<&Vec<String>>,
        enum_query_string: Option<String>,
        enum_query_integer: Option<i32>,
        enum_query_double: Option<f64>,
        enum_form_string: Option<String>,
        context: &C) -> Result<TestEnumParametersResponse, ApiError>;

    /// test inline additionalProperties
    async fn test_inline_additional_properties(
        &self,
        param: std::collections::HashMap<String, String>,
        context: &C) -> Result<TestInlineAdditionalPropertiesResponse, ApiError>;

    /// test json serialization of form data
    async fn test_json_form_data(
        &self,
        param: String,
        param2: String,
        context: &C) -> Result<TestJsonFormDataResponse, ApiError>;

    /// To test class name in snake case
    async fn test_classname(
        &self,
        body: models::Client,
        context: &C) -> Result<TestClassnameResponse, ApiError>;

    /// Add a new pet to the store
    async fn add_pet(
        &self,
        body: models::Pet,
        context: &C) -> Result<AddPetResponse, ApiError>;

    /// Deletes a pet
    async fn delete_pet(
        &self,
        pet_id: i64,
        api_key: Option<String>,
        context: &C) -> Result<DeletePetResponse, ApiError>;

    /// Finds Pets by status
    async fn find_pets_by_status(
        &self,
        status: &Vec<String>,
        context: &C) -> Result<FindPetsByStatusResponse, ApiError>;

    /// Finds Pets by tags
    async fn find_pets_by_tags(
        &self,
        tags: &Vec<String>,
        context: &C) -> Result<FindPetsByTagsResponse, ApiError>;

    /// Find pet by ID
    async fn get_pet_by_id(
        &self,
        pet_id: i64,
        context: &C) -> Result<GetPetByIdResponse, ApiError>;

    /// Update an existing pet
    async fn update_pet(
        &self,
        body: models::Pet,
        context: &C) -> Result<UpdatePetResponse, ApiError>;

    /// Updates a pet in the store with form data
    async fn update_pet_with_form(
        &self,
        pet_id: i64,
        name: Option<String>,
        status: Option<String>,
        context: &C) -> Result<UpdatePetWithFormResponse, ApiError>;

    /// uploads an image
    async fn upload_file(
        &self,
        pet_id: i64,
        additional_metadata: Option<String>,
        file: Option<swagger::ByteArray>,
        context: &C) -> Result<UploadFileResponse, ApiError>;

    /// Delete purchase order by ID
    async fn delete_order(
        &self,
        order_id: String,
        context: &C) -> Result<DeleteOrderResponse, ApiError>;

    /// Returns pet inventories by status
    async fn get_inventory(
        &self,
        context: &C) -> Result<GetInventoryResponse, ApiError>;

    /// Find purchase order by ID
    async fn get_order_by_id(
        &self,
        order_id: i64,
        context: &C) -> Result<GetOrderByIdResponse, ApiError>;

    /// Place an order for a pet
    async fn place_order(
        &self,
        body: models::Order,
        context: &C) -> Result<PlaceOrderResponse, ApiError>;

    /// Create user
    async fn create_user(
        &self,
        body: models::User,
        context: &C) -> Result<CreateUserResponse, ApiError>;

    /// Creates list of users with given input array
    async fn create_users_with_array_input(
        &self,
        body: &Vec<models::User>,
        context: &C) -> Result<CreateUsersWithArrayInputResponse, ApiError>;

    /// Creates list of users with given input array
    async fn create_users_with_list_input(
        &self,
        body: &Vec<models::User>,
        context: &C) -> Result<CreateUsersWithListInputResponse, ApiError>;

    /// Delete user
    async fn delete_user(
        &self,
        username: String,
        context: &C) -> Result<DeleteUserResponse, ApiError>;

    /// Get user by user name
    async fn get_user_by_name(
        &self,
        username: String,
        context: &C) -> Result<GetUserByNameResponse, ApiError>;

    /// Logs user into the system
    async fn login_user(
        &self,
        username: String,
        password: String,
        context: &C) -> Result<LoginUserResponse, ApiError>;

    /// Logs out current logged in user session
    async fn logout_user(
        &self,
        context: &C) -> Result<LogoutUserResponse, ApiError>;

    /// Updated user
    async fn update_user(
        &self,
        username: String,
        body: models::User,
        context: &C) -> Result<UpdateUserResponse, ApiError>;

}

/// API where `Context` isn't passed on every API call
#[async_trait]
pub trait ApiNoContext<C: Send + Sync> {

    fn poll_ready(&self, _cx: &mut Context) -> Poll<Result<(), Box<dyn Error + Send + Sync + 'static>>>;

    fn context(&self) -> &C;

    /// To test special tags
    async fn test_special_tags(
        &self,
        body: models::Client,
        ) -> Result<TestSpecialTagsResponse, ApiError>;

    async fn call123example(
        &self,
        ) -> Result<Call123exampleResponse, ApiError>;

    async fn fake_outer_boolean_serialize(
        &self,
        body: Option<models::OuterBoolean>,
        ) -> Result<FakeOuterBooleanSerializeResponse, ApiError>;

    async fn fake_outer_composite_serialize(
        &self,
        body: Option<models::OuterComposite>,
        ) -> Result<FakeOuterCompositeSerializeResponse, ApiError>;

    async fn fake_outer_number_serialize(
        &self,
        body: Option<models::OuterNumber>,
        ) -> Result<FakeOuterNumberSerializeResponse, ApiError>;

    async fn fake_outer_string_serialize(
        &self,
        body: Option<models::OuterString>,
        ) -> Result<FakeOuterStringSerializeResponse, ApiError>;

    async fn fake_response_with_numerical_description(
        &self,
        ) -> Result<FakeResponseWithNumericalDescriptionResponse, ApiError>;

    async fn hyphen_param(
        &self,
        hyphen_param: String,
        ) -> Result<HyphenParamResponse, ApiError>;

    async fn test_body_with_query_params(
        &self,
        query: String,
        body: models::User,
        ) -> Result<TestBodyWithQueryParamsResponse, ApiError>;

    /// To test \"client\" model
    async fn test_client_model(
        &self,
        body: models::Client,
        ) -> Result<TestClientModelResponse, ApiError>;

    /// Fake endpoint for testing various parameters  假端點  偽のエンドポイント  가짜 엔드 포인트
    async fn test_endpoint_parameters(
        &self,
        number: f64,
        double: f64,
        pattern_without_delimiter: String,
        byte: swagger::ByteArray,
        integer: Option<i32>,
        int32: Option<i32>,
        int64: Option<i64>,
        float: Option<f32>,
        string: Option<String>,
        binary: Option<swagger::ByteArray>,
        date: Option<chrono::DateTime::<chrono::Utc>>,
        date_time: Option<chrono::DateTime::<chrono::Utc>>,
        password: Option<String>,
        callback: Option<String>,
        ) -> Result<TestEndpointParametersResponse, ApiError>;

    /// To test enum parameters
    async fn test_enum_parameters(
        &self,
        enum_header_string_array: Option<&Vec<String>>,
        enum_header_string: Option<String>,
        enum_query_string_array: Option<&Vec<String>>,
        enum_query_string: Option<String>,
        enum_query_integer: Option<i32>,
        enum_query_double: Option<f64>,
        enum_form_string: Option<String>,
        ) -> Result<TestEnumParametersResponse, ApiError>;

    /// test inline additionalProperties
    async fn test_inline_additional_properties(
        &self,
        param: std::collections::HashMap<String, String>,
        ) -> Result<TestInlineAdditionalPropertiesResponse, ApiError>;

    /// test json serialization of form data
    async fn test_json_form_data(
        &self,
        param: String,
        param2: String,
        ) -> Result<TestJsonFormDataResponse, ApiError>;

    /// To test class name in snake case
    async fn test_classname(
        &self,
        body: models::Client,
        ) -> Result<TestClassnameResponse, ApiError>;

    /// Add a new pet to the store
    async fn add_pet(
        &self,
        body: models::Pet,
        ) -> Result<AddPetResponse, ApiError>;

    /// Deletes a pet
    async fn delete_pet(
        &self,
        pet_id: i64,
        api_key: Option<String>,
        ) -> Result<DeletePetResponse, ApiError>;

    /// Finds Pets by status
    async fn find_pets_by_status(
        &self,
        status: &Vec<String>,
        ) -> Result<FindPetsByStatusResponse, ApiError>;

    /// Finds Pets by tags
    async fn find_pets_by_tags(
        &self,
        tags: &Vec<String>,
        ) -> Result<FindPetsByTagsResponse, ApiError>;

    /// Find pet by ID
    async fn get_pet_by_id(
        &self,
        pet_id: i64,
        ) -> Result<GetPetByIdResponse, ApiError>;

    /// Update an existing pet
    async fn update_pet(
        &self,
        body: models::Pet,
        ) -> Result<UpdatePetResponse, ApiError>;

    /// Updates a pet in the store with form data
    async fn update_pet_with_form(
        &self,
        pet_id: i64,
        name: Option<String>,
        status: Option<String>,
        ) -> Result<UpdatePetWithFormResponse, ApiError>;

    /// uploads an image
    async fn upload_file(
        &self,
        pet_id: i64,
        additional_metadata: Option<String>,
        file: Option<swagger::ByteArray>,
        ) -> Result<UploadFileResponse, ApiError>;

    /// Delete purchase order by ID
    async fn delete_order(
        &self,
        order_id: String,
        ) -> Result<DeleteOrderResponse, ApiError>;

    /// Returns pet inventories by status
    async fn get_inventory(
        &self,
        ) -> Result<GetInventoryResponse, ApiError>;

    /// Find purchase order by ID
    async fn get_order_by_id(
        &self,
        order_id: i64,
        ) -> Result<GetOrderByIdResponse, ApiError>;

    /// Place an order for a pet
    async fn place_order(
        &self,
        body: models::Order,
        ) -> Result<PlaceOrderResponse, ApiError>;

    /// Create user
    async fn create_user(
        &self,
        body: models::User,
        ) -> Result<CreateUserResponse, ApiError>;

    /// Creates list of users with given input array
    async fn create_users_with_array_input(
        &self,
        body: &Vec<models::User>,
        ) -> Result<CreateUsersWithArrayInputResponse, ApiError>;

    /// Creates list of users with given input array
    async fn create_users_with_list_input(
        &self,
        body: &Vec<models::User>,
        ) -> Result<CreateUsersWithListInputResponse, ApiError>;

    /// Delete user
    async fn delete_user(
        &self,
        username: String,
        ) -> Result<DeleteUserResponse, ApiError>;

    /// Get user by user name
    async fn get_user_by_name(
        &self,
        username: String,
        ) -> Result<GetUserByNameResponse, ApiError>;

    /// Logs user into the system
    async fn login_user(
        &self,
        username: String,
        password: String,
        ) -> Result<LoginUserResponse, ApiError>;

    /// Logs out current logged in user session
    async fn logout_user(
        &self,
        ) -> Result<LogoutUserResponse, ApiError>;

    /// Updated user
    async fn update_user(
        &self,
        username: String,
        body: models::User,
        ) -> Result<UpdateUserResponse, ApiError>;

}

/// Trait to extend an API to make it easy to bind it to a context.
pub trait ContextWrapperExt<C: Send + Sync> where Self: Sized
{
    /// Binds this API to a context.
    fn with_context(self: Self, context: C) -> ContextWrapper<Self, C>;
}

impl<T: Api<C> + Send + Sync, C: Clone + Send + Sync> ContextWrapperExt<C> for T {
    fn with_context(self: T, context: C) -> ContextWrapper<T, C> {
         ContextWrapper::<T, C>::new(self, context)
    }
}

#[async_trait]
impl<T: Api<C> + Send + Sync, C: Clone + Send + Sync> ApiNoContext<C> for ContextWrapper<T, C> {
    fn poll_ready(&self, cx: &mut Context) -> Poll<Result<(), ServiceError>> {
        self.api().poll_ready(cx)
    }

    fn context(&self) -> &C {
        ContextWrapper::context(self)
    }

    /// To test special tags
    async fn test_special_tags(
        &self,
        body: models::Client,
        ) -> Result<TestSpecialTagsResponse, ApiError>
    {
        let context = self.context().clone();
        self.api().test_special_tags(body, &context).await
    }

    async fn call123example(
        &self,
        ) -> Result<Call123exampleResponse, ApiError>
    {
        let context = self.context().clone();
        self.api().call123example(&context).await
    }

    async fn fake_outer_boolean_serialize(
        &self,
        body: Option<models::OuterBoolean>,
        ) -> Result<FakeOuterBooleanSerializeResponse, ApiError>
    {
        let context = self.context().clone();
        self.api().fake_outer_boolean_serialize(body, &context).await
    }

    async fn fake_outer_composite_serialize(
        &self,
        body: Option<models::OuterComposite>,
        ) -> Result<FakeOuterCompositeSerializeResponse, ApiError>
    {
        let context = self.context().clone();
        self.api().fake_outer_composite_serialize(body, &context).await
    }

    async fn fake_outer_number_serialize(
        &self,
        body: Option<models::OuterNumber>,
        ) -> Result<FakeOuterNumberSerializeResponse, ApiError>
    {
        let context = self.context().clone();
        self.api().fake_outer_number_serialize(body, &context).await
    }

    async fn fake_outer_string_serialize(
        &self,
        body: Option<models::OuterString>,
        ) -> Result<FakeOuterStringSerializeResponse, ApiError>
    {
        let context = self.context().clone();
        self.api().fake_outer_string_serialize(body, &context).await
    }

    async fn fake_response_with_numerical_description(
        &self,
        ) -> Result<FakeResponseWithNumericalDescriptionResponse, ApiError>
    {
        let context = self.context().clone();
        self.api().fake_response_with_numerical_description(&context).await
    }

    async fn hyphen_param(
        &self,
        hyphen_param: String,
        ) -> Result<HyphenParamResponse, ApiError>
    {
        let context = self.context().clone();
        self.api().hyphen_param(hyphen_param, &context).await
    }

    async fn test_body_with_query_params(
        &self,
        query: String,
        body: models::User,
        ) -> Result<TestBodyWithQueryParamsResponse, ApiError>
    {
        let context = self.context().clone();
        self.api().test_body_with_query_params(query, body, &context).await
    }

    /// To test \"client\" model
    async fn test_client_model(
        &self,
        body: models::Client,
        ) -> Result<TestClientModelResponse, ApiError>
    {
        let context = self.context().clone();
        self.api().test_client_model(body, &context).await
    }

    /// Fake endpoint for testing various parameters  假端點  偽のエンドポイント  가짜 엔드 포인트
    async fn test_endpoint_parameters(
        &self,
        number: f64,
        double: f64,
        pattern_without_delimiter: String,
        byte: swagger::ByteArray,
        integer: Option<i32>,
        int32: Option<i32>,
        int64: Option<i64>,
        float: Option<f32>,
        string: Option<String>,
        binary: Option<swagger::ByteArray>,
        date: Option<chrono::DateTime::<chrono::Utc>>,
        date_time: Option<chrono::DateTime::<chrono::Utc>>,
        password: Option<String>,
        callback: Option<String>,
        ) -> Result<TestEndpointParametersResponse, ApiError>
    {
        let context = self.context().clone();
        self.api().test_endpoint_parameters(number, double, pattern_without_delimiter, byte, integer, int32, int64, float, string, binary, date, date_time, password, callback, &context).await
    }

    /// To test enum parameters
    async fn test_enum_parameters(
        &self,
        enum_header_string_array: Option<&Vec<String>>,
        enum_header_string: Option<String>,
        enum_query_string_array: Option<&Vec<String>>,
        enum_query_string: Option<String>,
        enum_query_integer: Option<i32>,
        enum_query_double: Option<f64>,
        enum_form_string: Option<String>,
        ) -> Result<TestEnumParametersResponse, ApiError>
    {
        let context = self.context().clone();
        self.api().test_enum_parameters(enum_header_string_array, enum_header_string, enum_query_string_array, enum_query_string, enum_query_integer, enum_query_double, enum_form_string, &context).await
    }

    /// test inline additionalProperties
    async fn test_inline_additional_properties(
        &self,
        param: std::collections::HashMap<String, String>,
        ) -> Result<TestInlineAdditionalPropertiesResponse, ApiError>
    {
        let context = self.context().clone();
        self.api().test_inline_additional_properties(param, &context).await
    }

    /// test json serialization of form data
    async fn test_json_form_data(
        &self,
        param: String,
        param2: String,
        ) -> Result<TestJsonFormDataResponse, ApiError>
    {
        let context = self.context().clone();
        self.api().test_json_form_data(param, param2, &context).await
    }

    /// To test class name in snake case
    async fn test_classname(
        &self,
        body: models::Client,
        ) -> Result<TestClassnameResponse, ApiError>
    {
        let context = self.context().clone();
        self.api().test_classname(body, &context).await
    }

    /// Add a new pet to the store
    async fn add_pet(
        &self,
        body: models::Pet,
        ) -> Result<AddPetResponse, ApiError>
    {
        let context = self.context().clone();
        self.api().add_pet(body, &context).await
    }

    /// Deletes a pet
    async fn delete_pet(
        &self,
        pet_id: i64,
        api_key: Option<String>,
        ) -> Result<DeletePetResponse, ApiError>
    {
        let context = self.context().clone();
        self.api().delete_pet(pet_id, api_key, &context).await
    }

    /// Finds Pets by status
    async fn find_pets_by_status(
        &self,
        status: &Vec<String>,
        ) -> Result<FindPetsByStatusResponse, ApiError>
    {
        let context = self.context().clone();
        self.api().find_pets_by_status(status, &context).await
    }

    /// Finds Pets by tags
    async fn find_pets_by_tags(
        &self,
        tags: &Vec<String>,
        ) -> Result<FindPetsByTagsResponse, ApiError>
    {
        let context = self.context().clone();
        self.api().find_pets_by_tags(tags, &context).await
    }

    /// Find pet by ID
    async fn get_pet_by_id(
        &self,
        pet_id: i64,
        ) -> Result<GetPetByIdResponse, ApiError>
    {
        let context = self.context().clone();
        self.api().get_pet_by_id(pet_id, &context).await
    }

    /// Update an existing pet
    async fn update_pet(
        &self,
        body: models::Pet,
        ) -> Result<UpdatePetResponse, ApiError>
    {
        let context = self.context().clone();
        self.api().update_pet(body, &context).await
    }

    /// Updates a pet in the store with form data
    async fn update_pet_with_form(
        &self,
        pet_id: i64,
        name: Option<String>,
        status: Option<String>,
        ) -> Result<UpdatePetWithFormResponse, ApiError>
    {
        let context = self.context().clone();
        self.api().update_pet_with_form(pet_id, name, status, &context).await
    }

    /// uploads an image
    async fn upload_file(
        &self,
        pet_id: i64,
        additional_metadata: Option<String>,
        file: Option<swagger::ByteArray>,
        ) -> Result<UploadFileResponse, ApiError>
    {
        let context = self.context().clone();
        self.api().upload_file(pet_id, additional_metadata, file, &context).await
    }

    /// Delete purchase order by ID
    async fn delete_order(
        &self,
        order_id: String,
        ) -> Result<DeleteOrderResponse, ApiError>
    {
        let context = self.context().clone();
        self.api().delete_order(order_id, &context).await
    }

    /// Returns pet inventories by status
    async fn get_inventory(
        &self,
        ) -> Result<GetInventoryResponse, ApiError>
    {
        let context = self.context().clone();
        self.api().get_inventory(&context).await
    }

    /// Find purchase order by ID
    async fn get_order_by_id(
        &self,
        order_id: i64,
        ) -> Result<GetOrderByIdResponse, ApiError>
    {
        let context = self.context().clone();
        self.api().get_order_by_id(order_id, &context).await
    }

    /// Place an order for a pet
    async fn place_order(
        &self,
        body: models::Order,
        ) -> Result<PlaceOrderResponse, ApiError>
    {
        let context = self.context().clone();
        self.api().place_order(body, &context).await
    }

    /// Create user
    async fn create_user(
        &self,
        body: models::User,
        ) -> Result<CreateUserResponse, ApiError>
    {
        let context = self.context().clone();
        self.api().create_user(body, &context).await
    }

    /// Creates list of users with given input array
    async fn create_users_with_array_input(
        &self,
        body: &Vec<models::User>,
        ) -> Result<CreateUsersWithArrayInputResponse, ApiError>
    {
        let context = self.context().clone();
        self.api().create_users_with_array_input(body, &context).await
    }

    /// Creates list of users with given input array
    async fn create_users_with_list_input(
        &self,
        body: &Vec<models::User>,
        ) -> Result<CreateUsersWithListInputResponse, ApiError>
    {
        let context = self.context().clone();
        self.api().create_users_with_list_input(body, &context).await
    }

    /// Delete user
    async fn delete_user(
        &self,
        username: String,
        ) -> Result<DeleteUserResponse, ApiError>
    {
        let context = self.context().clone();
        self.api().delete_user(username, &context).await
    }

    /// Get user by user name
    async fn get_user_by_name(
        &self,
        username: String,
        ) -> Result<GetUserByNameResponse, ApiError>
    {
        let context = self.context().clone();
        self.api().get_user_by_name(username, &context).await
    }

    /// Logs user into the system
    async fn login_user(
        &self,
        username: String,
        password: String,
        ) -> Result<LoginUserResponse, ApiError>
    {
        let context = self.context().clone();
        self.api().login_user(username, password, &context).await
    }

    /// Logs out current logged in user session
    async fn logout_user(
        &self,
        ) -> Result<LogoutUserResponse, ApiError>
    {
        let context = self.context().clone();
        self.api().logout_user(&context).await
    }

    /// Updated user
    async fn update_user(
        &self,
        username: String,
        body: models::User,
        ) -> Result<UpdateUserResponse, ApiError>
    {
        let context = self.context().clone();
        self.api().update_user(username, body, &context).await
    }

}


#[cfg(feature = "client")]
pub mod client;

// Re-export Client as a top-level name
#[cfg(feature = "client")]
pub use client::Client;

#[cfg(feature = "server")]
pub mod server;

// Re-export router() as a top-level name
#[cfg(feature = "server")]
pub use self::server::Service;

#[cfg(feature = "server")]
pub mod context;

pub mod models;

#[cfg(any(feature = "client", feature = "server"))]
pub(crate) mod header;
