#![allow(missing_docs, trivial_casts, unused_variables, unused_mut, unused_imports, unused_extern_crates, non_camel_case_types)]
use async_trait::async_trait;
use futures::{FutureExt, Stream, StreamExt, TryStreamExt};
use openapi_context::ContextWrapper;
use std::io::Error;

#[allow(unused_imports)]
use std::collections::HashMap;

#[cfg(any(feature = "client", feature = "server"))]
mod mimetypes;

pub use openapi_context::ApiError;
pub const BASE_PATH: &'static str = "/v2";
pub const API_VERSION: &'static str = "1.0.0";


#[derive(Debug, PartialEq)]
pub enum TestSpecialTagsResponse {
    /// successful operation
    SuccessfulOperation
    (crate::models::Client)
}

#[derive(Debug, PartialEq)]
pub enum FakeOuterBooleanSerializeResponse {
    /// Output boolean
    OutputBoolean
    (bool)
}

#[derive(Debug, PartialEq)]
pub enum FakeOuterCompositeSerializeResponse {
    /// Output composite
    OutputComposite
    (crate::models::OuterComposite)
}

#[derive(Debug, PartialEq)]
pub enum FakeOuterNumberSerializeResponse {
    /// Output number
    OutputNumber
    (f64)
}

#[derive(Debug, PartialEq)]
pub enum FakeOuterStringSerializeResponse {
    /// Output string
    OutputString
    (String)
}

#[derive(Debug, PartialEq)]
pub enum HyphenParamResponse {
    /// Success
    Success
}

#[derive(Debug, PartialEq)]
pub enum TestBodyWithQueryParamsResponse {
    /// Success
    Success
}

#[derive(Debug, PartialEq)]
pub enum TestClientModelResponse {
    /// successful operation
    SuccessfulOperation
    (crate::models::Client)
}

#[derive(Debug, PartialEq)]
pub enum TestEndpointParametersResponse {
    /// Invalid username supplied
    InvalidUsernameSupplied
    ,
    /// User not found
    UserNotFound
}

#[derive(Debug, PartialEq)]
pub enum TestEnumParametersResponse {
    /// Invalid request
    InvalidRequest
    ,
    /// Not found
    NotFound
}

#[derive(Debug, PartialEq)]
pub enum TestInlineAdditionalPropertiesResponse {
    /// successful operation
    SuccessfulOperation
}

#[derive(Debug, PartialEq)]
pub enum TestJsonFormDataResponse {
    /// successful operation
    SuccessfulOperation
}

#[derive(Debug, PartialEq)]
pub enum TestClassnameResponse {
    /// successful operation
    SuccessfulOperation
    (crate::models::Client)
}

#[derive(Debug, PartialEq)]
pub enum AddPetResponse {
    /// Invalid input
    InvalidInput
}

#[derive(Debug, PartialEq)]
pub enum DeletePetResponse {
    /// Invalid pet value
    InvalidPetValue
}

#[derive(Debug, PartialEq)]
pub enum FindPetsByStatusResponse {
    /// successful operation
    SuccessfulOperation
    (Vec<crate::models::Pet>)
    ,
    /// Invalid status value
    InvalidStatusValue
}

#[derive(Debug, PartialEq)]
pub enum FindPetsByTagsResponse {
    /// successful operation
    SuccessfulOperation
    (Vec<crate::models::Pet>)
    ,
    /// Invalid tag value
    InvalidTagValue
}

#[derive(Debug, PartialEq)]
pub enum GetPetByIdResponse {
    /// successful operation
    SuccessfulOperation
    (crate::models::Pet)
    ,
    /// Invalid ID supplied
    InvalidIDSupplied
    ,
    /// Pet not found
    PetNotFound
}

#[derive(Debug, PartialEq)]
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

#[derive(Debug, PartialEq)]
pub enum UpdatePetWithFormResponse {
    /// Invalid input
    InvalidInput
}

#[derive(Debug, PartialEq)]
pub enum UploadFileResponse {
    /// successful operation
    SuccessfulOperation
    (crate::models::ApiResponse)
}

#[derive(Debug, PartialEq)]
pub enum DeleteOrderResponse {
    /// Invalid ID supplied
    InvalidIDSupplied
    ,
    /// Order not found
    OrderNotFound
}

#[derive(Debug, PartialEq)]
pub enum GetInventoryResponse {
    /// successful operation
    SuccessfulOperation
    (HashMap<String, i32>)
}

#[derive(Debug, PartialEq)]
pub enum GetOrderByIdResponse {
    /// successful operation
    SuccessfulOperation
    (crate::models::Order)
    ,
    /// Invalid ID supplied
    InvalidIDSupplied
    ,
    /// Order not found
    OrderNotFound
}

#[derive(Debug, PartialEq)]
pub enum PlaceOrderResponse {
    /// successful operation
    SuccessfulOperation
    (crate::models::Order)
    ,
    /// Invalid Order
    InvalidOrder
}

#[derive(Debug, PartialEq)]
pub enum CreateUserResponse {
    /// successful operation
    SuccessfulOperation
}

#[derive(Debug, PartialEq)]
pub enum CreateUsersWithArrayInputResponse {
    /// successful operation
    SuccessfulOperation
}

#[derive(Debug, PartialEq)]
pub enum CreateUsersWithListInputResponse {
    /// successful operation
    SuccessfulOperation
}

#[derive(Debug, PartialEq)]
pub enum DeleteUserResponse {
    /// Invalid username supplied
    InvalidUsernameSupplied
    ,
    /// User not found
    UserNotFound
}

#[derive(Debug, PartialEq)]
pub enum GetUserByNameResponse {
    /// successful operation
    SuccessfulOperation
    (crate::models::User)
    ,
    /// Invalid username supplied
    InvalidUsernameSupplied
    ,
    /// User not found
    UserNotFound
}

#[derive(Debug, PartialEq)]
pub enum LoginUserResponse {
    /// successful operation
    SuccessfulOperation
    {
        body: String,
        x_rate_limit: i32,
        x_expires_after: chrono::DateTime<chrono::Utc>,
    }
    ,
    /// Invalid username/password supplied
    InvalidUsername
}

#[derive(Debug, PartialEq)]
pub enum LogoutUserResponse {
    /// successful operation
    SuccessfulOperation
}

#[derive(Debug, PartialEq)]
pub enum UpdateUserResponse {
    /// Invalid user supplied
    InvalidUserSupplied
    ,
    /// User not found
    UserNotFound
}


/// API
#[async_trait]
pub trait Api<C> {

    /// To test special tags
    async fn test_special_tags(&mut self, body: crate::models::Client, context: &C) -> Result<TestSpecialTagsResponse, ApiError>;


    async fn fake_outer_boolean_serialize(&mut self, body: Option<crate::models::OuterBoolean>, context: &C) -> Result<FakeOuterBooleanSerializeResponse, ApiError>;


    async fn fake_outer_composite_serialize(&mut self, body: Option<crate::models::OuterComposite>, context: &C) -> Result<FakeOuterCompositeSerializeResponse, ApiError>;


    async fn fake_outer_number_serialize(&mut self, body: Option<crate::models::OuterNumber>, context: &C) -> Result<FakeOuterNumberSerializeResponse, ApiError>;


    async fn fake_outer_string_serialize(&mut self, body: Option<crate::models::OuterString>, context: &C) -> Result<FakeOuterStringSerializeResponse, ApiError>;


    async fn hyphen_param(&mut self, hyphen_param: String, context: &C) -> Result<HyphenParamResponse, ApiError>;


    async fn test_body_with_query_params(&mut self, query: String, body: crate::models::User, context: &C) -> Result<TestBodyWithQueryParamsResponse, ApiError>;

    /// To test \"client\" model
    async fn test_client_model(&mut self, body: crate::models::Client, context: &C) -> Result<TestClientModelResponse, ApiError>;

    /// Fake endpoint for testing various parameters  假端點  偽のエンドポイント  가짜 엔드 포인트
    async fn test_endpoint_parameters(&mut self, number: f64, double: f64, pattern_without_delimiter: String, byte: openapi_context::ByteArray, integer: Option<i32>, int32: Option<i32>, int64: Option<i64>, float: Option<f32>, string: Option<String>, binary: Option<openapi_context::ByteArray>, date: Option<chrono::DateTime<chrono::Utc>>, date_time: Option<chrono::DateTime<chrono::Utc>>, password: Option<String>, callback: Option<String>, context: &C) -> Result<TestEndpointParametersResponse, ApiError>;

    /// To test enum parameters
    async fn test_enum_parameters(&mut self, enum_header_string_array: Option<&Vec<String>>, enum_header_string: Option<String>, enum_query_string_array: Option<&Vec<String>>, enum_query_string: Option<String>, enum_query_integer: Option<i32>, enum_query_double: Option<f64>, enum_form_string: Option<String>, context: &C) -> Result<TestEnumParametersResponse, ApiError>;

    /// test inline additionalProperties
    async fn test_inline_additional_properties(&mut self, param: HashMap<String, String>, context: &C) -> Result<TestInlineAdditionalPropertiesResponse, ApiError>;

    /// test json serialization of form data
    async fn test_json_form_data(&mut self, param: String, param2: String, context: &C) -> Result<TestJsonFormDataResponse, ApiError>;

    /// To test class name in snake case
    async fn test_classname(&mut self, body: crate::models::Client, context: &C) -> Result<TestClassnameResponse, ApiError>;

    /// Add a new pet to the store
    async fn add_pet(&mut self, body: crate::models::Pet, context: &C) -> Result<AddPetResponse, ApiError>;

    /// Deletes a pet
    async fn delete_pet(&mut self, pet_id: i64, api_key: Option<String>, context: &C) -> Result<DeletePetResponse, ApiError>;

    /// Finds Pets by status
    async fn find_pets_by_status(&mut self, status: &Vec<String>, context: &C) -> Result<FindPetsByStatusResponse, ApiError>;

    /// Finds Pets by tags
    async fn find_pets_by_tags(&mut self, tags: &Vec<String>, context: &C) -> Result<FindPetsByTagsResponse, ApiError>;

    /// Find pet by ID
    async fn get_pet_by_id(&mut self, pet_id: i64, context: &C) -> Result<GetPetByIdResponse, ApiError>;

    /// Update an existing pet
    async fn update_pet(&mut self, body: crate::models::Pet, context: &C) -> Result<UpdatePetResponse, ApiError>;

    /// Updates a pet in the store with form data
    async fn update_pet_with_form(&mut self, pet_id: i64, name: Option<String>, status: Option<String>, context: &C) -> Result<UpdatePetWithFormResponse, ApiError>;

    /// uploads an image
    async fn upload_file(&mut self, pet_id: i64, additional_metadata: Option<String>, file: Option<openapi_context::ByteArray>, context: &C) -> Result<UploadFileResponse, ApiError>;

    /// Delete purchase order by ID
    async fn delete_order(&mut self, order_id: String, context: &C) -> Result<DeleteOrderResponse, ApiError>;

    /// Returns pet inventories by status
    async fn get_inventory(&mut self, context: &C) -> Result<GetInventoryResponse, ApiError>;

    /// Find purchase order by ID
    async fn get_order_by_id(&mut self, order_id: i64, context: &C) -> Result<GetOrderByIdResponse, ApiError>;

    /// Place an order for a pet
    async fn place_order(&mut self, body: crate::models::Order, context: &C) -> Result<PlaceOrderResponse, ApiError>;

    /// Create user
    async fn create_user(&mut self, body: crate::models::User, context: &C) -> Result<CreateUserResponse, ApiError>;

    /// Creates list of users with given input array
    async fn create_users_with_array_input(&mut self, body: &Vec<crate::models::User>, context: &C) -> Result<CreateUsersWithArrayInputResponse, ApiError>;

    /// Creates list of users with given input array
    async fn create_users_with_list_input(&mut self, body: &Vec<crate::models::User>, context: &C) -> Result<CreateUsersWithListInputResponse, ApiError>;

    /// Delete user
    async fn delete_user(&mut self, username: String, context: &C) -> Result<DeleteUserResponse, ApiError>;

    /// Get user by user name
    async fn get_user_by_name(&mut self, username: String, context: &C) -> Result<GetUserByNameResponse, ApiError>;

    /// Logs user into the system
    async fn login_user(&mut self, username: String, password: String, context: &C) -> Result<LoginUserResponse, ApiError>;

    /// Logs out current logged in user session
    async fn logout_user(&mut self, context: &C) -> Result<LogoutUserResponse, ApiError>;

    /// Updated user
    async fn update_user(&mut self, username: String, body: crate::models::User, context: &C) -> Result<UpdateUserResponse, ApiError>;

}

/// API without a `Context`
#[async_trait]
pub trait ApiNoContext {

    /// To test special tags
    async fn test_special_tags(&mut self, body: crate::models::Client) -> Result<TestSpecialTagsResponse, ApiError>;


    async fn fake_outer_boolean_serialize(&mut self, body: Option<crate::models::OuterBoolean>) -> Result<FakeOuterBooleanSerializeResponse, ApiError>;


    async fn fake_outer_composite_serialize(&mut self, body: Option<crate::models::OuterComposite>) -> Result<FakeOuterCompositeSerializeResponse, ApiError>;


    async fn fake_outer_number_serialize(&mut self, body: Option<crate::models::OuterNumber>) -> Result<FakeOuterNumberSerializeResponse, ApiError>;


    async fn fake_outer_string_serialize(&mut self, body: Option<crate::models::OuterString>) -> Result<FakeOuterStringSerializeResponse, ApiError>;


    async fn hyphen_param(&mut self, hyphen_param: String) -> Result<HyphenParamResponse, ApiError>;


    async fn test_body_with_query_params(&mut self, query: String, body: crate::models::User) -> Result<TestBodyWithQueryParamsResponse, ApiError>;

    /// To test \"client\" model
    async fn test_client_model(&mut self, body: crate::models::Client) -> Result<TestClientModelResponse, ApiError>;

    /// Fake endpoint for testing various parameters  假端點  偽のエンドポイント  가짜 엔드 포인트
    async fn test_endpoint_parameters(&mut self, number: f64, double: f64, pattern_without_delimiter: String, byte: openapi_context::ByteArray, integer: Option<i32>, int32: Option<i32>, int64: Option<i64>, float: Option<f32>, string: Option<String>, binary: Option<openapi_context::ByteArray>, date: Option<chrono::DateTime<chrono::Utc>>, date_time: Option<chrono::DateTime<chrono::Utc>>, password: Option<String>, callback: Option<String>) -> Result<TestEndpointParametersResponse, ApiError>;

    /// To test enum parameters
    async fn test_enum_parameters(&mut self, enum_header_string_array: Option<&Vec<String>>, enum_header_string: Option<String>, enum_query_string_array: Option<&Vec<String>>, enum_query_string: Option<String>, enum_query_integer: Option<i32>, enum_query_double: Option<f64>, enum_form_string: Option<String>) -> Result<TestEnumParametersResponse, ApiError>;

    /// test inline additionalProperties
    async fn test_inline_additional_properties(&mut self, param: HashMap<String, String>) -> Result<TestInlineAdditionalPropertiesResponse, ApiError>;

    /// test json serialization of form data
    async fn test_json_form_data(&mut self, param: String, param2: String) -> Result<TestJsonFormDataResponse, ApiError>;

    /// To test class name in snake case
    async fn test_classname(&mut self, body: crate::models::Client) -> Result<TestClassnameResponse, ApiError>;

    /// Add a new pet to the store
    async fn add_pet(&mut self, body: crate::models::Pet) -> Result<AddPetResponse, ApiError>;

    /// Deletes a pet
    async fn delete_pet(&mut self, pet_id: i64, api_key: Option<String>) -> Result<DeletePetResponse, ApiError>;

    /// Finds Pets by status
    async fn find_pets_by_status(&mut self, status: &Vec<String>) -> Result<FindPetsByStatusResponse, ApiError>;

    /// Finds Pets by tags
    async fn find_pets_by_tags(&mut self, tags: &Vec<String>) -> Result<FindPetsByTagsResponse, ApiError>;

    /// Find pet by ID
    async fn get_pet_by_id(&mut self, pet_id: i64) -> Result<GetPetByIdResponse, ApiError>;

    /// Update an existing pet
    async fn update_pet(&mut self, body: crate::models::Pet) -> Result<UpdatePetResponse, ApiError>;

    /// Updates a pet in the store with form data
    async fn update_pet_with_form(&mut self, pet_id: i64, name: Option<String>, status: Option<String>) -> Result<UpdatePetWithFormResponse, ApiError>;

    /// uploads an image
    async fn upload_file(&mut self, pet_id: i64, additional_metadata: Option<String>, file: Option<openapi_context::ByteArray>) -> Result<UploadFileResponse, ApiError>;

    /// Delete purchase order by ID
    async fn delete_order(&mut self, order_id: String) -> Result<DeleteOrderResponse, ApiError>;

    /// Returns pet inventories by status
    async fn get_inventory(&mut self) -> Result<GetInventoryResponse, ApiError>;

    /// Find purchase order by ID
    async fn get_order_by_id(&mut self, order_id: i64) -> Result<GetOrderByIdResponse, ApiError>;

    /// Place an order for a pet
    async fn place_order(&mut self, body: crate::models::Order) -> Result<PlaceOrderResponse, ApiError>;

    /// Create user
    async fn create_user(&mut self, body: crate::models::User) -> Result<CreateUserResponse, ApiError>;

    /// Creates list of users with given input array
    async fn create_users_with_array_input(&mut self, body: &Vec<crate::models::User>) -> Result<CreateUsersWithArrayInputResponse, ApiError>;

    /// Creates list of users with given input array
    async fn create_users_with_list_input(&mut self, body: &Vec<crate::models::User>) -> Result<CreateUsersWithListInputResponse, ApiError>;

    /// Delete user
    async fn delete_user(&mut self, username: String) -> Result<DeleteUserResponse, ApiError>;

    /// Get user by user name
    async fn get_user_by_name(&mut self, username: String) -> Result<GetUserByNameResponse, ApiError>;

    /// Logs user into the system
    async fn login_user(&mut self, username: String, password: String) -> Result<LoginUserResponse, ApiError>;

    /// Logs out current logged in user session
    async fn logout_user(&mut self) -> Result<LogoutUserResponse, ApiError>;

    /// Updated user
    async fn update_user(&mut self, username: String, body: crate::models::User) -> Result<UpdateUserResponse, ApiError>;

}

/// Trait to extend an API to make it easy to bind it to a context.
pub trait ContextWrapperExt<C> where Self: Sized {
    /// Binds this API to a context.
    fn with_context(self, context: C) -> ContextWrapper<Self, C>;
}

impl<T: Api<C> + Sized, C> ContextWrapperExt<C> for T {
    fn with_context(self, context: C) -> ContextWrapper<T, C> {
         ContextWrapper::<T, C>::new(self, context)
    }
}

#[async_trait]
impl<T: Api<C>, C> ApiNoContext for ContextWrapper<T, C>
    where C: Clone + Send + Sync,
          T: Send + Sync,
{

    /// To test special tags
    async fn test_special_tags(&mut self, body: crate::models::Client) -> Result<TestSpecialTagsResponse, ApiError> {
        let ctx: C = self.context().clone();
        self.api_mut().test_special_tags(body, &ctx).await
    }


    async fn fake_outer_boolean_serialize(&mut self, body: Option<crate::models::OuterBoolean>) -> Result<FakeOuterBooleanSerializeResponse, ApiError> {
        let ctx: C = self.context().clone();
        self.api_mut().fake_outer_boolean_serialize(body, &ctx).await
    }


    async fn fake_outer_composite_serialize(&mut self, body: Option<crate::models::OuterComposite>) -> Result<FakeOuterCompositeSerializeResponse, ApiError> {
        let ctx: C = self.context().clone();
        self.api_mut().fake_outer_composite_serialize(body, &ctx).await
    }


    async fn fake_outer_number_serialize(&mut self, body: Option<crate::models::OuterNumber>) -> Result<FakeOuterNumberSerializeResponse, ApiError> {
        let ctx: C = self.context().clone();
        self.api_mut().fake_outer_number_serialize(body, &ctx).await
    }


    async fn fake_outer_string_serialize(&mut self, body: Option<crate::models::OuterString>) -> Result<FakeOuterStringSerializeResponse, ApiError> {
        let ctx: C = self.context().clone();
        self.api_mut().fake_outer_string_serialize(body, &ctx).await
    }


    async fn hyphen_param(&mut self, hyphen_param: String) -> Result<HyphenParamResponse, ApiError> {
        let ctx: C = self.context().clone();
        self.api_mut().hyphen_param(hyphen_param, &ctx).await
    }


    async fn test_body_with_query_params(&mut self, query: String, body: crate::models::User) -> Result<TestBodyWithQueryParamsResponse, ApiError> {
        let ctx: C = self.context().clone();
        self.api_mut().test_body_with_query_params(query, body, &ctx).await
    }

    /// To test \"client\" model
    async fn test_client_model(&mut self, body: crate::models::Client) -> Result<TestClientModelResponse, ApiError> {
        let ctx: C = self.context().clone();
        self.api_mut().test_client_model(body, &ctx).await
    }

    /// Fake endpoint for testing various parameters  假端點  偽のエンドポイント  가짜 엔드 포인트
    async fn test_endpoint_parameters(&mut self, number: f64, double: f64, pattern_without_delimiter: String, byte: openapi_context::ByteArray, integer: Option<i32>, int32: Option<i32>, int64: Option<i64>, float: Option<f32>, string: Option<String>, binary: Option<openapi_context::ByteArray>, date: Option<chrono::DateTime<chrono::Utc>>, date_time: Option<chrono::DateTime<chrono::Utc>>, password: Option<String>, callback: Option<String>) -> Result<TestEndpointParametersResponse, ApiError> {
        let ctx: C = self.context().clone();
        self.api_mut().test_endpoint_parameters(number, double, pattern_without_delimiter, byte, integer, int32, int64, float, string, binary, date, date_time, password, callback, &ctx).await
    }

    /// To test enum parameters
    async fn test_enum_parameters(&mut self, enum_header_string_array: Option<&Vec<String>>, enum_header_string: Option<String>, enum_query_string_array: Option<&Vec<String>>, enum_query_string: Option<String>, enum_query_integer: Option<i32>, enum_query_double: Option<f64>, enum_form_string: Option<String>) -> Result<TestEnumParametersResponse, ApiError> {
        let ctx: C = self.context().clone();
        self.api_mut().test_enum_parameters(enum_header_string_array, enum_header_string, enum_query_string_array, enum_query_string, enum_query_integer, enum_query_double, enum_form_string, &ctx).await
    }

    /// test inline additionalProperties
    async fn test_inline_additional_properties(&mut self, param: HashMap<String, String>) -> Result<TestInlineAdditionalPropertiesResponse, ApiError> {
        let ctx: C = self.context().clone();
        self.api_mut().test_inline_additional_properties(param, &ctx).await
    }

    /// test json serialization of form data
    async fn test_json_form_data(&mut self, param: String, param2: String) -> Result<TestJsonFormDataResponse, ApiError> {
        let ctx: C = self.context().clone();
        self.api_mut().test_json_form_data(param, param2, &ctx).await
    }

    /// To test class name in snake case
    async fn test_classname(&mut self, body: crate::models::Client) -> Result<TestClassnameResponse, ApiError> {
        let ctx: C = self.context().clone();
        self.api_mut().test_classname(body, &ctx).await
    }

    /// Add a new pet to the store
    async fn add_pet(&mut self, body: crate::models::Pet) -> Result<AddPetResponse, ApiError> {
        let ctx: C = self.context().clone();
        self.api_mut().add_pet(body, &ctx).await
    }

    /// Deletes a pet
    async fn delete_pet(&mut self, pet_id: i64, api_key: Option<String>) -> Result<DeletePetResponse, ApiError> {
        let ctx: C = self.context().clone();
        self.api_mut().delete_pet(pet_id, api_key, &ctx).await
    }

    /// Finds Pets by status
    async fn find_pets_by_status(&mut self, status: &Vec<String>) -> Result<FindPetsByStatusResponse, ApiError> {
        let ctx: C = self.context().clone();
        self.api_mut().find_pets_by_status(status, &ctx).await
    }

    /// Finds Pets by tags
    async fn find_pets_by_tags(&mut self, tags: &Vec<String>) -> Result<FindPetsByTagsResponse, ApiError> {
        let ctx: C = self.context().clone();
        self.api_mut().find_pets_by_tags(tags, &ctx).await
    }

    /// Find pet by ID
    async fn get_pet_by_id(&mut self, pet_id: i64) -> Result<GetPetByIdResponse, ApiError> {
        let ctx: C = self.context().clone();
        self.api_mut().get_pet_by_id(pet_id, &ctx).await
    }

    /// Update an existing pet
    async fn update_pet(&mut self, body: crate::models::Pet) -> Result<UpdatePetResponse, ApiError> {
        let ctx: C = self.context().clone();
        self.api_mut().update_pet(body, &ctx).await
    }

    /// Updates a pet in the store with form data
    async fn update_pet_with_form(&mut self, pet_id: i64, name: Option<String>, status: Option<String>) -> Result<UpdatePetWithFormResponse, ApiError> {
        let ctx: C = self.context().clone();
        self.api_mut().update_pet_with_form(pet_id, name, status, &ctx).await
    }

    /// uploads an image
    async fn upload_file(&mut self, pet_id: i64, additional_metadata: Option<String>, file: Option<openapi_context::ByteArray>) -> Result<UploadFileResponse, ApiError> {
        let ctx: C = self.context().clone();
        self.api_mut().upload_file(pet_id, additional_metadata, file, &ctx).await
    }

    /// Delete purchase order by ID
    async fn delete_order(&mut self, order_id: String) -> Result<DeleteOrderResponse, ApiError> {
        let ctx: C = self.context().clone();
        self.api_mut().delete_order(order_id, &ctx).await
    }

    /// Returns pet inventories by status
    async fn get_inventory(&mut self) -> Result<GetInventoryResponse, ApiError> {
        let ctx: C = self.context().clone();
        self.api_mut().get_inventory(&ctx).await
    }

    /// Find purchase order by ID
    async fn get_order_by_id(&mut self, order_id: i64) -> Result<GetOrderByIdResponse, ApiError> {
        let ctx: C = self.context().clone();
        self.api_mut().get_order_by_id(order_id, &ctx).await
    }

    /// Place an order for a pet
    async fn place_order(&mut self, body: crate::models::Order) -> Result<PlaceOrderResponse, ApiError> {
        let ctx: C = self.context().clone();
        self.api_mut().place_order(body, &ctx).await
    }

    /// Create user
    async fn create_user(&mut self, body: crate::models::User) -> Result<CreateUserResponse, ApiError> {
        let ctx: C = self.context().clone();
        self.api_mut().create_user(body, &ctx).await
    }

    /// Creates list of users with given input array
    async fn create_users_with_array_input(&mut self, body: &Vec<crate::models::User>) -> Result<CreateUsersWithArrayInputResponse, ApiError> {
        let ctx: C = self.context().clone();
        self.api_mut().create_users_with_array_input(body, &ctx).await
    }

    /// Creates list of users with given input array
    async fn create_users_with_list_input(&mut self, body: &Vec<crate::models::User>) -> Result<CreateUsersWithListInputResponse, ApiError> {
        let ctx: C = self.context().clone();
        self.api_mut().create_users_with_list_input(body, &ctx).await
    }

    /// Delete user
    async fn delete_user(&mut self, username: String) -> Result<DeleteUserResponse, ApiError> {
        let ctx: C = self.context().clone();
        self.api_mut().delete_user(username, &ctx).await
    }

    /// Get user by user name
    async fn get_user_by_name(&mut self, username: String) -> Result<GetUserByNameResponse, ApiError> {
        let ctx: C = self.context().clone();
        self.api_mut().get_user_by_name(username, &ctx).await
    }

    /// Logs user into the system
    async fn login_user(&mut self, username: String, password: String) -> Result<LoginUserResponse, ApiError> {
        let ctx: C = self.context().clone();
        self.api_mut().login_user(username, password, &ctx).await
    }

    /// Logs out current logged in user session
    async fn logout_user(&mut self) -> Result<LogoutUserResponse, ApiError> {
        let ctx: C = self.context().clone();
        self.api_mut().logout_user(&ctx).await
    }

    /// Updated user
    async fn update_user(&mut self, username: String, body: crate::models::User) -> Result<UpdateUserResponse, ApiError> {
        let ctx: C = self.context().clone();
        self.api_mut().update_user(username, body, &ctx).await
    }

}

#[cfg(feature = "client")]
pub mod client;

// Re-export Client as a top-level name
#[cfg(feature = "client")]
pub use self::client::Client;

#[cfg(feature = "server")]
pub mod server;

// Re-export router() as a top-level name
#[cfg(feature = "server")]
pub use self::server::Service;

pub mod models;
#[allow(non_upper_case_globals)]
pub mod headers;
