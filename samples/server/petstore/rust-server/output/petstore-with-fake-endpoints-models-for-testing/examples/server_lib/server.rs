//! Server implementation of petstore_with_fake_endpoints_models_for_testing.

#![allow(unused_imports)]
use async_trait::async_trait;
use chrono;
use std::collections::HashMap;
use std::marker::PhantomData;
use openapi_context;
use openapi_context::{Has, XSpanId};
use uuid;

use petstore_with_fake_endpoints_models_for_testing::{Api, ApiError,
                      TestSpecialTagsResponse,
                      FakeOuterBooleanSerializeResponse,
                      FakeOuterCompositeSerializeResponse,
                      FakeOuterNumberSerializeResponse,
                      FakeOuterStringSerializeResponse,
                      HyphenParamResponse,
                      TestBodyWithQueryParamsResponse,
                      TestClientModelResponse,
                      TestEndpointParametersResponse,
                      TestEnumParametersResponse,
                      TestInlineAdditionalPropertiesResponse,
                      TestJsonFormDataResponse,
                      TestClassnameResponse,
                      AddPetResponse,
                      DeletePetResponse,
                      FindPetsByStatusResponse,
                      FindPetsByTagsResponse,
                      GetPetByIdResponse,
                      UpdatePetResponse,
                      UpdatePetWithFormResponse,
                      UploadFileResponse,
                      DeleteOrderResponse,
                      GetInventoryResponse,
                      GetOrderByIdResponse,
                      PlaceOrderResponse,
                      CreateUserResponse,
                      CreateUsersWithArrayInputResponse,
                      CreateUsersWithListInputResponse,
                      DeleteUserResponse,
                      GetUserByNameResponse,
                      LoginUserResponse,
                      LogoutUserResponse,
                      UpdateUserResponse
};

#[derive(Copy, Clone)]
pub struct Server<C> {
    marker: PhantomData<C>,
}

impl<C> Server<C> {
    pub fn new() -> Self {
        Server{marker: PhantomData}
    }
}

#[async_trait]
impl<C> Api<C> for Server<C> where C: Has<XSpanId> + Send + Sync {

    /// To test special tags
    async fn test_special_tags(&mut self, body: crate::models::Client, context: &C) -> Result<TestSpecialTagsResponse, ApiError> {
        let context = context.clone();
        println!("test_special_tags({:?}) - X-Span-ID: {:?}", body, context.get().0.clone());
        Err("Generic failure".into())
    }


    async fn fake_outer_boolean_serialize(&mut self, body: Option<crate::models::OuterBoolean>, context: &C) -> Result<FakeOuterBooleanSerializeResponse, ApiError> {
        let context = context.clone();
        println!("fake_outer_boolean_serialize({:?}) - X-Span-ID: {:?}", body, context.get().0.clone());
        Err("Generic failure".into())
    }


    async fn fake_outer_composite_serialize(&mut self, body: Option<crate::models::OuterComposite>, context: &C) -> Result<FakeOuterCompositeSerializeResponse, ApiError> {
        let context = context.clone();
        println!("fake_outer_composite_serialize({:?}) - X-Span-ID: {:?}", body, context.get().0.clone());
        Err("Generic failure".into())
    }


    async fn fake_outer_number_serialize(&mut self, body: Option<crate::models::OuterNumber>, context: &C) -> Result<FakeOuterNumberSerializeResponse, ApiError> {
        let context = context.clone();
        println!("fake_outer_number_serialize({:?}) - X-Span-ID: {:?}", body, context.get().0.clone());
        Err("Generic failure".into())
    }


    async fn fake_outer_string_serialize(&mut self, body: Option<crate::models::OuterString>, context: &C) -> Result<FakeOuterStringSerializeResponse, ApiError> {
        let context = context.clone();
        println!("fake_outer_string_serialize({:?}) - X-Span-ID: {:?}", body, context.get().0.clone());
        Err("Generic failure".into())
    }


    async fn hyphen_param(&mut self, hyphen_param: String, context: &C) -> Result<HyphenParamResponse, ApiError> {
        let context = context.clone();
        println!("hyphen_param(\"{}\") - X-Span-ID: {:?}", hyphen_param, context.get().0.clone());
        Err("Generic failure".into())
    }


    async fn test_body_with_query_params(&mut self, query: String, body: crate::models::User, context: &C) -> Result<TestBodyWithQueryParamsResponse, ApiError> {
        let context = context.clone();
        println!("test_body_with_query_params(\"{}\", {:?}) - X-Span-ID: {:?}", query, body, context.get().0.clone());
        Err("Generic failure".into())
    }

    /// To test \"client\" model
    async fn test_client_model(&mut self, body: crate::models::Client, context: &C) -> Result<TestClientModelResponse, ApiError> {
        let context = context.clone();
        println!("test_client_model({:?}) - X-Span-ID: {:?}", body, context.get().0.clone());
        Err("Generic failure".into())
    }

    /// Fake endpoint for testing various parameters  假端點  偽のエンドポイント  가짜 엔드 포인트
    async fn test_endpoint_parameters(&mut self, number: f64, double: f64, pattern_without_delimiter: String, byte: openapi_context::ByteArray, integer: Option<i32>, int32: Option<i32>, int64: Option<i64>, float: Option<f32>, string: Option<String>, binary: Option<openapi_context::ByteArray>, date: Option<chrono::DateTime<chrono::Utc>>, date_time: Option<chrono::DateTime<chrono::Utc>>, password: Option<String>, callback: Option<String>, context: &C) -> Result<TestEndpointParametersResponse, ApiError> {
        let context = context.clone();
        println!("test_endpoint_parameters({}, {}, \"{}\", {:?}, {:?}, {:?}, {:?}, {:?}, {:?}, {:?}, {:?}, {:?}, {:?}, {:?}) - X-Span-ID: {:?}", number, double, pattern_without_delimiter, byte, integer, int32, int64, float, string, binary, date, date_time, password, callback, context.get().0.clone());
        Err("Generic failure".into())
    }

    /// To test enum parameters
    async fn test_enum_parameters(&mut self, enum_header_string_array: Option<&Vec<String>>, enum_header_string: Option<String>, enum_query_string_array: Option<&Vec<String>>, enum_query_string: Option<String>, enum_query_integer: Option<i32>, enum_query_double: Option<f64>, enum_form_string: Option<String>, context: &C) -> Result<TestEnumParametersResponse, ApiError> {
        let context = context.clone();
        println!("test_enum_parameters({:?}, {:?}, {:?}, {:?}, {:?}, {:?}, {:?}) - X-Span-ID: {:?}", enum_header_string_array, enum_header_string, enum_query_string_array, enum_query_string, enum_query_integer, enum_query_double, enum_form_string, context.get().0.clone());
        Err("Generic failure".into())
    }

    /// test inline additionalProperties
    async fn test_inline_additional_properties(&mut self, param: HashMap<String, String>, context: &C) -> Result<TestInlineAdditionalPropertiesResponse, ApiError> {
        let context = context.clone();
        println!("test_inline_additional_properties({:?}) - X-Span-ID: {:?}", param, context.get().0.clone());
        Err("Generic failure".into())
    }

    /// test json serialization of form data
    async fn test_json_form_data(&mut self, param: String, param2: String, context: &C) -> Result<TestJsonFormDataResponse, ApiError> {
        let context = context.clone();
        println!("test_json_form_data(\"{}\", \"{}\") - X-Span-ID: {:?}", param, param2, context.get().0.clone());
        Err("Generic failure".into())
    }

    /// To test class name in snake case
    async fn test_classname(&mut self, body: crate::models::Client, context: &C) -> Result<TestClassnameResponse, ApiError> {
        let context = context.clone();
        println!("test_classname({:?}) - X-Span-ID: {:?}", body, context.get().0.clone());
        Err("Generic failure".into())
    }

    /// Add a new pet to the store
    async fn add_pet(&mut self, body: crate::models::Pet, context: &C) -> Result<AddPetResponse, ApiError> {
        let context = context.clone();
        println!("add_pet({:?}) - X-Span-ID: {:?}", body, context.get().0.clone());
        Err("Generic failure".into())
    }

    /// Deletes a pet
    async fn delete_pet(&mut self, pet_id: i64, api_key: Option<String>, context: &C) -> Result<DeletePetResponse, ApiError> {
        let context = context.clone();
        println!("delete_pet({}, {:?}) - X-Span-ID: {:?}", pet_id, api_key, context.get().0.clone());
        Err("Generic failure".into())
    }

    /// Finds Pets by status
    async fn find_pets_by_status(&mut self, status: &Vec<String>, context: &C) -> Result<FindPetsByStatusResponse, ApiError> {
        let context = context.clone();
        println!("find_pets_by_status({:?}) - X-Span-ID: {:?}", status, context.get().0.clone());
        Err("Generic failure".into())
    }

    /// Finds Pets by tags
    async fn find_pets_by_tags(&mut self, tags: &Vec<String>, context: &C) -> Result<FindPetsByTagsResponse, ApiError> {
        let context = context.clone();
        println!("find_pets_by_tags({:?}) - X-Span-ID: {:?}", tags, context.get().0.clone());
        Err("Generic failure".into())
    }

    /// Find pet by ID
    async fn get_pet_by_id(&mut self, pet_id: i64, context: &C) -> Result<GetPetByIdResponse, ApiError> {
        let context = context.clone();
        println!("get_pet_by_id({}) - X-Span-ID: {:?}", pet_id, context.get().0.clone());
        Err("Generic failure".into())
    }

    /// Update an existing pet
    async fn update_pet(&mut self, body: crate::models::Pet, context: &C) -> Result<UpdatePetResponse, ApiError> {
        let context = context.clone();
        println!("update_pet({:?}) - X-Span-ID: {:?}", body, context.get().0.clone());
        Err("Generic failure".into())
    }

    /// Updates a pet in the store with form data
    async fn update_pet_with_form(&mut self, pet_id: i64, name: Option<String>, status: Option<String>, context: &C) -> Result<UpdatePetWithFormResponse, ApiError> {
        let context = context.clone();
        println!("update_pet_with_form({}, {:?}, {:?}) - X-Span-ID: {:?}", pet_id, name, status, context.get().0.clone());
        Err("Generic failure".into())
    }

    /// uploads an image
    async fn upload_file(&mut self, pet_id: i64, additional_metadata: Option<String>, file: Option<openapi_context::ByteArray>, context: &C) -> Result<UploadFileResponse, ApiError> {
        let context = context.clone();
        println!("upload_file({}, {:?}, {:?}) - X-Span-ID: {:?}", pet_id, additional_metadata, file, context.get().0.clone());
        Err("Generic failure".into())
    }

    /// Delete purchase order by ID
    async fn delete_order(&mut self, order_id: String, context: &C) -> Result<DeleteOrderResponse, ApiError> {
        let context = context.clone();
        println!("delete_order(\"{}\") - X-Span-ID: {:?}", order_id, context.get().0.clone());
        Err("Generic failure".into())
    }

    /// Returns pet inventories by status
    async fn get_inventory(&mut self, context: &C) -> Result<GetInventoryResponse, ApiError> {
        let context = context.clone();
        println!("get_inventory() - X-Span-ID: {:?}", context.get().0.clone());
        Err("Generic failure".into())
    }

    /// Find purchase order by ID
    async fn get_order_by_id(&mut self, order_id: i64, context: &C) -> Result<GetOrderByIdResponse, ApiError> {
        let context = context.clone();
        println!("get_order_by_id({}) - X-Span-ID: {:?}", order_id, context.get().0.clone());
        Err("Generic failure".into())
    }

    /// Place an order for a pet
    async fn place_order(&mut self, body: crate::models::Order, context: &C) -> Result<PlaceOrderResponse, ApiError> {
        let context = context.clone();
        println!("place_order({:?}) - X-Span-ID: {:?}", body, context.get().0.clone());
        Err("Generic failure".into())
    }

    /// Create user
    async fn create_user(&mut self, body: crate::models::User, context: &C) -> Result<CreateUserResponse, ApiError> {
        let context = context.clone();
        println!("create_user({:?}) - X-Span-ID: {:?}", body, context.get().0.clone());
        Err("Generic failure".into())
    }

    /// Creates list of users with given input array
    async fn create_users_with_array_input(&mut self, body: &Vec<crate::models::User>, context: &C) -> Result<CreateUsersWithArrayInputResponse, ApiError> {
        let context = context.clone();
        println!("create_users_with_array_input({:?}) - X-Span-ID: {:?}", body, context.get().0.clone());
        Err("Generic failure".into())
    }

    /// Creates list of users with given input array
    async fn create_users_with_list_input(&mut self, body: &Vec<crate::models::User>, context: &C) -> Result<CreateUsersWithListInputResponse, ApiError> {
        let context = context.clone();
        println!("create_users_with_list_input({:?}) - X-Span-ID: {:?}", body, context.get().0.clone());
        Err("Generic failure".into())
    }

    /// Delete user
    async fn delete_user(&mut self, username: String, context: &C) -> Result<DeleteUserResponse, ApiError> {
        let context = context.clone();
        println!("delete_user(\"{}\") - X-Span-ID: {:?}", username, context.get().0.clone());
        Err("Generic failure".into())
    }

    /// Get user by user name
    async fn get_user_by_name(&mut self, username: String, context: &C) -> Result<GetUserByNameResponse, ApiError> {
        let context = context.clone();
        println!("get_user_by_name(\"{}\") - X-Span-ID: {:?}", username, context.get().0.clone());
        Err("Generic failure".into())
    }

    /// Logs user into the system
    async fn login_user(&mut self, username: String, password: String, context: &C) -> Result<LoginUserResponse, ApiError> {
        let context = context.clone();
        println!("login_user(\"{}\", \"{}\") - X-Span-ID: {:?}", username, password, context.get().0.clone());
        Err("Generic failure".into())
    }

    /// Logs out current logged in user session
    async fn logout_user(&mut self, context: &C) -> Result<LogoutUserResponse, ApiError> {
        let context = context.clone();
        println!("logout_user() - X-Span-ID: {:?}", context.get().0.clone());
        Err("Generic failure".into())
    }

    /// Updated user
    async fn update_user(&mut self, username: String, body: crate::models::User, context: &C) -> Result<UpdateUserResponse, ApiError> {
        let context = context.clone();
        println!("update_user(\"{}\", {:?}) - X-Span-ID: {:?}", username, body, context.get().0.clone());
        Err("Generic failure".into())
    }

}
