//! Server implementation of petstore_with_fake_endpoints_models_for_testing.

#![allow(unused_imports)]

use futures::{self, Future};
use chrono;
use std::collections::HashMap;
use std::marker::PhantomData;

use swagger;
use swagger::{Has, XSpanIdString};

use petstore_with_fake_endpoints_models_for_testing::{Api, ApiError,
                      TestSpecialTagsResponse,
                      FakeOuterBooleanSerializeResponse,
                      FakeOuterCompositeSerializeResponse,
                      FakeOuterNumberSerializeResponse,
                      FakeOuterStringSerializeResponse,
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
use petstore_with_fake_endpoints_models_for_testing::models;

#[derive(Copy, Clone)]
pub struct Server<C> {
    marker: PhantomData<C>,
}

impl<C> Server<C> {
    pub fn new() -> Self {
        Server{marker: PhantomData}
    }
}

impl<C> Api<C> for Server<C> where C: Has<XSpanIdString>{

    /// To test special tags
    fn test_special_tags(&self, body: models::Client, context: &C) -> Box<Future<Item=TestSpecialTagsResponse, Error=ApiError>> {
        let context = context.clone();
        println!("test_special_tags({:?}) - X-Span-ID: {:?}", body, context.get().0.clone());
        Box::new(futures::failed("Generic failure".into()))
    }


    fn fake_outer_boolean_serialize(&self, body: Option<bool>, context: &C) -> Box<Future<Item=FakeOuterBooleanSerializeResponse, Error=ApiError>> {
        let context = context.clone();
        println!("fake_outer_boolean_serialize({:?}) - X-Span-ID: {:?}", body, context.get().0.clone());
        Box::new(futures::failed("Generic failure".into()))
    }


    fn fake_outer_composite_serialize(&self, body: Option<models::OuterComposite>, context: &C) -> Box<Future<Item=FakeOuterCompositeSerializeResponse, Error=ApiError>> {
        let context = context.clone();
        println!("fake_outer_composite_serialize({:?}) - X-Span-ID: {:?}", body, context.get().0.clone());
        Box::new(futures::failed("Generic failure".into()))
    }


    fn fake_outer_number_serialize(&self, body: Option<f64>, context: &C) -> Box<Future<Item=FakeOuterNumberSerializeResponse, Error=ApiError>> {
        let context = context.clone();
        println!("fake_outer_number_serialize({:?}) - X-Span-ID: {:?}", body, context.get().0.clone());
        Box::new(futures::failed("Generic failure".into()))
    }


    fn fake_outer_string_serialize(&self, body: Option<String>, context: &C) -> Box<Future<Item=FakeOuterStringSerializeResponse, Error=ApiError>> {
        let context = context.clone();
        println!("fake_outer_string_serialize({:?}) - X-Span-ID: {:?}", body, context.get().0.clone());
        Box::new(futures::failed("Generic failure".into()))
    }


    fn test_body_with_query_params(&self, query: String, body: models::User, context: &C) -> Box<Future<Item=TestBodyWithQueryParamsResponse, Error=ApiError>> {
        let context = context.clone();
        println!("test_body_with_query_params(\"{}\", {:?}) - X-Span-ID: {:?}", query, body, context.get().0.clone());
        Box::new(futures::failed("Generic failure".into()))
    }

    /// To test \"client\" model
    fn test_client_model(&self, body: models::Client, context: &C) -> Box<Future<Item=TestClientModelResponse, Error=ApiError>> {
        let context = context.clone();
        println!("test_client_model({:?}) - X-Span-ID: {:?}", body, context.get().0.clone());
        Box::new(futures::failed("Generic failure".into()))
    }

    /// Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 
    fn test_endpoint_parameters(&self, number: f64, double: f64, pattern_without_delimiter: String, byte: swagger::ByteArray, integer: Option<i32>, int32: Option<i32>, int64: Option<i64>, float: Option<f32>, string: Option<String>, binary: Option<swagger::ByteArray>, date: Option<chrono::DateTime<chrono::Utc>>, date_time: Option<chrono::DateTime<chrono::Utc>>, password: Option<String>, callback: Option<String>, context: &C) -> Box<Future<Item=TestEndpointParametersResponse, Error=ApiError>> {
        let context = context.clone();
        println!("test_endpoint_parameters({}, {}, \"{}\", {:?}, {:?}, {:?}, {:?}, {:?}, {:?}, {:?}, {:?}, {:?}, {:?}, {:?}) - X-Span-ID: {:?}", number, double, pattern_without_delimiter, byte, integer, int32, int64, float, string, binary, date, date_time, password, callback, context.get().0.clone());
        Box::new(futures::failed("Generic failure".into()))
    }

    /// To test enum parameters
    fn test_enum_parameters(&self, enum_header_string_array: Option<&Vec<String>>, enum_header_string: Option<String>, enum_query_string_array: Option<&Vec<String>>, enum_query_string: Option<String>, enum_query_integer: Option<i32>, enum_query_double: Option<f64>, enum_form_string: Option<String>, context: &C) -> Box<Future<Item=TestEnumParametersResponse, Error=ApiError>> {
        let context = context.clone();
        println!("test_enum_parameters({:?}, {:?}, {:?}, {:?}, {:?}, {:?}, {:?}) - X-Span-ID: {:?}", enum_header_string_array, enum_header_string, enum_query_string_array, enum_query_string, enum_query_integer, enum_query_double, enum_form_string, context.get().0.clone());
        Box::new(futures::failed("Generic failure".into()))
    }

    /// test inline additionalProperties
    fn test_inline_additional_properties(&self, param: HashMap<String, String>, context: &C) -> Box<Future<Item=TestInlineAdditionalPropertiesResponse, Error=ApiError>> {
        let context = context.clone();
        println!("test_inline_additional_properties({:?}) - X-Span-ID: {:?}", param, context.get().0.clone());
        Box::new(futures::failed("Generic failure".into()))
    }

    /// test json serialization of form data
    fn test_json_form_data(&self, param: String, param2: String, context: &C) -> Box<Future<Item=TestJsonFormDataResponse, Error=ApiError>> {
        let context = context.clone();
        println!("test_json_form_data(\"{}\", \"{}\") - X-Span-ID: {:?}", param, param2, context.get().0.clone());
        Box::new(futures::failed("Generic failure".into()))
    }

    /// To test class name in snake case
    fn test_classname(&self, body: models::Client, context: &C) -> Box<Future<Item=TestClassnameResponse, Error=ApiError>> {
        let context = context.clone();
        println!("test_classname({:?}) - X-Span-ID: {:?}", body, context.get().0.clone());
        Box::new(futures::failed("Generic failure".into()))
    }

    /// Add a new pet to the store
    fn add_pet(&self, body: models::Pet, context: &C) -> Box<Future<Item=AddPetResponse, Error=ApiError>> {
        let context = context.clone();
        println!("add_pet({:?}) - X-Span-ID: {:?}", body, context.get().0.clone());
        Box::new(futures::failed("Generic failure".into()))
    }

    /// Deletes a pet
    fn delete_pet(&self, pet_id: i64, api_key: Option<String>, context: &C) -> Box<Future<Item=DeletePetResponse, Error=ApiError>> {
        let context = context.clone();
        println!("delete_pet({}, {:?}) - X-Span-ID: {:?}", pet_id, api_key, context.get().0.clone());
        Box::new(futures::failed("Generic failure".into()))
    }

    /// Finds Pets by status
    fn find_pets_by_status(&self, status: &Vec<String>, context: &C) -> Box<Future<Item=FindPetsByStatusResponse, Error=ApiError>> {
        let context = context.clone();
        println!("find_pets_by_status({:?}) - X-Span-ID: {:?}", status, context.get().0.clone());
        Box::new(futures::failed("Generic failure".into()))
    }

    /// Finds Pets by tags
    fn find_pets_by_tags(&self, tags: &Vec<String>, context: &C) -> Box<Future<Item=FindPetsByTagsResponse, Error=ApiError>> {
        let context = context.clone();
        println!("find_pets_by_tags({:?}) - X-Span-ID: {:?}", tags, context.get().0.clone());
        Box::new(futures::failed("Generic failure".into()))
    }

    /// Find pet by ID
    fn get_pet_by_id(&self, pet_id: i64, context: &C) -> Box<Future<Item=GetPetByIdResponse, Error=ApiError>> {
        let context = context.clone();
        println!("get_pet_by_id({}) - X-Span-ID: {:?}", pet_id, context.get().0.clone());
        Box::new(futures::failed("Generic failure".into()))
    }

    /// Update an existing pet
    fn update_pet(&self, body: models::Pet, context: &C) -> Box<Future<Item=UpdatePetResponse, Error=ApiError>> {
        let context = context.clone();
        println!("update_pet({:?}) - X-Span-ID: {:?}", body, context.get().0.clone());
        Box::new(futures::failed("Generic failure".into()))
    }

    /// Updates a pet in the store with form data
    fn update_pet_with_form(&self, pet_id: i64, name: Option<String>, status: Option<String>, context: &C) -> Box<Future<Item=UpdatePetWithFormResponse, Error=ApiError>> {
        let context = context.clone();
        println!("update_pet_with_form({}, {:?}, {:?}) - X-Span-ID: {:?}", pet_id, name, status, context.get().0.clone());
        Box::new(futures::failed("Generic failure".into()))
    }

    /// uploads an image
    fn upload_file(&self, pet_id: i64, additional_metadata: Option<String>, file: Option<swagger::ByteArray>, context: &C) -> Box<Future<Item=UploadFileResponse, Error=ApiError>> {
        let context = context.clone();
        println!("upload_file({}, {:?}, {:?}) - X-Span-ID: {:?}", pet_id, additional_metadata, file, context.get().0.clone());
        Box::new(futures::failed("Generic failure".into()))
    }

    /// Delete purchase order by ID
    fn delete_order(&self, order_id: String, context: &C) -> Box<Future<Item=DeleteOrderResponse, Error=ApiError>> {
        let context = context.clone();
        println!("delete_order(\"{}\") - X-Span-ID: {:?}", order_id, context.get().0.clone());
        Box::new(futures::failed("Generic failure".into()))
    }

    /// Returns pet inventories by status
    fn get_inventory(&self, context: &C) -> Box<Future<Item=GetInventoryResponse, Error=ApiError>> {
        let context = context.clone();
        println!("get_inventory() - X-Span-ID: {:?}", context.get().0.clone());
        Box::new(futures::failed("Generic failure".into()))
    }

    /// Find purchase order by ID
    fn get_order_by_id(&self, order_id: i64, context: &C) -> Box<Future<Item=GetOrderByIdResponse, Error=ApiError>> {
        let context = context.clone();
        println!("get_order_by_id({}) - X-Span-ID: {:?}", order_id, context.get().0.clone());
        Box::new(futures::failed("Generic failure".into()))
    }

    /// Place an order for a pet
    fn place_order(&self, body: models::Order, context: &C) -> Box<Future<Item=PlaceOrderResponse, Error=ApiError>> {
        let context = context.clone();
        println!("place_order({:?}) - X-Span-ID: {:?}", body, context.get().0.clone());
        Box::new(futures::failed("Generic failure".into()))
    }

    /// Create user
    fn create_user(&self, body: models::User, context: &C) -> Box<Future<Item=CreateUserResponse, Error=ApiError>> {
        let context = context.clone();
        println!("create_user({:?}) - X-Span-ID: {:?}", body, context.get().0.clone());
        Box::new(futures::failed("Generic failure".into()))
    }

    /// Creates list of users with given input array
    fn create_users_with_array_input(&self, body: &Vec<models::User>, context: &C) -> Box<Future<Item=CreateUsersWithArrayInputResponse, Error=ApiError>> {
        let context = context.clone();
        println!("create_users_with_array_input({:?}) - X-Span-ID: {:?}", body, context.get().0.clone());
        Box::new(futures::failed("Generic failure".into()))
    }

    /// Creates list of users with given input array
    fn create_users_with_list_input(&self, body: &Vec<models::User>, context: &C) -> Box<Future<Item=CreateUsersWithListInputResponse, Error=ApiError>> {
        let context = context.clone();
        println!("create_users_with_list_input({:?}) - X-Span-ID: {:?}", body, context.get().0.clone());
        Box::new(futures::failed("Generic failure".into()))
    }

    /// Delete user
    fn delete_user(&self, username: String, context: &C) -> Box<Future<Item=DeleteUserResponse, Error=ApiError>> {
        let context = context.clone();
        println!("delete_user(\"{}\") - X-Span-ID: {:?}", username, context.get().0.clone());
        Box::new(futures::failed("Generic failure".into()))
    }

    /// Get user by user name
    fn get_user_by_name(&self, username: String, context: &C) -> Box<Future<Item=GetUserByNameResponse, Error=ApiError>> {
        let context = context.clone();
        println!("get_user_by_name(\"{}\") - X-Span-ID: {:?}", username, context.get().0.clone());
        Box::new(futures::failed("Generic failure".into()))
    }

    /// Logs user into the system
    fn login_user(&self, username: String, password: String, context: &C) -> Box<Future<Item=LoginUserResponse, Error=ApiError>> {
        let context = context.clone();
        println!("login_user(\"{}\", \"{}\") - X-Span-ID: {:?}", username, password, context.get().0.clone());
        Box::new(futures::failed("Generic failure".into()))
    }

    /// Logs out current logged in user session
    fn logout_user(&self, context: &C) -> Box<Future<Item=LogoutUserResponse, Error=ApiError>> {
        let context = context.clone();
        println!("logout_user() - X-Span-ID: {:?}", context.get().0.clone());
        Box::new(futures::failed("Generic failure".into()))
    }

    /// Updated user
    fn update_user(&self, username: String, body: models::User, context: &C) -> Box<Future<Item=UpdateUserResponse, Error=ApiError>> {
        let context = context.clone();
        println!("update_user(\"{}\", {:?}) - X-Span-ID: {:?}", username, body, context.get().0.clone());
        Box::new(futures::failed("Generic failure".into()))
    }

}
