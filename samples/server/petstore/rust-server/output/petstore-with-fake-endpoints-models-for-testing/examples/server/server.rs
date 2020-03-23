//! Main library entry point for petstore_with_fake_endpoints_models_for_testing implementation.

#![allow(unused_imports)]

mod errors {
    error_chain!{}
}

pub use self::errors::*;

use chrono;
use futures::{future, Future, Stream};
use hyper::server::conn::Http;
use hyper::service::MakeService as _;
use openssl::ssl::SslAcceptorBuilder;
use std::collections::HashMap;
use std::marker::PhantomData;
use std::net::SocketAddr;
use std::sync::{Arc, Mutex};
use swagger;
use swagger::{Has, XSpanIdString};
use swagger::auth::MakeAllowAllAuthenticator;
use swagger::EmptyContext;
use tokio::net::TcpListener;
use uuid;

#[cfg(not(any(target_os = "macos", target_os = "windows", target_os = "ios")))]
use tokio_openssl::SslAcceptorExt;
#[cfg(not(any(target_os = "macos", target_os = "windows", target_os = "ios")))]
use openssl::ssl::{SslAcceptor, SslFiletype, SslMethod};

use petstore_with_fake_endpoints_models_for_testing::models;

#[cfg(not(any(target_os = "macos", target_os = "windows", target_os = "ios")))]
/// Builds an SSL implementation for Simple HTTPS from some hard-coded file names
pub fn create(addr: &str, https: bool) -> Box<dyn Future<Item = (), Error = ()> + Send> {
    let addr = addr.parse().expect("Failed to parse bind address");

    let server = Server::new();

    let service_fn = MakeService::new(server);

    let service_fn = MakeAllowAllAuthenticator::new(service_fn, "cosmo");

    let service_fn =
        petstore_with_fake_endpoints_models_for_testing::server::context::MakeAddContext::<_, EmptyContext>::new(
            service_fn
        );

    if https {
        #[cfg(any(target_os = "macos", target_os = "windows", target_os = "ios"))]
        {
            unimplemented!("SSL is not implemented for the examples on MacOS, Windows or iOS");
        }

        #[cfg(not(any(target_os = "macos", target_os = "windows", target_os = "ios")))]
        {
            let mut ssl = SslAcceptor::mozilla_intermediate_v5(SslMethod::tls()).expect("Failed to create SSL Acceptor");

            // Server authentication
            ssl.set_private_key_file("examples/server-key.pem", SslFiletype::PEM).expect("Failed to set private key");
            ssl.set_certificate_chain_file("examples/server-chain.pem").expect("Failed to set cerificate chain");
            ssl.check_private_key().expect("Failed to check private key");

            let tls_acceptor = ssl.build();
            let service_fn = Arc::new(Mutex::new(service_fn));
            let tls_listener = TcpListener::bind(&addr).unwrap().incoming().for_each(move |tcp| {
                let addr = tcp.peer_addr().expect("Unable to get remote address");

                let service_fn = service_fn.clone();

                hyper::rt::spawn(tls_acceptor.accept_async(tcp).map_err(|_| ()).and_then(move |tls| {
                    let ms = {
                        let mut service_fn = service_fn.lock().unwrap();
                        service_fn.make_service(&addr)
                    };

                    ms.and_then(move |service| {
                        Http::new().serve_connection(tls, service)
                    }).map_err(|_| ())
                }));

                Ok(())
            }).map_err(|_| ());

            Box::new(tls_listener)
        }
    } else {
        // Using HTTP
        Box::new(hyper::server::Server::bind(&addr).serve(service_fn).map_err(|e| panic!("{:?}", e)))
    }
}

#[derive(Copy, Clone)]
pub struct Server<C> {
    marker: PhantomData<C>,
}

impl<C> Server<C> {
    pub fn new() -> Self {
        Server{marker: PhantomData}
    }
}


use petstore_with_fake_endpoints_models_for_testing::{
    Api,
    ApiError,
    TestSpecialTagsResponse,
    Call123exampleResponse,
    FakeOuterBooleanSerializeResponse,
    FakeOuterCompositeSerializeResponse,
    FakeOuterNumberSerializeResponse,
    FakeOuterStringSerializeResponse,
    FakeResponseWithNumericalDescriptionResponse,
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
    UpdateUserResponse,
};
use petstore_with_fake_endpoints_models_for_testing::server::MakeService;

impl<C> Api<C> for Server<C> where C: Has<XSpanIdString>{
    /// To test special tags
    fn test_special_tags(
        &self,
        body: models::Client,
        context: &C) -> Box<Future<Item=TestSpecialTagsResponse, Error=ApiError> + Send>
    {
        let context = context.clone();
        info!("test_special_tags({:?}) - X-Span-ID: {:?}", body, context.get().0.clone());
        Box::new(future::err("Generic failure".into()))
    }

    fn call123example(
        &self,
        context: &C) -> Box<Future<Item=Call123exampleResponse, Error=ApiError> + Send>
    {
        let context = context.clone();
        info!("call123example() - X-Span-ID: {:?}", context.get().0.clone());
        Box::new(future::err("Generic failure".into()))
    }

    fn fake_outer_boolean_serialize(
        &self,
        body: Option<models::OuterBoolean>,
        context: &C) -> Box<Future<Item=FakeOuterBooleanSerializeResponse, Error=ApiError> + Send>
    {
        let context = context.clone();
        info!("fake_outer_boolean_serialize({:?}) - X-Span-ID: {:?}", body, context.get().0.clone());
        Box::new(future::err("Generic failure".into()))
    }

    fn fake_outer_composite_serialize(
        &self,
        body: Option<models::OuterComposite>,
        context: &C) -> Box<Future<Item=FakeOuterCompositeSerializeResponse, Error=ApiError> + Send>
    {
        let context = context.clone();
        info!("fake_outer_composite_serialize({:?}) - X-Span-ID: {:?}", body, context.get().0.clone());
        Box::new(future::err("Generic failure".into()))
    }

    fn fake_outer_number_serialize(
        &self,
        body: Option<models::OuterNumber>,
        context: &C) -> Box<Future<Item=FakeOuterNumberSerializeResponse, Error=ApiError> + Send>
    {
        let context = context.clone();
        info!("fake_outer_number_serialize({:?}) - X-Span-ID: {:?}", body, context.get().0.clone());
        Box::new(future::err("Generic failure".into()))
    }

    fn fake_outer_string_serialize(
        &self,
        body: Option<models::OuterString>,
        context: &C) -> Box<Future<Item=FakeOuterStringSerializeResponse, Error=ApiError> + Send>
    {
        let context = context.clone();
        info!("fake_outer_string_serialize({:?}) - X-Span-ID: {:?}", body, context.get().0.clone());
        Box::new(future::err("Generic failure".into()))
    }

    fn fake_response_with_numerical_description(
        &self,
        context: &C) -> Box<Future<Item=FakeResponseWithNumericalDescriptionResponse, Error=ApiError> + Send>
    {
        let context = context.clone();
        info!("fake_response_with_numerical_description() - X-Span-ID: {:?}", context.get().0.clone());
        Box::new(future::err("Generic failure".into()))
    }

    fn hyphen_param(
        &self,
        hyphen_param: String,
        context: &C) -> Box<Future<Item=HyphenParamResponse, Error=ApiError> + Send>
    {
        let context = context.clone();
        info!("hyphen_param(\"{}\") - X-Span-ID: {:?}", hyphen_param, context.get().0.clone());
        Box::new(future::err("Generic failure".into()))
    }

    fn test_body_with_query_params(
        &self,
        query: String,
        body: models::User,
        context: &C) -> Box<Future<Item=TestBodyWithQueryParamsResponse, Error=ApiError> + Send>
    {
        let context = context.clone();
        info!("test_body_with_query_params(\"{}\", {:?}) - X-Span-ID: {:?}", query, body, context.get().0.clone());
        Box::new(future::err("Generic failure".into()))
    }

    /// To test \"client\" model
    fn test_client_model(
        &self,
        body: models::Client,
        context: &C) -> Box<Future<Item=TestClientModelResponse, Error=ApiError> + Send>
    {
        let context = context.clone();
        info!("test_client_model({:?}) - X-Span-ID: {:?}", body, context.get().0.clone());
        Box::new(future::err("Generic failure".into()))
    }

    /// Fake endpoint for testing various parameters  假端點  偽のエンドポイント  가짜 엔드 포인트
    fn test_endpoint_parameters(
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
        context: &C) -> Box<Future<Item=TestEndpointParametersResponse, Error=ApiError> + Send>
    {
        let context = context.clone();
        info!("test_endpoint_parameters({}, {}, \"{}\", {:?}, {:?}, {:?}, {:?}, {:?}, {:?}, {:?}, {:?}, {:?}, {:?}, {:?}) - X-Span-ID: {:?}", number, double, pattern_without_delimiter, byte, integer, int32, int64, float, string, binary, date, date_time, password, callback, context.get().0.clone());
        Box::new(future::err("Generic failure".into()))
    }

    /// To test enum parameters
    fn test_enum_parameters(
        &self,
        enum_header_string_array: Option<&Vec<String>>,
        enum_header_string: Option<String>,
        enum_query_string_array: Option<&Vec<String>>,
        enum_query_string: Option<String>,
        enum_query_integer: Option<i32>,
        enum_query_double: Option<f64>,
        enum_form_string: Option<String>,
        context: &C) -> Box<Future<Item=TestEnumParametersResponse, Error=ApiError> + Send>
    {
        let context = context.clone();
        info!("test_enum_parameters({:?}, {:?}, {:?}, {:?}, {:?}, {:?}, {:?}) - X-Span-ID: {:?}", enum_header_string_array, enum_header_string, enum_query_string_array, enum_query_string, enum_query_integer, enum_query_double, enum_form_string, context.get().0.clone());
        Box::new(future::err("Generic failure".into()))
    }

    /// test inline additionalProperties
    fn test_inline_additional_properties(
        &self,
        param: HashMap<String, String>,
        context: &C) -> Box<Future<Item=TestInlineAdditionalPropertiesResponse, Error=ApiError> + Send>
    {
        let context = context.clone();
        info!("test_inline_additional_properties({:?}) - X-Span-ID: {:?}", param, context.get().0.clone());
        Box::new(future::err("Generic failure".into()))
    }

    /// test json serialization of form data
    fn test_json_form_data(
        &self,
        param: String,
        param2: String,
        context: &C) -> Box<Future<Item=TestJsonFormDataResponse, Error=ApiError> + Send>
    {
        let context = context.clone();
        info!("test_json_form_data(\"{}\", \"{}\") - X-Span-ID: {:?}", param, param2, context.get().0.clone());
        Box::new(future::err("Generic failure".into()))
    }

    /// To test class name in snake case
    fn test_classname(
        &self,
        body: models::Client,
        context: &C) -> Box<Future<Item=TestClassnameResponse, Error=ApiError> + Send>
    {
        let context = context.clone();
        info!("test_classname({:?}) - X-Span-ID: {:?}", body, context.get().0.clone());
        Box::new(future::err("Generic failure".into()))
    }

    /// Add a new pet to the store
    fn add_pet(
        &self,
        body: models::Pet,
        context: &C) -> Box<Future<Item=AddPetResponse, Error=ApiError> + Send>
    {
        let context = context.clone();
        info!("add_pet({:?}) - X-Span-ID: {:?}", body, context.get().0.clone());
        Box::new(future::err("Generic failure".into()))
    }

    /// Deletes a pet
    fn delete_pet(
        &self,
        pet_id: i64,
        api_key: Option<String>,
        context: &C) -> Box<Future<Item=DeletePetResponse, Error=ApiError> + Send>
    {
        let context = context.clone();
        info!("delete_pet({}, {:?}) - X-Span-ID: {:?}", pet_id, api_key, context.get().0.clone());
        Box::new(future::err("Generic failure".into()))
    }

    /// Finds Pets by status
    fn find_pets_by_status(
        &self,
        status: &Vec<String>,
        context: &C) -> Box<Future<Item=FindPetsByStatusResponse, Error=ApiError> + Send>
    {
        let context = context.clone();
        info!("find_pets_by_status({:?}) - X-Span-ID: {:?}", status, context.get().0.clone());
        Box::new(future::err("Generic failure".into()))
    }

    /// Finds Pets by tags
    fn find_pets_by_tags(
        &self,
        tags: &Vec<String>,
        context: &C) -> Box<Future<Item=FindPetsByTagsResponse, Error=ApiError> + Send>
    {
        let context = context.clone();
        info!("find_pets_by_tags({:?}) - X-Span-ID: {:?}", tags, context.get().0.clone());
        Box::new(future::err("Generic failure".into()))
    }

    /// Find pet by ID
    fn get_pet_by_id(
        &self,
        pet_id: i64,
        context: &C) -> Box<Future<Item=GetPetByIdResponse, Error=ApiError> + Send>
    {
        let context = context.clone();
        info!("get_pet_by_id({}) - X-Span-ID: {:?}", pet_id, context.get().0.clone());
        Box::new(future::err("Generic failure".into()))
    }

    /// Update an existing pet
    fn update_pet(
        &self,
        body: models::Pet,
        context: &C) -> Box<Future<Item=UpdatePetResponse, Error=ApiError> + Send>
    {
        let context = context.clone();
        info!("update_pet({:?}) - X-Span-ID: {:?}", body, context.get().0.clone());
        Box::new(future::err("Generic failure".into()))
    }

    /// Updates a pet in the store with form data
    fn update_pet_with_form(
        &self,
        pet_id: i64,
        name: Option<String>,
        status: Option<String>,
        context: &C) -> Box<Future<Item=UpdatePetWithFormResponse, Error=ApiError> + Send>
    {
        let context = context.clone();
        info!("update_pet_with_form({}, {:?}, {:?}) - X-Span-ID: {:?}", pet_id, name, status, context.get().0.clone());
        Box::new(future::err("Generic failure".into()))
    }

    /// uploads an image
    fn upload_file(
        &self,
        pet_id: i64,
        additional_metadata: Option<String>,
        file: Option<swagger::ByteArray>,
        context: &C) -> Box<Future<Item=UploadFileResponse, Error=ApiError> + Send>
    {
        let context = context.clone();
        info!("upload_file({}, {:?}, {:?}) - X-Span-ID: {:?}", pet_id, additional_metadata, file, context.get().0.clone());
        Box::new(future::err("Generic failure".into()))
    }

    /// Delete purchase order by ID
    fn delete_order(
        &self,
        order_id: String,
        context: &C) -> Box<Future<Item=DeleteOrderResponse, Error=ApiError> + Send>
    {
        let context = context.clone();
        info!("delete_order(\"{}\") - X-Span-ID: {:?}", order_id, context.get().0.clone());
        Box::new(future::err("Generic failure".into()))
    }

    /// Returns pet inventories by status
    fn get_inventory(
        &self,
        context: &C) -> Box<Future<Item=GetInventoryResponse, Error=ApiError> + Send>
    {
        let context = context.clone();
        info!("get_inventory() - X-Span-ID: {:?}", context.get().0.clone());
        Box::new(future::err("Generic failure".into()))
    }

    /// Find purchase order by ID
    fn get_order_by_id(
        &self,
        order_id: i64,
        context: &C) -> Box<Future<Item=GetOrderByIdResponse, Error=ApiError> + Send>
    {
        let context = context.clone();
        info!("get_order_by_id({}) - X-Span-ID: {:?}", order_id, context.get().0.clone());
        Box::new(future::err("Generic failure".into()))
    }

    /// Place an order for a pet
    fn place_order(
        &self,
        body: models::Order,
        context: &C) -> Box<Future<Item=PlaceOrderResponse, Error=ApiError> + Send>
    {
        let context = context.clone();
        info!("place_order({:?}) - X-Span-ID: {:?}", body, context.get().0.clone());
        Box::new(future::err("Generic failure".into()))
    }

    /// Create user
    fn create_user(
        &self,
        body: models::User,
        context: &C) -> Box<Future<Item=CreateUserResponse, Error=ApiError> + Send>
    {
        let context = context.clone();
        info!("create_user({:?}) - X-Span-ID: {:?}", body, context.get().0.clone());
        Box::new(future::err("Generic failure".into()))
    }

    /// Creates list of users with given input array
    fn create_users_with_array_input(
        &self,
        body: &Vec<models::User>,
        context: &C) -> Box<Future<Item=CreateUsersWithArrayInputResponse, Error=ApiError> + Send>
    {
        let context = context.clone();
        info!("create_users_with_array_input({:?}) - X-Span-ID: {:?}", body, context.get().0.clone());
        Box::new(future::err("Generic failure".into()))
    }

    /// Creates list of users with given input array
    fn create_users_with_list_input(
        &self,
        body: &Vec<models::User>,
        context: &C) -> Box<Future<Item=CreateUsersWithListInputResponse, Error=ApiError> + Send>
    {
        let context = context.clone();
        info!("create_users_with_list_input({:?}) - X-Span-ID: {:?}", body, context.get().0.clone());
        Box::new(future::err("Generic failure".into()))
    }

    /// Delete user
    fn delete_user(
        &self,
        username: String,
        context: &C) -> Box<Future<Item=DeleteUserResponse, Error=ApiError> + Send>
    {
        let context = context.clone();
        info!("delete_user(\"{}\") - X-Span-ID: {:?}", username, context.get().0.clone());
        Box::new(future::err("Generic failure".into()))
    }

    /// Get user by user name
    fn get_user_by_name(
        &self,
        username: String,
        context: &C) -> Box<Future<Item=GetUserByNameResponse, Error=ApiError> + Send>
    {
        let context = context.clone();
        info!("get_user_by_name(\"{}\") - X-Span-ID: {:?}", username, context.get().0.clone());
        Box::new(future::err("Generic failure".into()))
    }

    /// Logs user into the system
    fn login_user(
        &self,
        username: String,
        password: String,
        context: &C) -> Box<Future<Item=LoginUserResponse, Error=ApiError> + Send>
    {
        let context = context.clone();
        info!("login_user(\"{}\", \"{}\") - X-Span-ID: {:?}", username, password, context.get().0.clone());
        Box::new(future::err("Generic failure".into()))
    }

    /// Logs out current logged in user session
    fn logout_user(
        &self,
        context: &C) -> Box<Future<Item=LogoutUserResponse, Error=ApiError> + Send>
    {
        let context = context.clone();
        info!("logout_user() - X-Span-ID: {:?}", context.get().0.clone());
        Box::new(future::err("Generic failure".into()))
    }

    /// Updated user
    fn update_user(
        &self,
        username: String,
        body: models::User,
        context: &C) -> Box<Future<Item=UpdateUserResponse, Error=ApiError> + Send>
    {
        let context = context.clone();
        info!("update_user(\"{}\", {:?}) - X-Span-ID: {:?}", username, body, context.get().0.clone());
        Box::new(future::err("Generic failure".into()))
    }

}
