//! Main library entry point for petstore_with_fake_endpoints_models_for_testing implementation.

#![allow(unused_imports)]

use async_trait::async_trait;
use futures::{future, Stream, StreamExt, TryFutureExt, TryStreamExt};
use hyper::server::conn::Http;
use hyper::service::Service;
use log::info;
use std::future::Future;
use std::marker::PhantomData;
use std::net::SocketAddr;
use std::sync::{Arc, Mutex};
use std::task::{Context, Poll};
use swagger::{Has, XSpanIdString};
use swagger::auth::MakeAllowAllAuthenticator;
use swagger::EmptyContext;
use tokio::net::TcpListener;

#[cfg(not(any(target_os = "macos", target_os = "windows", target_os = "ios")))]
use openssl::ssl::{Ssl, SslAcceptor, SslAcceptorBuilder, SslFiletype, SslMethod};

use petstore_with_fake_endpoints_models_for_testing::models;

/// Builds an SSL implementation for Simple HTTPS from some hard-coded file names
pub async fn create(addr: &str, https: bool) {
    let addr = addr.parse().expect("Failed to parse bind address");

    let server = Server::new();

    let service = MakeService::new(server);

    let service = MakeAllowAllAuthenticator::new(service, "cosmo");

    #[allow(unused_mut)]
    let mut service =
        petstore_with_fake_endpoints_models_for_testing::server::context::MakeAddContext::<_, EmptyContext>::new(
            service
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
            ssl.set_certificate_chain_file("examples/server-chain.pem").expect("Failed to set certificate chain");
            ssl.check_private_key().expect("Failed to check private key");

            let tls_acceptor = ssl.build();
            let tcp_listener = TcpListener::bind(&addr).await.unwrap();

            info!("Starting a server (with https)");
            loop {
                if let Ok((tcp, _)) = tcp_listener.accept().await {
                    let ssl = Ssl::new(tls_acceptor.context()).unwrap();
                    let addr = tcp.peer_addr().expect("Unable to get remote address");
                    let service = service.call(addr);

                    tokio::spawn(async move {
                        let tls = tokio_openssl::SslStream::new(ssl, tcp).map_err(|_| ())?;
                        let service = service.await.map_err(|_| ())?;

                        Http::new()
                            .serve_connection(tls, service)
                            .await
                            .map_err(|_| ())
                    });
                }
            }
        }
    } else {
        info!("Starting a server (over http, so no TLS)");
        // Using HTTP
        hyper::server::Server::bind(&addr).serve(service).await.unwrap()
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


use jsonwebtoken::{decode, encode, errors::Error as JwtError, Algorithm, DecodingKey, EncodingKey, Header, TokenData, Validation};
use serde::{Deserialize, Serialize};
use swagger::auth::Authorization;
use crate::server_auth;


use petstore_with_fake_endpoints_models_for_testing::{
    Api,
    TestSpecialTagsResponse,
    Call123exampleResponse,
    FakeOuterBooleanSerializeResponse,
    FakeOuterCompositeSerializeResponse,
    FakeOuterNumberSerializeResponse,
    FakeOuterStringSerializeResponse,
    FakeResponseWithNumericalDescriptionResponse,
    TestBodyWithQueryParamsResponse,
    TestClientModelResponse,
    TestEndpointParametersResponse,
    TestEnumParametersResponse,
    TestInlineAdditionalPropertiesResponse,
    TestJsonFormDataResponse,
    HyphenParamResponse,
    TestClassnameResponse,
    AddPetResponse,
    FindPetsByStatusResponse,
    FindPetsByTagsResponse,
    UpdatePetResponse,
    DeletePetResponse,
    GetPetByIdResponse,
    UpdatePetWithFormResponse,
    UploadFileResponse,
    GetInventoryResponse,
    PlaceOrderResponse,
    DeleteOrderResponse,
    GetOrderByIdResponse,
    CreateUserResponse,
    CreateUsersWithArrayInputResponse,
    CreateUsersWithListInputResponse,
    LoginUserResponse,
    LogoutUserResponse,
    DeleteUserResponse,
    GetUserByNameResponse,
    UpdateUserResponse,
};
use petstore_with_fake_endpoints_models_for_testing::server::MakeService;
use std::error::Error;
use swagger::ApiError;

#[async_trait]
impl<C> Api<C> for Server<C> where C: Has<XSpanIdString> + Send + Sync
{
    /// To test special tags
    async fn test_special_tags(
        &self,
        body: models::Client,
        context: &C) -> Result<TestSpecialTagsResponse, ApiError>
    {
        info!("test_special_tags({:?}) - X-Span-ID: {:?}", body, context.get().0.clone());
        Err(ApiError("Api-Error: Operation is NOT implemented".into()))
    }
    async fn call123example(
        &self,
        context: &C) -> Result<Call123exampleResponse, ApiError>
    {
        info!("call123example() - X-Span-ID: {:?}", context.get().0.clone());
        Err(ApiError("Api-Error: Operation is NOT implemented".into()))
    }
    async fn fake_outer_boolean_serialize(
        &self,
        body: Option<models::OuterBoolean>,
        context: &C) -> Result<FakeOuterBooleanSerializeResponse, ApiError>
    {
        info!("fake_outer_boolean_serialize({:?}) - X-Span-ID: {:?}", body, context.get().0.clone());
        Err(ApiError("Api-Error: Operation is NOT implemented".into()))
    }
    async fn fake_outer_composite_serialize(
        &self,
        body: Option<models::OuterComposite>,
        context: &C) -> Result<FakeOuterCompositeSerializeResponse, ApiError>
    {
        info!("fake_outer_composite_serialize({:?}) - X-Span-ID: {:?}", body, context.get().0.clone());
        Err(ApiError("Api-Error: Operation is NOT implemented".into()))
    }
    async fn fake_outer_number_serialize(
        &self,
        body: Option<models::OuterNumber>,
        context: &C) -> Result<FakeOuterNumberSerializeResponse, ApiError>
    {
        info!("fake_outer_number_serialize({:?}) - X-Span-ID: {:?}", body, context.get().0.clone());
        Err(ApiError("Api-Error: Operation is NOT implemented".into()))
    }
    async fn fake_outer_string_serialize(
        &self,
        body: Option<models::OuterString>,
        context: &C) -> Result<FakeOuterStringSerializeResponse, ApiError>
    {
        info!("fake_outer_string_serialize({:?}) - X-Span-ID: {:?}", body, context.get().0.clone());
        Err(ApiError("Api-Error: Operation is NOT implemented".into()))
    }
    async fn fake_response_with_numerical_description(
        &self,
        context: &C) -> Result<FakeResponseWithNumericalDescriptionResponse, ApiError>
    {
        info!("fake_response_with_numerical_description() - X-Span-ID: {:?}", context.get().0.clone());
        Err(ApiError("Api-Error: Operation is NOT implemented".into()))
    }
    async fn test_body_with_query_params(
        &self,
        query: String,
        body: models::User,
        context: &C) -> Result<TestBodyWithQueryParamsResponse, ApiError>
    {
        info!("test_body_with_query_params(\"{}\", {:?}) - X-Span-ID: {:?}", query, body, context.get().0.clone());
        Err(ApiError("Api-Error: Operation is NOT implemented".into()))
    }
    /// To test \"client\" model
    async fn test_client_model(
        &self,
        body: models::Client,
        context: &C) -> Result<TestClientModelResponse, ApiError>
    {
        info!("test_client_model({:?}) - X-Span-ID: {:?}", body, context.get().0.clone());
        Err(ApiError("Api-Error: Operation is NOT implemented".into()))
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
        date: Option<chrono::naive::NaiveDate>,
        date_time: Option<chrono::DateTime::<chrono::Utc>>,
        password: Option<String>,
        callback: Option<String>,
        context: &C) -> Result<TestEndpointParametersResponse, ApiError>
    {
        info!("test_endpoint_parameters({}, {}, \"{}\", {:?}, {:?}, {:?}, {:?}, {:?}, {:?}, {:?}, {:?}, {:?}, {:?}, {:?}) - X-Span-ID: {:?}", number, double, pattern_without_delimiter, byte, integer, int32, int64, float, string, binary, date, date_time, password, callback, context.get().0.clone());
        Err(ApiError("Api-Error: Operation is NOT implemented".into()))
    }
    /// To test enum parameters
    async fn test_enum_parameters(
        &self,
        enum_header_string_array: Option<&Vec<models::TestEnumParametersEnumHeaderStringArrayParameterInner>>,
        enum_header_string: Option<models::TestEnumParametersEnumHeaderStringParameter>,
        enum_query_string_array: Option<&Vec<models::TestEnumParametersEnumHeaderStringArrayParameterInner>>,
        enum_query_string: Option<models::TestEnumParametersEnumHeaderStringParameter>,
        enum_query_integer: Option<models::TestEnumParametersEnumQueryIntegerParameter>,
        enum_query_double: Option<models::TestEnumParametersEnumQueryDoubleParameter>,
        enum_form_string: Option<models::TestEnumParametersRequestEnumFormString>,
        context: &C) -> Result<TestEnumParametersResponse, ApiError>
    {
        info!("test_enum_parameters({:?}, {:?}, {:?}, {:?}, {:?}, {:?}, {:?}) - X-Span-ID: {:?}", enum_header_string_array, enum_header_string, enum_query_string_array, enum_query_string, enum_query_integer, enum_query_double, enum_form_string, context.get().0.clone());
        Err(ApiError("Api-Error: Operation is NOT implemented".into()))
    }
    /// test inline additionalProperties
    async fn test_inline_additional_properties(
        &self,
        param: std::collections::HashMap<String, String>,
        context: &C) -> Result<TestInlineAdditionalPropertiesResponse, ApiError>
    {
        info!("test_inline_additional_properties({:?}) - X-Span-ID: {:?}", param, context.get().0.clone());
        Err(ApiError("Api-Error: Operation is NOT implemented".into()))
    }
    /// test json serialization of form data
    async fn test_json_form_data(
        &self,
        param: String,
        param2: String,
        context: &C) -> Result<TestJsonFormDataResponse, ApiError>
    {
        info!("test_json_form_data(\"{}\", \"{}\") - X-Span-ID: {:?}", param, param2, context.get().0.clone());
        Err(ApiError("Api-Error: Operation is NOT implemented".into()))
    }
    async fn hyphen_param(
        &self,
        hyphen_param: String,
        context: &C) -> Result<HyphenParamResponse, ApiError>
    {
        info!("hyphen_param(\"{}\") - X-Span-ID: {:?}", hyphen_param, context.get().0.clone());
        Err(ApiError("Api-Error: Operation is NOT implemented".into()))
    }
    /// To test class name in snake case
    async fn test_classname(
        &self,
        body: models::Client,
        context: &C) -> Result<TestClassnameResponse, ApiError>
    {
        info!("test_classname({:?}) - X-Span-ID: {:?}", body, context.get().0.clone());
        Err(ApiError("Api-Error: Operation is NOT implemented".into()))
    }
    /// Add a new pet to the store
    async fn add_pet(
        &self,
        body: models::Pet,
        context: &C) -> Result<AddPetResponse, ApiError>
    {
        info!("add_pet({:?}) - X-Span-ID: {:?}", body, context.get().0.clone());
        Err(ApiError("Api-Error: Operation is NOT implemented".into()))
    }
    /// Finds Pets by status
    async fn find_pets_by_status(
        &self,
        status: &Vec<models::FindPetsByStatusStatusParameterInner>,
        context: &C) -> Result<FindPetsByStatusResponse, ApiError>
    {
        info!("find_pets_by_status({:?}) - X-Span-ID: {:?}", status, context.get().0.clone());
        Err(ApiError("Api-Error: Operation is NOT implemented".into()))
    }
    /// Finds Pets by tags
    async fn find_pets_by_tags(
        &self,
        tags: &Vec<String>,
        context: &C) -> Result<FindPetsByTagsResponse, ApiError>
    {
        info!("find_pets_by_tags({:?}) - X-Span-ID: {:?}", tags, context.get().0.clone());
        Err(ApiError("Api-Error: Operation is NOT implemented".into()))
    }
    /// Update an existing pet
    async fn update_pet(
        &self,
        body: models::Pet,
        context: &C) -> Result<UpdatePetResponse, ApiError>
    {
        info!("update_pet({:?}) - X-Span-ID: {:?}", body, context.get().0.clone());
        Err(ApiError("Api-Error: Operation is NOT implemented".into()))
    }
    /// Deletes a pet
    async fn delete_pet(
        &self,
        pet_id: i64,
        api_key: Option<String>,
        context: &C) -> Result<DeletePetResponse, ApiError>
    {
        info!("delete_pet({}, {:?}) - X-Span-ID: {:?}", pet_id, api_key, context.get().0.clone());
        Err(ApiError("Api-Error: Operation is NOT implemented".into()))
    }
    /// Find pet by ID
    async fn get_pet_by_id(
        &self,
        pet_id: i64,
        context: &C) -> Result<GetPetByIdResponse, ApiError>
    {
        info!("get_pet_by_id({}) - X-Span-ID: {:?}", pet_id, context.get().0.clone());
        Err(ApiError("Api-Error: Operation is NOT implemented".into()))
    }
    /// Updates a pet in the store with form data
    async fn update_pet_with_form(
        &self,
        pet_id: i64,
        name: Option<String>,
        status: Option<String>,
        context: &C) -> Result<UpdatePetWithFormResponse, ApiError>
    {
        info!("update_pet_with_form({}, {:?}, {:?}) - X-Span-ID: {:?}", pet_id, name, status, context.get().0.clone());
        Err(ApiError("Api-Error: Operation is NOT implemented".into()))
    }
    /// uploads an image
    async fn upload_file(
        &self,
        pet_id: i64,
        additional_metadata: Option<String>,
        file: Option<swagger::ByteArray>,
        context: &C) -> Result<UploadFileResponse, ApiError>
    {
        info!("upload_file({}, {:?}, {:?}) - X-Span-ID: {:?}", pet_id, additional_metadata, file, context.get().0.clone());
        Err(ApiError("Api-Error: Operation is NOT implemented".into()))
    }
    /// Returns pet inventories by status
    async fn get_inventory(
        &self,
        context: &C) -> Result<GetInventoryResponse, ApiError>
    {
        info!("get_inventory() - X-Span-ID: {:?}", context.get().0.clone());
        Err(ApiError("Api-Error: Operation is NOT implemented".into()))
    }
    /// Place an order for a pet
    async fn place_order(
        &self,
        body: models::Order,
        context: &C) -> Result<PlaceOrderResponse, ApiError>
    {
        info!("place_order({:?}) - X-Span-ID: {:?}", body, context.get().0.clone());
        Err(ApiError("Api-Error: Operation is NOT implemented".into()))
    }
    /// Delete purchase order by ID
    async fn delete_order(
        &self,
        order_id: String,
        context: &C) -> Result<DeleteOrderResponse, ApiError>
    {
        info!("delete_order(\"{}\") - X-Span-ID: {:?}", order_id, context.get().0.clone());
        Err(ApiError("Api-Error: Operation is NOT implemented".into()))
    }
    /// Find purchase order by ID
    async fn get_order_by_id(
        &self,
        order_id: i64,
        context: &C) -> Result<GetOrderByIdResponse, ApiError>
    {
        info!("get_order_by_id({}) - X-Span-ID: {:?}", order_id, context.get().0.clone());
        Err(ApiError("Api-Error: Operation is NOT implemented".into()))
    }
    /// Create user
    async fn create_user(
        &self,
        body: models::User,
        context: &C) -> Result<CreateUserResponse, ApiError>
    {
        info!("create_user({:?}) - X-Span-ID: {:?}", body, context.get().0.clone());
        Err(ApiError("Api-Error: Operation is NOT implemented".into()))
    }
    /// Creates list of users with given input array
    async fn create_users_with_array_input(
        &self,
        body: &Vec<models::User>,
        context: &C) -> Result<CreateUsersWithArrayInputResponse, ApiError>
    {
        info!("create_users_with_array_input({:?}) - X-Span-ID: {:?}", body, context.get().0.clone());
        Err(ApiError("Api-Error: Operation is NOT implemented".into()))
    }
    /// Creates list of users with given input array
    async fn create_users_with_list_input(
        &self,
        body: &Vec<models::User>,
        context: &C) -> Result<CreateUsersWithListInputResponse, ApiError>
    {
        info!("create_users_with_list_input({:?}) - X-Span-ID: {:?}", body, context.get().0.clone());
        Err(ApiError("Api-Error: Operation is NOT implemented".into()))
    }
    /// Logs user into the system
    async fn login_user(
        &self,
        username: String,
        password: String,
        context: &C) -> Result<LoginUserResponse, ApiError>
    {
        info!("login_user(\"{}\", \"{}\") - X-Span-ID: {:?}", username, password, context.get().0.clone());
        Err(ApiError("Api-Error: Operation is NOT implemented".into()))
    }
    /// Logs out current logged in user session
    async fn logout_user(
        &self,
        context: &C) -> Result<LogoutUserResponse, ApiError>
    {
        info!("logout_user() - X-Span-ID: {:?}", context.get().0.clone());
        Err(ApiError("Api-Error: Operation is NOT implemented".into()))
    }
    /// Delete user
    async fn delete_user(
        &self,
        username: String,
        context: &C) -> Result<DeleteUserResponse, ApiError>
    {
        info!("delete_user(\"{}\") - X-Span-ID: {:?}", username, context.get().0.clone());
        Err(ApiError("Api-Error: Operation is NOT implemented".into()))
    }
    /// Get user by user name
    async fn get_user_by_name(
        &self,
        username: String,
        context: &C) -> Result<GetUserByNameResponse, ApiError>
    {
        info!("get_user_by_name(\"{}\") - X-Span-ID: {:?}", username, context.get().0.clone());
        Err(ApiError("Api-Error: Operation is NOT implemented".into()))
    }
    /// Updated user
    async fn update_user(
        &self,
        username: String,
        body: models::User,
        context: &C) -> Result<UpdateUserResponse, ApiError>
    {
        info!("update_user(\"{}\", {:?}) - X-Span-ID: {:?}", username, body, context.get().0.clone());
        Err(ApiError("Api-Error: Operation is NOT implemented".into()))
    }
}
