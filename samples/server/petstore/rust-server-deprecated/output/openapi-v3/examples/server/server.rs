//! Main library entry point for openapi_v3 implementation.

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

use openapi_v3::models;

/// Builds an SSL implementation for Simple HTTPS from some hard-coded file names
pub async fn create(addr: &str, https: bool) {
    let addr = addr.parse().expect("Failed to parse bind address");

    let server = Server::new();

    let service = MakeService::new(server);

    let service = MakeAllowAllAuthenticator::new(service, "cosmo");

    #[allow(unused_mut)]
    let mut service =
        openapi_v3::server::context::MakeAddContext::<_, EmptyContext>::new(
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


use openapi_v3::{
    Api,
    AnyOfGetResponse,
    CallbackWithHeaderPostResponse,
    ComplexQueryParamGetResponse,
    ExamplesTestResponse,
    FormTestResponse,
    GetWithBooleanParameterResponse,
    JsonComplexQueryParamGetResponse,
    MandatoryRequestHeaderGetResponse,
    MergePatchJsonGetResponse,
    MultigetGetResponse,
    MultipleAuthSchemeGetResponse,
    OneOfGetResponse,
    OverrideServerGetResponse,
    ParamgetGetResponse,
    ReadonlyAuthSchemeGetResponse,
    RegisterCallbackPostResponse,
    RequiredOctetStreamPutResponse,
    ResponsesWithHeadersGetResponse,
    Rfc7807GetResponse,
    TwoFirstLetterHeadersResponse,
    UntypedPropertyGetResponse,
    UuidGetResponse,
    XmlExtraPostResponse,
    XmlOtherPostResponse,
    XmlOtherPutResponse,
    XmlPostResponse,
    XmlPutResponse,
    EnumInPathPathParamGetResponse,
    MultiplePathParamsWithVeryLongPathToTestFormattingPathParamAPathParamBGetResponse,
    CreateRepoResponse,
    GetRepoInfoResponse,
};
use openapi_v3::server::MakeService;
use std::error::Error;
use swagger::ApiError;

#[async_trait]
impl<C> Api<C> for Server<C> where C: Has<XSpanIdString> + Send + Sync
{
    async fn any_of_get(
        &self,
        any_of: Option<&Vec<models::AnyOfObject>>,
        context: &C) -> Result<AnyOfGetResponse, ApiError>
    {
        info!("any_of_get({:?}) - X-Span-ID: {:?}", any_of, context.get().0.clone());
        Err(ApiError("Api-Error: Operation is NOT implemented".into()))
    }
    async fn callback_with_header_post(
        &self,
        url: String,
        context: &C) -> Result<CallbackWithHeaderPostResponse, ApiError>
    {
        info!("callback_with_header_post(\"{}\") - X-Span-ID: {:?}", url, context.get().0.clone());
        Err(ApiError("Api-Error: Operation is NOT implemented".into()))
    }
    async fn complex_query_param_get(
        &self,
        list_of_strings: Option<&Vec<models::StringObject>>,
        context: &C) -> Result<ComplexQueryParamGetResponse, ApiError>
    {
        info!("complex_query_param_get({:?}) - X-Span-ID: {:?}", list_of_strings, context.get().0.clone());
        Err(ApiError("Api-Error: Operation is NOT implemented".into()))
    }
    /// Test examples
    async fn examples_test(
        &self,
        ids: Option<&Vec<String>>,
        context: &C) -> Result<ExamplesTestResponse, ApiError>
    {
        info!("examples_test({:?}) - X-Span-ID: {:?}", ids, context.get().0.clone());
        Err(ApiError("Api-Error: Operation is NOT implemented".into()))
    }
    /// Test a Form Post
    async fn form_test(
        &self,
        required_array: Option<&Vec<String>>,
        context: &C) -> Result<FormTestResponse, ApiError>
    {
        info!("form_test({:?}) - X-Span-ID: {:?}", required_array, context.get().0.clone());
        Err(ApiError("Api-Error: Operation is NOT implemented".into()))
    }
    async fn get_with_boolean_parameter(
        &self,
        iambool: bool,
        context: &C) -> Result<GetWithBooleanParameterResponse, ApiError>
    {
        info!("get_with_boolean_parameter({}) - X-Span-ID: {:?}", iambool, context.get().0.clone());
        Err(ApiError("Api-Error: Operation is NOT implemented".into()))
    }
    async fn json_complex_query_param_get(
        &self,
        list_of_strings: Option<&Vec<models::StringObject>>,
        context: &C) -> Result<JsonComplexQueryParamGetResponse, ApiError>
    {
        info!("json_complex_query_param_get({:?}) - X-Span-ID: {:?}", list_of_strings, context.get().0.clone());
        Err(ApiError("Api-Error: Operation is NOT implemented".into()))
    }
    async fn mandatory_request_header_get(
        &self,
        x_header: String,
        context: &C) -> Result<MandatoryRequestHeaderGetResponse, ApiError>
    {
        info!("mandatory_request_header_get(\"{}\") - X-Span-ID: {:?}", x_header, context.get().0.clone());
        Err(ApiError("Api-Error: Operation is NOT implemented".into()))
    }
    async fn merge_patch_json_get(
        &self,
        context: &C) -> Result<MergePatchJsonGetResponse, ApiError>
    {
        info!("merge_patch_json_get() - X-Span-ID: {:?}", context.get().0.clone());
        Err(ApiError("Api-Error: Operation is NOT implemented".into()))
    }
    /// Get some stuff.
    async fn multiget_get(
        &self,
        context: &C) -> Result<MultigetGetResponse, ApiError>
    {
        info!("multiget_get() - X-Span-ID: {:?}", context.get().0.clone());
        Err(ApiError("Api-Error: Operation is NOT implemented".into()))
    }
    async fn multiple_auth_scheme_get(
        &self,
        context: &C) -> Result<MultipleAuthSchemeGetResponse, ApiError>
    {
        info!("multiple_auth_scheme_get() - X-Span-ID: {:?}", context.get().0.clone());
        Err(ApiError("Api-Error: Operation is NOT implemented".into()))
    }
    async fn one_of_get(
        &self,
        context: &C) -> Result<OneOfGetResponse, ApiError>
    {
        info!("one_of_get() - X-Span-ID: {:?}", context.get().0.clone());
        Err(ApiError("Api-Error: Operation is NOT implemented".into()))
    }
    async fn override_server_get(
        &self,
        context: &C) -> Result<OverrideServerGetResponse, ApiError>
    {
        info!("override_server_get() - X-Span-ID: {:?}", context.get().0.clone());
        Err(ApiError("Api-Error: Operation is NOT implemented".into()))
    }
    /// Get some stuff with parameters.
    async fn paramget_get(
        &self,
        uuid: Option<uuid::Uuid>,
        some_object: Option<serde_json::Value>,
        some_list: Option<&Vec<models::MyId>>,
        context: &C) -> Result<ParamgetGetResponse, ApiError>
    {
        info!("paramget_get({:?}, {:?}, {:?}) - X-Span-ID: {:?}", uuid, some_object, some_list, context.get().0.clone());
        Err(ApiError("Api-Error: Operation is NOT implemented".into()))
    }
    async fn readonly_auth_scheme_get(
        &self,
        context: &C) -> Result<ReadonlyAuthSchemeGetResponse, ApiError>
    {
        info!("readonly_auth_scheme_get() - X-Span-ID: {:?}", context.get().0.clone());
        Err(ApiError("Api-Error: Operation is NOT implemented".into()))
    }
    async fn register_callback_post(
        &self,
        url: String,
        context: &C) -> Result<RegisterCallbackPostResponse, ApiError>
    {
        info!("register_callback_post(\"{}\") - X-Span-ID: {:?}", url, context.get().0.clone());
        Err(ApiError("Api-Error: Operation is NOT implemented".into()))
    }
    async fn required_octet_stream_put(
        &self,
        body: swagger::ByteArray,
        context: &C) -> Result<RequiredOctetStreamPutResponse, ApiError>
    {
        info!("required_octet_stream_put({:?}) - X-Span-ID: {:?}", body, context.get().0.clone());
        Err(ApiError("Api-Error: Operation is NOT implemented".into()))
    }
    async fn responses_with_headers_get(
        &self,
        context: &C) -> Result<ResponsesWithHeadersGetResponse, ApiError>
    {
        info!("responses_with_headers_get() - X-Span-ID: {:?}", context.get().0.clone());
        Err(ApiError("Api-Error: Operation is NOT implemented".into()))
    }
    async fn rfc7807_get(
        &self,
        context: &C) -> Result<Rfc7807GetResponse, ApiError>
    {
        info!("rfc7807_get() - X-Span-ID: {:?}", context.get().0.clone());
        Err(ApiError("Api-Error: Operation is NOT implemented".into()))
    }
    async fn two_first_letter_headers(
        &self,
        x_header_one: Option<bool>,
        x_header_two: Option<bool>,
        context: &C) -> Result<TwoFirstLetterHeadersResponse, ApiError>
    {
        info!("two_first_letter_headers({:?}, {:?}) - X-Span-ID: {:?}", x_header_one, x_header_two, context.get().0.clone());
        Err(ApiError("Api-Error: Operation is NOT implemented".into()))
    }
    async fn untyped_property_get(
        &self,
        object_untyped_props: Option<models::ObjectUntypedProps>,
        context: &C) -> Result<UntypedPropertyGetResponse, ApiError>
    {
        info!("untyped_property_get({:?}) - X-Span-ID: {:?}", object_untyped_props, context.get().0.clone());
        Err(ApiError("Api-Error: Operation is NOT implemented".into()))
    }
    async fn uuid_get(
        &self,
        context: &C) -> Result<UuidGetResponse, ApiError>
    {
        info!("uuid_get() - X-Span-ID: {:?}", context.get().0.clone());
        Err(ApiError("Api-Error: Operation is NOT implemented".into()))
    }
    async fn xml_extra_post(
        &self,
        duplicate_xml_object: Option<models::DuplicateXmlObject>,
        context: &C) -> Result<XmlExtraPostResponse, ApiError>
    {
        info!("xml_extra_post({:?}) - X-Span-ID: {:?}", duplicate_xml_object, context.get().0.clone());
        Err(ApiError("Api-Error: Operation is NOT implemented".into()))
    }
    async fn xml_other_post(
        &self,
        another_xml_object: Option<models::AnotherXmlObject>,
        context: &C) -> Result<XmlOtherPostResponse, ApiError>
    {
        info!("xml_other_post({:?}) - X-Span-ID: {:?}", another_xml_object, context.get().0.clone());
        Err(ApiError("Api-Error: Operation is NOT implemented".into()))
    }
    async fn xml_other_put(
        &self,
        another_xml_array: Option<models::AnotherXmlArray>,
        context: &C) -> Result<XmlOtherPutResponse, ApiError>
    {
        info!("xml_other_put({:?}) - X-Span-ID: {:?}", another_xml_array, context.get().0.clone());
        Err(ApiError("Api-Error: Operation is NOT implemented".into()))
    }
    /// Post an array.  It's important we test apostrophes, so include one here.
    async fn xml_post(
        &self,
        xml_array: Option<models::XmlArray>,
        context: &C) -> Result<XmlPostResponse, ApiError>
    {
        info!("xml_post({:?}) - X-Span-ID: {:?}", xml_array, context.get().0.clone());
        Err(ApiError("Api-Error: Operation is NOT implemented".into()))
    }
    async fn xml_put(
        &self,
        xml_object: Option<models::XmlObject>,
        context: &C) -> Result<XmlPutResponse, ApiError>
    {
        info!("xml_put({:?}) - X-Span-ID: {:?}", xml_object, context.get().0.clone());
        Err(ApiError("Api-Error: Operation is NOT implemented".into()))
    }
    async fn enum_in_path_path_param_get(
        &self,
        path_param: models::StringEnum,
        context: &C) -> Result<EnumInPathPathParamGetResponse, ApiError>
    {
        info!("enum_in_path_path_param_get({:?}) - X-Span-ID: {:?}", path_param, context.get().0.clone());
        Err(ApiError("Api-Error: Operation is NOT implemented".into()))
    }
    async fn multiple_path_params_with_very_long_path_to_test_formatting_path_param_a_path_param_b_get(
        &self,
        path_param_a: String,
        path_param_b: String,
        context: &C) -> Result<MultiplePathParamsWithVeryLongPathToTestFormattingPathParamAPathParamBGetResponse, ApiError>
    {
        info!("multiple_path_params_with_very_long_path_to_test_formatting_path_param_a_path_param_b_get(\"{}\", \"{}\") - X-Span-ID: {:?}", path_param_a, path_param_b, context.get().0.clone());
        Err(ApiError("Api-Error: Operation is NOT implemented".into()))
    }
    async fn create_repo(
        &self,
        object_param: models::ObjectParam,
        context: &C) -> Result<CreateRepoResponse, ApiError>
    {
        info!("create_repo({:?}) - X-Span-ID: {:?}", object_param, context.get().0.clone());
        Err(ApiError("Api-Error: Operation is NOT implemented".into()))
    }
    async fn get_repo_info(
        &self,
        repo_id: String,
        context: &C) -> Result<GetRepoInfoResponse, ApiError>
    {
        info!("get_repo_info(\"{}\") - X-Span-ID: {:?}", repo_id, context.get().0.clone());
        Err(ApiError("Api-Error: Operation is NOT implemented".into()))
    }
}
