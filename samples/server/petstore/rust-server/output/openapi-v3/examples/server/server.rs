//! Main library entry point for openapi_v3 implementation.

#![allow(unused_imports)]

use async_trait::async_trait;
use futures::{future, Stream, StreamExt, TryFutureExt, TryStreamExt};
use hyper::server::conn::Http;
use hyper::service::Service;
use log::info;
#[cfg(not(any(target_os = "macos", target_os = "windows", target_os = "ios")))]
use openssl::ssl::SslAcceptorBuilder;
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
use openssl::ssl::{SslAcceptor, SslFiletype, SslMethod};

use openapi_v3::models;

/// Builds an SSL implementation for Simple HTTPS from some hard-coded file names
pub async fn create(addr: &str, https: bool) {
    let addr = addr.parse().expect("Failed to parse bind address");

    let server = Server::new();

    let service = MakeService::new(server);

    let service = MakeAllowAllAuthenticator::new(service, "cosmo");

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

            let tls_acceptor = Arc::new(ssl.build());
            let mut tcp_listener = TcpListener::bind(&addr).await.unwrap();
            let mut incoming = tcp_listener.incoming();

            while let (Some(tcp), rest) = incoming.into_future().await {
                if let Ok(tcp) = tcp {
                    let addr = tcp.peer_addr().expect("Unable to get remote address");
                    let service = service.call(addr);
                    let tls_acceptor = Arc::clone(&tls_acceptor);

                    tokio::spawn(async move {
                        let tls = tokio_openssl::accept(&*tls_acceptor, tcp).await.map_err(|_| ())?;

                        let service = service.await.map_err(|_| ())?;

                        Http::new().serve_connection(tls, service).await.map_err(|_| ())
                    });
                }

                incoming = rest;
            }
        }
    } else {
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


use openapi_v3::{
    Api,
    AnyOfGetResponse,
    CallbackWithHeaderPostResponse,
    ComplexQueryParamGetResponse,
    EnumInPathPathParamGetResponse,
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
    UntypedPropertyGetResponse,
    UuidGetResponse,
    XmlExtraPostResponse,
    XmlOtherPostResponse,
    XmlOtherPutResponse,
    XmlPostResponse,
    XmlPutResponse,
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
        let context = context.clone();
        info!("any_of_get({:?}) - X-Span-ID: {:?}", any_of, context.get().0.clone());
        Err("Generic failure".into())
    }

    async fn callback_with_header_post(
        &self,
        url: String,
        context: &C) -> Result<CallbackWithHeaderPostResponse, ApiError>
    {
        let context = context.clone();
        info!("callback_with_header_post(\"{}\") - X-Span-ID: {:?}", url, context.get().0.clone());
        Err("Generic failure".into())
    }

    async fn complex_query_param_get(
        &self,
        list_of_strings: Option<&Vec<models::StringObject>>,
        context: &C) -> Result<ComplexQueryParamGetResponse, ApiError>
    {
        let context = context.clone();
        info!("complex_query_param_get({:?}) - X-Span-ID: {:?}", list_of_strings, context.get().0.clone());
        Err("Generic failure".into())
    }

    async fn enum_in_path_path_param_get(
        &self,
        path_param: models::StringEnum,
        context: &C) -> Result<EnumInPathPathParamGetResponse, ApiError>
    {
        let context = context.clone();
        info!("enum_in_path_path_param_get({:?}) - X-Span-ID: {:?}", path_param, context.get().0.clone());
        Err("Generic failure".into())
    }

    async fn json_complex_query_param_get(
        &self,
        list_of_strings: Option<&Vec<models::StringObject>>,
        context: &C) -> Result<JsonComplexQueryParamGetResponse, ApiError>
    {
        let context = context.clone();
        info!("json_complex_query_param_get({:?}) - X-Span-ID: {:?}", list_of_strings, context.get().0.clone());
        Err("Generic failure".into())
    }

    async fn mandatory_request_header_get(
        &self,
        x_header: String,
        context: &C) -> Result<MandatoryRequestHeaderGetResponse, ApiError>
    {
        let context = context.clone();
        info!("mandatory_request_header_get(\"{}\") - X-Span-ID: {:?}", x_header, context.get().0.clone());
        Err("Generic failure".into())
    }

    async fn merge_patch_json_get(
        &self,
        context: &C) -> Result<MergePatchJsonGetResponse, ApiError>
    {
        let context = context.clone();
        info!("merge_patch_json_get() - X-Span-ID: {:?}", context.get().0.clone());
        Err("Generic failure".into())
    }

    /// Get some stuff.
    async fn multiget_get(
        &self,
        context: &C) -> Result<MultigetGetResponse, ApiError>
    {
        let context = context.clone();
        info!("multiget_get() - X-Span-ID: {:?}", context.get().0.clone());
        Err("Generic failure".into())
    }

    async fn multiple_auth_scheme_get(
        &self,
        context: &C) -> Result<MultipleAuthSchemeGetResponse, ApiError>
    {
        let context = context.clone();
        info!("multiple_auth_scheme_get() - X-Span-ID: {:?}", context.get().0.clone());
        Err("Generic failure".into())
    }

    async fn one_of_get(
        &self,
        context: &C) -> Result<OneOfGetResponse, ApiError>
    {
        let context = context.clone();
        info!("one_of_get() - X-Span-ID: {:?}", context.get().0.clone());
        Err("Generic failure".into())
    }

    async fn override_server_get(
        &self,
        context: &C) -> Result<OverrideServerGetResponse, ApiError>
    {
        let context = context.clone();
        info!("override_server_get() - X-Span-ID: {:?}", context.get().0.clone());
        Err("Generic failure".into())
    }

    /// Get some stuff with parameters.
    async fn paramget_get(
        &self,
        uuid: Option<uuid::Uuid>,
        some_object: Option<models::ObjectParam>,
        some_list: Option<models::MyIdList>,
        context: &C) -> Result<ParamgetGetResponse, ApiError>
    {
        let context = context.clone();
        info!("paramget_get({:?}, {:?}, {:?}) - X-Span-ID: {:?}", uuid, some_object, some_list, context.get().0.clone());
        Err("Generic failure".into())
    }

    async fn readonly_auth_scheme_get(
        &self,
        context: &C) -> Result<ReadonlyAuthSchemeGetResponse, ApiError>
    {
        let context = context.clone();
        info!("readonly_auth_scheme_get() - X-Span-ID: {:?}", context.get().0.clone());
        Err("Generic failure".into())
    }

    async fn register_callback_post(
        &self,
        url: String,
        context: &C) -> Result<RegisterCallbackPostResponse, ApiError>
    {
        let context = context.clone();
        info!("register_callback_post(\"{}\") - X-Span-ID: {:?}", url, context.get().0.clone());
        Err("Generic failure".into())
    }

    async fn required_octet_stream_put(
        &self,
        body: swagger::ByteArray,
        context: &C) -> Result<RequiredOctetStreamPutResponse, ApiError>
    {
        let context = context.clone();
        info!("required_octet_stream_put({:?}) - X-Span-ID: {:?}", body, context.get().0.clone());
        Err("Generic failure".into())
    }

    async fn responses_with_headers_get(
        &self,
        context: &C) -> Result<ResponsesWithHeadersGetResponse, ApiError>
    {
        let context = context.clone();
        info!("responses_with_headers_get() - X-Span-ID: {:?}", context.get().0.clone());
        Err("Generic failure".into())
    }

    async fn rfc7807_get(
        &self,
        context: &C) -> Result<Rfc7807GetResponse, ApiError>
    {
        let context = context.clone();
        info!("rfc7807_get() - X-Span-ID: {:?}", context.get().0.clone());
        Err("Generic failure".into())
    }

    async fn untyped_property_get(
        &self,
        object_untyped_props: Option<models::ObjectUntypedProps>,
        context: &C) -> Result<UntypedPropertyGetResponse, ApiError>
    {
        let context = context.clone();
        info!("untyped_property_get({:?}) - X-Span-ID: {:?}", object_untyped_props, context.get().0.clone());
        Err("Generic failure".into())
    }

    async fn uuid_get(
        &self,
        context: &C) -> Result<UuidGetResponse, ApiError>
    {
        let context = context.clone();
        info!("uuid_get() - X-Span-ID: {:?}", context.get().0.clone());
        Err("Generic failure".into())
    }

    async fn xml_extra_post(
        &self,
        duplicate_xml_object: Option<models::DuplicateXmlObject>,
        context: &C) -> Result<XmlExtraPostResponse, ApiError>
    {
        let context = context.clone();
        info!("xml_extra_post({:?}) - X-Span-ID: {:?}", duplicate_xml_object, context.get().0.clone());
        Err("Generic failure".into())
    }

    async fn xml_other_post(
        &self,
        another_xml_object: Option<models::AnotherXmlObject>,
        context: &C) -> Result<XmlOtherPostResponse, ApiError>
    {
        let context = context.clone();
        info!("xml_other_post({:?}) - X-Span-ID: {:?}", another_xml_object, context.get().0.clone());
        Err("Generic failure".into())
    }

    async fn xml_other_put(
        &self,
        another_xml_array: Option<models::AnotherXmlArray>,
        context: &C) -> Result<XmlOtherPutResponse, ApiError>
    {
        let context = context.clone();
        info!("xml_other_put({:?}) - X-Span-ID: {:?}", another_xml_array, context.get().0.clone());
        Err("Generic failure".into())
    }

    /// Post an array
    async fn xml_post(
        &self,
        xml_array: Option<models::XmlArray>,
        context: &C) -> Result<XmlPostResponse, ApiError>
    {
        let context = context.clone();
        info!("xml_post({:?}) - X-Span-ID: {:?}", xml_array, context.get().0.clone());
        Err("Generic failure".into())
    }

    async fn xml_put(
        &self,
        xml_object: Option<models::XmlObject>,
        context: &C) -> Result<XmlPutResponse, ApiError>
    {
        let context = context.clone();
        info!("xml_put({:?}) - X-Span-ID: {:?}", xml_object, context.get().0.clone());
        Err("Generic failure".into())
    }

    async fn create_repo(
        &self,
        object_param: models::ObjectParam,
        context: &C) -> Result<CreateRepoResponse, ApiError>
    {
        let context = context.clone();
        info!("create_repo({:?}) - X-Span-ID: {:?}", object_param, context.get().0.clone());
        Err("Generic failure".into())
    }

    async fn get_repo_info(
        &self,
        repo_id: String,
        context: &C) -> Result<GetRepoInfoResponse, ApiError>
    {
        let context = context.clone();
        info!("get_repo_info(\"{}\") - X-Span-ID: {:?}", repo_id, context.get().0.clone());
        Err("Generic failure".into())
    }

}
