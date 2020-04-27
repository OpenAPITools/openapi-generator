//! Main library entry point for openapi_v3 implementation.

#![allow(unused_imports)]

mod errors {
    error_chain::error_chain!{}
}

pub use self::errors::*;

use chrono;
use futures::{future, Future, Stream};
use hyper::server::conn::Http;
use hyper::service::MakeService as _;
use log::info;
use openssl::ssl::SslAcceptorBuilder;
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

use openapi_v3::models;

#[cfg(not(any(target_os = "macos", target_os = "windows", target_os = "ios")))]
/// Builds an SSL implementation for Simple HTTPS from some hard-coded file names
pub fn create(addr: &str, https: bool) -> Box<dyn Future<Item = (), Error = ()> + Send> {
    let addr = addr.parse().expect("Failed to parse bind address");

    let server = Server::new();

    let service_fn = MakeService::new(server);

    let service_fn = MakeAllowAllAuthenticator::new(service_fn, "cosmo");

    let service_fn =
        openapi_v3::server::context::MakeAddContext::<_, EmptyContext>::new(
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


use openapi_v3::{
    Api,
    ApiError,
    CallbackWithHeaderPostResponse,
    ComplexQueryParamGetResponse,
    EnumInPathPathParamGetResponse,
    MandatoryRequestHeaderGetResponse,
    MergePatchJsonGetResponse,
    MultigetGetResponse,
    MultipleAuthSchemeGetResponse,
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

impl<C> Api<C> for Server<C> where C: Has<XSpanIdString>{
    fn callback_with_header_post(
        &self,
        url: String,
        context: &C) -> Box<dyn Future<Item=CallbackWithHeaderPostResponse, Error=ApiError> + Send>
    {
        let context = context.clone();
        info!("callback_with_header_post(\"{}\") - X-Span-ID: {:?}", url, context.get().0.clone());
        Box::new(future::err("Generic failure".into()))
    }

    fn complex_query_param_get(
        &self,
        list_of_strings: Option<&Vec<models::StringObject>>,
        context: &C) -> Box<dyn Future<Item=ComplexQueryParamGetResponse, Error=ApiError> + Send>
    {
        let context = context.clone();
        info!("complex_query_param_get({:?}) - X-Span-ID: {:?}", list_of_strings, context.get().0.clone());
        Box::new(future::err("Generic failure".into()))
    }

    fn enum_in_path_path_param_get(
        &self,
        path_param: models::StringEnum,
        context: &C) -> Box<dyn Future<Item=EnumInPathPathParamGetResponse, Error=ApiError> + Send>
    {
        let context = context.clone();
        info!("enum_in_path_path_param_get({:?}) - X-Span-ID: {:?}", path_param, context.get().0.clone());
        Box::new(future::err("Generic failure".into()))
    }

    fn mandatory_request_header_get(
        &self,
        x_header: String,
        context: &C) -> Box<dyn Future<Item=MandatoryRequestHeaderGetResponse, Error=ApiError> + Send>
    {
        let context = context.clone();
        info!("mandatory_request_header_get(\"{}\") - X-Span-ID: {:?}", x_header, context.get().0.clone());
        Box::new(future::err("Generic failure".into()))
    }

    fn merge_patch_json_get(
        &self,
        context: &C) -> Box<dyn Future<Item=MergePatchJsonGetResponse, Error=ApiError> + Send>
    {
        let context = context.clone();
        info!("merge_patch_json_get() - X-Span-ID: {:?}", context.get().0.clone());
        Box::new(future::err("Generic failure".into()))
    }

    /// Get some stuff.
    fn multiget_get(
        &self,
        context: &C) -> Box<dyn Future<Item=MultigetGetResponse, Error=ApiError> + Send>
    {
        let context = context.clone();
        info!("multiget_get() - X-Span-ID: {:?}", context.get().0.clone());
        Box::new(future::err("Generic failure".into()))
    }

    fn multiple_auth_scheme_get(
        &self,
        context: &C) -> Box<dyn Future<Item=MultipleAuthSchemeGetResponse, Error=ApiError> + Send>
    {
        let context = context.clone();
        info!("multiple_auth_scheme_get() - X-Span-ID: {:?}", context.get().0.clone());
        Box::new(future::err("Generic failure".into()))
    }

    fn override_server_get(
        &self,
        context: &C) -> Box<dyn Future<Item=OverrideServerGetResponse, Error=ApiError> + Send>
    {
        let context = context.clone();
        info!("override_server_get() - X-Span-ID: {:?}", context.get().0.clone());
        Box::new(future::err("Generic failure".into()))
    }

    /// Get some stuff with parameters.
    fn paramget_get(
        &self,
        uuid: Option<uuid::Uuid>,
        some_object: Option<models::ObjectParam>,
        some_list: Option<models::MyIdList>,
        context: &C) -> Box<dyn Future<Item=ParamgetGetResponse, Error=ApiError> + Send>
    {
        let context = context.clone();
        info!("paramget_get({:?}, {:?}, {:?}) - X-Span-ID: {:?}", uuid, some_object, some_list, context.get().0.clone());
        Box::new(future::err("Generic failure".into()))
    }

    fn readonly_auth_scheme_get(
        &self,
        context: &C) -> Box<dyn Future<Item=ReadonlyAuthSchemeGetResponse, Error=ApiError> + Send>
    {
        let context = context.clone();
        info!("readonly_auth_scheme_get() - X-Span-ID: {:?}", context.get().0.clone());
        Box::new(future::err("Generic failure".into()))
    }

    fn register_callback_post(
        &self,
        url: String,
        context: &C) -> Box<dyn Future<Item=RegisterCallbackPostResponse, Error=ApiError> + Send>
    {
        let context = context.clone();
        info!("register_callback_post(\"{}\") - X-Span-ID: {:?}", url, context.get().0.clone());
        Box::new(future::err("Generic failure".into()))
    }

    fn required_octet_stream_put(
        &self,
        body: swagger::ByteArray,
        context: &C) -> Box<dyn Future<Item=RequiredOctetStreamPutResponse, Error=ApiError> + Send>
    {
        let context = context.clone();
        info!("required_octet_stream_put({:?}) - X-Span-ID: {:?}", body, context.get().0.clone());
        Box::new(future::err("Generic failure".into()))
    }

    fn responses_with_headers_get(
        &self,
        context: &C) -> Box<dyn Future<Item=ResponsesWithHeadersGetResponse, Error=ApiError> + Send>
    {
        let context = context.clone();
        info!("responses_with_headers_get() - X-Span-ID: {:?}", context.get().0.clone());
        Box::new(future::err("Generic failure".into()))
    }

    fn rfc7807_get(
        &self,
        context: &C) -> Box<dyn Future<Item=Rfc7807GetResponse, Error=ApiError> + Send>
    {
        let context = context.clone();
        info!("rfc7807_get() - X-Span-ID: {:?}", context.get().0.clone());
        Box::new(future::err("Generic failure".into()))
    }

    fn untyped_property_get(
        &self,
        object_untyped_props: Option<models::ObjectUntypedProps>,
        context: &C) -> Box<dyn Future<Item=UntypedPropertyGetResponse, Error=ApiError> + Send>
    {
        let context = context.clone();
        info!("untyped_property_get({:?}) - X-Span-ID: {:?}", object_untyped_props, context.get().0.clone());
        Box::new(future::err("Generic failure".into()))
    }

    fn uuid_get(
        &self,
        context: &C) -> Box<dyn Future<Item=UuidGetResponse, Error=ApiError> + Send>
    {
        let context = context.clone();
        info!("uuid_get() - X-Span-ID: {:?}", context.get().0.clone());
        Box::new(future::err("Generic failure".into()))
    }

    fn xml_extra_post(
        &self,
        duplicate_xml_object: Option<models::DuplicateXmlObject>,
        context: &C) -> Box<dyn Future<Item=XmlExtraPostResponse, Error=ApiError> + Send>
    {
        let context = context.clone();
        info!("xml_extra_post({:?}) - X-Span-ID: {:?}", duplicate_xml_object, context.get().0.clone());
        Box::new(future::err("Generic failure".into()))
    }

    fn xml_other_post(
        &self,
        another_xml_object: Option<models::AnotherXmlObject>,
        context: &C) -> Box<dyn Future<Item=XmlOtherPostResponse, Error=ApiError> + Send>
    {
        let context = context.clone();
        info!("xml_other_post({:?}) - X-Span-ID: {:?}", another_xml_object, context.get().0.clone());
        Box::new(future::err("Generic failure".into()))
    }

    fn xml_other_put(
        &self,
        another_xml_array: Option<models::AnotherXmlArray>,
        context: &C) -> Box<dyn Future<Item=XmlOtherPutResponse, Error=ApiError> + Send>
    {
        let context = context.clone();
        info!("xml_other_put({:?}) - X-Span-ID: {:?}", another_xml_array, context.get().0.clone());
        Box::new(future::err("Generic failure".into()))
    }

    /// Post an array
    fn xml_post(
        &self,
        xml_array: Option<models::XmlArray>,
        context: &C) -> Box<dyn Future<Item=XmlPostResponse, Error=ApiError> + Send>
    {
        let context = context.clone();
        info!("xml_post({:?}) - X-Span-ID: {:?}", xml_array, context.get().0.clone());
        Box::new(future::err("Generic failure".into()))
    }

    fn xml_put(
        &self,
        xml_object: Option<models::XmlObject>,
        context: &C) -> Box<dyn Future<Item=XmlPutResponse, Error=ApiError> + Send>
    {
        let context = context.clone();
        info!("xml_put({:?}) - X-Span-ID: {:?}", xml_object, context.get().0.clone());
        Box::new(future::err("Generic failure".into()))
    }

    fn create_repo(
        &self,
        object_param: models::ObjectParam,
        context: &C) -> Box<dyn Future<Item=CreateRepoResponse, Error=ApiError> + Send>
    {
        let context = context.clone();
        info!("create_repo({:?}) - X-Span-ID: {:?}", object_param, context.get().0.clone());
        Box::new(future::err("Generic failure".into()))
    }

    fn get_repo_info(
        &self,
        repo_id: String,
        context: &C) -> Box<dyn Future<Item=GetRepoInfoResponse, Error=ApiError> + Send>
    {
        let context = context.clone();
        info!("get_repo_info(\"{}\") - X-Span-ID: {:?}", repo_id, context.get().0.clone());
        Box::new(future::err("Generic failure".into()))
    }

}
