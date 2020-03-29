//! Main library entry point for rust_server_test implementation.

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
use std::marker::PhantomData;
use std::net::SocketAddr;
use std::sync::{Arc, Mutex};
use swagger;
use swagger::{Has, XSpanIdString};
use swagger::auth::MakeAllowAllAuthenticator;
use swagger::EmptyContext;
use tokio::net::TcpListener;


#[cfg(not(any(target_os = "macos", target_os = "windows", target_os = "ios")))]
use tokio_openssl::SslAcceptorExt;
#[cfg(not(any(target_os = "macos", target_os = "windows", target_os = "ios")))]
use openssl::ssl::{SslAcceptor, SslFiletype, SslMethod};

use rust_server_test::models;

#[cfg(not(any(target_os = "macos", target_os = "windows", target_os = "ios")))]
/// Builds an SSL implementation for Simple HTTPS from some hard-coded file names
pub fn create(addr: &str, https: bool) -> Box<dyn Future<Item = (), Error = ()> + Send> {
    let addr = addr.parse().expect("Failed to parse bind address");

    let server = Server::new();

    let service_fn = MakeService::new(server);

    let service_fn = MakeAllowAllAuthenticator::new(service_fn, "cosmo");

    let service_fn =
        rust_server_test::server::context::MakeAddContext::<_, EmptyContext>::new(
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


use rust_server_test::{
    Api,
    ApiError,
    AllOfGetResponse,
    DummyGetResponse,
    DummyPutResponse,
    FileResponseGetResponse,
    GetStructuredYamlResponse,
    HtmlPostResponse,
    PostYamlResponse,
    RawJsonGetResponse,
    SoloObjectPostResponse,
};
use rust_server_test::server::MakeService;

impl<C> Api<C> for Server<C> where C: Has<XSpanIdString>{
    fn all_of_get(
        &self,
        context: &C) -> Box<Future<Item=AllOfGetResponse, Error=ApiError> + Send>
    {
        let context = context.clone();
        info!("all_of_get() - X-Span-ID: {:?}", context.get().0.clone());
        Box::new(future::err("Generic failure".into()))
    }

    /// A dummy endpoint to make the spec valid.
    fn dummy_get(
        &self,
        context: &C) -> Box<Future<Item=DummyGetResponse, Error=ApiError> + Send>
    {
        let context = context.clone();
        info!("dummy_get() - X-Span-ID: {:?}", context.get().0.clone());
        Box::new(future::err("Generic failure".into()))
    }

    fn dummy_put(
        &self,
        nested_response: models::InlineObject,
        context: &C) -> Box<Future<Item=DummyPutResponse, Error=ApiError> + Send>
    {
        let context = context.clone();
        info!("dummy_put({:?}) - X-Span-ID: {:?}", nested_response, context.get().0.clone());
        Box::new(future::err("Generic failure".into()))
    }

    /// Get a file
    fn file_response_get(
        &self,
        context: &C) -> Box<Future<Item=FileResponseGetResponse, Error=ApiError> + Send>
    {
        let context = context.clone();
        info!("file_response_get() - X-Span-ID: {:?}", context.get().0.clone());
        Box::new(future::err("Generic failure".into()))
    }

    fn get_structured_yaml(
        &self,
        context: &C) -> Box<Future<Item=GetStructuredYamlResponse, Error=ApiError> + Send>
    {
        let context = context.clone();
        info!("get_structured_yaml() - X-Span-ID: {:?}", context.get().0.clone());
        Box::new(future::err("Generic failure".into()))
    }

    /// Test HTML handling
    fn html_post(
        &self,
        body: String,
        context: &C) -> Box<Future<Item=HtmlPostResponse, Error=ApiError> + Send>
    {
        let context = context.clone();
        info!("html_post(\"{}\") - X-Span-ID: {:?}", body, context.get().0.clone());
        Box::new(future::err("Generic failure".into()))
    }

    fn post_yaml(
        &self,
        value: String,
        context: &C) -> Box<Future<Item=PostYamlResponse, Error=ApiError> + Send>
    {
        let context = context.clone();
        info!("post_yaml(\"{}\") - X-Span-ID: {:?}", value, context.get().0.clone());
        Box::new(future::err("Generic failure".into()))
    }

    /// Get an arbitrary JSON blob.
    fn raw_json_get(
        &self,
        context: &C) -> Box<Future<Item=RawJsonGetResponse, Error=ApiError> + Send>
    {
        let context = context.clone();
        info!("raw_json_get() - X-Span-ID: {:?}", context.get().0.clone());
        Box::new(future::err("Generic failure".into()))
    }

    /// Send an arbitrary JSON blob
    fn solo_object_post(
        &self,
        value: serde_json::Value,
        context: &C) -> Box<Future<Item=SoloObjectPostResponse, Error=ApiError> + Send>
    {
        let context = context.clone();
        info!("solo_object_post({:?}) - X-Span-ID: {:?}", value, context.get().0.clone());
        Box::new(future::err("Generic failure".into()))
    }

}
