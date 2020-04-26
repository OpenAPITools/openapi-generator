//! Main library entry point for multipart_v3 implementation.

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


#[cfg(not(any(target_os = "macos", target_os = "windows", target_os = "ios")))]
use tokio_openssl::SslAcceptorExt;
#[cfg(not(any(target_os = "macos", target_os = "windows", target_os = "ios")))]
use openssl::ssl::{SslAcceptor, SslFiletype, SslMethod};

use multipart_v3::models;

#[cfg(not(any(target_os = "macos", target_os = "windows", target_os = "ios")))]
/// Builds an SSL implementation for Simple HTTPS from some hard-coded file names
pub fn create(addr: &str, https: bool) -> Box<dyn Future<Item = (), Error = ()> + Send> {
    let addr = addr.parse().expect("Failed to parse bind address");

    let server = Server::new();

    let service_fn = MakeService::new(server);

    let service_fn = MakeAllowAllAuthenticator::new(service_fn, "cosmo");

    let service_fn =
        multipart_v3::server::context::MakeAddContext::<_, EmptyContext>::new(
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


use multipart_v3::{
    Api,
    ApiError,
    MultipartRelatedRequestPostResponse,
    MultipartRequestPostResponse,
    MultipleIdenticalMimeTypesPostResponse,
};
use multipart_v3::server::MakeService;

impl<C> Api<C> for Server<C> where C: Has<XSpanIdString>{
    fn multipart_related_request_post(
        &self,
        required_binary_field: swagger::ByteArray,
        object_field: Option<models::MultipartRequestObjectField>,
        optional_binary_field: Option<swagger::ByteArray>,
        context: &C) -> Box<dyn Future<Item=MultipartRelatedRequestPostResponse, Error=ApiError> + Send>
    {
        let context = context.clone();
        info!("multipart_related_request_post({:?}, {:?}, {:?}) - X-Span-ID: {:?}", required_binary_field, object_field, optional_binary_field, context.get().0.clone());
        Box::new(future::err("Generic failure".into()))
    }

    fn multipart_request_post(
        &self,
        string_field: String,
        binary_field: swagger::ByteArray,
        optional_string_field: Option<String>,
        object_field: Option<models::MultipartRequestObjectField>,
        context: &C) -> Box<dyn Future<Item=MultipartRequestPostResponse, Error=ApiError> + Send>
    {
        let context = context.clone();
        info!("multipart_request_post(\"{}\", {:?}, {:?}, {:?}) - X-Span-ID: {:?}", string_field, binary_field, optional_string_field, object_field, context.get().0.clone());
        Box::new(future::err("Generic failure".into()))
    }

    fn multiple_identical_mime_types_post(
        &self,
        binary1: Option<swagger::ByteArray>,
        binary2: Option<swagger::ByteArray>,
        context: &C) -> Box<dyn Future<Item=MultipleIdenticalMimeTypesPostResponse, Error=ApiError> + Send>
    {
        let context = context.clone();
        info!("multiple_identical_mime_types_post({:?}, {:?}) - X-Span-ID: {:?}", binary1, binary2, context.get().0.clone());
        Box::new(future::err("Generic failure".into()))
    }

}
