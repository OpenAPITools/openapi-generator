//! Main library entry point for rust_server_test implementation.

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

use rust_server_test::models;

/// Builds an SSL implementation for Simple HTTPS from some hard-coded file names
pub async fn create(addr: &str, https: bool) {
    let addr = addr.parse().expect("Failed to parse bind address");

    let server = Server::new();

    let service = MakeService::new(server);

    let service = MakeAllowAllAuthenticator::new(service, "cosmo");

    let mut service =
        rust_server_test::server::context::MakeAddContext::<_, EmptyContext>::new(
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


use rust_server_test::{
    Api,
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
use std::error::Error;
use swagger::ApiError;

#[async_trait]
impl<C> Api<C> for Server<C> where C: Has<XSpanIdString> + Send + Sync
{
    async fn all_of_get(
        &self,
        context: &C) -> Result<AllOfGetResponse, ApiError>
    {
        let context = context.clone();
        info!("all_of_get() - X-Span-ID: {:?}", context.get().0.clone());
        Err("Generic failure".into())
    }

    /// A dummy endpoint to make the spec valid.
    async fn dummy_get(
        &self,
        context: &C) -> Result<DummyGetResponse, ApiError>
    {
        let context = context.clone();
        info!("dummy_get() - X-Span-ID: {:?}", context.get().0.clone());
        Err("Generic failure".into())
    }

    async fn dummy_put(
        &self,
        nested_response: models::InlineObject,
        context: &C) -> Result<DummyPutResponse, ApiError>
    {
        let context = context.clone();
        info!("dummy_put({:?}) - X-Span-ID: {:?}", nested_response, context.get().0.clone());
        Err("Generic failure".into())
    }

    /// Get a file
    async fn file_response_get(
        &self,
        context: &C) -> Result<FileResponseGetResponse, ApiError>
    {
        let context = context.clone();
        info!("file_response_get() - X-Span-ID: {:?}", context.get().0.clone());
        Err("Generic failure".into())
    }

    async fn get_structured_yaml(
        &self,
        context: &C) -> Result<GetStructuredYamlResponse, ApiError>
    {
        let context = context.clone();
        info!("get_structured_yaml() - X-Span-ID: {:?}", context.get().0.clone());
        Err("Generic failure".into())
    }

    /// Test HTML handling
    async fn html_post(
        &self,
        body: String,
        context: &C) -> Result<HtmlPostResponse, ApiError>
    {
        let context = context.clone();
        info!("html_post(\"{}\") - X-Span-ID: {:?}", body, context.get().0.clone());
        Err("Generic failure".into())
    }

    async fn post_yaml(
        &self,
        value: String,
        context: &C) -> Result<PostYamlResponse, ApiError>
    {
        let context = context.clone();
        info!("post_yaml(\"{}\") - X-Span-ID: {:?}", value, context.get().0.clone());
        Err("Generic failure".into())
    }

    /// Get an arbitrary JSON blob.
    async fn raw_json_get(
        &self,
        context: &C) -> Result<RawJsonGetResponse, ApiError>
    {
        let context = context.clone();
        info!("raw_json_get() - X-Span-ID: {:?}", context.get().0.clone());
        Err("Generic failure".into())
    }

    /// Send an arbitrary JSON blob
    async fn solo_object_post(
        &self,
        value: serde_json::Value,
        context: &C) -> Result<SoloObjectPostResponse, ApiError>
    {
        let context = context.clone();
        info!("solo_object_post({:?}) - X-Span-ID: {:?}", value, context.get().0.clone());
        Err("Generic failure".into())
    }

}
