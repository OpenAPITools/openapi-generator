//! Main library entry point for rust_server_test implementation.

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

use rust_server_test::models;

/// Builds an SSL implementation for Simple HTTPS from some hard-coded file names
pub async fn create(addr: &str, https: bool) {
    let addr = addr.parse().expect("Failed to parse bind address");

    let server = Server::new();

    let service = MakeService::new(server);

    let service = MakeAllowAllAuthenticator::new(service, "cosmo");

    #[allow(unused_mut)]
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
        info!("all_of_get() - X-Span-ID: {:?}", context.get().0.clone());
        Err(ApiError("Api-Error: Operation is NOT implemented".into()))
    }

    /// A dummy endpoint to make the spec valid.
    async fn dummy_get(
        &self,
        context: &C) -> Result<DummyGetResponse, ApiError>
    {
        info!("dummy_get() - X-Span-ID: {:?}", context.get().0.clone());
        Err(ApiError("Api-Error: Operation is NOT implemented".into()))
    }

    async fn dummy_put(
        &self,
        nested_response: models::DummyPutRequest,
        context: &C) -> Result<DummyPutResponse, ApiError>
    {
        info!("dummy_put({:?}) - X-Span-ID: {:?}", nested_response, context.get().0.clone());
        Err(ApiError("Api-Error: Operation is NOT implemented".into()))
    }

    /// Get a file
    async fn file_response_get(
        &self,
        context: &C) -> Result<FileResponseGetResponse, ApiError>
    {
        info!("file_response_get() - X-Span-ID: {:?}", context.get().0.clone());
        Err(ApiError("Api-Error: Operation is NOT implemented".into()))
    }

    async fn get_structured_yaml(
        &self,
        context: &C) -> Result<GetStructuredYamlResponse, ApiError>
    {
        info!("get_structured_yaml() - X-Span-ID: {:?}", context.get().0.clone());
        Err(ApiError("Api-Error: Operation is NOT implemented".into()))
    }

    /// Test HTML handling
    async fn html_post(
        &self,
        body: String,
        context: &C) -> Result<HtmlPostResponse, ApiError>
    {
        info!("html_post(\"{}\") - X-Span-ID: {:?}", body, context.get().0.clone());
        Err(ApiError("Api-Error: Operation is NOT implemented".into()))
    }

    async fn post_yaml(
        &self,
        value: String,
        context: &C) -> Result<PostYamlResponse, ApiError>
    {
        info!("post_yaml(\"{}\") - X-Span-ID: {:?}", value, context.get().0.clone());
        Err(ApiError("Api-Error: Operation is NOT implemented".into()))
    }

    /// Get an arbitrary JSON blob.
    async fn raw_json_get(
        &self,
        context: &C) -> Result<RawJsonGetResponse, ApiError>
    {
        info!("raw_json_get() - X-Span-ID: {:?}", context.get().0.clone());
        Err(ApiError("Api-Error: Operation is NOT implemented".into()))
    }

    /// Send an arbitrary JSON blob
    async fn solo_object_post(
        &self,
        value: serde_json::Value,
        context: &C) -> Result<SoloObjectPostResponse, ApiError>
    {
        info!("solo_object_post({:?}) - X-Span-ID: {:?}", value, context.get().0.clone());
        Err(ApiError("Api-Error: Operation is NOT implemented".into()))
    }

}
