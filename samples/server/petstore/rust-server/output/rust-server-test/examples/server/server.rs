//! Main library entry point for rust_server_test implementation.

#![allow(unused_imports)]

use async_trait::async_trait;
use futures::{future, Stream, StreamExt, TryFutureExt, TryStreamExt};
use hyper::server::conn::http1;
use hyper::service::{service_fn, Service};
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

use crate::tokio_io::TokioIo;

#[cfg(not(any(target_os = "macos", target_os = "windows", target_os = "ios")))]
use openssl::ssl::{Ssl, SslAcceptor, SslAcceptorBuilder, SslFiletype, SslMethod};

use rust_server_test::models;

/// Needed because `hyper`'s `service_fn` is sent to a `tokio::task::spawn`,
/// which requires the future to be `'static`.
///
/// Because `MakeAllowAllAuthenticator` is not `Clone`, this is a shorthand way
/// of creating the `service`.
///
/// This is not a `fn` because the generics are extremely deeply nested.
macro_rules! create_service {
    () => {
        {
            let server = Server::new();
            let service = MakeService::new(server);
            let service = MakeAllowAllAuthenticator::new(service, "cosmo");
            rust_server_test::server::context::MakeAddContext::<_, EmptyContext>::new(
                service
            )
        }
    };
}

/// Builds an SSL implementation for Simple HTTPS from some hard-coded file names
pub async fn create(addr: &str, https: bool) {
    let addr: SocketAddr = addr.parse().expect("Failed to parse bind address");

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
                    let service = create_service!().call(addr);

                    tokio::spawn(async move {
                        let tls = tokio_openssl::SslStream::new(ssl, tcp).map_err(|_| ())?;
                        let service = service.await.map_err(|_| ())?;

                        http1::Builder::new()
                            .serve_connection(TokioIo::new(tcp_stream), service)
                            .await
                            .map_err(|_| ())
                    });
                }
            }
        }
    } else {
        info!("Starting a server (over http, so no TLS)");
        // Using HTTP
        let listener = TcpListener::bind(&addr).await.unwrap();
        println!("Listening on http://{}", addr);

        loop {
            // When an incoming TCP connection is received grab a TCP stream for
            // client<->server communication.
            //
            // Note, this is a .await point, this loop will loop forever but is not a busy loop. The
            // .await point allows the Tokio runtime to pull the task off of the thread until the task
            // has work to do. In this case, a connection arrives on the port we are listening on and
            // the task is woken up, at which point the task is then put back on a thread, and is
            // driven forward by the runtime, eventually yielding a TCP stream.
            let (tcp_stream, _addr) = listener.accept().await.expect("Failed to accept connection");

            let service = create_service!();
            let my_service_fn = service_fn(move |req| {
                let add_context = service.call(());

                async move {
                    let add_context = add_context.await?;
                    add_context.call(req).await
                }
            });

            // Spin up a new task in Tokio so we can continue to listen for new TCP connection on the
            // current task without waiting for the processing of the HTTP1 connection we just received
            // to finish
            tokio::task::spawn(async move {
                // Handle the connection from the client using HTTP1 and pass any
                // HTTP requests received on that connection to the `hello` function
                let result = hyper::server::conn::http1::Builder::new()
                    .serve_connection(TokioIo::new(tcp_stream), my_service_fn)
                    // `always_send` is here, because we run into:
                    //
                    // ```md
                    // implementation of `From` is not general enough
                    //
                    // `Box<(dyn StdError + std::marker::Send + Sync + 'static)>` must implement `From<Box<(dyn StdError + std::marker::Send + Sync + '0)>>`, for any lifetime `'0`...
                    // ...but it actually implements `From<Box<(dyn StdError + std::marker::Send + Sync + 'static)>>`
                    // ```
                    //
                    // This is caused by this rust bug:
                    //
                    // <https://users.rust-lang.org/t/implementation-of-from-is-not-general-enough-with-hyper/105799>
                    // <https://github.com/rust-lang/rust/issues/102211>
                    .always_send()
                    .await;
                if let Err(err) = result
                {
                    println!("Error serving connection: {:?}", err);
                }
            });
        }
    }
}

#[derive(Copy)]
pub struct Server<C> {
    marker: PhantomData<C>,
}

impl<C> Server<C> {
    pub fn new() -> Self {
        Server{marker: PhantomData}
    }
}

impl<C> Clone for Server<C> {
    fn clone(&self) -> Self {
        Self {
            marker: PhantomData,
        }
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
