//! Main library entry point for ops_v3 implementation.

#![allow(unused_imports)]

use async_trait::async_trait;
use futures::{future, Stream, StreamExt, TryFutureExt, TryStreamExt};
use hyper::server::conn::http1;
use hyper_util::rt::TokioIo;
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

#[cfg(not(any(target_os = "macos", target_os = "windows", target_os = "ios")))]
use openssl::ssl::{Ssl, SslAcceptor, SslAcceptorBuilder, SslFiletype, SslMethod};

use ops_v3::models;

/// Builds an SSL implementation for Simple HTTPS from some hard-coded file names
pub async fn create(addr: &str, https: bool) {
    let addr: SocketAddr = addr.parse().expect("Failed to parse bind address");
    let listener = TcpListener::bind(&addr).await.unwrap();

    let server = Server::new();

    let service = MakeService::new(server);
    let service = MakeAllowAllAuthenticator::new(service, "cosmo");

    #[allow(unused_mut)]
    let mut service =
        ops_v3::server::context::MakeAddContext::<_, EmptyContext>::new(
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

            info!("Starting a server (with https)");
            loop {
                if let Ok((tcp, addr)) = listener.accept().await {
                    let ssl = Ssl::new(tls_acceptor.context()).unwrap();
                    let service = service.call(addr);

                    tokio::spawn(async move {
                        let tls = tokio_openssl::SslStream::new(ssl, tcp).map_err(|_| ())?;
                        let service = service.await.map_err(|_| ())?;

                        http1::Builder::new()
                            .serve_connection(TokioIo::new(tls), service)
                            .await
                            .map_err(|_| ())
                    });
                }
            }
        }
    } else {
        info!("Starting a server (over http, so no TLS)");
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
            let (tcp_stream, addr) = listener.accept().await.expect("Failed to accept connection");

            let service = service.call(addr).await.unwrap();
            let io = TokioIo::new(tcp_stream);
            // Spin up a new task in Tokio so we can continue to listen for new TCP connection on the
            // current task without waiting for the processing of the HTTP1 connection we just received
            // to finish
            tokio::task::spawn(async move {
                // Handle the connection from the client using HTTP1 and pass any
                // HTTP requests received on that connection to the `hello` function
                let result = http1::Builder::new()
                    .serve_connection(io, service)
                    .await;
                if let Err(err) = result
                {
                    println!("Error serving connection: {err:?}");
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


use ops_v3::{
    Api,
    Op10GetResponse,
    Op11GetResponse,
    Op12GetResponse,
    Op13GetResponse,
    Op14GetResponse,
    Op15GetResponse,
    Op16GetResponse,
    Op17GetResponse,
    Op18GetResponse,
    Op19GetResponse,
    Op1GetResponse,
    Op20GetResponse,
    Op21GetResponse,
    Op22GetResponse,
    Op23GetResponse,
    Op24GetResponse,
    Op25GetResponse,
    Op26GetResponse,
    Op27GetResponse,
    Op28GetResponse,
    Op29GetResponse,
    Op2GetResponse,
    Op30GetResponse,
    Op31GetResponse,
    Op32GetResponse,
    Op33GetResponse,
    Op34GetResponse,
    Op35GetResponse,
    Op36GetResponse,
    Op37GetResponse,
    Op3GetResponse,
    Op4GetResponse,
    Op5GetResponse,
    Op6GetResponse,
    Op7GetResponse,
    Op8GetResponse,
    Op9GetResponse,
};
use ops_v3::server::MakeService;
use std::error::Error;
use swagger::ApiError;

#[async_trait]
impl<C> Api<C> for Server<C> where C: Has<XSpanIdString> + Send + Sync
{
    async fn op10_get(
        &self,
        context: &C) -> Result<Op10GetResponse, ApiError>
    {
        info!("op10_get() - X-Span-ID: {:?}", context.get().0.clone());
        Err(ApiError("Api-Error: Operation is NOT implemented".into()))
    }

    async fn op11_get(
        &self,
        context: &C) -> Result<Op11GetResponse, ApiError>
    {
        info!("op11_get() - X-Span-ID: {:?}", context.get().0.clone());
        Err(ApiError("Api-Error: Operation is NOT implemented".into()))
    }

    async fn op12_get(
        &self,
        context: &C) -> Result<Op12GetResponse, ApiError>
    {
        info!("op12_get() - X-Span-ID: {:?}", context.get().0.clone());
        Err(ApiError("Api-Error: Operation is NOT implemented".into()))
    }

    async fn op13_get(
        &self,
        context: &C) -> Result<Op13GetResponse, ApiError>
    {
        info!("op13_get() - X-Span-ID: {:?}", context.get().0.clone());
        Err(ApiError("Api-Error: Operation is NOT implemented".into()))
    }

    async fn op14_get(
        &self,
        context: &C) -> Result<Op14GetResponse, ApiError>
    {
        info!("op14_get() - X-Span-ID: {:?}", context.get().0.clone());
        Err(ApiError("Api-Error: Operation is NOT implemented".into()))
    }

    async fn op15_get(
        &self,
        context: &C) -> Result<Op15GetResponse, ApiError>
    {
        info!("op15_get() - X-Span-ID: {:?}", context.get().0.clone());
        Err(ApiError("Api-Error: Operation is NOT implemented".into()))
    }

    async fn op16_get(
        &self,
        context: &C) -> Result<Op16GetResponse, ApiError>
    {
        info!("op16_get() - X-Span-ID: {:?}", context.get().0.clone());
        Err(ApiError("Api-Error: Operation is NOT implemented".into()))
    }

    async fn op17_get(
        &self,
        context: &C) -> Result<Op17GetResponse, ApiError>
    {
        info!("op17_get() - X-Span-ID: {:?}", context.get().0.clone());
        Err(ApiError("Api-Error: Operation is NOT implemented".into()))
    }

    async fn op18_get(
        &self,
        context: &C) -> Result<Op18GetResponse, ApiError>
    {
        info!("op18_get() - X-Span-ID: {:?}", context.get().0.clone());
        Err(ApiError("Api-Error: Operation is NOT implemented".into()))
    }

    async fn op19_get(
        &self,
        context: &C) -> Result<Op19GetResponse, ApiError>
    {
        info!("op19_get() - X-Span-ID: {:?}", context.get().0.clone());
        Err(ApiError("Api-Error: Operation is NOT implemented".into()))
    }

    async fn op1_get(
        &self,
        context: &C) -> Result<Op1GetResponse, ApiError>
    {
        info!("op1_get() - X-Span-ID: {:?}", context.get().0.clone());
        Err(ApiError("Api-Error: Operation is NOT implemented".into()))
    }

    async fn op20_get(
        &self,
        context: &C) -> Result<Op20GetResponse, ApiError>
    {
        info!("op20_get() - X-Span-ID: {:?}", context.get().0.clone());
        Err(ApiError("Api-Error: Operation is NOT implemented".into()))
    }

    async fn op21_get(
        &self,
        context: &C) -> Result<Op21GetResponse, ApiError>
    {
        info!("op21_get() - X-Span-ID: {:?}", context.get().0.clone());
        Err(ApiError("Api-Error: Operation is NOT implemented".into()))
    }

    async fn op22_get(
        &self,
        context: &C) -> Result<Op22GetResponse, ApiError>
    {
        info!("op22_get() - X-Span-ID: {:?}", context.get().0.clone());
        Err(ApiError("Api-Error: Operation is NOT implemented".into()))
    }

    async fn op23_get(
        &self,
        context: &C) -> Result<Op23GetResponse, ApiError>
    {
        info!("op23_get() - X-Span-ID: {:?}", context.get().0.clone());
        Err(ApiError("Api-Error: Operation is NOT implemented".into()))
    }

    async fn op24_get(
        &self,
        context: &C) -> Result<Op24GetResponse, ApiError>
    {
        info!("op24_get() - X-Span-ID: {:?}", context.get().0.clone());
        Err(ApiError("Api-Error: Operation is NOT implemented".into()))
    }

    async fn op25_get(
        &self,
        context: &C) -> Result<Op25GetResponse, ApiError>
    {
        info!("op25_get() - X-Span-ID: {:?}", context.get().0.clone());
        Err(ApiError("Api-Error: Operation is NOT implemented".into()))
    }

    async fn op26_get(
        &self,
        context: &C) -> Result<Op26GetResponse, ApiError>
    {
        info!("op26_get() - X-Span-ID: {:?}", context.get().0.clone());
        Err(ApiError("Api-Error: Operation is NOT implemented".into()))
    }

    async fn op27_get(
        &self,
        context: &C) -> Result<Op27GetResponse, ApiError>
    {
        info!("op27_get() - X-Span-ID: {:?}", context.get().0.clone());
        Err(ApiError("Api-Error: Operation is NOT implemented".into()))
    }

    async fn op28_get(
        &self,
        context: &C) -> Result<Op28GetResponse, ApiError>
    {
        info!("op28_get() - X-Span-ID: {:?}", context.get().0.clone());
        Err(ApiError("Api-Error: Operation is NOT implemented".into()))
    }

    async fn op29_get(
        &self,
        context: &C) -> Result<Op29GetResponse, ApiError>
    {
        info!("op29_get() - X-Span-ID: {:?}", context.get().0.clone());
        Err(ApiError("Api-Error: Operation is NOT implemented".into()))
    }

    async fn op2_get(
        &self,
        context: &C) -> Result<Op2GetResponse, ApiError>
    {
        info!("op2_get() - X-Span-ID: {:?}", context.get().0.clone());
        Err(ApiError("Api-Error: Operation is NOT implemented".into()))
    }

    async fn op30_get(
        &self,
        context: &C) -> Result<Op30GetResponse, ApiError>
    {
        info!("op30_get() - X-Span-ID: {:?}", context.get().0.clone());
        Err(ApiError("Api-Error: Operation is NOT implemented".into()))
    }

    async fn op31_get(
        &self,
        context: &C) -> Result<Op31GetResponse, ApiError>
    {
        info!("op31_get() - X-Span-ID: {:?}", context.get().0.clone());
        Err(ApiError("Api-Error: Operation is NOT implemented".into()))
    }

    async fn op32_get(
        &self,
        context: &C) -> Result<Op32GetResponse, ApiError>
    {
        info!("op32_get() - X-Span-ID: {:?}", context.get().0.clone());
        Err(ApiError("Api-Error: Operation is NOT implemented".into()))
    }

    async fn op33_get(
        &self,
        context: &C) -> Result<Op33GetResponse, ApiError>
    {
        info!("op33_get() - X-Span-ID: {:?}", context.get().0.clone());
        Err(ApiError("Api-Error: Operation is NOT implemented".into()))
    }

    async fn op34_get(
        &self,
        context: &C) -> Result<Op34GetResponse, ApiError>
    {
        info!("op34_get() - X-Span-ID: {:?}", context.get().0.clone());
        Err(ApiError("Api-Error: Operation is NOT implemented".into()))
    }

    async fn op35_get(
        &self,
        context: &C) -> Result<Op35GetResponse, ApiError>
    {
        info!("op35_get() - X-Span-ID: {:?}", context.get().0.clone());
        Err(ApiError("Api-Error: Operation is NOT implemented".into()))
    }

    async fn op36_get(
        &self,
        context: &C) -> Result<Op36GetResponse, ApiError>
    {
        info!("op36_get() - X-Span-ID: {:?}", context.get().0.clone());
        Err(ApiError("Api-Error: Operation is NOT implemented".into()))
    }

    async fn op37_get(
        &self,
        context: &C) -> Result<Op37GetResponse, ApiError>
    {
        info!("op37_get() - X-Span-ID: {:?}", context.get().0.clone());
        Err(ApiError("Api-Error: Operation is NOT implemented".into()))
    }

    async fn op3_get(
        &self,
        context: &C) -> Result<Op3GetResponse, ApiError>
    {
        info!("op3_get() - X-Span-ID: {:?}", context.get().0.clone());
        Err(ApiError("Api-Error: Operation is NOT implemented".into()))
    }

    async fn op4_get(
        &self,
        context: &C) -> Result<Op4GetResponse, ApiError>
    {
        info!("op4_get() - X-Span-ID: {:?}", context.get().0.clone());
        Err(ApiError("Api-Error: Operation is NOT implemented".into()))
    }

    async fn op5_get(
        &self,
        context: &C) -> Result<Op5GetResponse, ApiError>
    {
        info!("op5_get() - X-Span-ID: {:?}", context.get().0.clone());
        Err(ApiError("Api-Error: Operation is NOT implemented".into()))
    }

    async fn op6_get(
        &self,
        context: &C) -> Result<Op6GetResponse, ApiError>
    {
        info!("op6_get() - X-Span-ID: {:?}", context.get().0.clone());
        Err(ApiError("Api-Error: Operation is NOT implemented".into()))
    }

    async fn op7_get(
        &self,
        context: &C) -> Result<Op7GetResponse, ApiError>
    {
        info!("op7_get() - X-Span-ID: {:?}", context.get().0.clone());
        Err(ApiError("Api-Error: Operation is NOT implemented".into()))
    }

    async fn op8_get(
        &self,
        context: &C) -> Result<Op8GetResponse, ApiError>
    {
        info!("op8_get() - X-Span-ID: {:?}", context.get().0.clone());
        Err(ApiError("Api-Error: Operation is NOT implemented".into()))
    }

    async fn op9_get(
        &self,
        context: &C) -> Result<Op9GetResponse, ApiError>
    {
        info!("op9_get() - X-Span-ID: {:?}", context.get().0.clone());
        Err(ApiError("Api-Error: Operation is NOT implemented".into()))
    }

}
