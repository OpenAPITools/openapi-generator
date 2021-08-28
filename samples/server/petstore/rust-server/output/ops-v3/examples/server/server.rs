//! Main library entry point for ops_v3 implementation.

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

use ops_v3::models;

/// Builds an SSL implementation for Simple HTTPS from some hard-coded file names
pub async fn create(addr: &str, https: bool) {
    let addr = addr.parse().expect("Failed to parse bind address");

    let server = Server::new();

    let service = MakeService::new(server);

    let service = MakeAllowAllAuthenticator::new(service, "cosmo");

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
        let context = context.clone();
        info!("op10_get() - X-Span-ID: {:?}", context.get().0.clone());
        Err("Generic failure".into())
    }

    async fn op11_get(
        &self,
        context: &C) -> Result<Op11GetResponse, ApiError>
    {
        let context = context.clone();
        info!("op11_get() - X-Span-ID: {:?}", context.get().0.clone());
        Err("Generic failure".into())
    }

    async fn op12_get(
        &self,
        context: &C) -> Result<Op12GetResponse, ApiError>
    {
        let context = context.clone();
        info!("op12_get() - X-Span-ID: {:?}", context.get().0.clone());
        Err("Generic failure".into())
    }

    async fn op13_get(
        &self,
        context: &C) -> Result<Op13GetResponse, ApiError>
    {
        let context = context.clone();
        info!("op13_get() - X-Span-ID: {:?}", context.get().0.clone());
        Err("Generic failure".into())
    }

    async fn op14_get(
        &self,
        context: &C) -> Result<Op14GetResponse, ApiError>
    {
        let context = context.clone();
        info!("op14_get() - X-Span-ID: {:?}", context.get().0.clone());
        Err("Generic failure".into())
    }

    async fn op15_get(
        &self,
        context: &C) -> Result<Op15GetResponse, ApiError>
    {
        let context = context.clone();
        info!("op15_get() - X-Span-ID: {:?}", context.get().0.clone());
        Err("Generic failure".into())
    }

    async fn op16_get(
        &self,
        context: &C) -> Result<Op16GetResponse, ApiError>
    {
        let context = context.clone();
        info!("op16_get() - X-Span-ID: {:?}", context.get().0.clone());
        Err("Generic failure".into())
    }

    async fn op17_get(
        &self,
        context: &C) -> Result<Op17GetResponse, ApiError>
    {
        let context = context.clone();
        info!("op17_get() - X-Span-ID: {:?}", context.get().0.clone());
        Err("Generic failure".into())
    }

    async fn op18_get(
        &self,
        context: &C) -> Result<Op18GetResponse, ApiError>
    {
        let context = context.clone();
        info!("op18_get() - X-Span-ID: {:?}", context.get().0.clone());
        Err("Generic failure".into())
    }

    async fn op19_get(
        &self,
        context: &C) -> Result<Op19GetResponse, ApiError>
    {
        let context = context.clone();
        info!("op19_get() - X-Span-ID: {:?}", context.get().0.clone());
        Err("Generic failure".into())
    }

    async fn op1_get(
        &self,
        context: &C) -> Result<Op1GetResponse, ApiError>
    {
        let context = context.clone();
        info!("op1_get() - X-Span-ID: {:?}", context.get().0.clone());
        Err("Generic failure".into())
    }

    async fn op20_get(
        &self,
        context: &C) -> Result<Op20GetResponse, ApiError>
    {
        let context = context.clone();
        info!("op20_get() - X-Span-ID: {:?}", context.get().0.clone());
        Err("Generic failure".into())
    }

    async fn op21_get(
        &self,
        context: &C) -> Result<Op21GetResponse, ApiError>
    {
        let context = context.clone();
        info!("op21_get() - X-Span-ID: {:?}", context.get().0.clone());
        Err("Generic failure".into())
    }

    async fn op22_get(
        &self,
        context: &C) -> Result<Op22GetResponse, ApiError>
    {
        let context = context.clone();
        info!("op22_get() - X-Span-ID: {:?}", context.get().0.clone());
        Err("Generic failure".into())
    }

    async fn op23_get(
        &self,
        context: &C) -> Result<Op23GetResponse, ApiError>
    {
        let context = context.clone();
        info!("op23_get() - X-Span-ID: {:?}", context.get().0.clone());
        Err("Generic failure".into())
    }

    async fn op24_get(
        &self,
        context: &C) -> Result<Op24GetResponse, ApiError>
    {
        let context = context.clone();
        info!("op24_get() - X-Span-ID: {:?}", context.get().0.clone());
        Err("Generic failure".into())
    }

    async fn op25_get(
        &self,
        context: &C) -> Result<Op25GetResponse, ApiError>
    {
        let context = context.clone();
        info!("op25_get() - X-Span-ID: {:?}", context.get().0.clone());
        Err("Generic failure".into())
    }

    async fn op26_get(
        &self,
        context: &C) -> Result<Op26GetResponse, ApiError>
    {
        let context = context.clone();
        info!("op26_get() - X-Span-ID: {:?}", context.get().0.clone());
        Err("Generic failure".into())
    }

    async fn op27_get(
        &self,
        context: &C) -> Result<Op27GetResponse, ApiError>
    {
        let context = context.clone();
        info!("op27_get() - X-Span-ID: {:?}", context.get().0.clone());
        Err("Generic failure".into())
    }

    async fn op28_get(
        &self,
        context: &C) -> Result<Op28GetResponse, ApiError>
    {
        let context = context.clone();
        info!("op28_get() - X-Span-ID: {:?}", context.get().0.clone());
        Err("Generic failure".into())
    }

    async fn op29_get(
        &self,
        context: &C) -> Result<Op29GetResponse, ApiError>
    {
        let context = context.clone();
        info!("op29_get() - X-Span-ID: {:?}", context.get().0.clone());
        Err("Generic failure".into())
    }

    async fn op2_get(
        &self,
        context: &C) -> Result<Op2GetResponse, ApiError>
    {
        let context = context.clone();
        info!("op2_get() - X-Span-ID: {:?}", context.get().0.clone());
        Err("Generic failure".into())
    }

    async fn op30_get(
        &self,
        context: &C) -> Result<Op30GetResponse, ApiError>
    {
        let context = context.clone();
        info!("op30_get() - X-Span-ID: {:?}", context.get().0.clone());
        Err("Generic failure".into())
    }

    async fn op31_get(
        &self,
        context: &C) -> Result<Op31GetResponse, ApiError>
    {
        let context = context.clone();
        info!("op31_get() - X-Span-ID: {:?}", context.get().0.clone());
        Err("Generic failure".into())
    }

    async fn op32_get(
        &self,
        context: &C) -> Result<Op32GetResponse, ApiError>
    {
        let context = context.clone();
        info!("op32_get() - X-Span-ID: {:?}", context.get().0.clone());
        Err("Generic failure".into())
    }

    async fn op33_get(
        &self,
        context: &C) -> Result<Op33GetResponse, ApiError>
    {
        let context = context.clone();
        info!("op33_get() - X-Span-ID: {:?}", context.get().0.clone());
        Err("Generic failure".into())
    }

    async fn op34_get(
        &self,
        context: &C) -> Result<Op34GetResponse, ApiError>
    {
        let context = context.clone();
        info!("op34_get() - X-Span-ID: {:?}", context.get().0.clone());
        Err("Generic failure".into())
    }

    async fn op35_get(
        &self,
        context: &C) -> Result<Op35GetResponse, ApiError>
    {
        let context = context.clone();
        info!("op35_get() - X-Span-ID: {:?}", context.get().0.clone());
        Err("Generic failure".into())
    }

    async fn op36_get(
        &self,
        context: &C) -> Result<Op36GetResponse, ApiError>
    {
        let context = context.clone();
        info!("op36_get() - X-Span-ID: {:?}", context.get().0.clone());
        Err("Generic failure".into())
    }

    async fn op37_get(
        &self,
        context: &C) -> Result<Op37GetResponse, ApiError>
    {
        let context = context.clone();
        info!("op37_get() - X-Span-ID: {:?}", context.get().0.clone());
        Err("Generic failure".into())
    }

    async fn op3_get(
        &self,
        context: &C) -> Result<Op3GetResponse, ApiError>
    {
        let context = context.clone();
        info!("op3_get() - X-Span-ID: {:?}", context.get().0.clone());
        Err("Generic failure".into())
    }

    async fn op4_get(
        &self,
        context: &C) -> Result<Op4GetResponse, ApiError>
    {
        let context = context.clone();
        info!("op4_get() - X-Span-ID: {:?}", context.get().0.clone());
        Err("Generic failure".into())
    }

    async fn op5_get(
        &self,
        context: &C) -> Result<Op5GetResponse, ApiError>
    {
        let context = context.clone();
        info!("op5_get() - X-Span-ID: {:?}", context.get().0.clone());
        Err("Generic failure".into())
    }

    async fn op6_get(
        &self,
        context: &C) -> Result<Op6GetResponse, ApiError>
    {
        let context = context.clone();
        info!("op6_get() - X-Span-ID: {:?}", context.get().0.clone());
        Err("Generic failure".into())
    }

    async fn op7_get(
        &self,
        context: &C) -> Result<Op7GetResponse, ApiError>
    {
        let context = context.clone();
        info!("op7_get() - X-Span-ID: {:?}", context.get().0.clone());
        Err("Generic failure".into())
    }

    async fn op8_get(
        &self,
        context: &C) -> Result<Op8GetResponse, ApiError>
    {
        let context = context.clone();
        info!("op8_get() - X-Span-ID: {:?}", context.get().0.clone());
        Err("Generic failure".into())
    }

    async fn op9_get(
        &self,
        context: &C) -> Result<Op9GetResponse, ApiError>
    {
        let context = context.clone();
        info!("op9_get() - X-Span-ID: {:?}", context.get().0.clone());
        Err("Generic failure".into())
    }

}
