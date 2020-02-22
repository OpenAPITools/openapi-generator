//! Main library entry point for ops_v3 implementation.

#![allow(unused_imports)]

mod errors {
    error_chain!{}
}

pub use self::errors::*;

use chrono;
use futures::{future, Future, Stream};
use hyper::server::conn::Http;
use hyper::service::MakeService as _;
use native_tls;
use openssl::ssl::SslAcceptorBuilder;
use std::collections::HashMap;
use std::marker::PhantomData;
use std::net::SocketAddr;
use std::sync::{Arc, Mutex};
use swagger;
use swagger::{Has, XSpanIdString};
use swagger::auth::MakeAllowAllAuthenticator;
use swagger::EmptyContext;
use tokio::net::TcpListener;
use tokio_tls::TlsAcceptorExt;


use ops_v3::models;

pub fn create(addr: &str, https: Option<SslAcceptorBuilder>) -> Box<Future<Item = (), Error = ()> + Send> {
    let addr = addr.parse().expect("Failed to parse bind address");

    let server = Server::new();

    let service_fn = MakeService::new(server);

    let service_fn = MakeAllowAllAuthenticator::new(service_fn, "cosmo");

    let service_fn =
        ops_v3::server::context::MakeAddContext::<_, EmptyContext>::new(
            service_fn
        );

    if let Some(ssl) = https {
        let builder: native_tls::TlsAcceptorBuilder = native_tls::backend::openssl::TlsAcceptorBuilderExt::from_openssl(ssl);
        let tls_acceptor = builder.build().expect("Failed to build TLS acceptor");
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


use ops_v3::{
    Api,
    ApiError,
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

impl<C> Api<C> for Server<C> where C: Has<XSpanIdString>{
    fn op10_get(
        &self,
        context: &C) -> Box<Future<Item=Op10GetResponse, Error=ApiError> + Send>
    {
        let context = context.clone();
        info!("op10_get() - X-Span-ID: {:?}", context.get().0.clone());
        Box::new(future::err("Generic failure".into()))
    }

    fn op11_get(
        &self,
        context: &C) -> Box<Future<Item=Op11GetResponse, Error=ApiError> + Send>
    {
        let context = context.clone();
        info!("op11_get() - X-Span-ID: {:?}", context.get().0.clone());
        Box::new(future::err("Generic failure".into()))
    }

    fn op12_get(
        &self,
        context: &C) -> Box<Future<Item=Op12GetResponse, Error=ApiError> + Send>
    {
        let context = context.clone();
        info!("op12_get() - X-Span-ID: {:?}", context.get().0.clone());
        Box::new(future::err("Generic failure".into()))
    }

    fn op13_get(
        &self,
        context: &C) -> Box<Future<Item=Op13GetResponse, Error=ApiError> + Send>
    {
        let context = context.clone();
        info!("op13_get() - X-Span-ID: {:?}", context.get().0.clone());
        Box::new(future::err("Generic failure".into()))
    }

    fn op14_get(
        &self,
        context: &C) -> Box<Future<Item=Op14GetResponse, Error=ApiError> + Send>
    {
        let context = context.clone();
        info!("op14_get() - X-Span-ID: {:?}", context.get().0.clone());
        Box::new(future::err("Generic failure".into()))
    }

    fn op15_get(
        &self,
        context: &C) -> Box<Future<Item=Op15GetResponse, Error=ApiError> + Send>
    {
        let context = context.clone();
        info!("op15_get() - X-Span-ID: {:?}", context.get().0.clone());
        Box::new(future::err("Generic failure".into()))
    }

    fn op16_get(
        &self,
        context: &C) -> Box<Future<Item=Op16GetResponse, Error=ApiError> + Send>
    {
        let context = context.clone();
        info!("op16_get() - X-Span-ID: {:?}", context.get().0.clone());
        Box::new(future::err("Generic failure".into()))
    }

    fn op17_get(
        &self,
        context: &C) -> Box<Future<Item=Op17GetResponse, Error=ApiError> + Send>
    {
        let context = context.clone();
        info!("op17_get() - X-Span-ID: {:?}", context.get().0.clone());
        Box::new(future::err("Generic failure".into()))
    }

    fn op18_get(
        &self,
        context: &C) -> Box<Future<Item=Op18GetResponse, Error=ApiError> + Send>
    {
        let context = context.clone();
        info!("op18_get() - X-Span-ID: {:?}", context.get().0.clone());
        Box::new(future::err("Generic failure".into()))
    }

    fn op19_get(
        &self,
        context: &C) -> Box<Future<Item=Op19GetResponse, Error=ApiError> + Send>
    {
        let context = context.clone();
        info!("op19_get() - X-Span-ID: {:?}", context.get().0.clone());
        Box::new(future::err("Generic failure".into()))
    }

    fn op1_get(
        &self,
        context: &C) -> Box<Future<Item=Op1GetResponse, Error=ApiError> + Send>
    {
        let context = context.clone();
        info!("op1_get() - X-Span-ID: {:?}", context.get().0.clone());
        Box::new(future::err("Generic failure".into()))
    }

    fn op20_get(
        &self,
        context: &C) -> Box<Future<Item=Op20GetResponse, Error=ApiError> + Send>
    {
        let context = context.clone();
        info!("op20_get() - X-Span-ID: {:?}", context.get().0.clone());
        Box::new(future::err("Generic failure".into()))
    }

    fn op21_get(
        &self,
        context: &C) -> Box<Future<Item=Op21GetResponse, Error=ApiError> + Send>
    {
        let context = context.clone();
        info!("op21_get() - X-Span-ID: {:?}", context.get().0.clone());
        Box::new(future::err("Generic failure".into()))
    }

    fn op22_get(
        &self,
        context: &C) -> Box<Future<Item=Op22GetResponse, Error=ApiError> + Send>
    {
        let context = context.clone();
        info!("op22_get() - X-Span-ID: {:?}", context.get().0.clone());
        Box::new(future::err("Generic failure".into()))
    }

    fn op23_get(
        &self,
        context: &C) -> Box<Future<Item=Op23GetResponse, Error=ApiError> + Send>
    {
        let context = context.clone();
        info!("op23_get() - X-Span-ID: {:?}", context.get().0.clone());
        Box::new(future::err("Generic failure".into()))
    }

    fn op24_get(
        &self,
        context: &C) -> Box<Future<Item=Op24GetResponse, Error=ApiError> + Send>
    {
        let context = context.clone();
        info!("op24_get() - X-Span-ID: {:?}", context.get().0.clone());
        Box::new(future::err("Generic failure".into()))
    }

    fn op25_get(
        &self,
        context: &C) -> Box<Future<Item=Op25GetResponse, Error=ApiError> + Send>
    {
        let context = context.clone();
        info!("op25_get() - X-Span-ID: {:?}", context.get().0.clone());
        Box::new(future::err("Generic failure".into()))
    }

    fn op26_get(
        &self,
        context: &C) -> Box<Future<Item=Op26GetResponse, Error=ApiError> + Send>
    {
        let context = context.clone();
        info!("op26_get() - X-Span-ID: {:?}", context.get().0.clone());
        Box::new(future::err("Generic failure".into()))
    }

    fn op27_get(
        &self,
        context: &C) -> Box<Future<Item=Op27GetResponse, Error=ApiError> + Send>
    {
        let context = context.clone();
        info!("op27_get() - X-Span-ID: {:?}", context.get().0.clone());
        Box::new(future::err("Generic failure".into()))
    }

    fn op28_get(
        &self,
        context: &C) -> Box<Future<Item=Op28GetResponse, Error=ApiError> + Send>
    {
        let context = context.clone();
        info!("op28_get() - X-Span-ID: {:?}", context.get().0.clone());
        Box::new(future::err("Generic failure".into()))
    }

    fn op29_get(
        &self,
        context: &C) -> Box<Future<Item=Op29GetResponse, Error=ApiError> + Send>
    {
        let context = context.clone();
        info!("op29_get() - X-Span-ID: {:?}", context.get().0.clone());
        Box::new(future::err("Generic failure".into()))
    }

    fn op2_get(
        &self,
        context: &C) -> Box<Future<Item=Op2GetResponse, Error=ApiError> + Send>
    {
        let context = context.clone();
        info!("op2_get() - X-Span-ID: {:?}", context.get().0.clone());
        Box::new(future::err("Generic failure".into()))
    }

    fn op30_get(
        &self,
        context: &C) -> Box<Future<Item=Op30GetResponse, Error=ApiError> + Send>
    {
        let context = context.clone();
        info!("op30_get() - X-Span-ID: {:?}", context.get().0.clone());
        Box::new(future::err("Generic failure".into()))
    }

    fn op31_get(
        &self,
        context: &C) -> Box<Future<Item=Op31GetResponse, Error=ApiError> + Send>
    {
        let context = context.clone();
        info!("op31_get() - X-Span-ID: {:?}", context.get().0.clone());
        Box::new(future::err("Generic failure".into()))
    }

    fn op32_get(
        &self,
        context: &C) -> Box<Future<Item=Op32GetResponse, Error=ApiError> + Send>
    {
        let context = context.clone();
        info!("op32_get() - X-Span-ID: {:?}", context.get().0.clone());
        Box::new(future::err("Generic failure".into()))
    }

    fn op33_get(
        &self,
        context: &C) -> Box<Future<Item=Op33GetResponse, Error=ApiError> + Send>
    {
        let context = context.clone();
        info!("op33_get() - X-Span-ID: {:?}", context.get().0.clone());
        Box::new(future::err("Generic failure".into()))
    }

    fn op34_get(
        &self,
        context: &C) -> Box<Future<Item=Op34GetResponse, Error=ApiError> + Send>
    {
        let context = context.clone();
        info!("op34_get() - X-Span-ID: {:?}", context.get().0.clone());
        Box::new(future::err("Generic failure".into()))
    }

    fn op35_get(
        &self,
        context: &C) -> Box<Future<Item=Op35GetResponse, Error=ApiError> + Send>
    {
        let context = context.clone();
        info!("op35_get() - X-Span-ID: {:?}", context.get().0.clone());
        Box::new(future::err("Generic failure".into()))
    }

    fn op36_get(
        &self,
        context: &C) -> Box<Future<Item=Op36GetResponse, Error=ApiError> + Send>
    {
        let context = context.clone();
        info!("op36_get() - X-Span-ID: {:?}", context.get().0.clone());
        Box::new(future::err("Generic failure".into()))
    }

    fn op37_get(
        &self,
        context: &C) -> Box<Future<Item=Op37GetResponse, Error=ApiError> + Send>
    {
        let context = context.clone();
        info!("op37_get() - X-Span-ID: {:?}", context.get().0.clone());
        Box::new(future::err("Generic failure".into()))
    }

    fn op3_get(
        &self,
        context: &C) -> Box<Future<Item=Op3GetResponse, Error=ApiError> + Send>
    {
        let context = context.clone();
        info!("op3_get() - X-Span-ID: {:?}", context.get().0.clone());
        Box::new(future::err("Generic failure".into()))
    }

    fn op4_get(
        &self,
        context: &C) -> Box<Future<Item=Op4GetResponse, Error=ApiError> + Send>
    {
        let context = context.clone();
        info!("op4_get() - X-Span-ID: {:?}", context.get().0.clone());
        Box::new(future::err("Generic failure".into()))
    }

    fn op5_get(
        &self,
        context: &C) -> Box<Future<Item=Op5GetResponse, Error=ApiError> + Send>
    {
        let context = context.clone();
        info!("op5_get() - X-Span-ID: {:?}", context.get().0.clone());
        Box::new(future::err("Generic failure".into()))
    }

    fn op6_get(
        &self,
        context: &C) -> Box<Future<Item=Op6GetResponse, Error=ApiError> + Send>
    {
        let context = context.clone();
        info!("op6_get() - X-Span-ID: {:?}", context.get().0.clone());
        Box::new(future::err("Generic failure".into()))
    }

    fn op7_get(
        &self,
        context: &C) -> Box<Future<Item=Op7GetResponse, Error=ApiError> + Send>
    {
        let context = context.clone();
        info!("op7_get() - X-Span-ID: {:?}", context.get().0.clone());
        Box::new(future::err("Generic failure".into()))
    }

    fn op8_get(
        &self,
        context: &C) -> Box<Future<Item=Op8GetResponse, Error=ApiError> + Send>
    {
        let context = context.clone();
        info!("op8_get() - X-Span-ID: {:?}", context.get().0.clone());
        Box::new(future::err("Generic failure".into()))
    }

    fn op9_get(
        &self,
        context: &C) -> Box<Future<Item=Op9GetResponse, Error=ApiError> + Send>
    {
        let context = context.clone();
        info!("op9_get() - X-Span-ID: {:?}", context.get().0.clone());
        Box::new(future::err("Generic failure".into()))
    }

}
