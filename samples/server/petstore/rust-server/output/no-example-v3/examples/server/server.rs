//! Main library entry point for no_example_v3 implementation.

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


use no_example_v3::models;

pub fn create(addr: &str, https: Option<SslAcceptorBuilder>) -> Box<Future<Item = (), Error = ()> + Send> {
    let addr = addr.parse().expect("Failed to parse bind address");

    let server = Server::new();

    let service_fn = MakeService::new(server);

    let service_fn = MakeAllowAllAuthenticator::new(service_fn, "cosmo");

    let service_fn =
        no_example_v3::server::context::MakeAddContext::<_, EmptyContext>::new(
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


use no_example_v3::{
    Api,
    ApiError,
    OpGetResponse,
};
use no_example_v3::server::MakeService;

impl<C> Api<C> for Server<C> where C: Has<XSpanIdString>{
    fn op_get(
        &self,
        inline_object: models::InlineObject,
        context: &C) -> Box<Future<Item=OpGetResponse, Error=ApiError> + Send>
    {
        let context = context.clone();
        info!("op_get({:?}) - X-Span-ID: {:?}", inline_object, context.get().0.clone());
        Box::new(future::err("Generic failure".into()))
    }

}
