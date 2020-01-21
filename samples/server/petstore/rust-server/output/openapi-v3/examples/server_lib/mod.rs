//! Main library entry point for openapi_v3 implementation.

mod server;

mod errors {
    error_chain!{}
}

pub use self::errors::*;
use std::io;
use std::clone::Clone;
use std::marker::PhantomData;
use hyper;
use openapi_v3;
use swagger::{Has, XSpanIdString};
use swagger::auth::Authorization;

pub struct NewService<C>{
    marker: PhantomData<C>
}

impl<C> NewService<C>{
    pub fn new() -> Self {
        NewService{marker:PhantomData}
    }
}

impl<C> hyper::server::NewService for NewService<C> where C: Has<XSpanIdString> + Has<Option<Authorization>> + Clone + 'static {
    type Request = (hyper::Request, C);
    type Response = hyper::Response;
    type Error = hyper::Error;
    type Instance = openapi_v3::server::Service<server::Server<C>, C>;

    /// Instantiate a new server.
    fn new_service(&self) -> io::Result<Self::Instance> {
        Ok(openapi_v3::server::Service::new(server::Server::new()))
    }
}
