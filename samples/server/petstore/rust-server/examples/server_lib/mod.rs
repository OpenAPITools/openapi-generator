//! Main library entry point for petstore_api implementation.

mod server;

mod errors {
    error_chain!{}
}

pub use self::errors::*;
use std::io;
use hyper;
use petstore_api;

pub struct NewService;

impl hyper::server::NewService for NewService {
    type Request = (hyper::Request, petstore_api::Context);
    type Response = hyper::Response;
    type Error = hyper::Error;
    type Instance = petstore_api::server::Service<server::Server>;

    /// Instantiate a new server.
    fn new_service(&self) -> io::Result<Self::Instance> {
        Ok(petstore_api::server::Service::new(server::Server))
    }
}
