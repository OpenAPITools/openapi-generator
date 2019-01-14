#![allow(missing_docs, trivial_casts, unused_variables, unused_mut, unused_imports, unused_extern_crates, non_camel_case_types)]
extern crate serde;
#[macro_use]
extern crate serde_derive;
extern crate serde_json;


extern crate futures;
extern crate chrono;
#[macro_use]
extern crate lazy_static;
#[macro_use]
extern crate log;

// Logically this should be in the client and server modules, but rust doesn't allow `macro_use` from a module.
#[cfg(any(feature = "client", feature = "server"))]
#[macro_use]
extern crate hyper;

extern crate swagger;

#[macro_use]
extern crate url;

use futures::Stream;
use std::io::Error;

#[allow(unused_imports)]
use std::collections::HashMap;

pub use futures::Future;

#[cfg(any(feature = "client", feature = "server"))]
mod mimetypes;

pub use swagger::{ApiError, ContextWrapper};

pub const BASE_PATH: &'static str = "";
pub const API_VERSION: &'static str = "2.3.4";


#[derive(Debug, PartialEq)]
pub enum DummyGetResponse {
    /// Success
    Success ,
}

#[derive(Debug, PartialEq)]
pub enum DummyPutResponse {
    /// Success
    Success ,
}

#[derive(Debug, PartialEq)]
pub enum FileResponseGetResponse {
    /// Success
    Success ( swagger::ByteArray ) ,
}

#[derive(Debug, PartialEq)]
pub enum HtmlPostResponse {
    /// Success
    Success ( String ) ,
}

#[derive(Debug, PartialEq)]
pub enum RawJsonGetResponse {
    /// Success
    Success ( serde_json::Value ) ,
}


/// API
pub trait Api<C> {

    /// A dummy endpoint to make the spec valid.
    fn dummy_get(&self, context: &C) -> Box<Future<Item=DummyGetResponse, Error=ApiError>>;


    fn dummy_put(&self, nested_response: models::InlineObject, context: &C) -> Box<Future<Item=DummyPutResponse, Error=ApiError>>;

    /// Get a file
    fn file_response_get(&self, context: &C) -> Box<Future<Item=FileResponseGetResponse, Error=ApiError>>;

    /// Test HTML handling
    fn html_post(&self, body: String, context: &C) -> Box<Future<Item=HtmlPostResponse, Error=ApiError>>;

    /// Get an arbitrary JSON blob.
    fn raw_json_get(&self, context: &C) -> Box<Future<Item=RawJsonGetResponse, Error=ApiError>>;

}

/// API without a `Context`
pub trait ApiNoContext {

    /// A dummy endpoint to make the spec valid.
    fn dummy_get(&self) -> Box<Future<Item=DummyGetResponse, Error=ApiError>>;


    fn dummy_put(&self, nested_response: models::InlineObject) -> Box<Future<Item=DummyPutResponse, Error=ApiError>>;

    /// Get a file
    fn file_response_get(&self) -> Box<Future<Item=FileResponseGetResponse, Error=ApiError>>;

    /// Test HTML handling
    fn html_post(&self, body: String) -> Box<Future<Item=HtmlPostResponse, Error=ApiError>>;

    /// Get an arbitrary JSON blob.
    fn raw_json_get(&self) -> Box<Future<Item=RawJsonGetResponse, Error=ApiError>>;

}

/// Trait to extend an API to make it easy to bind it to a context.
pub trait ContextWrapperExt<'a, C> where Self: Sized {
    /// Binds this API to a context.
    fn with_context(self: &'a Self, context: C) -> ContextWrapper<'a, Self, C>;
}

impl<'a, T: Api<C> + Sized, C> ContextWrapperExt<'a, C> for T {
    fn with_context(self: &'a T, context: C) -> ContextWrapper<'a, T, C> {
         ContextWrapper::<T, C>::new(self, context)
    }
}

impl<'a, T: Api<C>, C> ApiNoContext for ContextWrapper<'a, T, C> {

    /// A dummy endpoint to make the spec valid.
    fn dummy_get(&self) -> Box<Future<Item=DummyGetResponse, Error=ApiError>> {
        self.api().dummy_get(&self.context())
    }


    fn dummy_put(&self, nested_response: models::InlineObject) -> Box<Future<Item=DummyPutResponse, Error=ApiError>> {
        self.api().dummy_put(nested_response, &self.context())
    }

    /// Get a file
    fn file_response_get(&self) -> Box<Future<Item=FileResponseGetResponse, Error=ApiError>> {
        self.api().file_response_get(&self.context())
    }

    /// Test HTML handling
    fn html_post(&self, body: String) -> Box<Future<Item=HtmlPostResponse, Error=ApiError>> {
        self.api().html_post(body, &self.context())
    }

    /// Get an arbitrary JSON blob.
    fn raw_json_get(&self) -> Box<Future<Item=RawJsonGetResponse, Error=ApiError>> {
        self.api().raw_json_get(&self.context())
    }

}

#[cfg(feature = "client")]
pub mod client;

// Re-export Client as a top-level name
#[cfg(feature = "client")]
pub use self::client::Client;

#[cfg(feature = "server")]
pub mod server;

// Re-export router() as a top-level name
#[cfg(feature = "server")]
pub use self::server::Service;

pub mod models;
