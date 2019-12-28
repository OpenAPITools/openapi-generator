#![allow(missing_docs, trivial_casts, unused_variables, unused_mut, unused_imports, unused_extern_crates, non_camel_case_types)]

// Crates with macros

#[macro_use]
extern crate serde_derive;
#[cfg(any(feature = "server"))]
#[macro_use]
extern crate lazy_static;
#[cfg(any(feature = "client", feature = "server"))]
#[macro_use]
extern crate url;
#[macro_use]
extern crate log;

// Crates for conversion support
#[cfg(feature = "conversion")]
#[macro_use]
extern crate frunk_derives;
#[cfg(feature = "conversion")]
#[macro_use]
extern crate frunk_enum_derive;
#[cfg(feature = "conversion")]
extern crate frunk_core;

extern crate mime;
extern crate serde;
extern crate futures;
extern crate chrono;
extern crate swagger;

#[cfg(any(feature = "client", feature = "server"))]
extern crate hyper;
#[cfg(any(feature = "client"))]
extern crate hyper_tls;
#[cfg(any(feature = "client", feature = "server"))]
extern crate openssl;
#[cfg(any(feature = "client", feature = "server"))]
extern crate native_tls;
#[cfg(feature = "server")]
extern crate percent_encoding;
#[cfg(any(feature = "client", feature = "server"))]
extern crate serde_json;
#[cfg(any(feature = "client", feature = "server"))]
extern crate serde_ignored;
#[cfg(any(feature = "client", feature = "server"))]
extern crate tokio;

#[cfg(any(feature = "client", feature = "server"))]




#[cfg(any(feature = "client", feature = "server"))]
extern crate multipart;
extern crate mime_0_2;

#[cfg(any(feature = "client", feature = "server"))]


use hyper::header::HeaderValue;
use futures::Stream;
use std::io::Error;

#[allow(unused_imports)]
use std::collections::HashMap;

#[cfg(any(feature = "client", feature = "server"))]
mod mimetypes;

#[deprecated(note = "Import swagger-rs directly")]
pub use swagger::{ApiError, ContextWrapper};
#[deprecated(note = "Import futures directly")]
pub use futures::Future;

pub const BASE_PATH: &'static str = "";
pub const API_VERSION: &'static str = "1.0.7";


#[derive(Debug, PartialEq)]
pub enum MultipartRequestPostResponse {
    /// OK
    OK
}


/// API
pub trait Api<C> {


    fn multipart_request_post(&self, string_field: String, binary_field: swagger::ByteArray, optional_string_field: Option<String>, object_field: Option<models::MultipartRequestObjectField>, context: &C) -> Box<dyn Future<Item=MultipartRequestPostResponse, Error=ApiError> + Send>;

}

/// API without a `Context`
pub trait ApiNoContext {


    fn multipart_request_post(&self, string_field: String, binary_field: swagger::ByteArray, optional_string_field: Option<String>, object_field: Option<models::MultipartRequestObjectField>) -> Box<dyn Future<Item=MultipartRequestPostResponse, Error=ApiError> + Send>;

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


    fn multipart_request_post(&self, string_field: String, binary_field: swagger::ByteArray, optional_string_field: Option<String>, object_field: Option<models::MultipartRequestObjectField>) -> Box<dyn Future<Item=MultipartRequestPostResponse, Error=ApiError> + Send> {
        self.api().multipart_request_post(string_field, binary_field, optional_string_field, object_field, &self.context())
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
