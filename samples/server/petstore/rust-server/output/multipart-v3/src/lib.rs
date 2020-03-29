#![allow(missing_docs, trivial_casts, unused_variables, unused_mut, unused_imports, unused_extern_crates, non_camel_case_types)]

// Crates with macros

#[macro_use]
extern crate serde_derive;
#[cfg(feature = "server")]
#[macro_use]
extern crate lazy_static;
#[cfg(any(feature = "client", feature = "server"))]
#[macro_use]
extern crate url;
#[macro_use]
extern crate log;
#[cfg(any(feature = "client", feature = "server"))]
#[macro_use]
extern crate hyper_0_10;

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
#[cfg(feature = "server")]
#[cfg(not(any(target_os = "macos", target_os = "windows", target_os = "ios")))]
extern crate hyper_openssl;
#[cfg(any(feature = "client", feature = "server"))]
extern crate mime_0_2;
#[cfg(any(feature = "client", feature = "server"))]
extern crate mime_multipart;
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

#[cfg(any(feature = "client", feature = "server"))]


use hyper::header::HeaderValue;
use futures::Stream;
use std::io::Error;

#[deprecated(note = "Import swagger-rs directly")]
pub use swagger::{ApiError, ContextWrapper};
#[deprecated(note = "Import futures directly")]
pub use futures::Future;

pub const BASE_PATH: &'static str = "";
pub const API_VERSION: &'static str = "1.0.7";

#[derive(Debug, PartialEq)]
pub enum MultipartRelatedRequestPostResponse {
    /// OK
    OK
}

#[derive(Debug, PartialEq)]
pub enum MultipartRequestPostResponse {
    /// OK
    OK
}

#[derive(Debug, PartialEq)]
pub enum MultipleIdenticalMimeTypesPostResponse {
    /// OK
    OK
}

/// API
pub trait Api<C> {
    fn multipart_related_request_post(
        &self,
        required_binary_field: swagger::ByteArray,
        object_field: Option<models::MultipartRequestObjectField>,
        optional_binary_field: Option<swagger::ByteArray>,
        context: &C) -> Box<dyn Future<Item=MultipartRelatedRequestPostResponse, Error=ApiError> + Send>;

    fn multipart_request_post(
        &self,
        string_field: String,
        binary_field: swagger::ByteArray,
        optional_string_field: Option<String>,
        object_field: Option<models::MultipartRequestObjectField>,
        context: &C) -> Box<dyn Future<Item=MultipartRequestPostResponse, Error=ApiError> + Send>;

    fn multiple_identical_mime_types_post(
        &self,
        binary1: Option<swagger::ByteArray>,
        binary2: Option<swagger::ByteArray>,
        context: &C) -> Box<dyn Future<Item=MultipleIdenticalMimeTypesPostResponse, Error=ApiError> + Send>;

}

/// API without a `Context`
pub trait ApiNoContext {
    fn multipart_related_request_post(
        &self,
        required_binary_field: swagger::ByteArray,
        object_field: Option<models::MultipartRequestObjectField>,
        optional_binary_field: Option<swagger::ByteArray>,
        ) -> Box<dyn Future<Item=MultipartRelatedRequestPostResponse, Error=ApiError> + Send>;

    fn multipart_request_post(
        &self,
        string_field: String,
        binary_field: swagger::ByteArray,
        optional_string_field: Option<String>,
        object_field: Option<models::MultipartRequestObjectField>,
        ) -> Box<dyn Future<Item=MultipartRequestPostResponse, Error=ApiError> + Send>;

    fn multiple_identical_mime_types_post(
        &self,
        binary1: Option<swagger::ByteArray>,
        binary2: Option<swagger::ByteArray>,
        ) -> Box<dyn Future<Item=MultipleIdenticalMimeTypesPostResponse, Error=ApiError> + Send>;

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
    fn multipart_related_request_post(
        &self,
        required_binary_field: swagger::ByteArray,
        object_field: Option<models::MultipartRequestObjectField>,
        optional_binary_field: Option<swagger::ByteArray>,
        ) -> Box<dyn Future<Item=MultipartRelatedRequestPostResponse, Error=ApiError> + Send>
    {
        self.api().multipart_related_request_post(required_binary_field, object_field, optional_binary_field, &self.context())
    }

    fn multipart_request_post(
        &self,
        string_field: String,
        binary_field: swagger::ByteArray,
        optional_string_field: Option<String>,
        object_field: Option<models::MultipartRequestObjectField>,
        ) -> Box<dyn Future<Item=MultipartRequestPostResponse, Error=ApiError> + Send>
    {
        self.api().multipart_request_post(string_field, binary_field, optional_string_field, object_field, &self.context())
    }

    fn multiple_identical_mime_types_post(
        &self,
        binary1: Option<swagger::ByteArray>,
        binary2: Option<swagger::ByteArray>,
        ) -> Box<dyn Future<Item=MultipleIdenticalMimeTypesPostResponse, Error=ApiError> + Send>
    {
        self.api().multiple_identical_mime_types_post(binary1, binary2, &self.context())
    }

}

#[cfg(feature = "client")]
pub mod client;

// Re-export Client as a top-level name
#[cfg(feature = "client")]
pub use client::Client;

#[cfg(feature = "server")]
pub mod server;

// Re-export router() as a top-level name
#[cfg(feature = "server")]
pub use self::server::Service;

#[cfg(feature = "server")]
pub mod context;

pub mod models;
pub mod header;
