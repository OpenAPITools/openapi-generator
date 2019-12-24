#![allow(missing_docs, trivial_casts, unused_variables, unused_mut, unused_imports, unused_extern_crates, non_camel_case_types)]

#[macro_use]
extern crate lazy_static;
#[macro_use]
extern crate log;
#[macro_use]
extern crate serde_derive;

#[cfg(any(feature = "client", feature = "server"))]
#[macro_use]
extern crate hyper;
#[cfg(any(feature = "client", feature = "server"))]
#[macro_use]
extern crate url;

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
extern crate serde_json;
extern crate serde_xml_rs;
extern crate futures;
extern crate chrono;
extern crate swagger;
extern crate uuid;

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
pub enum MultipleAuthSchemeGetResponse {
    /// Check that limiting to multiple required auth schemes works
    CheckThatLimitingToMultipleRequiredAuthSchemesWorks
}

#[derive(Debug, PartialEq)]
pub enum ReadonlyAuthSchemeGetResponse {
    /// Check that limiting to a single required auth scheme works
    CheckThatLimitingToASingleRequiredAuthSchemeWorks
}

#[derive(Debug, PartialEq)]
pub enum RequiredOctetStreamPutResponse {
    /// OK
    OK
}

#[derive(Debug, PartialEq)]
pub enum ResponsesWithHeadersGetResponse {
    /// Success
    Success
    {
        body: String,
        success_info: String,
    }
    ,
    /// Precondition Failed
    PreconditionFailed
    {
        further_info: String,
        failure_info: String,
    }
}

#[derive(Debug, PartialEq)]
pub enum UuidGetResponse {
    /// Duplicate Response long text. One.
    DuplicateResponseLongText
    (uuid::Uuid)
}

#[derive(Debug, PartialEq)]
pub enum XmlExtraPostResponse {
    /// OK
    OK
    ,
    /// Bad Request
    BadRequest
}

#[derive(Debug, PartialEq)]
pub enum XmlOtherPostResponse {
    /// OK
    OK
    ,
    /// Bad Request
    BadRequest
}

#[derive(Debug, PartialEq)]
pub enum XmlOtherPutResponse {
    /// OK
    OK
    ,
    /// Bad Request
    BadRequest
}

#[derive(Debug, PartialEq)]
pub enum XmlPostResponse {
    /// OK
    OK
    ,
    /// Bad Request
    BadRequest
}

#[derive(Debug, PartialEq)]
pub enum XmlPutResponse {
    /// OK
    OK
    ,
    /// Bad Request
    BadRequest
}


/// API
pub trait Api<C> {


    fn multiple_auth_scheme_get(&self, context: &C) -> Box<Future<Item=MultipleAuthSchemeGetResponse, Error=ApiError>>;


    fn readonly_auth_scheme_get(&self, context: &C) -> Box<Future<Item=ReadonlyAuthSchemeGetResponse, Error=ApiError>>;


    fn required_octet_stream_put(&self, body: swagger::ByteArray, context: &C) -> Box<Future<Item=RequiredOctetStreamPutResponse, Error=ApiError>>;


    fn responses_with_headers_get(&self, context: &C) -> Box<Future<Item=ResponsesWithHeadersGetResponse, Error=ApiError>>;


    fn uuid_get(&self, context: &C) -> Box<Future<Item=UuidGetResponse, Error=ApiError>>;


    fn xml_extra_post(&self, duplicate_xml_object: Option<models::DuplicateXmlObject>, context: &C) -> Box<Future<Item=XmlExtraPostResponse, Error=ApiError>>;


    fn xml_other_post(&self, another_xml_object: Option<models::AnotherXmlObject>, context: &C) -> Box<Future<Item=XmlOtherPostResponse, Error=ApiError>>;


    fn xml_other_put(&self, string: Option<models::AnotherXmlArray>, context: &C) -> Box<Future<Item=XmlOtherPutResponse, Error=ApiError>>;

    /// Post an array
    fn xml_post(&self, string: Option<models::XmlArray>, context: &C) -> Box<Future<Item=XmlPostResponse, Error=ApiError>>;


    fn xml_put(&self, xml_object: Option<models::XmlObject>, context: &C) -> Box<Future<Item=XmlPutResponse, Error=ApiError>>;

}

/// API without a `Context`
pub trait ApiNoContext {


    fn multiple_auth_scheme_get(&self) -> Box<Future<Item=MultipleAuthSchemeGetResponse, Error=ApiError>>;


    fn readonly_auth_scheme_get(&self) -> Box<Future<Item=ReadonlyAuthSchemeGetResponse, Error=ApiError>>;


    fn required_octet_stream_put(&self, body: swagger::ByteArray) -> Box<Future<Item=RequiredOctetStreamPutResponse, Error=ApiError>>;


    fn responses_with_headers_get(&self) -> Box<Future<Item=ResponsesWithHeadersGetResponse, Error=ApiError>>;


    fn uuid_get(&self) -> Box<Future<Item=UuidGetResponse, Error=ApiError>>;


    fn xml_extra_post(&self, duplicate_xml_object: Option<models::DuplicateXmlObject>) -> Box<Future<Item=XmlExtraPostResponse, Error=ApiError>>;


    fn xml_other_post(&self, another_xml_object: Option<models::AnotherXmlObject>) -> Box<Future<Item=XmlOtherPostResponse, Error=ApiError>>;


    fn xml_other_put(&self, string: Option<models::AnotherXmlArray>) -> Box<Future<Item=XmlOtherPutResponse, Error=ApiError>>;

    /// Post an array
    fn xml_post(&self, string: Option<models::XmlArray>) -> Box<Future<Item=XmlPostResponse, Error=ApiError>>;


    fn xml_put(&self, xml_object: Option<models::XmlObject>) -> Box<Future<Item=XmlPutResponse, Error=ApiError>>;

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


    fn multiple_auth_scheme_get(&self) -> Box<Future<Item=MultipleAuthSchemeGetResponse, Error=ApiError>> {
        self.api().multiple_auth_scheme_get(&self.context())
    }


    fn readonly_auth_scheme_get(&self) -> Box<Future<Item=ReadonlyAuthSchemeGetResponse, Error=ApiError>> {
        self.api().readonly_auth_scheme_get(&self.context())
    }


    fn required_octet_stream_put(&self, body: swagger::ByteArray) -> Box<Future<Item=RequiredOctetStreamPutResponse, Error=ApiError>> {
        self.api().required_octet_stream_put(body, &self.context())
    }


    fn responses_with_headers_get(&self) -> Box<Future<Item=ResponsesWithHeadersGetResponse, Error=ApiError>> {
        self.api().responses_with_headers_get(&self.context())
    }


    fn uuid_get(&self) -> Box<Future<Item=UuidGetResponse, Error=ApiError>> {
        self.api().uuid_get(&self.context())
    }


    fn xml_extra_post(&self, duplicate_xml_object: Option<models::DuplicateXmlObject>) -> Box<Future<Item=XmlExtraPostResponse, Error=ApiError>> {
        self.api().xml_extra_post(duplicate_xml_object, &self.context())
    }


    fn xml_other_post(&self, another_xml_object: Option<models::AnotherXmlObject>) -> Box<Future<Item=XmlOtherPostResponse, Error=ApiError>> {
        self.api().xml_other_post(another_xml_object, &self.context())
    }


    fn xml_other_put(&self, string: Option<models::AnotherXmlArray>) -> Box<Future<Item=XmlOtherPutResponse, Error=ApiError>> {
        self.api().xml_other_put(string, &self.context())
    }

    /// Post an array
    fn xml_post(&self, string: Option<models::XmlArray>) -> Box<Future<Item=XmlPostResponse, Error=ApiError>> {
        self.api().xml_post(string, &self.context())
    }


    fn xml_put(&self, xml_object: Option<models::XmlObject>) -> Box<Future<Item=XmlPutResponse, Error=ApiError>> {
        self.api().xml_put(xml_object, &self.context())
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
