#![allow(missing_docs, trivial_casts, unused_variables, unused_mut, unused_imports, unused_extern_crates, non_camel_case_types)]

use futures::Stream;
use std::io::Error;

#[deprecated(note = "Import swagger-rs directly")]
pub use swagger::{ApiError, ContextWrapper};
#[deprecated(note = "Import futures directly")]
pub use futures::Future;

pub const BASE_PATH: &'static str = "";
pub const API_VERSION: &'static str = "2.3.4";

#[derive(Debug, PartialEq)]
pub enum AllOfGetResponse {
    /// OK
    OK
    (models::AllOfObject)
}

#[derive(Debug, PartialEq)]
pub enum DummyGetResponse {
    /// Success
    Success
}

#[derive(Debug, PartialEq)]
pub enum DummyPutResponse {
    /// Success
    Success
}

#[derive(Debug, PartialEq)]
pub enum FileResponseGetResponse {
    /// Success
    Success
    (swagger::ByteArray)
}

#[derive(Debug, PartialEq)]
pub enum GetStructuredYamlResponse {
    /// OK
    OK
    (String)
}

#[derive(Debug, PartialEq)]
pub enum HtmlPostResponse {
    /// Success
    Success
    (String)
}

#[derive(Debug, PartialEq)]
pub enum PostYamlResponse {
    /// OK
    OK
}

#[derive(Debug, PartialEq)]
pub enum RawJsonGetResponse {
    /// Success
    Success
    (serde_json::Value)
}

#[derive(Debug, PartialEq)]
pub enum SoloObjectPostResponse {
    /// OK
    OK
}

/// API
pub trait Api<C> {
    fn all_of_get(
        &self,
        context: &C) -> Box<dyn Future<Item=AllOfGetResponse, Error=ApiError> + Send>;

    /// A dummy endpoint to make the spec valid.
    fn dummy_get(
        &self,
        context: &C) -> Box<dyn Future<Item=DummyGetResponse, Error=ApiError> + Send>;

    fn dummy_put(
        &self,
        nested_response: models::DummyPutBody,
        context: &C) -> Box<dyn Future<Item=DummyPutResponse, Error=ApiError> + Send>;

    /// Get a file
    fn file_response_get(
        &self,
        context: &C) -> Box<dyn Future<Item=FileResponseGetResponse, Error=ApiError> + Send>;

    fn get_structured_yaml(
        &self,
        context: &C) -> Box<dyn Future<Item=GetStructuredYamlResponse, Error=ApiError> + Send>;

    /// Test HTML handling
    fn html_post(
        &self,
        body: String,
        context: &C) -> Box<dyn Future<Item=HtmlPostResponse, Error=ApiError> + Send>;

    fn post_yaml(
        &self,
        value: String,
        context: &C) -> Box<dyn Future<Item=PostYamlResponse, Error=ApiError> + Send>;

    /// Get an arbitrary JSON blob.
    fn raw_json_get(
        &self,
        context: &C) -> Box<dyn Future<Item=RawJsonGetResponse, Error=ApiError> + Send>;

    /// Send an arbitrary JSON blob
    fn solo_object_post(
        &self,
        value: serde_json::Value,
        context: &C) -> Box<dyn Future<Item=SoloObjectPostResponse, Error=ApiError> + Send>;

}

/// API without a `Context`
pub trait ApiNoContext {
    fn all_of_get(
        &self,
        ) -> Box<dyn Future<Item=AllOfGetResponse, Error=ApiError> + Send>;

    /// A dummy endpoint to make the spec valid.
    fn dummy_get(
        &self,
        ) -> Box<dyn Future<Item=DummyGetResponse, Error=ApiError> + Send>;

    fn dummy_put(
        &self,
        nested_response: models::DummyPutBody,
        ) -> Box<dyn Future<Item=DummyPutResponse, Error=ApiError> + Send>;

    /// Get a file
    fn file_response_get(
        &self,
        ) -> Box<dyn Future<Item=FileResponseGetResponse, Error=ApiError> + Send>;

    fn get_structured_yaml(
        &self,
        ) -> Box<dyn Future<Item=GetStructuredYamlResponse, Error=ApiError> + Send>;

    /// Test HTML handling
    fn html_post(
        &self,
        body: String,
        ) -> Box<dyn Future<Item=HtmlPostResponse, Error=ApiError> + Send>;

    fn post_yaml(
        &self,
        value: String,
        ) -> Box<dyn Future<Item=PostYamlResponse, Error=ApiError> + Send>;

    /// Get an arbitrary JSON blob.
    fn raw_json_get(
        &self,
        ) -> Box<dyn Future<Item=RawJsonGetResponse, Error=ApiError> + Send>;

    /// Send an arbitrary JSON blob
    fn solo_object_post(
        &self,
        value: serde_json::Value,
        ) -> Box<dyn Future<Item=SoloObjectPostResponse, Error=ApiError> + Send>;

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
    fn all_of_get(
        &self,
        ) -> Box<dyn Future<Item=AllOfGetResponse, Error=ApiError> + Send>
    {
        self.api().all_of_get(&self.context())
    }

    /// A dummy endpoint to make the spec valid.
    fn dummy_get(
        &self,
        ) -> Box<dyn Future<Item=DummyGetResponse, Error=ApiError> + Send>
    {
        self.api().dummy_get(&self.context())
    }

    fn dummy_put(
        &self,
        nested_response: models::DummyPutBody,
        ) -> Box<dyn Future<Item=DummyPutResponse, Error=ApiError> + Send>
    {
        self.api().dummy_put(nested_response, &self.context())
    }

    /// Get a file
    fn file_response_get(
        &self,
        ) -> Box<dyn Future<Item=FileResponseGetResponse, Error=ApiError> + Send>
    {
        self.api().file_response_get(&self.context())
    }

    fn get_structured_yaml(
        &self,
        ) -> Box<dyn Future<Item=GetStructuredYamlResponse, Error=ApiError> + Send>
    {
        self.api().get_structured_yaml(&self.context())
    }

    /// Test HTML handling
    fn html_post(
        &self,
        body: String,
        ) -> Box<dyn Future<Item=HtmlPostResponse, Error=ApiError> + Send>
    {
        self.api().html_post(body, &self.context())
    }

    fn post_yaml(
        &self,
        value: String,
        ) -> Box<dyn Future<Item=PostYamlResponse, Error=ApiError> + Send>
    {
        self.api().post_yaml(value, &self.context())
    }

    /// Get an arbitrary JSON blob.
    fn raw_json_get(
        &self,
        ) -> Box<dyn Future<Item=RawJsonGetResponse, Error=ApiError> + Send>
    {
        self.api().raw_json_get(&self.context())
    }

    /// Send an arbitrary JSON blob
    fn solo_object_post(
        &self,
        value: serde_json::Value,
        ) -> Box<dyn Future<Item=SoloObjectPostResponse, Error=ApiError> + Send>
    {
        self.api().solo_object_post(value, &self.context())
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

#[cfg(any(feature = "client", feature = "server"))]
pub(crate) mod header;
