#![allow(missing_docs, trivial_casts, unused_variables, unused_mut, unused_imports, unused_extern_crates, non_camel_case_types)]
use async_trait::async_trait;
use futures::{FutureExt, Stream, StreamExt, TryStreamExt};
use openapi_context::ContextWrapper;
use std::io::Error;

#[allow(unused_imports)]
use std::collections::HashMap;

#[cfg(any(feature = "client", feature = "server"))]
mod mimetypes;

pub use openapi_context::ApiError;
pub const BASE_PATH: &'static str = "";
pub const API_VERSION: &'static str = "2.3.4";


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
    (openapi_context::ByteArray)
}

#[derive(Debug, PartialEq)]
pub enum HtmlPostResponse {
    /// Success
    Success
    (String)
}

#[derive(Debug, PartialEq)]
pub enum RawJsonGetResponse {
    /// Success
    Success
    (serde_json::Value)
}


/// API
#[async_trait]
pub trait Api<C> {

    /// A dummy endpoint to make the spec valid.
    async fn dummy_get(&mut self, context: &C) -> Result<DummyGetResponse, ApiError>;


    async fn dummy_put(&mut self, nested_response: crate::models::InlineObject, context: &C) -> Result<DummyPutResponse, ApiError>;

    /// Get a file
    async fn file_response_get(&mut self, context: &C) -> Result<FileResponseGetResponse, ApiError>;

    /// Test HTML handling
    async fn html_post(&mut self, body: String, context: &C) -> Result<HtmlPostResponse, ApiError>;

    /// Get an arbitrary JSON blob.
    async fn raw_json_get(&mut self, context: &C) -> Result<RawJsonGetResponse, ApiError>;

}

/// API without a `Context`
#[async_trait]
pub trait ApiNoContext {

    /// A dummy endpoint to make the spec valid.
    async fn dummy_get(&mut self) -> Result<DummyGetResponse, ApiError>;


    async fn dummy_put(&mut self, nested_response: crate::models::InlineObject) -> Result<DummyPutResponse, ApiError>;

    /// Get a file
    async fn file_response_get(&mut self) -> Result<FileResponseGetResponse, ApiError>;

    /// Test HTML handling
    async fn html_post(&mut self, body: String) -> Result<HtmlPostResponse, ApiError>;

    /// Get an arbitrary JSON blob.
    async fn raw_json_get(&mut self) -> Result<RawJsonGetResponse, ApiError>;

}

/// Trait to extend an API to make it easy to bind it to a context.
pub trait ContextWrapperExt<C> where Self: Sized {
    /// Binds this API to a context.
    fn with_context(self, context: C) -> ContextWrapper<Self, C>;
}

impl<T: Api<C> + Sized, C> ContextWrapperExt<C> for T {
    fn with_context(self, context: C) -> ContextWrapper<T, C> {
         ContextWrapper::<T, C>::new(self, context)
    }
}

#[async_trait]
impl<T: Api<C>, C> ApiNoContext for ContextWrapper<T, C>
    where C: Clone + Send + Sync,
          T: Send + Sync,
{

    /// A dummy endpoint to make the spec valid.
    async fn dummy_get(&mut self) -> Result<DummyGetResponse, ApiError> {
        let ctx: C = self.context().clone();
        self.api_mut().dummy_get(&ctx).await
    }


    async fn dummy_put(&mut self, nested_response: crate::models::InlineObject) -> Result<DummyPutResponse, ApiError> {
        let ctx: C = self.context().clone();
        self.api_mut().dummy_put(nested_response, &ctx).await
    }

    /// Get a file
    async fn file_response_get(&mut self) -> Result<FileResponseGetResponse, ApiError> {
        let ctx: C = self.context().clone();
        self.api_mut().file_response_get(&ctx).await
    }

    /// Test HTML handling
    async fn html_post(&mut self, body: String) -> Result<HtmlPostResponse, ApiError> {
        let ctx: C = self.context().clone();
        self.api_mut().html_post(body, &ctx).await
    }

    /// Get an arbitrary JSON blob.
    async fn raw_json_get(&mut self) -> Result<RawJsonGetResponse, ApiError> {
        let ctx: C = self.context().clone();
        self.api_mut().raw_json_get(&ctx).await
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
#[allow(non_upper_case_globals)]
pub mod headers;
