#![allow(missing_docs, trivial_casts, unused_variables, unused_mut, unused_imports, unused_extern_crates, non_camel_case_types)]

use async_trait::async_trait;
use futures::Stream;
use std::error::Error;
use std::task::{Poll, Context};
use swagger::{ApiError, ContextWrapper};
use serde::{Serialize, Deserialize};

type ServiceError = Box<dyn Error + Send + Sync + 'static>;

pub const BASE_PATH: &'static str = "";
pub const API_VERSION: &'static str = "2.3.4";

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub enum AllOfGetResponse {
    /// OK
    OK
    (models::AllOfObject)
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub enum DummyGetResponse {
    /// Success
    Success
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub enum DummyPutResponse {
    /// Success
    Success
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub enum FileResponseGetResponse {
    /// Success
    Success
    (swagger::ByteArray)
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub enum GetStructuredYamlResponse {
    /// OK
    OK
    (String)
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub enum HtmlPostResponse {
    /// Success
    Success
    (String)
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub enum PostYamlResponse {
    /// OK
    OK
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub enum RawJsonGetResponse {
    /// Success
    Success
    (serde_json::Value)
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub enum SoloObjectPostResponse {
    /// OK
    OK
}

/// API
#[async_trait]
pub trait Api<C: Send + Sync> {
    fn poll_ready(&self, _cx: &mut Context) -> Poll<Result<(), Box<dyn Error + Send + Sync + 'static>>> {
        Poll::Ready(Ok(()))
    }

    async fn all_of_get(
        &self,
        context: &C) -> Result<AllOfGetResponse, ApiError>;

    /// A dummy endpoint to make the spec valid.
    async fn dummy_get(
        &self,
        context: &C) -> Result<DummyGetResponse, ApiError>;

    async fn dummy_put(
        &self,
        nested_response: models::InlineObject,
        context: &C) -> Result<DummyPutResponse, ApiError>;

    /// Get a file
    async fn file_response_get(
        &self,
        context: &C) -> Result<FileResponseGetResponse, ApiError>;

    async fn get_structured_yaml(
        &self,
        context: &C) -> Result<GetStructuredYamlResponse, ApiError>;

    /// Test HTML handling
    async fn html_post(
        &self,
        body: String,
        context: &C) -> Result<HtmlPostResponse, ApiError>;

    async fn post_yaml(
        &self,
        value: String,
        context: &C) -> Result<PostYamlResponse, ApiError>;

    /// Get an arbitrary JSON blob.
    async fn raw_json_get(
        &self,
        context: &C) -> Result<RawJsonGetResponse, ApiError>;

    /// Send an arbitrary JSON blob
    async fn solo_object_post(
        &self,
        value: serde_json::Value,
        context: &C) -> Result<SoloObjectPostResponse, ApiError>;

}

/// API where `Context` isn't passed on every API call
#[async_trait]
pub trait ApiNoContext<C: Send + Sync> {

    fn poll_ready(&self, _cx: &mut Context) -> Poll<Result<(), Box<dyn Error + Send + Sync + 'static>>>;

    fn context(&self) -> &C;

    async fn all_of_get(
        &self,
        ) -> Result<AllOfGetResponse, ApiError>;

    /// A dummy endpoint to make the spec valid.
    async fn dummy_get(
        &self,
        ) -> Result<DummyGetResponse, ApiError>;

    async fn dummy_put(
        &self,
        nested_response: models::InlineObject,
        ) -> Result<DummyPutResponse, ApiError>;

    /// Get a file
    async fn file_response_get(
        &self,
        ) -> Result<FileResponseGetResponse, ApiError>;

    async fn get_structured_yaml(
        &self,
        ) -> Result<GetStructuredYamlResponse, ApiError>;

    /// Test HTML handling
    async fn html_post(
        &self,
        body: String,
        ) -> Result<HtmlPostResponse, ApiError>;

    async fn post_yaml(
        &self,
        value: String,
        ) -> Result<PostYamlResponse, ApiError>;

    /// Get an arbitrary JSON blob.
    async fn raw_json_get(
        &self,
        ) -> Result<RawJsonGetResponse, ApiError>;

    /// Send an arbitrary JSON blob
    async fn solo_object_post(
        &self,
        value: serde_json::Value,
        ) -> Result<SoloObjectPostResponse, ApiError>;

}

/// Trait to extend an API to make it easy to bind it to a context.
pub trait ContextWrapperExt<C: Send + Sync> where Self: Sized
{
    /// Binds this API to a context.
    fn with_context(self: Self, context: C) -> ContextWrapper<Self, C>;
}

impl<T: Api<C> + Send + Sync, C: Clone + Send + Sync> ContextWrapperExt<C> for T {
    fn with_context(self: T, context: C) -> ContextWrapper<T, C> {
         ContextWrapper::<T, C>::new(self, context)
    }
}

#[async_trait]
impl<T: Api<C> + Send + Sync, C: Clone + Send + Sync> ApiNoContext<C> for ContextWrapper<T, C> {
    fn poll_ready(&self, cx: &mut Context) -> Poll<Result<(), ServiceError>> {
        self.api().poll_ready(cx)
    }

    fn context(&self) -> &C {
        ContextWrapper::context(self)
    }

    async fn all_of_get(
        &self,
        ) -> Result<AllOfGetResponse, ApiError>
    {
        let context = self.context().clone();
        self.api().all_of_get(&context).await
    }

    /// A dummy endpoint to make the spec valid.
    async fn dummy_get(
        &self,
        ) -> Result<DummyGetResponse, ApiError>
    {
        let context = self.context().clone();
        self.api().dummy_get(&context).await
    }

    async fn dummy_put(
        &self,
        nested_response: models::InlineObject,
        ) -> Result<DummyPutResponse, ApiError>
    {
        let context = self.context().clone();
        self.api().dummy_put(nested_response, &context).await
    }

    /// Get a file
    async fn file_response_get(
        &self,
        ) -> Result<FileResponseGetResponse, ApiError>
    {
        let context = self.context().clone();
        self.api().file_response_get(&context).await
    }

    async fn get_structured_yaml(
        &self,
        ) -> Result<GetStructuredYamlResponse, ApiError>
    {
        let context = self.context().clone();
        self.api().get_structured_yaml(&context).await
    }

    /// Test HTML handling
    async fn html_post(
        &self,
        body: String,
        ) -> Result<HtmlPostResponse, ApiError>
    {
        let context = self.context().clone();
        self.api().html_post(body, &context).await
    }

    async fn post_yaml(
        &self,
        value: String,
        ) -> Result<PostYamlResponse, ApiError>
    {
        let context = self.context().clone();
        self.api().post_yaml(value, &context).await
    }

    /// Get an arbitrary JSON blob.
    async fn raw_json_get(
        &self,
        ) -> Result<RawJsonGetResponse, ApiError>
    {
        let context = self.context().clone();
        self.api().raw_json_get(&context).await
    }

    /// Send an arbitrary JSON blob
    async fn solo_object_post(
        &self,
        value: serde_json::Value,
        ) -> Result<SoloObjectPostResponse, ApiError>
    {
        let context = self.context().clone();
        self.api().solo_object_post(value, &context).await
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
