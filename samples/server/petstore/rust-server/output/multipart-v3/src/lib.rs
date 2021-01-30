#![allow(missing_docs, trivial_casts, unused_variables, unused_mut, unused_imports, unused_extern_crates, non_camel_case_types)]

use async_trait::async_trait;
use futures::Stream;
use std::error::Error;
use std::task::{Poll, Context};
use swagger::{ApiError, ContextWrapper};
use serde::{Serialize, Deserialize};

type ServiceError = Box<dyn Error + Send + Sync + 'static>;

pub const BASE_PATH: &'static str = "";
pub const API_VERSION: &'static str = "1.0.7";

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub enum MultipartRelatedRequestPostResponse {
    /// OK
    OK
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub enum MultipartRequestPostResponse {
    /// OK
    OK
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub enum MultipleIdenticalMimeTypesPostResponse {
    /// OK
    OK
}

/// API
#[async_trait]
pub trait Api<C: Send + Sync> {
    fn poll_ready(&self, _cx: &mut Context) -> Poll<Result<(), Box<dyn Error + Send + Sync + 'static>>> {
        Poll::Ready(Ok(()))
    }

    async fn multipart_related_request_post(
        &self,
        required_binary_field: swagger::ByteArray,
        object_field: Option<models::MultipartRequestObjectField>,
        optional_binary_field: Option<swagger::ByteArray>,
        context: &C) -> Result<MultipartRelatedRequestPostResponse, ApiError>;

    async fn multipart_request_post(
        &self,
        string_field: String,
        binary_field: swagger::ByteArray,
        optional_string_field: Option<String>,
        object_field: Option<models::MultipartRequestObjectField>,
        context: &C) -> Result<MultipartRequestPostResponse, ApiError>;

    async fn multiple_identical_mime_types_post(
        &self,
        binary1: Option<swagger::ByteArray>,
        binary2: Option<swagger::ByteArray>,
        context: &C) -> Result<MultipleIdenticalMimeTypesPostResponse, ApiError>;

}

/// API where `Context` isn't passed on every API call
#[async_trait]
pub trait ApiNoContext<C: Send + Sync> {

    fn poll_ready(&self, _cx: &mut Context) -> Poll<Result<(), Box<dyn Error + Send + Sync + 'static>>>;

    fn context(&self) -> &C;

    async fn multipart_related_request_post(
        &self,
        required_binary_field: swagger::ByteArray,
        object_field: Option<models::MultipartRequestObjectField>,
        optional_binary_field: Option<swagger::ByteArray>,
        ) -> Result<MultipartRelatedRequestPostResponse, ApiError>;

    async fn multipart_request_post(
        &self,
        string_field: String,
        binary_field: swagger::ByteArray,
        optional_string_field: Option<String>,
        object_field: Option<models::MultipartRequestObjectField>,
        ) -> Result<MultipartRequestPostResponse, ApiError>;

    async fn multiple_identical_mime_types_post(
        &self,
        binary1: Option<swagger::ByteArray>,
        binary2: Option<swagger::ByteArray>,
        ) -> Result<MultipleIdenticalMimeTypesPostResponse, ApiError>;

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

    async fn multipart_related_request_post(
        &self,
        required_binary_field: swagger::ByteArray,
        object_field: Option<models::MultipartRequestObjectField>,
        optional_binary_field: Option<swagger::ByteArray>,
        ) -> Result<MultipartRelatedRequestPostResponse, ApiError>
    {
        let context = self.context().clone();
        self.api().multipart_related_request_post(required_binary_field, object_field, optional_binary_field, &context).await
    }

    async fn multipart_request_post(
        &self,
        string_field: String,
        binary_field: swagger::ByteArray,
        optional_string_field: Option<String>,
        object_field: Option<models::MultipartRequestObjectField>,
        ) -> Result<MultipartRequestPostResponse, ApiError>
    {
        let context = self.context().clone();
        self.api().multipart_request_post(string_field, binary_field, optional_string_field, object_field, &context).await
    }

    async fn multiple_identical_mime_types_post(
        &self,
        binary1: Option<swagger::ByteArray>,
        binary2: Option<swagger::ByteArray>,
        ) -> Result<MultipleIdenticalMimeTypesPostResponse, ApiError>
    {
        let context = self.context().clone();
        self.api().multiple_identical_mime_types_post(binary1, binary2, &context).await
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
