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
pub const API_VERSION: &'static str = "1.0.7";


#[derive(Debug, PartialEq)]
pub enum MultipartRequestPostResponse {
    /// OK
    OK
}


/// API
#[async_trait]
pub trait Api<C> {


    async fn multipart_request_post(&mut self, string_field: String, binary_field: openapi_context::ByteArray, optional_string_field: Option<String>, object_field: Option<crate::models::MultipartRequestObjectField>, context: &C) -> Result<MultipartRequestPostResponse, ApiError>;

}

/// API without a `Context`
#[async_trait]
pub trait ApiNoContext {


    async fn multipart_request_post(&mut self, string_field: String, binary_field: openapi_context::ByteArray, optional_string_field: Option<String>, object_field: Option<crate::models::MultipartRequestObjectField>) -> Result<MultipartRequestPostResponse, ApiError>;

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


    async fn multipart_request_post(&mut self, string_field: String, binary_field: openapi_context::ByteArray, optional_string_field: Option<String>, object_field: Option<crate::models::MultipartRequestObjectField>) -> Result<MultipartRequestPostResponse, ApiError> {
        let ctx: C = self.context().clone();
        self.api_mut().multipart_request_post(string_field, binary_field, optional_string_field, object_field, &ctx).await
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
