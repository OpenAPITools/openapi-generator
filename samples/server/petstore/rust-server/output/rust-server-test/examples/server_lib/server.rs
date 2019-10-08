//! Server implementation of rust_server_test.

#![allow(unused_imports)]
use async_trait::async_trait;
use chrono;
use std::collections::HashMap;
use std::marker::PhantomData;
use openapi_context;
use openapi_context::{Has, XSpanId};

use rust_server_test::{Api, ApiError,
                      DummyGetResponse,
                      DummyPutResponse,
                      FileResponseGetResponse,
                      HtmlPostResponse,
                      RawJsonGetResponse
};

#[derive(Copy, Clone)]
pub struct Server<C> {
    marker: PhantomData<C>,
}

impl<C> Server<C> {
    pub fn new() -> Self {
        Server{marker: PhantomData}
    }
}

#[async_trait]
impl<C> Api<C> for Server<C> where C: Has<XSpanId> + Send + Sync {

    /// A dummy endpoint to make the spec valid.
    async fn dummy_get(&mut self, context: &C) -> Result<DummyGetResponse, ApiError> {
        let context = context.clone();
        println!("dummy_get() - X-Span-ID: {:?}", context.get().0.clone());
        Err("Generic failure".into())
    }


    async fn dummy_put(&mut self, nested_response: crate::models::InlineObject, context: &C) -> Result<DummyPutResponse, ApiError> {
        let context = context.clone();
        println!("dummy_put({:?}) - X-Span-ID: {:?}", nested_response, context.get().0.clone());
        Err("Generic failure".into())
    }

    /// Get a file
    async fn file_response_get(&mut self, context: &C) -> Result<FileResponseGetResponse, ApiError> {
        let context = context.clone();
        println!("file_response_get() - X-Span-ID: {:?}", context.get().0.clone());
        Err("Generic failure".into())
    }

    /// Test HTML handling
    async fn html_post(&mut self, body: String, context: &C) -> Result<HtmlPostResponse, ApiError> {
        let context = context.clone();
        println!("html_post(\"{}\") - X-Span-ID: {:?}", body, context.get().0.clone());
        Err("Generic failure".into())
    }

    /// Get an arbitrary JSON blob.
    async fn raw_json_get(&mut self, context: &C) -> Result<RawJsonGetResponse, ApiError> {
        let context = context.clone();
        println!("raw_json_get() - X-Span-ID: {:?}", context.get().0.clone());
        Err("Generic failure".into())
    }

}
