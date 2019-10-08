//! Server implementation of multipart_v3.

#![allow(unused_imports)]
use async_trait::async_trait;
use chrono;
use std::collections::HashMap;
use std::marker::PhantomData;
use openapi_context;
use openapi_context::{Has, XSpanId};

use multipart_v3::{Api, ApiError,
                      MultipartRequestPostResponse
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


    async fn multipart_request_post(&mut self, string_field: String, binary_field: openapi_context::ByteArray, optional_string_field: Option<String>, object_field: Option<crate::models::MultipartRequestObjectField>, context: &C) -> Result<MultipartRequestPostResponse, ApiError> {
        let context = context.clone();
        println!("multipart_request_post(\"{}\", {:?}, {:?}, {:?}) - X-Span-ID: {:?}", string_field, binary_field, optional_string_field, object_field, context.get().0.clone());
        Err("Generic failure".into())
    }

}
