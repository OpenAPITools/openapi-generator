//! Main library entry point for multipart_v3 implementation.

#![allow(unused_imports)]

mod errors {
    error_chain!{}
}

pub use self::errors::*;

use futures::{self, Future};
use chrono;
use std::collections::HashMap;
use std::marker::PhantomData;

use swagger;
use swagger::{Has, XSpanIdString};

use multipart_v3::{Api, ApiError,
                      MultipartRelatedRequestPostResponse,
                      MultipartRequestPostResponse
};
use multipart_v3::models;

#[derive(Copy, Clone)]
pub struct Server<C> {
    marker: PhantomData<C>,
}

impl<C> Server<C> {
    pub fn new() -> Self {
        Server{marker: PhantomData}
    }
}

impl<C> Api<C> for Server<C> where C: Has<XSpanIdString>{


    fn multipart_related_request_post(&self, required_binary_field: swagger::ByteArray, object_field: Option<models::MultipartRequestObjectField>, optional_binary_field: Option<swagger::ByteArray>, context: &C) -> Box<Future<Item=MultipartRelatedRequestPostResponse, Error=ApiError> + Send> {
        let context = context.clone();
        println!("multipart_related_request_post({:?}, {:?}, {:?}) - X-Span-ID: {:?}", required_binary_field, object_field, optional_binary_field, context.get().0.clone());
        Box::new(futures::failed("Generic failure".into()))
    }


    fn multipart_request_post(&self, string_field: String, binary_field: swagger::ByteArray, optional_string_field: Option<String>, object_field: Option<models::MultipartRequestObjectField>, context: &C) -> Box<Future<Item=MultipartRequestPostResponse, Error=ApiError> + Send> {
        let context = context.clone();
        println!("multipart_request_post(\"{}\", {:?}, {:?}, {:?}) - X-Span-ID: {:?}", string_field, binary_field, optional_string_field, object_field, context.get().0.clone());
        Box::new(futures::failed("Generic failure".into()))
    }

}
