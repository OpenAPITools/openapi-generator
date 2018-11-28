//! Server implementation of rust_server_test.

#![allow(unused_imports)]

use futures::{self, Future};
use chrono;
use std::collections::HashMap;
use std::marker::PhantomData;

use swagger;
use swagger::{Has, XSpanIdString};

use rust_server_test::{Api, ApiError,
                      DummyGetResponse,
                      DummyPutResponse,
                      FileResponseGetResponse,
                      HtmlPostResponse
};
use rust_server_test::models;

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

    /// A dummy endpoint to make the spec valid.
    fn dummy_get(&self, context: &C) -> Box<Future<Item=DummyGetResponse, Error=ApiError>> {
        let context = context.clone();
        println!("dummy_get() - X-Span-ID: {:?}", context.get().0.clone());
        Box::new(futures::failed("Generic failure".into()))
    }


    fn dummy_put(&self, inline_object: Option<models::InlineObject>, context: &C) -> Box<Future<Item=DummyPutResponse, Error=ApiError>> {
        let context = context.clone();
        println!("dummy_put({:?}) - X-Span-ID: {:?}", inline_object, context.get().0.clone());
        Box::new(futures::failed("Generic failure".into()))
    }

    /// Get a file
    fn file_response_get(&self, context: &C) -> Box<Future<Item=FileResponseGetResponse, Error=ApiError>> {
        let context = context.clone();
        println!("file_response_get() - X-Span-ID: {:?}", context.get().0.clone());
        Box::new(futures::failed("Generic failure".into()))
    }

    /// Test HTML handling
    fn html_post(&self, body: String, context: &C) -> Box<Future<Item=HtmlPostResponse, Error=ApiError>> {
        let context = context.clone();
        println!("html_post(\"{}\") - X-Span-ID: {:?}", body, context.get().0.clone());
        Box::new(futures::failed("Generic failure".into()))
    }

}
