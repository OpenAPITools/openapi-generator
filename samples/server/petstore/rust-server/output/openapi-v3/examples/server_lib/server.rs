//! Server implementation of openapi_v3.

#![allow(unused_imports)]

use futures::{self, Future};
use chrono;
use std::collections::HashMap;
use std::marker::PhantomData;

use swagger;
use swagger::{Has, XSpanIdString};

use openapi_v3::{Api, ApiError,
                      XmlPostResponse
};
use openapi_v3::models;

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


    fn xml_post(&self, string: Option<&Vec<models::XmlInner>>, context: &C) -> Box<Future<Item=XmlPostResponse, Error=ApiError>> {
        let context = context.clone();
        println!("xml_post({:?}) - X-Span-ID: {:?}", string, context.get().0.clone());
        Box::new(futures::failed("Generic failure".into()))
    }

}
