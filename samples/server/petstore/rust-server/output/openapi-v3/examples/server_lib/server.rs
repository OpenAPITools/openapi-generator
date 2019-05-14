//! Server implementation of openapi_v3.

#![allow(unused_imports)]

use futures::{self, Future};
use chrono;
use std::collections::HashMap;
use std::marker::PhantomData;

use swagger;
use swagger::{Has, XSpanIdString};

use openapi_v3::{Api, ApiError,
                      RequiredOctetStreamPutResponse,
                      XmlExtraPostResponse,
                      XmlOtherPostResponse,
                      XmlOtherPutResponse,
                      XmlPostResponse,
                      XmlPutResponse
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


    fn required_octet_stream_put(&self, body: swagger::ByteArray, context: &C) -> Box<Future<Item=RequiredOctetStreamPutResponse, Error=ApiError>> {
        let context = context.clone();
        println!("required_octet_stream_put({:?}) - X-Span-ID: {:?}", body, context.get().0.clone());
        Box::new(futures::failed("Generic failure".into()))
    }


    fn xml_extra_post(&self, duplicate_xml_object: Option<models::DuplicateXmlObject>, context: &C) -> Box<Future<Item=XmlExtraPostResponse, Error=ApiError>> {
        let context = context.clone();
        println!("xml_extra_post({:?}) - X-Span-ID: {:?}", duplicate_xml_object, context.get().0.clone());
        Box::new(futures::failed("Generic failure".into()))
    }


    fn xml_other_post(&self, another_xml_object: Option<models::AnotherXmlObject>, context: &C) -> Box<Future<Item=XmlOtherPostResponse, Error=ApiError>> {
        let context = context.clone();
        println!("xml_other_post({:?}) - X-Span-ID: {:?}", another_xml_object, context.get().0.clone());
        Box::new(futures::failed("Generic failure".into()))
    }


    fn xml_other_put(&self, string: Option<models::AnotherXmlArray>, context: &C) -> Box<Future<Item=XmlOtherPutResponse, Error=ApiError>> {
        let context = context.clone();
        println!("xml_other_put({:?}) - X-Span-ID: {:?}", string, context.get().0.clone());
        Box::new(futures::failed("Generic failure".into()))
    }

    /// Post an array
    fn xml_post(&self, string: Option<models::XmlArray>, context: &C) -> Box<Future<Item=XmlPostResponse, Error=ApiError>> {
        let context = context.clone();
        println!("xml_post({:?}) - X-Span-ID: {:?}", string, context.get().0.clone());
        Box::new(futures::failed("Generic failure".into()))
    }


    fn xml_put(&self, xml_object: Option<models::XmlObject>, context: &C) -> Box<Future<Item=XmlPutResponse, Error=ApiError>> {
        let context = context.clone();
        println!("xml_put({:?}) - X-Span-ID: {:?}", xml_object, context.get().0.clone());
        Box::new(futures::failed("Generic failure".into()))
    }

}
