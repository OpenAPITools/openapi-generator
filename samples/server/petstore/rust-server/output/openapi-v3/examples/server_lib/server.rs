//! Server implementation of openapi_v3.

#![allow(unused_imports)]
use async_trait::async_trait;
use chrono;
use std::collections::HashMap;
use std::marker::PhantomData;
use openapi_context;
use openapi_context::{Has, XSpanId};
use uuid;

use openapi_v3::{Api, ApiError,
                      MultigetGetResponse,
                      MultipleAuthSchemeGetResponse,
                      ReadonlyAuthSchemeGetResponse,
                      RequiredOctetStreamPutResponse,
                      ResponsesWithHeadersGetResponse,
                      UuidGetResponse,
                      XmlExtraPostResponse,
                      XmlOtherPostResponse,
                      XmlOtherPutResponse,
                      XmlPostResponse,
                      XmlPutResponse
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

    /// Get some stuff.
    async fn multiget_get(&mut self, context: &C) -> Result<MultigetGetResponse, ApiError> {
        let context = context.clone();
        println!("multiget_get() - X-Span-ID: {:?}", context.get().0.clone());
        Err("Generic failure".into())
    }


    async fn multiple_auth_scheme_get(&mut self, context: &C) -> Result<MultipleAuthSchemeGetResponse, ApiError> {
        let context = context.clone();
        println!("multiple_auth_scheme_get() - X-Span-ID: {:?}", context.get().0.clone());
        Err("Generic failure".into())
    }


    async fn readonly_auth_scheme_get(&mut self, context: &C) -> Result<ReadonlyAuthSchemeGetResponse, ApiError> {
        let context = context.clone();
        println!("readonly_auth_scheme_get() - X-Span-ID: {:?}", context.get().0.clone());
        Err("Generic failure".into())
    }


    async fn required_octet_stream_put(&mut self, body: openapi_context::ByteArray, context: &C) -> Result<RequiredOctetStreamPutResponse, ApiError> {
        let context = context.clone();
        println!("required_octet_stream_put({:?}) - X-Span-ID: {:?}", body, context.get().0.clone());
        Err("Generic failure".into())
    }


    async fn responses_with_headers_get(&mut self, context: &C) -> Result<ResponsesWithHeadersGetResponse, ApiError> {
        let context = context.clone();
        println!("responses_with_headers_get() - X-Span-ID: {:?}", context.get().0.clone());
        Err("Generic failure".into())
    }


    async fn uuid_get(&mut self, context: &C) -> Result<UuidGetResponse, ApiError> {
        let context = context.clone();
        println!("uuid_get() - X-Span-ID: {:?}", context.get().0.clone());
        Err("Generic failure".into())
    }


    async fn xml_extra_post(&mut self, duplicate_xml_object: Option<crate::models::DuplicateXmlObject>, context: &C) -> Result<XmlExtraPostResponse, ApiError> {
        let context = context.clone();
        println!("xml_extra_post({:?}) - X-Span-ID: {:?}", duplicate_xml_object, context.get().0.clone());
        Err("Generic failure".into())
    }


    async fn xml_other_post(&mut self, another_xml_object: Option<crate::models::AnotherXmlObject>, context: &C) -> Result<XmlOtherPostResponse, ApiError> {
        let context = context.clone();
        println!("xml_other_post({:?}) - X-Span-ID: {:?}", another_xml_object, context.get().0.clone());
        Err("Generic failure".into())
    }


    async fn xml_other_put(&mut self, another_xml_array: Option<crate::models::AnotherXmlArray>, context: &C) -> Result<XmlOtherPutResponse, ApiError> {
        let context = context.clone();
        println!("xml_other_put({:?}) - X-Span-ID: {:?}", string, context.get().0.clone());
        Err("Generic failure".into())
    }

    /// Post an array
    async fn xml_post(&mut self, xml_array: Option<crate::models::XmlArray>, context: &C) -> Result<XmlPostResponse, ApiError> {
        let context = context.clone();
        println!("xml_post({:?}) - X-Span-ID: {:?}", string, context.get().0.clone());
        Err("Generic failure".into())
    }


    async fn xml_put(&mut self, xml_object: Option<crate::models::XmlObject>, context: &C) -> Result<XmlPutResponse, ApiError> {
        let context = context.clone();
        println!("xml_put({:?}) - X-Span-ID: {:?}", xml_object, context.get().0.clone());
        Err("Generic failure".into())
    }

}
