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
pub enum MultigetGetResponse {
    /// JSON rsp
    JSONRsp
    (crate::models::AnotherXmlObject)
    ,
    /// XML rsp
    XMLRsp
    (crate::models::InlineResponse201)
    ,
    /// octet rsp
    OctetRsp
    (openapi_context::ByteArray)
    ,
    /// string rsp
    StringRsp
    (String)
    ,
    /// Duplicate Response long text. One.
    DuplicateResponseLongText
    (crate::models::AnotherXmlObject)
    ,
    /// Duplicate Response long text. Two.
    DuplicateResponseLongText_2
    (crate::models::AnotherXmlObject)
    ,
    /// Duplicate Response long text. Three.
    DuplicateResponseLongText_3
    (crate::models::AnotherXmlObject)
}

#[derive(Debug, PartialEq)]
pub enum MultipleAuthSchemeGetResponse {
    /// Check that limiting to multiple required auth schemes works
    CheckThatLimitingToMultipleRequiredAuthSchemesWorks
}

#[derive(Debug, PartialEq)]
pub enum ReadonlyAuthSchemeGetResponse {
    /// Check that limiting to a single required auth scheme works
    CheckThatLimitingToASingleRequiredAuthSchemeWorks
}

#[derive(Debug, PartialEq)]
pub enum RequiredOctetStreamPutResponse {
    /// OK
    OK
}

#[derive(Debug, PartialEq)]
pub enum ResponsesWithHeadersGetResponse {
    /// Success
    Success
    {
        body: String,
        success_info: String,
    }
    ,
    /// Precondition Failed
    PreconditionFailed
    {
        further_info: String,
        failure_info: String,
    }
}

#[derive(Debug, PartialEq)]
pub enum UuidGetResponse {
    /// Duplicate Response long text. One.
    DuplicateResponseLongText
    (uuid::Uuid)
}

#[derive(Debug, PartialEq)]
pub enum XmlExtraPostResponse {
    /// OK
    OK
    ,
    /// Bad Request
    BadRequest
}

#[derive(Debug, PartialEq)]
pub enum XmlOtherPostResponse {
    /// OK
    OK
    (crate::models::AnotherXmlObject)
    ,
    /// Bad Request
    BadRequest
}

#[derive(Debug, PartialEq)]
pub enum XmlOtherPutResponse {
    /// OK
    OK
    ,
    /// Bad Request
    BadRequest
}

#[derive(Debug, PartialEq)]
pub enum XmlPostResponse {
    /// OK
    OK
    ,
    /// Bad Request
    BadRequest
}

#[derive(Debug, PartialEq)]
pub enum XmlPutResponse {
    /// OK
    OK
    ,
    /// Bad Request
    BadRequest
}


/// API
#[async_trait]
pub trait Api<C> {

    /// Get some stuff.
    async fn multiget_get(&mut self, context: &C) -> Result<MultigetGetResponse, ApiError>;


    async fn multiple_auth_scheme_get(&mut self, context: &C) -> Result<MultipleAuthSchemeGetResponse, ApiError>;


    async fn readonly_auth_scheme_get(&mut self, context: &C) -> Result<ReadonlyAuthSchemeGetResponse, ApiError>;


    async fn required_octet_stream_put(&mut self, body: openapi_context::ByteArray, context: &C) -> Result<RequiredOctetStreamPutResponse, ApiError>;


    async fn responses_with_headers_get(&mut self, context: &C) -> Result<ResponsesWithHeadersGetResponse, ApiError>;


    async fn uuid_get(&mut self, context: &C) -> Result<UuidGetResponse, ApiError>;


    async fn xml_extra_post(&mut self, duplicate_xml_object: Option<crate::models::DuplicateXmlObject>, context: &C) -> Result<XmlExtraPostResponse, ApiError>;


    async fn xml_other_post(&mut self, another_xml_object: Option<crate::models::AnotherXmlObject>, context: &C) -> Result<XmlOtherPostResponse, ApiError>;


    async fn xml_other_put(&mut self, another_xml_array: Option<crate::models::AnotherXmlArray>, context: &C) -> Result<XmlOtherPutResponse, ApiError>;

    /// Post an array
    async fn xml_post(&mut self, xml_array: Option<crate::models::XmlArray>, context: &C) -> Result<XmlPostResponse, ApiError>;


    async fn xml_put(&mut self, xml_object: Option<crate::models::XmlObject>, context: &C) -> Result<XmlPutResponse, ApiError>;

}

/// API without a `Context`
#[async_trait]
pub trait ApiNoContext {

    /// Get some stuff.
    async fn multiget_get(&mut self) -> Result<MultigetGetResponse, ApiError>;


    async fn multiple_auth_scheme_get(&mut self) -> Result<MultipleAuthSchemeGetResponse, ApiError>;


    async fn readonly_auth_scheme_get(&mut self) -> Result<ReadonlyAuthSchemeGetResponse, ApiError>;


    async fn required_octet_stream_put(&mut self, body: openapi_context::ByteArray) -> Result<RequiredOctetStreamPutResponse, ApiError>;


    async fn responses_with_headers_get(&mut self) -> Result<ResponsesWithHeadersGetResponse, ApiError>;


    async fn uuid_get(&mut self) -> Result<UuidGetResponse, ApiError>;


    async fn xml_extra_post(&mut self, duplicate_xml_object: Option<crate::models::DuplicateXmlObject>) -> Result<XmlExtraPostResponse, ApiError>;


    async fn xml_other_post(&mut self, another_xml_object: Option<crate::models::AnotherXmlObject>) -> Result<XmlOtherPostResponse, ApiError>;


    async fn xml_other_put(&mut self, another_xml_array: Option<crate::models::AnotherXmlArray>) -> Result<XmlOtherPutResponse, ApiError>;

    /// Post an array
    async fn xml_post(&mut self, xml_array: Option<crate::models::XmlArray>) -> Result<XmlPostResponse, ApiError>;


    async fn xml_put(&mut self, xml_object: Option<crate::models::XmlObject>) -> Result<XmlPutResponse, ApiError>;

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

    /// Get some stuff.
    async fn multiget_get(&mut self) -> Result<MultigetGetResponse, ApiError> {
        let ctx: C = self.context().clone();
        self.api_mut().multiget_get(&ctx).await
    }


    async fn multiple_auth_scheme_get(&mut self) -> Result<MultipleAuthSchemeGetResponse, ApiError> {
        let ctx: C = self.context().clone();
        self.api_mut().multiple_auth_scheme_get(&ctx).await
    }


    async fn readonly_auth_scheme_get(&mut self) -> Result<ReadonlyAuthSchemeGetResponse, ApiError> {
        let ctx: C = self.context().clone();
        self.api_mut().readonly_auth_scheme_get(&ctx).await
    }


    async fn required_octet_stream_put(&mut self, body: openapi_context::ByteArray) -> Result<RequiredOctetStreamPutResponse, ApiError> {
        let ctx: C = self.context().clone();
        self.api_mut().required_octet_stream_put(body, &ctx).await
    }


    async fn responses_with_headers_get(&mut self) -> Result<ResponsesWithHeadersGetResponse, ApiError> {
        let ctx: C = self.context().clone();
        self.api_mut().responses_with_headers_get(&ctx).await
    }


    async fn uuid_get(&mut self) -> Result<UuidGetResponse, ApiError> {
        let ctx: C = self.context().clone();
        self.api_mut().uuid_get(&ctx).await
    }


    async fn xml_extra_post(&mut self, duplicate_xml_object: Option<crate::models::DuplicateXmlObject>) -> Result<XmlExtraPostResponse, ApiError> {
        let ctx: C = self.context().clone();
        self.api_mut().xml_extra_post(duplicate_xml_object, &ctx).await
    }


    async fn xml_other_post(&mut self, another_xml_object: Option<crate::models::AnotherXmlObject>) -> Result<XmlOtherPostResponse, ApiError> {
        let ctx: C = self.context().clone();
        self.api_mut().xml_other_post(another_xml_object, &ctx).await
    }


    async fn xml_other_put(&mut self, another_xml_array: Option<crate::models::AnotherXmlArray>) -> Result<XmlOtherPutResponse, ApiError> {
        let ctx: C = self.context().clone();
        self.api_mut().xml_other_put(another_xml_array, &ctx).await
    }

    /// Post an array
    async fn xml_post(&mut self, xml_array: Option<crate::models::XmlArray>) -> Result<XmlPostResponse, ApiError> {
        let ctx: C = self.context().clone();
        self.api_mut().xml_post(xml_array, &ctx).await
    }


    async fn xml_put(&mut self, xml_object: Option<crate::models::XmlObject>) -> Result<XmlPutResponse, ApiError> {
        let ctx: C = self.context().clone();
        self.api_mut().xml_put(xml_object, &ctx).await
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
