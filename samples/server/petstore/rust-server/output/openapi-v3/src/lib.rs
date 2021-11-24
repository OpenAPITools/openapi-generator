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
#[must_use]
pub enum AnyOfGetResponse {
    /// Success
    Success
    (models::AnyOfObject)
    ,
    /// AlternateSuccess
    AlternateSuccess
    (models::Model12345AnyOfObject)
    ,
    /// AnyOfSuccess
    AnyOfSuccess
    (swagger::AnyOf2<models::StringObject,models::UuidObject>)
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub enum CallbackWithHeaderPostResponse {
    /// OK
    OK
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub enum ComplexQueryParamGetResponse {
    /// Success
    Success
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub enum EnumInPathPathParamGetResponse {
    /// Success
    Success
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub enum JsonComplexQueryParamGetResponse {
    /// Success
    Success
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub enum MandatoryRequestHeaderGetResponse {
    /// Success
    Success
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub enum MergePatchJsonGetResponse {
    /// merge-patch+json-encoded response
    Merge
    (models::AnotherXmlObject)
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
#[must_use]
pub enum MultigetGetResponse {
    /// JSON rsp
    JSONRsp
    (models::AnotherXmlObject)
    ,
    /// XML rsp
    XMLRsp
    (models::InlineResponse201)
    ,
    /// octet rsp
    OctetRsp
    (swagger::ByteArray)
    ,
    /// string rsp
    StringRsp
    (String)
    ,
    /// Duplicate Response long text. One.
    DuplicateResponseLongText
    (models::AnotherXmlObject)
    ,
    /// Duplicate Response long text. Two.
    DuplicateResponseLongText_2
    (models::AnotherXmlObject)
    ,
    /// Duplicate Response long text. Three.
    DuplicateResponseLongText_3
    (models::AnotherXmlObject)
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub enum MultipleAuthSchemeGetResponse {
    /// Check that limiting to multiple required auth schemes works
    CheckThatLimitingToMultipleRequiredAuthSchemesWorks
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub enum OneOfGetResponse {
    /// Success
    Success
    (swagger::OneOf2<i32,Vec<String>>)
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub enum OverrideServerGetResponse {
    /// Success.
    Success
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub enum ParamgetGetResponse {
    /// JSON rsp
    JSONRsp
    (models::AnotherXmlObject)
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub enum ReadonlyAuthSchemeGetResponse {
    /// Check that limiting to a single required auth scheme works
    CheckThatLimitingToASingleRequiredAuthSchemeWorks
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub enum RegisterCallbackPostResponse {
    /// OK
    OK
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub enum RequiredOctetStreamPutResponse {
    /// OK
    OK
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
#[must_use]
pub enum ResponsesWithHeadersGetResponse {
    /// Success
    Success
    {
        body: String,
        success_info:
        String
        ,
        bool_header:
        Option<
        bool
        >
        ,
        object_header:
        Option<
        models::ObjectHeader
        >
    }
    ,
    /// Precondition Failed
    PreconditionFailed
    {
        further_info:
        Option<
        String
        >
        ,
        failure_info:
        Option<
        String
        >
    }
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
#[must_use]
pub enum Rfc7807GetResponse {
    /// OK
    OK
    (models::ObjectWithArrayOfObjects)
    ,
    /// NotFound
    NotFound
    (models::ObjectWithArrayOfObjects)
    ,
    /// NotAcceptable
    NotAcceptable
    (models::ObjectWithArrayOfObjects)
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub enum UntypedPropertyGetResponse {
    /// Check that untyped properties works
    CheckThatUntypedPropertiesWorks
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub enum UuidGetResponse {
    /// Duplicate Response long text. One.
    DuplicateResponseLongText
    (uuid::Uuid)
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
#[must_use]
pub enum XmlExtraPostResponse {
    /// OK
    OK
    ,
    /// Bad Request
    BadRequest
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
#[must_use]
pub enum XmlOtherPostResponse {
    /// OK
    OK
    (models::AnotherXmlObject)
    ,
    /// Bad Request
    BadRequest
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
#[must_use]
pub enum XmlOtherPutResponse {
    /// OK
    OK
    ,
    /// Bad Request
    BadRequest
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
#[must_use]
pub enum XmlPostResponse {
    /// OK
    OK
    ,
    /// Bad Request
    BadRequest
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
#[must_use]
pub enum XmlPutResponse {
    /// OK
    OK
    ,
    /// Bad Request
    BadRequest
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub enum CreateRepoResponse {
    /// Success
    Success
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub enum GetRepoInfoResponse {
    /// OK
    OK
    (String)
}

/// API
#[async_trait]
pub trait Api<C: Send + Sync> {
    fn poll_ready(&self, _cx: &mut Context) -> Poll<Result<(), Box<dyn Error + Send + Sync + 'static>>> {
        Poll::Ready(Ok(()))
    }

    async fn any_of_get(
        &self,
        any_of: Option<&Vec<models::AnyOfObject>>,
        context: &C) -> Result<AnyOfGetResponse, ApiError>;

    async fn callback_with_header_post(
        &self,
        url: String,
        context: &C) -> Result<CallbackWithHeaderPostResponse, ApiError>;

    async fn complex_query_param_get(
        &self,
        list_of_strings: Option<&Vec<models::StringObject>>,
        context: &C) -> Result<ComplexQueryParamGetResponse, ApiError>;

    async fn enum_in_path_path_param_get(
        &self,
        path_param: models::StringEnum,
        context: &C) -> Result<EnumInPathPathParamGetResponse, ApiError>;

    async fn json_complex_query_param_get(
        &self,
        list_of_strings: Option<&Vec<models::StringObject>>,
        context: &C) -> Result<JsonComplexQueryParamGetResponse, ApiError>;

    async fn mandatory_request_header_get(
        &self,
        x_header: String,
        context: &C) -> Result<MandatoryRequestHeaderGetResponse, ApiError>;

    async fn merge_patch_json_get(
        &self,
        context: &C) -> Result<MergePatchJsonGetResponse, ApiError>;

    /// Get some stuff.
    async fn multiget_get(
        &self,
        context: &C) -> Result<MultigetGetResponse, ApiError>;

    async fn multiple_auth_scheme_get(
        &self,
        context: &C) -> Result<MultipleAuthSchemeGetResponse, ApiError>;

    async fn one_of_get(
        &self,
        context: &C) -> Result<OneOfGetResponse, ApiError>;

    async fn override_server_get(
        &self,
        context: &C) -> Result<OverrideServerGetResponse, ApiError>;

    /// Get some stuff with parameters.
    async fn paramget_get(
        &self,
        uuid: Option<uuid::Uuid>,
        some_object: Option<models::ObjectParam>,
        some_list: Option<models::MyIdList>,
        context: &C) -> Result<ParamgetGetResponse, ApiError>;

    async fn readonly_auth_scheme_get(
        &self,
        context: &C) -> Result<ReadonlyAuthSchemeGetResponse, ApiError>;

    async fn register_callback_post(
        &self,
        url: String,
        context: &C) -> Result<RegisterCallbackPostResponse, ApiError>;

    async fn required_octet_stream_put(
        &self,
        body: swagger::ByteArray,
        context: &C) -> Result<RequiredOctetStreamPutResponse, ApiError>;

    async fn responses_with_headers_get(
        &self,
        context: &C) -> Result<ResponsesWithHeadersGetResponse, ApiError>;

    async fn rfc7807_get(
        &self,
        context: &C) -> Result<Rfc7807GetResponse, ApiError>;

    async fn untyped_property_get(
        &self,
        object_untyped_props: Option<models::ObjectUntypedProps>,
        context: &C) -> Result<UntypedPropertyGetResponse, ApiError>;

    async fn uuid_get(
        &self,
        context: &C) -> Result<UuidGetResponse, ApiError>;

    async fn xml_extra_post(
        &self,
        duplicate_xml_object: Option<models::DuplicateXmlObject>,
        context: &C) -> Result<XmlExtraPostResponse, ApiError>;

    async fn xml_other_post(
        &self,
        another_xml_object: Option<models::AnotherXmlObject>,
        context: &C) -> Result<XmlOtherPostResponse, ApiError>;

    async fn xml_other_put(
        &self,
        another_xml_array: Option<models::AnotherXmlArray>,
        context: &C) -> Result<XmlOtherPutResponse, ApiError>;

    /// Post an array
    async fn xml_post(
        &self,
        xml_array: Option<models::XmlArray>,
        context: &C) -> Result<XmlPostResponse, ApiError>;

    async fn xml_put(
        &self,
        xml_object: Option<models::XmlObject>,
        context: &C) -> Result<XmlPutResponse, ApiError>;

    async fn create_repo(
        &self,
        object_param: models::ObjectParam,
        context: &C) -> Result<CreateRepoResponse, ApiError>;

    async fn get_repo_info(
        &self,
        repo_id: String,
        context: &C) -> Result<GetRepoInfoResponse, ApiError>;

}

/// API where `Context` isn't passed on every API call
#[async_trait]
pub trait ApiNoContext<C: Send + Sync> {

    fn poll_ready(&self, _cx: &mut Context) -> Poll<Result<(), Box<dyn Error + Send + Sync + 'static>>>;

    fn context(&self) -> &C;

    async fn any_of_get(
        &self,
        any_of: Option<&Vec<models::AnyOfObject>>,
        ) -> Result<AnyOfGetResponse, ApiError>;

    async fn callback_with_header_post(
        &self,
        url: String,
        ) -> Result<CallbackWithHeaderPostResponse, ApiError>;

    async fn complex_query_param_get(
        &self,
        list_of_strings: Option<&Vec<models::StringObject>>,
        ) -> Result<ComplexQueryParamGetResponse, ApiError>;

    async fn enum_in_path_path_param_get(
        &self,
        path_param: models::StringEnum,
        ) -> Result<EnumInPathPathParamGetResponse, ApiError>;

    async fn json_complex_query_param_get(
        &self,
        list_of_strings: Option<&Vec<models::StringObject>>,
        ) -> Result<JsonComplexQueryParamGetResponse, ApiError>;

    async fn mandatory_request_header_get(
        &self,
        x_header: String,
        ) -> Result<MandatoryRequestHeaderGetResponse, ApiError>;

    async fn merge_patch_json_get(
        &self,
        ) -> Result<MergePatchJsonGetResponse, ApiError>;

    /// Get some stuff.
    async fn multiget_get(
        &self,
        ) -> Result<MultigetGetResponse, ApiError>;

    async fn multiple_auth_scheme_get(
        &self,
        ) -> Result<MultipleAuthSchemeGetResponse, ApiError>;

    async fn one_of_get(
        &self,
        ) -> Result<OneOfGetResponse, ApiError>;

    async fn override_server_get(
        &self,
        ) -> Result<OverrideServerGetResponse, ApiError>;

    /// Get some stuff with parameters.
    async fn paramget_get(
        &self,
        uuid: Option<uuid::Uuid>,
        some_object: Option<models::ObjectParam>,
        some_list: Option<models::MyIdList>,
        ) -> Result<ParamgetGetResponse, ApiError>;

    async fn readonly_auth_scheme_get(
        &self,
        ) -> Result<ReadonlyAuthSchemeGetResponse, ApiError>;

    async fn register_callback_post(
        &self,
        url: String,
        ) -> Result<RegisterCallbackPostResponse, ApiError>;

    async fn required_octet_stream_put(
        &self,
        body: swagger::ByteArray,
        ) -> Result<RequiredOctetStreamPutResponse, ApiError>;

    async fn responses_with_headers_get(
        &self,
        ) -> Result<ResponsesWithHeadersGetResponse, ApiError>;

    async fn rfc7807_get(
        &self,
        ) -> Result<Rfc7807GetResponse, ApiError>;

    async fn untyped_property_get(
        &self,
        object_untyped_props: Option<models::ObjectUntypedProps>,
        ) -> Result<UntypedPropertyGetResponse, ApiError>;

    async fn uuid_get(
        &self,
        ) -> Result<UuidGetResponse, ApiError>;

    async fn xml_extra_post(
        &self,
        duplicate_xml_object: Option<models::DuplicateXmlObject>,
        ) -> Result<XmlExtraPostResponse, ApiError>;

    async fn xml_other_post(
        &self,
        another_xml_object: Option<models::AnotherXmlObject>,
        ) -> Result<XmlOtherPostResponse, ApiError>;

    async fn xml_other_put(
        &self,
        another_xml_array: Option<models::AnotherXmlArray>,
        ) -> Result<XmlOtherPutResponse, ApiError>;

    /// Post an array
    async fn xml_post(
        &self,
        xml_array: Option<models::XmlArray>,
        ) -> Result<XmlPostResponse, ApiError>;

    async fn xml_put(
        &self,
        xml_object: Option<models::XmlObject>,
        ) -> Result<XmlPutResponse, ApiError>;

    async fn create_repo(
        &self,
        object_param: models::ObjectParam,
        ) -> Result<CreateRepoResponse, ApiError>;

    async fn get_repo_info(
        &self,
        repo_id: String,
        ) -> Result<GetRepoInfoResponse, ApiError>;

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

    async fn any_of_get(
        &self,
        any_of: Option<&Vec<models::AnyOfObject>>,
        ) -> Result<AnyOfGetResponse, ApiError>
    {
        let context = self.context().clone();
        self.api().any_of_get(any_of, &context).await
    }

    async fn callback_with_header_post(
        &self,
        url: String,
        ) -> Result<CallbackWithHeaderPostResponse, ApiError>
    {
        let context = self.context().clone();
        self.api().callback_with_header_post(url, &context).await
    }

    async fn complex_query_param_get(
        &self,
        list_of_strings: Option<&Vec<models::StringObject>>,
        ) -> Result<ComplexQueryParamGetResponse, ApiError>
    {
        let context = self.context().clone();
        self.api().complex_query_param_get(list_of_strings, &context).await
    }

    async fn enum_in_path_path_param_get(
        &self,
        path_param: models::StringEnum,
        ) -> Result<EnumInPathPathParamGetResponse, ApiError>
    {
        let context = self.context().clone();
        self.api().enum_in_path_path_param_get(path_param, &context).await
    }

    async fn json_complex_query_param_get(
        &self,
        list_of_strings: Option<&Vec<models::StringObject>>,
        ) -> Result<JsonComplexQueryParamGetResponse, ApiError>
    {
        let context = self.context().clone();
        self.api().json_complex_query_param_get(list_of_strings, &context).await
    }

    async fn mandatory_request_header_get(
        &self,
        x_header: String,
        ) -> Result<MandatoryRequestHeaderGetResponse, ApiError>
    {
        let context = self.context().clone();
        self.api().mandatory_request_header_get(x_header, &context).await
    }

    async fn merge_patch_json_get(
        &self,
        ) -> Result<MergePatchJsonGetResponse, ApiError>
    {
        let context = self.context().clone();
        self.api().merge_patch_json_get(&context).await
    }

    /// Get some stuff.
    async fn multiget_get(
        &self,
        ) -> Result<MultigetGetResponse, ApiError>
    {
        let context = self.context().clone();
        self.api().multiget_get(&context).await
    }

    async fn multiple_auth_scheme_get(
        &self,
        ) -> Result<MultipleAuthSchemeGetResponse, ApiError>
    {
        let context = self.context().clone();
        self.api().multiple_auth_scheme_get(&context).await
    }

    async fn one_of_get(
        &self,
        ) -> Result<OneOfGetResponse, ApiError>
    {
        let context = self.context().clone();
        self.api().one_of_get(&context).await
    }

    async fn override_server_get(
        &self,
        ) -> Result<OverrideServerGetResponse, ApiError>
    {
        let context = self.context().clone();
        self.api().override_server_get(&context).await
    }

    /// Get some stuff with parameters.
    async fn paramget_get(
        &self,
        uuid: Option<uuid::Uuid>,
        some_object: Option<models::ObjectParam>,
        some_list: Option<models::MyIdList>,
        ) -> Result<ParamgetGetResponse, ApiError>
    {
        let context = self.context().clone();
        self.api().paramget_get(uuid, some_object, some_list, &context).await
    }

    async fn readonly_auth_scheme_get(
        &self,
        ) -> Result<ReadonlyAuthSchemeGetResponse, ApiError>
    {
        let context = self.context().clone();
        self.api().readonly_auth_scheme_get(&context).await
    }

    async fn register_callback_post(
        &self,
        url: String,
        ) -> Result<RegisterCallbackPostResponse, ApiError>
    {
        let context = self.context().clone();
        self.api().register_callback_post(url, &context).await
    }

    async fn required_octet_stream_put(
        &self,
        body: swagger::ByteArray,
        ) -> Result<RequiredOctetStreamPutResponse, ApiError>
    {
        let context = self.context().clone();
        self.api().required_octet_stream_put(body, &context).await
    }

    async fn responses_with_headers_get(
        &self,
        ) -> Result<ResponsesWithHeadersGetResponse, ApiError>
    {
        let context = self.context().clone();
        self.api().responses_with_headers_get(&context).await
    }

    async fn rfc7807_get(
        &self,
        ) -> Result<Rfc7807GetResponse, ApiError>
    {
        let context = self.context().clone();
        self.api().rfc7807_get(&context).await
    }

    async fn untyped_property_get(
        &self,
        object_untyped_props: Option<models::ObjectUntypedProps>,
        ) -> Result<UntypedPropertyGetResponse, ApiError>
    {
        let context = self.context().clone();
        self.api().untyped_property_get(object_untyped_props, &context).await
    }

    async fn uuid_get(
        &self,
        ) -> Result<UuidGetResponse, ApiError>
    {
        let context = self.context().clone();
        self.api().uuid_get(&context).await
    }

    async fn xml_extra_post(
        &self,
        duplicate_xml_object: Option<models::DuplicateXmlObject>,
        ) -> Result<XmlExtraPostResponse, ApiError>
    {
        let context = self.context().clone();
        self.api().xml_extra_post(duplicate_xml_object, &context).await
    }

    async fn xml_other_post(
        &self,
        another_xml_object: Option<models::AnotherXmlObject>,
        ) -> Result<XmlOtherPostResponse, ApiError>
    {
        let context = self.context().clone();
        self.api().xml_other_post(another_xml_object, &context).await
    }

    async fn xml_other_put(
        &self,
        another_xml_array: Option<models::AnotherXmlArray>,
        ) -> Result<XmlOtherPutResponse, ApiError>
    {
        let context = self.context().clone();
        self.api().xml_other_put(another_xml_array, &context).await
    }

    /// Post an array
    async fn xml_post(
        &self,
        xml_array: Option<models::XmlArray>,
        ) -> Result<XmlPostResponse, ApiError>
    {
        let context = self.context().clone();
        self.api().xml_post(xml_array, &context).await
    }

    async fn xml_put(
        &self,
        xml_object: Option<models::XmlObject>,
        ) -> Result<XmlPutResponse, ApiError>
    {
        let context = self.context().clone();
        self.api().xml_put(xml_object, &context).await
    }

    async fn create_repo(
        &self,
        object_param: models::ObjectParam,
        ) -> Result<CreateRepoResponse, ApiError>
    {
        let context = self.context().clone();
        self.api().create_repo(object_param, &context).await
    }

    async fn get_repo_info(
        &self,
        repo_id: String,
        ) -> Result<GetRepoInfoResponse, ApiError>
    {
        let context = self.context().clone();
        self.api().get_repo_info(repo_id, &context).await
    }

}


#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub enum CallbackCallbackWithHeaderPostResponse {
    /// OK
    OK
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub enum CallbackCallbackPostResponse {
    /// OK
    OK
}


/// Callback API
#[async_trait]
pub trait CallbackApi<C: Send + Sync> {
    fn poll_ready(&self, _cx: &mut Context) -> Poll<Result<(), Box<dyn Error + Send + Sync + 'static>>> {
        Poll::Ready(Ok(()))
    }

    async fn callback_callback_with_header_post(
        &self,
        callback_request_query_url: String,
        information: Option<String>,
        context: &C) -> Result<CallbackCallbackWithHeaderPostResponse, ApiError>;

    async fn callback_callback_post(
        &self,
        callback_request_query_url: String,
        context: &C) -> Result<CallbackCallbackPostResponse, ApiError>;

}

/// Callback API without a `Context`
#[async_trait]
pub trait CallbackApiNoContext<C: Send + Sync> {
    fn poll_ready(&self, _cx: &mut Context) -> Poll<Result<(), Box<dyn Error + Send + Sync + 'static>>>;

    fn context(&self) -> &C;

    async fn callback_callback_with_header_post(
        &self,
        callback_request_query_url: String,
        information: Option<String>,
        ) -> Result<CallbackCallbackWithHeaderPostResponse, ApiError>;

    async fn callback_callback_post(
        &self,
        callback_request_query_url: String,
        ) -> Result<CallbackCallbackPostResponse, ApiError>;

}

pub trait CallbackContextWrapperExt<C: Send + Sync> where Self: Sized
{
    /// Binds this API to a context.
    fn with_context(self: Self, context: C) -> ContextWrapper<Self, C>;
}

impl<T: CallbackApi<C> + Send + Sync, C: Clone + Send + Sync> CallbackContextWrapperExt<C> for T {
    fn with_context(self: T, context: C) -> ContextWrapper<T, C> {
         ContextWrapper::<T, C>::new(self, context)
    }
}

#[async_trait]
impl<T: CallbackApi<C> + Send + Sync, C: Clone + Send + Sync> CallbackApiNoContext<C> for ContextWrapper<T, C> {
    fn poll_ready(&self, cx: &mut Context) -> Poll<Result<(), ServiceError>> {
        self.api().poll_ready(cx)
    }

    fn context(&self) -> &C {
        ContextWrapper::context(self)
    }

    async fn callback_callback_with_header_post(
        &self,
        callback_request_query_url: String,
        information: Option<String>,
        ) -> Result<CallbackCallbackWithHeaderPostResponse, ApiError>
    {
        let context = self.context().clone();
        self.api().callback_callback_with_header_post(
            callback_request_query_url,
            information,
            &context).await
    }

    async fn callback_callback_post(
        &self,
        callback_request_query_url: String,
        ) -> Result<CallbackCallbackPostResponse, ApiError>
    {
        let context = self.context().clone();
        self.api().callback_callback_post(
            callback_request_query_url,
            &context).await
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

#[cfg(any(feature = "client", feature = "server"))]
pub mod context;

pub mod models;

#[cfg(any(feature = "client", feature = "server"))]
pub(crate) mod header;
