use crate::{mimetypes, headers::*};
use bytes::{Buf, buf::BufExt};
use std::future::Future;
use std::pin::Pin;
use std::sync::Arc;
use std::marker::PhantomData;
use futures::{future, FutureExt, Stream, stream, TryStreamExt};
use headers::{ContentType, HeaderMapExt};
use hyper;
use hyper::{Body, HeaderMap, Request, Response, StatusCode};
use lazy_static::lazy_static;
use log::*;
use url::form_urlencoded;
use mime::Mime;
use multipart::server::Multipart;
use multipart::server::save::SaveResult;
use std::io::Read;
use serde_json;
use serde_xml_rs;

#[allow(unused_imports)]
use std::collections::{HashMap, BTreeMap};
use std::io;

#[allow(unused_imports)]
use std::collections::BTreeSet;

pub use openapi_context::auth::Authorization;
use openapi_context::{ApiError, ContextualPayload, XSpanId, Has, RequestParser};
use openapi_context::auth::Scopes;

use crate::{
    Api
    ,
    TestSpecialTagsResponse
    ,
    FakeOuterBooleanSerializeResponse,
    FakeOuterCompositeSerializeResponse,
    FakeOuterNumberSerializeResponse,
    FakeOuterStringSerializeResponse,
    HyphenParamResponse,
    TestBodyWithQueryParamsResponse,
    TestClientModelResponse,
    TestEndpointParametersResponse,
    TestEnumParametersResponse,
    TestInlineAdditionalPropertiesResponse,
    TestJsonFormDataResponse
    ,
    TestClassnameResponse
    ,
    AddPetResponse,
    DeletePetResponse,
    FindPetsByStatusResponse,
    FindPetsByTagsResponse,
    GetPetByIdResponse,
    UpdatePetResponse,
    UpdatePetWithFormResponse,
    UploadFileResponse
    ,
    DeleteOrderResponse,
    GetInventoryResponse,
    GetOrderByIdResponse,
    PlaceOrderResponse
    ,
    CreateUserResponse,
    CreateUsersWithArrayInputResponse,
    CreateUsersWithListInputResponse,
    DeleteUserResponse,
    GetUserByNameResponse,
    LoginUserResponse,
    LogoutUserResponse,
    UpdateUserResponse
};
#[allow(unused_imports)]
use crate::models;

pub mod context;
pub mod tls;

mod paths {
    use lazy_static::lazy_static;

    lazy_static! {
        pub static ref GLOBAL_REGEX_SET: regex::RegexSet = regex::RegexSet::new(vec![
            r"^/v2/another-fake/dummy$",
            r"^/v2/fake$",
            r"^/v2/fake/body-with-query-params$",
            r"^/v2/fake/hyphenParam/(?P<hyphen_param>[^/?#]*)$",
            r"^/v2/fake/inline-additionalProperties$",
            r"^/v2/fake/jsonFormData$",
            r"^/v2/fake/outer/boolean$",
            r"^/v2/fake/outer/composite$",
            r"^/v2/fake/outer/number$",
            r"^/v2/fake/outer/string$",
            r"^/v2/fake_classname_test$",
            r"^/v2/pet$",
            r"^/v2/pet/findByStatus$",
            r"^/v2/pet/findByTags$",
            r"^/v2/pet/(?P<pet_id>[^/?#]*)$",
            r"^/v2/pet/(?P<pet_id>[^/?#]*)/uploadImage$",
            r"^/v2/store/inventory$",
            r"^/v2/store/order$",
            r"^/v2/store/order/(?P<order_id>[^/?#]*)$",
            r"^/v2/user$",
            r"^/v2/user/createWithArray$",
            r"^/v2/user/createWithList$",
            r"^/v2/user/login$",
            r"^/v2/user/logout$",
            r"^/v2/user/(?P<username>[^/?#]*)$"
        ]).unwrap();
    }
    pub static ID_ANOTHER_FAKE_DUMMY: usize = 0;
    pub static ID_FAKE: usize = 1;
    pub static ID_FAKE_BODY_WITH_QUERY_PARAMS: usize = 2;
    pub static ID_FAKE_HYPHENPARAM_HYPHEN_PARAM: usize = 3;
    lazy_static! {
        pub static ref REGEX_FAKE_HYPHENPARAM_HYPHEN_PARAM: regex::Regex = regex::Regex::new(r"^/v2/fake/hyphenParam/(?P<hyphen_param>[^/?#]*)$").unwrap();
    }
    pub static ID_FAKE_INLINE_ADDITIONALPROPERTIES: usize = 4;
    pub static ID_FAKE_JSONFORMDATA: usize = 5;
    pub static ID_FAKE_OUTER_BOOLEAN: usize = 6;
    pub static ID_FAKE_OUTER_COMPOSITE: usize = 7;
    pub static ID_FAKE_OUTER_NUMBER: usize = 8;
    pub static ID_FAKE_OUTER_STRING: usize = 9;
    pub static ID_FAKE_CLASSNAME_TEST: usize = 10;
    pub static ID_PET: usize = 11;
    pub static ID_PET_FINDBYSTATUS: usize = 12;
    pub static ID_PET_FINDBYTAGS: usize = 13;
    pub static ID_PET_PETID: usize = 14;
    lazy_static! {
        pub static ref REGEX_PET_PETID: regex::Regex = regex::Regex::new(r"^/v2/pet/(?P<pet_id>[^/?#]*)$").unwrap();
    }
    pub static ID_PET_PETID_UPLOADIMAGE: usize = 15;
    lazy_static! {
        pub static ref REGEX_PET_PETID_UPLOADIMAGE: regex::Regex = regex::Regex::new(r"^/v2/pet/(?P<pet_id>[^/?#]*)/uploadImage$").unwrap();
    }
    pub static ID_STORE_INVENTORY: usize = 16;
    pub static ID_STORE_ORDER: usize = 17;
    pub static ID_STORE_ORDER_ORDER_ID: usize = 18;
    lazy_static! {
        pub static ref REGEX_STORE_ORDER_ORDER_ID: regex::Regex = regex::Regex::new(r"^/v2/store/order/(?P<order_id>[^/?#]*)$").unwrap();
    }
    pub static ID_USER: usize = 19;
    pub static ID_USER_CREATEWITHARRAY: usize = 20;
    pub static ID_USER_CREATEWITHLIST: usize = 21;
    pub static ID_USER_LOGIN: usize = 22;
    pub static ID_USER_LOGOUT: usize = 23;
    pub static ID_USER_USERNAME: usize = 24;
    lazy_static! {
        pub static ref REGEX_USER_USERNAME: regex::Regex = regex::Regex::new(r"^/v2/user/(?P<username>[^/?#]*)$").unwrap();
    }
}

pub struct Service<T, C> {
    api_impl: T,
    marker: PhantomData<C>,
}

impl<T, C> Service<T, C> {
    pub fn new(api_impl: T) -> Service<T, C> {
        Service{api_impl: api_impl, marker: PhantomData}
    }
}

pub type ServiceRequest<C> = ContextualPayload<C>;
pub type ServiceResponse = Response<Body>;
impl<T, C> hyper::service::Service<ServiceRequest<C>> for Service<T, C>
where
    T: Api<C> + Send + Sync + Clone + 'static,
    C: Has<XSpanId> + Has<Option<Authorization>> + Send + Sync + 'static
{
    type Response = ServiceResponse;
    type Error = ApiError;
    type Future = Pin<Box<dyn Future<Output=Result<Self::Response, Self::Error>> + Send>>;

    fn poll_ready(&mut self, cx: &mut std::task::Context<'_>) -> std::task::Poll<Result<(), Self::Error>> {
        std::task::Poll::Ready(Ok(()))
    }

    fn call(&mut self, r: ServiceRequest<C>) -> Self::Future {
        Box::pin(do_call(self.api_impl.clone(), r))
    }
}

async fn do_call<T, C>(api_impl: T, r: ServiceRequest<C>) -> Result<ServiceResponse, ApiError>
where
    T: Api<C> + Send + Sync + Clone + 'static,
    C: Has<XSpanId> + Has<Option<Authorization>> + Send + Sync + 'static
{
    let mut api_impl = api_impl;
    let mut context = r.context;
    let (parts, body) = r.inner.into_parts();
    let uri = parts.uri;
    let method = parts.method;
    let headers = parts.headers;
    let path = paths::GLOBAL_REGEX_SET.matches(uri.path());

    // This match statement is duplicated below in `parse_operation_id()`.
    // Please update both places if changing how this code is autogenerated.
    match &method {
    
        // TestSpecialTags - PATCH /another-fake/dummy
        &hyper::Method::PATCH if path.matched(paths::ID_ANOTHER_FAKE_DUMMY) => {
            // Body parameters (note that non-required body parameters will ignore garbage
            // values, rather than causing a 400 response). Produce warning header and logs for
            // any unused fields

            let body = hyper::body::aggregate(body).await.map_err(|e| ApiError(format!("Error getting response: {}", e)));
            let body = match body {
                Err(e) => {
                    return Ok(
                        Response::builder()
                            .status(StatusCode::BAD_REQUEST)
                            .body(hyper::Body::from(format!("Couldn't receive body parameter body: {}", e)))
                            .unwrap()
                    );
                }
                Ok(v) => v,
            };
            let mut unused_elements = Vec::new();
            let param_body: Option<crate::models::Client> = if body.remaining() > 0 {
                let deserializer = &mut serde_json::Deserializer::from_reader(body.reader());
                match serde_ignored::deserialize(deserializer, |path| {
                    warn!("Ignoring unknown field in body: {}", path);
                    unused_elements.push(path.to_string());
                }) {
                    Ok(param_body) => param_body,
                    Err(e) => return Ok(Response::builder().status(StatusCode::BAD_REQUEST).body(hyper::Body::from(format!("Couldn't parse body parameter body - doesn't match schema: {}", e))).unwrap()),
                }
            } else {
                None
            };
            let param_body = match param_body {
                Some(param_body) => param_body,
                None => return Ok(Response::builder().status(StatusCode::BAD_REQUEST).body(hyper::Body::from("Missing required body parameter body")).unwrap()),
            };
            let result = api_impl.test_special_tags(param_body, &context).await;
            let mut response = Response::builder();
            response.headers_mut().unwrap().typed_insert(XSpanId( (&context as &dyn Has<XSpanId>).get().0.to_string() ));
            let result = match result {
                Err(e) => {
                    // Application code returned an error. This should not happen, as the implementation should
                    // return a valid response.
                    response = response.status(StatusCode::INTERNAL_SERVER_ERROR);
                    return Ok(response.body(hyper::Body::from("An internal error occurred")).unwrap());
                }
                Ok(r) => r,
            };
        
            if !unused_elements.is_empty() {
                response.headers_mut().unwrap().typed_insert(Warning(format!("Ignoring unknown fields in body: {:?}", unused_elements)));
            }
        
            match result {
                TestSpecialTagsResponse::SuccessfulOperation(body)
            
                => {
                    response = response.status(hyper::http::StatusCode::from_u16(200).unwrap());
            
                    let header = headers::ContentType::from(mimetypes::responses::TEST_SPECIAL_TAGS_SUCCESSFUL_OPERATION.clone());
                    response.headers_mut().unwrap().typed_insert(header);
            
                
                    let body = serde_json::to_string(&body).expect("impossible to fail to serialize");
                
                    Ok(response.body(Body::from(body)).unwrap())
                },
            }
        },
    
        // FakeOuterBooleanSerialize - POST /fake/outer/boolean
        &hyper::Method::POST if path.matched(paths::ID_FAKE_OUTER_BOOLEAN) => {
            // Body parameters (note that non-required body parameters will ignore garbage
            // values, rather than causing a 400 response). Produce warning header and logs for
            // any unused fields

            let body = hyper::body::aggregate(body).await.map_err(|e| ApiError(format!("Error getting response: {}", e)));
            let body = match body {
                Err(e) => {
                    return Ok(
                        Response::builder()
                            .status(StatusCode::BAD_REQUEST)
                            .body(hyper::Body::from(format!("Couldn't receive body parameter body: {}", e)))
                            .unwrap()
                    );
                }
                Ok(v) => v,
            };
            let mut unused_elements = Vec::new();
            let param_body: Option<crate::models::OuterBoolean> = if body.remaining() > 0 {
                let deserializer = &mut serde_json::Deserializer::from_reader(body.reader());
                match serde_ignored::deserialize(deserializer, |path| {
                    warn!("Ignoring unknown field in body: {}", path);
                    unused_elements.push(path.to_string());
                }) {
                    Ok(param_body) => param_body,
                    Err(_) => None,
                }
            } else {
                None
            };
            let result = api_impl.fake_outer_boolean_serialize(param_body, &context).await;
            let mut response = Response::builder();
            response.headers_mut().unwrap().typed_insert(XSpanId( (&context as &dyn Has<XSpanId>).get().0.to_string() ));
            let result = match result {
                Err(e) => {
                    // Application code returned an error. This should not happen, as the implementation should
                    // return a valid response.
                    response = response.status(StatusCode::INTERNAL_SERVER_ERROR);
                    return Ok(response.body(hyper::Body::from("An internal error occurred")).unwrap());
                }
                Ok(r) => r,
            };
        
            if !unused_elements.is_empty() {
                response.headers_mut().unwrap().typed_insert(Warning(format!("Ignoring unknown fields in body: {:?}", unused_elements)));
            }
        
            match result {
                FakeOuterBooleanSerializeResponse::OutputBoolean(body)
            
                => {
                    response = response.status(hyper::http::StatusCode::from_u16(200).unwrap());
            
                    let header = headers::ContentType::from(mimetypes::responses::FAKE_OUTER_BOOLEAN_SERIALIZE_OUTPUT_BOOLEAN.clone());
                    response.headers_mut().unwrap().typed_insert(header);
            
                
                    let body = serde_json::to_string(&body).expect("impossible to fail to serialize");
                
                    Ok(response.body(Body::from(body)).unwrap())
                },
            }
        },
    
        // FakeOuterCompositeSerialize - POST /fake/outer/composite
        &hyper::Method::POST if path.matched(paths::ID_FAKE_OUTER_COMPOSITE) => {
            // Body parameters (note that non-required body parameters will ignore garbage
            // values, rather than causing a 400 response). Produce warning header and logs for
            // any unused fields

            let body = hyper::body::aggregate(body).await.map_err(|e| ApiError(format!("Error getting response: {}", e)));
            let body = match body {
                Err(e) => {
                    return Ok(
                        Response::builder()
                            .status(StatusCode::BAD_REQUEST)
                            .body(hyper::Body::from(format!("Couldn't receive body parameter body: {}", e)))
                            .unwrap()
                    );
                }
                Ok(v) => v,
            };
            let mut unused_elements = Vec::new();
            let param_body: Option<crate::models::OuterComposite> = if body.remaining() > 0 {
                let deserializer = &mut serde_json::Deserializer::from_reader(body.reader());
                match serde_ignored::deserialize(deserializer, |path| {
                    warn!("Ignoring unknown field in body: {}", path);
                    unused_elements.push(path.to_string());
                }) {
                    Ok(param_body) => param_body,
                    Err(_) => None,
                }
            } else {
                None
            };
            let result = api_impl.fake_outer_composite_serialize(param_body, &context).await;
            let mut response = Response::builder();
            response.headers_mut().unwrap().typed_insert(XSpanId( (&context as &dyn Has<XSpanId>).get().0.to_string() ));
            let result = match result {
                Err(e) => {
                    // Application code returned an error. This should not happen, as the implementation should
                    // return a valid response.
                    response = response.status(StatusCode::INTERNAL_SERVER_ERROR);
                    return Ok(response.body(hyper::Body::from("An internal error occurred")).unwrap());
                }
                Ok(r) => r,
            };
        
            if !unused_elements.is_empty() {
                response.headers_mut().unwrap().typed_insert(Warning(format!("Ignoring unknown fields in body: {:?}", unused_elements)));
            }
        
            match result {
                FakeOuterCompositeSerializeResponse::OutputComposite(body)
            
                => {
                    response = response.status(hyper::http::StatusCode::from_u16(200).unwrap());
            
                    let header = headers::ContentType::from(mimetypes::responses::FAKE_OUTER_COMPOSITE_SERIALIZE_OUTPUT_COMPOSITE.clone());
                    response.headers_mut().unwrap().typed_insert(header);
            
                
                    let body = serde_json::to_string(&body).expect("impossible to fail to serialize");
                
                    Ok(response.body(Body::from(body)).unwrap())
                },
            }
        },
    
        // FakeOuterNumberSerialize - POST /fake/outer/number
        &hyper::Method::POST if path.matched(paths::ID_FAKE_OUTER_NUMBER) => {
            // Body parameters (note that non-required body parameters will ignore garbage
            // values, rather than causing a 400 response). Produce warning header and logs for
            // any unused fields

            let body = hyper::body::aggregate(body).await.map_err(|e| ApiError(format!("Error getting response: {}", e)));
            let body = match body {
                Err(e) => {
                    return Ok(
                        Response::builder()
                            .status(StatusCode::BAD_REQUEST)
                            .body(hyper::Body::from(format!("Couldn't receive body parameter body: {}", e)))
                            .unwrap()
                    );
                }
                Ok(v) => v,
            };
            let mut unused_elements = Vec::new();
            let param_body: Option<crate::models::OuterNumber> = if body.remaining() > 0 {
                let deserializer = &mut serde_json::Deserializer::from_reader(body.reader());
                match serde_ignored::deserialize(deserializer, |path| {
                    warn!("Ignoring unknown field in body: {}", path);
                    unused_elements.push(path.to_string());
                }) {
                    Ok(param_body) => param_body,
                    Err(_) => None,
                }
            } else {
                None
            };
            let result = api_impl.fake_outer_number_serialize(param_body, &context).await;
            let mut response = Response::builder();
            response.headers_mut().unwrap().typed_insert(XSpanId( (&context as &dyn Has<XSpanId>).get().0.to_string() ));
            let result = match result {
                Err(e) => {
                    // Application code returned an error. This should not happen, as the implementation should
                    // return a valid response.
                    response = response.status(StatusCode::INTERNAL_SERVER_ERROR);
                    return Ok(response.body(hyper::Body::from("An internal error occurred")).unwrap());
                }
                Ok(r) => r,
            };
        
            if !unused_elements.is_empty() {
                response.headers_mut().unwrap().typed_insert(Warning(format!("Ignoring unknown fields in body: {:?}", unused_elements)));
            }
        
            match result {
                FakeOuterNumberSerializeResponse::OutputNumber(body)
            
                => {
                    response = response.status(hyper::http::StatusCode::from_u16(200).unwrap());
            
                    let header = headers::ContentType::from(mimetypes::responses::FAKE_OUTER_NUMBER_SERIALIZE_OUTPUT_NUMBER.clone());
                    response.headers_mut().unwrap().typed_insert(header);
            
                
                    let body = serde_json::to_string(&body).expect("impossible to fail to serialize");
                
                    Ok(response.body(Body::from(body)).unwrap())
                },
            }
        },
    
        // FakeOuterStringSerialize - POST /fake/outer/string
        &hyper::Method::POST if path.matched(paths::ID_FAKE_OUTER_STRING) => {
            // Body parameters (note that non-required body parameters will ignore garbage
            // values, rather than causing a 400 response). Produce warning header and logs for
            // any unused fields

            let body = hyper::body::aggregate(body).await.map_err(|e| ApiError(format!("Error getting response: {}", e)));
            let body = match body {
                Err(e) => {
                    return Ok(
                        Response::builder()
                            .status(StatusCode::BAD_REQUEST)
                            .body(hyper::Body::from(format!("Couldn't receive body parameter body: {}", e)))
                            .unwrap()
                    );
                }
                Ok(v) => v,
            };
            let mut unused_elements = Vec::new();
            let param_body: Option<crate::models::OuterString> = if body.remaining() > 0 {
                let deserializer = &mut serde_json::Deserializer::from_reader(body.reader());
                match serde_ignored::deserialize(deserializer, |path| {
                    warn!("Ignoring unknown field in body: {}", path);
                    unused_elements.push(path.to_string());
                }) {
                    Ok(param_body) => param_body,
                    Err(_) => None,
                }
            } else {
                None
            };
            let result = api_impl.fake_outer_string_serialize(param_body, &context).await;
            let mut response = Response::builder();
            response.headers_mut().unwrap().typed_insert(XSpanId( (&context as &dyn Has<XSpanId>).get().0.to_string() ));
            let result = match result {
                Err(e) => {
                    // Application code returned an error. This should not happen, as the implementation should
                    // return a valid response.
                    response = response.status(StatusCode::INTERNAL_SERVER_ERROR);
                    return Ok(response.body(hyper::Body::from("An internal error occurred")).unwrap());
                }
                Ok(r) => r,
            };
        
            if !unused_elements.is_empty() {
                response.headers_mut().unwrap().typed_insert(Warning(format!("Ignoring unknown fields in body: {:?}", unused_elements)));
            }
        
            match result {
                FakeOuterStringSerializeResponse::OutputString(body)
            
                => {
                    response = response.status(hyper::http::StatusCode::from_u16(200).unwrap());
            
                    let header = headers::ContentType::from(mimetypes::responses::FAKE_OUTER_STRING_SERIALIZE_OUTPUT_STRING.clone());
                    response.headers_mut().unwrap().typed_insert(header);
            
                
                    let body = serde_json::to_string(&body).expect("impossible to fail to serialize");
                
                    Ok(response.body(Body::from(body)).unwrap())
                },
            }
        },
    
        // HyphenParam - GET /fake/hyphenParam/{hyphen-param}
        &hyper::Method::GET if path.matched(paths::ID_FAKE_HYPHENPARAM_HYPHEN_PARAM) => {
            // Path parameters
            let path = uri.path().to_string();
            let path_params = paths::REGEX_FAKE_HYPHENPARAM_HYPHEN_PARAM
                .captures(&path)
                .unwrap_or_else(||
                    panic!("Path {} matched RE FAKE_HYPHENPARAM_HYPHEN_PARAM in set but failed match against \"{}\"", path, paths::REGEX_FAKE_HYPHENPARAM_HYPHEN_PARAM.as_str())
                );
            let param_hyphen_param = match percent_encoding::percent_decode(path_params["hyphen_param"].as_bytes()).decode_utf8() {
                Ok(param_hyphen_param) => match param_hyphen_param.parse::<String>() {
                    Ok(param_hyphen_param) => param_hyphen_param,
                    Err(e) => return Ok(Response::builder().status(StatusCode::BAD_REQUEST).body(hyper::Body::from(format!("Couldn't parse path parameter hyphen-param: {:?}", e))).unwrap()),
                },
                Err(_) => return Ok(Response::builder().status(StatusCode::BAD_REQUEST).body(hyper::Body::from(format!("Couldn't percent-decode path parameter as UTF-8: {}", &path_params["hyphen-param"]))).unwrap())
            };
            let result = api_impl.hyphen_param(param_hyphen_param, &context).await;
            let mut response = Response::builder();
            response.headers_mut().unwrap().typed_insert(XSpanId( (&context as &dyn Has<XSpanId>).get().0.to_string() ));
            let result = match result {
                Err(e) => {
                    // Application code returned an error. This should not happen, as the implementation should
                    // return a valid response.
                    response = response.status(StatusCode::INTERNAL_SERVER_ERROR);
                    return Ok(response.body(hyper::Body::from("An internal error occurred")).unwrap());
                }
                Ok(r) => r,
            };
        
            match result {
                HyphenParamResponse::Success
            
                => {
                    response = response.status(hyper::http::StatusCode::from_u16(200).unwrap());
            
                    Ok(response.body(Body::empty()).unwrap())
                },
            }
        },
    
        // TestBodyWithQueryParams - PUT /fake/body-with-query-params
        &hyper::Method::PUT if path.matched(paths::ID_FAKE_BODY_WITH_QUERY_PARAMS) => {
            // Query parameters (note that non-required or collection query parameters will ignore garbage values, rather than causing a 400 response)
            let query_params = form_urlencoded::parse(uri.query().unwrap_or_default().as_bytes()).collect::<Vec<_>>();
            let param_query = query_params.iter().filter(|e| e.0 == "query").map(|e| e.1.to_owned())
                .nth(0);
            let param_query = match param_query {
                Some(param_query) => match param_query.parse::<String>() {
                    Ok(param_query) => param_query,
                    Err(e) => return Ok(Response::builder().status(StatusCode::BAD_REQUEST).body(hyper::Body::from(format!("Couldn't parse query parameter query - doesn't match schema: {}", e))).unwrap()),
                },
                None => return Ok(Response::builder().status(StatusCode::BAD_REQUEST).body(hyper::Body::from("Missing required query parameter query")).unwrap()),
            };
            // Body parameters (note that non-required body parameters will ignore garbage
            // values, rather than causing a 400 response). Produce warning header and logs for
            // any unused fields

            let body = hyper::body::aggregate(body).await.map_err(|e| ApiError(format!("Error getting response: {}", e)));
            let body = match body {
                Err(e) => {
                    return Ok(
                        Response::builder()
                            .status(StatusCode::BAD_REQUEST)
                            .body(hyper::Body::from(format!("Couldn't receive body parameter body: {}", e)))
                            .unwrap()
                    );
                }
                Ok(v) => v,
            };
            let mut unused_elements = Vec::new();
            let param_body: Option<crate::models::User> = if body.remaining() > 0 {
                let deserializer = &mut serde_json::Deserializer::from_reader(body.reader());
                match serde_ignored::deserialize(deserializer, |path| {
                    warn!("Ignoring unknown field in body: {}", path);
                    unused_elements.push(path.to_string());
                }) {
                    Ok(param_body) => param_body,
                    Err(e) => return Ok(Response::builder().status(StatusCode::BAD_REQUEST).body(hyper::Body::from(format!("Couldn't parse body parameter body - doesn't match schema: {}", e))).unwrap()),
                }
            } else {
                None
            };
            let param_body = match param_body {
                Some(param_body) => param_body,
                None => return Ok(Response::builder().status(StatusCode::BAD_REQUEST).body(hyper::Body::from("Missing required body parameter body")).unwrap()),
            };
            let result = api_impl.test_body_with_query_params(param_query, param_body, &context).await;
            let mut response = Response::builder();
            response.headers_mut().unwrap().typed_insert(XSpanId( (&context as &dyn Has<XSpanId>).get().0.to_string() ));
            let result = match result {
                Err(e) => {
                    // Application code returned an error. This should not happen, as the implementation should
                    // return a valid response.
                    response = response.status(StatusCode::INTERNAL_SERVER_ERROR);
                    return Ok(response.body(hyper::Body::from("An internal error occurred")).unwrap());
                }
                Ok(r) => r,
            };
        
            if !unused_elements.is_empty() {
                response.headers_mut().unwrap().typed_insert(Warning(format!("Ignoring unknown fields in body: {:?}", unused_elements)));
            }
        
            match result {
                TestBodyWithQueryParamsResponse::Success
            
                => {
                    response = response.status(hyper::http::StatusCode::from_u16(200).unwrap());
            
                    Ok(response.body(Body::empty()).unwrap())
                },
            }
        },
    
        // TestClientModel - PATCH /fake
        &hyper::Method::PATCH if path.matched(paths::ID_FAKE) => {
            // Body parameters (note that non-required body parameters will ignore garbage
            // values, rather than causing a 400 response). Produce warning header and logs for
            // any unused fields

            let body = hyper::body::aggregate(body).await.map_err(|e| ApiError(format!("Error getting response: {}", e)));
            let body = match body {
                Err(e) => {
                    return Ok(
                        Response::builder()
                            .status(StatusCode::BAD_REQUEST)
                            .body(hyper::Body::from(format!("Couldn't receive body parameter body: {}", e)))
                            .unwrap()
                    );
                }
                Ok(v) => v,
            };
            let mut unused_elements = Vec::new();
            let param_body: Option<crate::models::Client> = if body.remaining() > 0 {
                let deserializer = &mut serde_json::Deserializer::from_reader(body.reader());
                match serde_ignored::deserialize(deserializer, |path| {
                    warn!("Ignoring unknown field in body: {}", path);
                    unused_elements.push(path.to_string());
                }) {
                    Ok(param_body) => param_body,
                    Err(e) => return Ok(Response::builder().status(StatusCode::BAD_REQUEST).body(hyper::Body::from(format!("Couldn't parse body parameter body - doesn't match schema: {}", e))).unwrap()),
                }
            } else {
                None
            };
            let param_body = match param_body {
                Some(param_body) => param_body,
                None => return Ok(Response::builder().status(StatusCode::BAD_REQUEST).body(hyper::Body::from("Missing required body parameter body")).unwrap()),
            };
            let result = api_impl.test_client_model(param_body, &context).await;
            let mut response = Response::builder();
            response.headers_mut().unwrap().typed_insert(XSpanId( (&context as &dyn Has<XSpanId>).get().0.to_string() ));
            let result = match result {
                Err(e) => {
                    // Application code returned an error. This should not happen, as the implementation should
                    // return a valid response.
                    response = response.status(StatusCode::INTERNAL_SERVER_ERROR);
                    return Ok(response.body(hyper::Body::from("An internal error occurred")).unwrap());
                }
                Ok(r) => r,
            };
        
            if !unused_elements.is_empty() {
                response.headers_mut().unwrap().typed_insert(Warning(format!("Ignoring unknown fields in body: {:?}", unused_elements)));
            }
        
            match result {
                TestClientModelResponse::SuccessfulOperation(body)
            
                => {
                    response = response.status(hyper::http::StatusCode::from_u16(200).unwrap());
            
                    let header = headers::ContentType::from(mimetypes::responses::TEST_CLIENT_MODEL_SUCCESSFUL_OPERATION.clone());
                    response.headers_mut().unwrap().typed_insert(header);
            
                
                    let body = serde_json::to_string(&body).expect("impossible to fail to serialize");
                
                    Ok(response.body(Body::from(body)).unwrap())
                },
            }
        },
    
        // TestEndpointParameters - POST /fake
        &hyper::Method::POST if path.matched(paths::ID_FAKE) => {
            let authorization = match (&context as &dyn Has<Option<Authorization>>).get() {
                &Some(ref authorization) => authorization,
                &None => return Ok(Response::builder().status(StatusCode::FORBIDDEN).body(hyper::Body::from("Unauthenticated")).unwrap()),
            };

            // Form parameters
            let param_integer = Some(56);
            let param_int32 = Some(56);
            let param_int64 = Some(789);
            let param_number = 8.14;
            let param_float = Some(3.4);
            let param_double = 1.2;
            let param_string = Some("string_example".to_string());
            let param_pattern_without_delimiter = "pattern_without_delimiter_example".to_string();
            let param_byte = openapi_context::ByteArray(Vec::from("BYTE_ARRAY_DATA_HERE"));
            let param_binary = Some(openapi_context::ByteArray(Vec::from("BINARY_DATA_HERE")));
            let param_date = None;
            let param_date_time = None;
            let param_password = Some("password_example".to_string());
            let param_callback = Some("callback_example".to_string());
            let result = api_impl.test_endpoint_parameters(param_number, param_double, param_pattern_without_delimiter, param_byte, param_integer, param_int32, param_int64, param_float, param_string, param_binary, param_date, param_date_time, param_password, param_callback, &context).await;
            let mut response = Response::builder();
            response.headers_mut().unwrap().typed_insert(XSpanId( (&context as &dyn Has<XSpanId>).get().0.to_string() ));
            let result = match result {
                Err(e) => {
                    // Application code returned an error. This should not happen, as the implementation should
                    // return a valid response.
                    response = response.status(StatusCode::INTERNAL_SERVER_ERROR);
                    return Ok(response.body(hyper::Body::from("An internal error occurred")).unwrap());
                }
                Ok(r) => r,
            };
        
            match result {
                TestEndpointParametersResponse::InvalidUsernameSupplied
            
                => {
                    response = response.status(hyper::http::StatusCode::from_u16(400).unwrap());
            
                    Ok(response.body(Body::empty()).unwrap())
                },
                TestEndpointParametersResponse::UserNotFound
            
                => {
                    response = response.status(hyper::http::StatusCode::from_u16(404).unwrap());
            
                    Ok(response.body(Body::empty()).unwrap())
                },
            }
        },
    
        // TestEnumParameters - GET /fake
        &hyper::Method::GET if path.matched(paths::ID_FAKE) => {
            // Header parameters
            let param_enum_header_string_array = headers.typed_get::<RequestEnumHeaderStringArray>().map(|header| header.0.clone());
            let param_enum_header_string = headers.typed_get::<RequestEnumHeaderString>().map(|header| header.0.clone());
            // Query parameters (note that non-required or collection query parameters will ignore garbage values, rather than causing a 400 response)
            let query_params = form_urlencoded::parse(uri.query().unwrap_or_default().as_bytes()).collect::<Vec<_>>();
            let param_enum_query_string_array = query_params.iter().filter(|e| e.0 == "enum_query_string_array").map(|e| e.1.to_owned())
                .filter_map(|param_enum_query_string_array| param_enum_query_string_array.parse::<String>().ok())
                .collect::<Vec<_>>();
            let param_enum_query_string_array = if !param_enum_query_string_array.is_empty() {
                Some(param_enum_query_string_array)
            } else {
                None
            };
            let param_enum_query_string = query_params.iter().filter(|e| e.0 == "enum_query_string").map(|e| e.1.to_owned())
                .nth(0);
            let param_enum_query_string = param_enum_query_string.and_then(|param_enum_query_string| param_enum_query_string.parse::<>().ok());
            let param_enum_query_integer = query_params.iter().filter(|e| e.0 == "enum_query_integer").map(|e| e.1.to_owned())
                .nth(0);
            let param_enum_query_integer = param_enum_query_integer.and_then(|param_enum_query_integer| param_enum_query_integer.parse::<>().ok());
            let param_enum_query_double = query_params.iter().filter(|e| e.0 == "enum_query_double").map(|e| e.1.to_owned())
                .nth(0);
            let param_enum_query_double = param_enum_query_double.and_then(|param_enum_query_double| param_enum_query_double.parse::<>().ok());
            // Form parameters
            let param_enum_form_string = Some("enum_form_string_example".to_string());
            let result = api_impl.test_enum_parameters(param_enum_header_string_array.as_ref(), param_enum_header_string, param_enum_query_string_array.as_ref(), param_enum_query_string, param_enum_query_integer, param_enum_query_double, param_enum_form_string, &context).await;
            let mut response = Response::builder();
            response.headers_mut().unwrap().typed_insert(XSpanId( (&context as &dyn Has<XSpanId>).get().0.to_string() ));
            let result = match result {
                Err(e) => {
                    // Application code returned an error. This should not happen, as the implementation should
                    // return a valid response.
                    response = response.status(StatusCode::INTERNAL_SERVER_ERROR);
                    return Ok(response.body(hyper::Body::from("An internal error occurred")).unwrap());
                }
                Ok(r) => r,
            };
        
            match result {
                TestEnumParametersResponse::InvalidRequest
            
                => {
                    response = response.status(hyper::http::StatusCode::from_u16(400).unwrap());
            
                    Ok(response.body(Body::empty()).unwrap())
                },
                TestEnumParametersResponse::NotFound
            
                => {
                    response = response.status(hyper::http::StatusCode::from_u16(404).unwrap());
            
                    Ok(response.body(Body::empty()).unwrap())
                },
            }
        },
    
        // TestInlineAdditionalProperties - POST /fake/inline-additionalProperties
        &hyper::Method::POST if path.matched(paths::ID_FAKE_INLINE_ADDITIONALPROPERTIES) => {
            // Body parameters (note that non-required body parameters will ignore garbage
            // values, rather than causing a 400 response). Produce warning header and logs for
            // any unused fields

            let body = hyper::body::aggregate(body).await.map_err(|e| ApiError(format!("Error getting response: {}", e)));
            let body = match body {
                Err(e) => {
                    return Ok(
                        Response::builder()
                            .status(StatusCode::BAD_REQUEST)
                            .body(hyper::Body::from(format!("Couldn't receive body parameter body: {}", e)))
                            .unwrap()
                    );
                }
                Ok(v) => v,
            };
            let mut unused_elements = Vec::new();
            let param_param: Option<HashMap<String, String>> = if body.remaining() > 0 {
                let deserializer = &mut serde_json::Deserializer::from_reader(body.reader());
                match serde_ignored::deserialize(deserializer, |path| {
                    warn!("Ignoring unknown field in body: {}", path);
                    unused_elements.push(path.to_string());
                }) {
                    Ok(param_param) => param_param,
                    Err(e) => return Ok(Response::builder().status(StatusCode::BAD_REQUEST).body(hyper::Body::from(format!("Couldn't parse body parameter param - doesn't match schema: {}", e))).unwrap()),
                }
            } else {
                None
            };
            let param_param = match param_param {
                Some(param_param) => param_param,
                None => return Ok(Response::builder().status(StatusCode::BAD_REQUEST).body(hyper::Body::from("Missing required body parameter param")).unwrap()),
            };
            let result = api_impl.test_inline_additional_properties(param_param, &context).await;
            let mut response = Response::builder();
            response.headers_mut().unwrap().typed_insert(XSpanId( (&context as &dyn Has<XSpanId>).get().0.to_string() ));
            let result = match result {
                Err(e) => {
                    // Application code returned an error. This should not happen, as the implementation should
                    // return a valid response.
                    response = response.status(StatusCode::INTERNAL_SERVER_ERROR);
                    return Ok(response.body(hyper::Body::from("An internal error occurred")).unwrap());
                }
                Ok(r) => r,
            };
        
            if !unused_elements.is_empty() {
                response.headers_mut().unwrap().typed_insert(Warning(format!("Ignoring unknown fields in body: {:?}", unused_elements)));
            }
        
            match result {
                TestInlineAdditionalPropertiesResponse::SuccessfulOperation
            
                => {
                    response = response.status(hyper::http::StatusCode::from_u16(200).unwrap());
            
                    Ok(response.body(Body::empty()).unwrap())
                },
            }
        },
    
        // TestJsonFormData - GET /fake/jsonFormData
        &hyper::Method::GET if path.matched(paths::ID_FAKE_JSONFORMDATA) => {
            // Form parameters
            let param_param = "param_example".to_string();
            let param_param2 = "param2_example".to_string();
            let result = api_impl.test_json_form_data(param_param, param_param2, &context).await;
            let mut response = Response::builder();
            response.headers_mut().unwrap().typed_insert(XSpanId( (&context as &dyn Has<XSpanId>).get().0.to_string() ));
            let result = match result {
                Err(e) => {
                    // Application code returned an error. This should not happen, as the implementation should
                    // return a valid response.
                    response = response.status(StatusCode::INTERNAL_SERVER_ERROR);
                    return Ok(response.body(hyper::Body::from("An internal error occurred")).unwrap());
                }
                Ok(r) => r,
            };
        
            match result {
                TestJsonFormDataResponse::SuccessfulOperation
            
                => {
                    response = response.status(hyper::http::StatusCode::from_u16(200).unwrap());
            
                    Ok(response.body(Body::empty()).unwrap())
                },
            }
        },
    
        // TestClassname - PATCH /fake_classname_test
        &hyper::Method::PATCH if path.matched(paths::ID_FAKE_CLASSNAME_TEST) => {
            let authorization = match (&context as &dyn Has<Option<Authorization>>).get() {
                &Some(ref authorization) => authorization,
                &None => return Ok(Response::builder().status(StatusCode::FORBIDDEN).body(hyper::Body::from("Unauthenticated")).unwrap()),
            };

            // Body parameters (note that non-required body parameters will ignore garbage
            // values, rather than causing a 400 response). Produce warning header and logs for
            // any unused fields

            let body = hyper::body::aggregate(body).await.map_err(|e| ApiError(format!("Error getting response: {}", e)));
            let body = match body {
                Err(e) => {
                    return Ok(
                        Response::builder()
                            .status(StatusCode::BAD_REQUEST)
                            .body(hyper::Body::from(format!("Couldn't receive body parameter body: {}", e)))
                            .unwrap()
                    );
                }
                Ok(v) => v,
            };
            let mut unused_elements = Vec::new();
            let param_body: Option<crate::models::Client> = if body.remaining() > 0 {
                let deserializer = &mut serde_json::Deserializer::from_reader(body.reader());
                match serde_ignored::deserialize(deserializer, |path| {
                    warn!("Ignoring unknown field in body: {}", path);
                    unused_elements.push(path.to_string());
                }) {
                    Ok(param_body) => param_body,
                    Err(e) => return Ok(Response::builder().status(StatusCode::BAD_REQUEST).body(hyper::Body::from(format!("Couldn't parse body parameter body - doesn't match schema: {}", e))).unwrap()),
                }
            } else {
                None
            };
            let param_body = match param_body {
                Some(param_body) => param_body,
                None => return Ok(Response::builder().status(StatusCode::BAD_REQUEST).body(hyper::Body::from("Missing required body parameter body")).unwrap()),
            };
            let result = api_impl.test_classname(param_body, &context).await;
            let mut response = Response::builder();
            response.headers_mut().unwrap().typed_insert(XSpanId( (&context as &dyn Has<XSpanId>).get().0.to_string() ));
            let result = match result {
                Err(e) => {
                    // Application code returned an error. This should not happen, as the implementation should
                    // return a valid response.
                    response = response.status(StatusCode::INTERNAL_SERVER_ERROR);
                    return Ok(response.body(hyper::Body::from("An internal error occurred")).unwrap());
                }
                Ok(r) => r,
            };
        
            if !unused_elements.is_empty() {
                response.headers_mut().unwrap().typed_insert(Warning(format!("Ignoring unknown fields in body: {:?}", unused_elements)));
            }
        
            match result {
                TestClassnameResponse::SuccessfulOperation(body)
            
                => {
                    response = response.status(hyper::http::StatusCode::from_u16(200).unwrap());
            
                    let header = headers::ContentType::from(mimetypes::responses::TEST_CLASSNAME_SUCCESSFUL_OPERATION.clone());
                    response.headers_mut().unwrap().typed_insert(header);
            
                
                    let body = serde_json::to_string(&body).expect("impossible to fail to serialize");
                
                    Ok(response.body(Body::from(body)).unwrap())
                },
            }
        },
    
        // AddPet - POST /pet
        &hyper::Method::POST if path.matched(paths::ID_PET) => {
            let authorization = match (&context as &dyn Has<Option<Authorization>>).get() {
                &Some(ref authorization) => authorization,
                &None => return Ok(Response::builder().status(StatusCode::FORBIDDEN).body(hyper::Body::from("Unauthenticated")).unwrap()),
            };

            // Authorization
            if let Scopes::Some(ref scopes) = authorization.scopes {
                let required_scopes: BTreeSet<String> = vec![
                "write:pets".to_string(), // modify pets in your account
                "read:pets".to_string(), // read your pets
                ].into_iter().collect();

                if !required_scopes.is_subset(scopes) {
                    let missing_scopes = required_scopes.difference(scopes);
                    return Ok(Response::builder()
                        .status(StatusCode::FORBIDDEN)
                        .body(hyper::Body::from(missing_scopes.fold(
                            "Insufficient authorization, missing scopes".to_string(),
                            |s, scope| format!("{} {}", s, scope)
                        ))).unwrap())
                    ;
                }
            }
            // Body parameters (note that non-required body parameters will ignore garbage
            // values, rather than causing a 400 response). Produce warning header and logs for
            // any unused fields

            let body = hyper::body::aggregate(body).await.map_err(|e| ApiError(format!("Error getting response: {}", e)));
            let body = match body {
                Err(e) => {
                    return Ok(
                        Response::builder()
                            .status(StatusCode::BAD_REQUEST)
                            .body(hyper::Body::from(format!("Couldn't receive body parameter body: {}", e)))
                            .unwrap()
                    );
                }
                Ok(v) => v,
            };
            let mut unused_elements = Vec::new();
            let param_body: Option<crate::models::Pet> = if body.remaining() > 0 {
                let deserializer = &mut serde_xml_rs::de::Deserializer::new_from_reader(body.reader());
                match serde_ignored::deserialize(deserializer, |path| {
                    warn!("Ignoring unknown field in body: {}", path);
                    unused_elements.push(path.to_string());
                }) {
                    Ok(param_body) => param_body,
                    Err(e) => return Ok(Response::builder().status(StatusCode::BAD_REQUEST).body(hyper::Body::from(format!("Couldn't parse body parameter body - doesn't match schema: {}", e))).unwrap()),
                }
            } else {
                None
            };
            let param_body = match param_body {
                Some(param_body) => param_body,
                None => return Ok(Response::builder().status(StatusCode::BAD_REQUEST).body(hyper::Body::from("Missing required body parameter body")).unwrap()),
            };
            let result = api_impl.add_pet(param_body, &context).await;
            let mut response = Response::builder();
            response.headers_mut().unwrap().typed_insert(XSpanId( (&context as &dyn Has<XSpanId>).get().0.to_string() ));
            let result = match result {
                Err(e) => {
                    // Application code returned an error. This should not happen, as the implementation should
                    // return a valid response.
                    response = response.status(StatusCode::INTERNAL_SERVER_ERROR);
                    return Ok(response.body(hyper::Body::from("An internal error occurred")).unwrap());
                }
                Ok(r) => r,
            };
        
            if !unused_elements.is_empty() {
                response.headers_mut().unwrap().typed_insert(Warning(format!("Ignoring unknown fields in body: {:?}", unused_elements)));
            }
        
            match result {
                AddPetResponse::InvalidInput
            
                => {
                    response = response.status(hyper::http::StatusCode::from_u16(405).unwrap());
            
                    Ok(response.body(Body::empty()).unwrap())
                },
            }
        },
    
        // DeletePet - DELETE /pet/{petId}
        &hyper::Method::DELETE if path.matched(paths::ID_PET_PETID) => {
            let authorization = match (&context as &dyn Has<Option<Authorization>>).get() {
                &Some(ref authorization) => authorization,
                &None => return Ok(Response::builder().status(StatusCode::FORBIDDEN).body(hyper::Body::from("Unauthenticated")).unwrap()),
            };

            // Authorization
            if let Scopes::Some(ref scopes) = authorization.scopes {
                let required_scopes: BTreeSet<String> = vec![
                "write:pets".to_string(), // modify pets in your account
                "read:pets".to_string(), // read your pets
                ].into_iter().collect();

                if !required_scopes.is_subset(scopes) {
                    let missing_scopes = required_scopes.difference(scopes);
                    return Ok(Response::builder()
                        .status(StatusCode::FORBIDDEN)
                        .body(hyper::Body::from(missing_scopes.fold(
                            "Insufficient authorization, missing scopes".to_string(),
                            |s, scope| format!("{} {}", s, scope)
                        ))).unwrap())
                    ;
                }
            }
            // Path parameters
            let path = uri.path().to_string();
            let path_params = paths::REGEX_PET_PETID
                .captures(&path)
                .unwrap_or_else(||
                    panic!("Path {} matched RE PET_PETID in set but failed match against \"{}\"", path, paths::REGEX_PET_PETID.as_str())
                );
            let param_pet_id = match percent_encoding::percent_decode(path_params["pet_id"].as_bytes()).decode_utf8() {
                Ok(param_pet_id) => match param_pet_id.parse::<i64>() {
                    Ok(param_pet_id) => param_pet_id,
                    Err(e) => return Ok(Response::builder().status(StatusCode::BAD_REQUEST).body(hyper::Body::from(format!("Couldn't parse path parameter petId: {:?}", e))).unwrap()),
                },
                Err(_) => return Ok(Response::builder().status(StatusCode::BAD_REQUEST).body(hyper::Body::from(format!("Couldn't percent-decode path parameter as UTF-8: {}", &path_params["petId"]))).unwrap())
            };
            // Header parameters
            let param_api_key = headers.typed_get::<RequestApiKey>().map(|header| header.0.clone());
            let result = api_impl.delete_pet(param_pet_id, param_api_key, &context).await;
            let mut response = Response::builder();
            response.headers_mut().unwrap().typed_insert(XSpanId( (&context as &dyn Has<XSpanId>).get().0.to_string() ));
            let result = match result {
                Err(e) => {
                    // Application code returned an error. This should not happen, as the implementation should
                    // return a valid response.
                    response = response.status(StatusCode::INTERNAL_SERVER_ERROR);
                    return Ok(response.body(hyper::Body::from("An internal error occurred")).unwrap());
                }
                Ok(r) => r,
            };
        
            match result {
                DeletePetResponse::InvalidPetValue
            
                => {
                    response = response.status(hyper::http::StatusCode::from_u16(400).unwrap());
            
                    Ok(response.body(Body::empty()).unwrap())
                },
            }
        },
    
        // FindPetsByStatus - GET /pet/findByStatus
        &hyper::Method::GET if path.matched(paths::ID_PET_FINDBYSTATUS) => {
            let authorization = match (&context as &dyn Has<Option<Authorization>>).get() {
                &Some(ref authorization) => authorization,
                &None => return Ok(Response::builder().status(StatusCode::FORBIDDEN).body(hyper::Body::from("Unauthenticated")).unwrap()),
            };

            // Authorization
            if let Scopes::Some(ref scopes) = authorization.scopes {
                let required_scopes: BTreeSet<String> = vec![
                "write:pets".to_string(), // modify pets in your account
                "read:pets".to_string(), // read your pets
                ].into_iter().collect();

                if !required_scopes.is_subset(scopes) {
                    let missing_scopes = required_scopes.difference(scopes);
                    return Ok(Response::builder()
                        .status(StatusCode::FORBIDDEN)
                        .body(hyper::Body::from(missing_scopes.fold(
                            "Insufficient authorization, missing scopes".to_string(),
                            |s, scope| format!("{} {}", s, scope)
                        ))).unwrap())
                    ;
                }
            }
            // Query parameters (note that non-required or collection query parameters will ignore garbage values, rather than causing a 400 response)
            let query_params = form_urlencoded::parse(uri.query().unwrap_or_default().as_bytes()).collect::<Vec<_>>();
            let param_status = query_params.iter().filter(|e| e.0 == "status").map(|e| e.1.to_owned())
                .filter_map(|param_status| param_status.parse::<String>().ok())
                .collect::<Vec<_>>();
            let result = api_impl.find_pets_by_status(param_status.as_ref(), &context).await;
            let mut response = Response::builder();
            response.headers_mut().unwrap().typed_insert(XSpanId( (&context as &dyn Has<XSpanId>).get().0.to_string() ));
            let result = match result {
                Err(e) => {
                    // Application code returned an error. This should not happen, as the implementation should
                    // return a valid response.
                    response = response.status(StatusCode::INTERNAL_SERVER_ERROR);
                    return Ok(response.body(hyper::Body::from("An internal error occurred")).unwrap());
                }
                Ok(r) => r,
            };
        
            match result {
                FindPetsByStatusResponse::SuccessfulOperation(body)
            
                => {
                    response = response.status(hyper::http::StatusCode::from_u16(200).unwrap());
            
                    let header = headers::ContentType::from(mimetypes::responses::FIND_PETS_BY_STATUS_SUCCESSFUL_OPERATION.clone());
                    response.headers_mut().unwrap().typed_insert(header);
            
                
                    let body = serde_xml_rs::to_string(&body).expect("impossible to fail to serialize");
                
                    Ok(response.body(Body::from(body)).unwrap())
                },
                FindPetsByStatusResponse::InvalidStatusValue
            
                => {
                    response = response.status(hyper::http::StatusCode::from_u16(400).unwrap());
            
                    Ok(response.body(Body::empty()).unwrap())
                },
            }
        },
    
        // FindPetsByTags - GET /pet/findByTags
        &hyper::Method::GET if path.matched(paths::ID_PET_FINDBYTAGS) => {
            let authorization = match (&context as &dyn Has<Option<Authorization>>).get() {
                &Some(ref authorization) => authorization,
                &None => return Ok(Response::builder().status(StatusCode::FORBIDDEN).body(hyper::Body::from("Unauthenticated")).unwrap()),
            };

            // Authorization
            if let Scopes::Some(ref scopes) = authorization.scopes {
                let required_scopes: BTreeSet<String> = vec![
                "write:pets".to_string(), // modify pets in your account
                "read:pets".to_string(), // read your pets
                ].into_iter().collect();

                if !required_scopes.is_subset(scopes) {
                    let missing_scopes = required_scopes.difference(scopes);
                    return Ok(Response::builder()
                        .status(StatusCode::FORBIDDEN)
                        .body(hyper::Body::from(missing_scopes.fold(
                            "Insufficient authorization, missing scopes".to_string(),
                            |s, scope| format!("{} {}", s, scope)
                        ))).unwrap())
                    ;
                }
            }
            // Query parameters (note that non-required or collection query parameters will ignore garbage values, rather than causing a 400 response)
            let query_params = form_urlencoded::parse(uri.query().unwrap_or_default().as_bytes()).collect::<Vec<_>>();
            let param_tags = query_params.iter().filter(|e| e.0 == "tags").map(|e| e.1.to_owned())
                .filter_map(|param_tags| param_tags.parse::<String>().ok())
                .collect::<Vec<_>>();
            let result = api_impl.find_pets_by_tags(param_tags.as_ref(), &context).await;
            let mut response = Response::builder();
            response.headers_mut().unwrap().typed_insert(XSpanId( (&context as &dyn Has<XSpanId>).get().0.to_string() ));
            let result = match result {
                Err(e) => {
                    // Application code returned an error. This should not happen, as the implementation should
                    // return a valid response.
                    response = response.status(StatusCode::INTERNAL_SERVER_ERROR);
                    return Ok(response.body(hyper::Body::from("An internal error occurred")).unwrap());
                }
                Ok(r) => r,
            };
        
            match result {
                FindPetsByTagsResponse::SuccessfulOperation(body)
            
                => {
                    response = response.status(hyper::http::StatusCode::from_u16(200).unwrap());
            
                    let header = headers::ContentType::from(mimetypes::responses::FIND_PETS_BY_TAGS_SUCCESSFUL_OPERATION.clone());
                    response.headers_mut().unwrap().typed_insert(header);
            
                
                    let body = serde_xml_rs::to_string(&body).expect("impossible to fail to serialize");
                
                    Ok(response.body(Body::from(body)).unwrap())
                },
                FindPetsByTagsResponse::InvalidTagValue
            
                => {
                    response = response.status(hyper::http::StatusCode::from_u16(400).unwrap());
            
                    Ok(response.body(Body::empty()).unwrap())
                },
            }
        },
    
        // GetPetById - GET /pet/{petId}
        &hyper::Method::GET if path.matched(paths::ID_PET_PETID) => {
            let authorization = match (&context as &dyn Has<Option<Authorization>>).get() {
                &Some(ref authorization) => authorization,
                &None => return Ok(Response::builder().status(StatusCode::FORBIDDEN).body(hyper::Body::from("Unauthenticated")).unwrap()),
            };

            // Path parameters
            let path = uri.path().to_string();
            let path_params = paths::REGEX_PET_PETID
                .captures(&path)
                .unwrap_or_else(||
                    panic!("Path {} matched RE PET_PETID in set but failed match against \"{}\"", path, paths::REGEX_PET_PETID.as_str())
                );
            let param_pet_id = match percent_encoding::percent_decode(path_params["pet_id"].as_bytes()).decode_utf8() {
                Ok(param_pet_id) => match param_pet_id.parse::<i64>() {
                    Ok(param_pet_id) => param_pet_id,
                    Err(e) => return Ok(Response::builder().status(StatusCode::BAD_REQUEST).body(hyper::Body::from(format!("Couldn't parse path parameter petId: {:?}", e))).unwrap()),
                },
                Err(_) => return Ok(Response::builder().status(StatusCode::BAD_REQUEST).body(hyper::Body::from(format!("Couldn't percent-decode path parameter as UTF-8: {}", &path_params["petId"]))).unwrap())
            };
            let result = api_impl.get_pet_by_id(param_pet_id, &context).await;
            let mut response = Response::builder();
            response.headers_mut().unwrap().typed_insert(XSpanId( (&context as &dyn Has<XSpanId>).get().0.to_string() ));
            let result = match result {
                Err(e) => {
                    // Application code returned an error. This should not happen, as the implementation should
                    // return a valid response.
                    response = response.status(StatusCode::INTERNAL_SERVER_ERROR);
                    return Ok(response.body(hyper::Body::from("An internal error occurred")).unwrap());
                }
                Ok(r) => r,
            };
        
            match result {
                GetPetByIdResponse::SuccessfulOperation(body)
            
                => {
                    response = response.status(hyper::http::StatusCode::from_u16(200).unwrap());
            
                    let header = headers::ContentType::from(mimetypes::responses::GET_PET_BY_ID_SUCCESSFUL_OPERATION.clone());
                    response.headers_mut().unwrap().typed_insert(header);
            
                
                    let body = serde_xml_rs::to_string(&body).expect("impossible to fail to serialize");
                
                    Ok(response.body(Body::from(body)).unwrap())
                },
                GetPetByIdResponse::InvalidIDSupplied
            
                => {
                    response = response.status(hyper::http::StatusCode::from_u16(400).unwrap());
            
                    Ok(response.body(Body::empty()).unwrap())
                },
                GetPetByIdResponse::PetNotFound
            
                => {
                    response = response.status(hyper::http::StatusCode::from_u16(404).unwrap());
            
                    Ok(response.body(Body::empty()).unwrap())
                },
            }
        },
    
        // UpdatePet - PUT /pet
        &hyper::Method::PUT if path.matched(paths::ID_PET) => {
            let authorization = match (&context as &dyn Has<Option<Authorization>>).get() {
                &Some(ref authorization) => authorization,
                &None => return Ok(Response::builder().status(StatusCode::FORBIDDEN).body(hyper::Body::from("Unauthenticated")).unwrap()),
            };

            // Authorization
            if let Scopes::Some(ref scopes) = authorization.scopes {
                let required_scopes: BTreeSet<String> = vec![
                "write:pets".to_string(), // modify pets in your account
                "read:pets".to_string(), // read your pets
                ].into_iter().collect();

                if !required_scopes.is_subset(scopes) {
                    let missing_scopes = required_scopes.difference(scopes);
                    return Ok(Response::builder()
                        .status(StatusCode::FORBIDDEN)
                        .body(hyper::Body::from(missing_scopes.fold(
                            "Insufficient authorization, missing scopes".to_string(),
                            |s, scope| format!("{} {}", s, scope)
                        ))).unwrap())
                    ;
                }
            }
            // Body parameters (note that non-required body parameters will ignore garbage
            // values, rather than causing a 400 response). Produce warning header and logs for
            // any unused fields

            let body = hyper::body::aggregate(body).await.map_err(|e| ApiError(format!("Error getting response: {}", e)));
            let body = match body {
                Err(e) => {
                    return Ok(
                        Response::builder()
                            .status(StatusCode::BAD_REQUEST)
                            .body(hyper::Body::from(format!("Couldn't receive body parameter body: {}", e)))
                            .unwrap()
                    );
                }
                Ok(v) => v,
            };
            let mut unused_elements = Vec::new();
            let param_body: Option<crate::models::Pet> = if body.remaining() > 0 {
                let deserializer = &mut serde_xml_rs::de::Deserializer::new_from_reader(body.reader());
                match serde_ignored::deserialize(deserializer, |path| {
                    warn!("Ignoring unknown field in body: {}", path);
                    unused_elements.push(path.to_string());
                }) {
                    Ok(param_body) => param_body,
                    Err(e) => return Ok(Response::builder().status(StatusCode::BAD_REQUEST).body(hyper::Body::from(format!("Couldn't parse body parameter body - doesn't match schema: {}", e))).unwrap()),
                }
            } else {
                None
            };
            let param_body = match param_body {
                Some(param_body) => param_body,
                None => return Ok(Response::builder().status(StatusCode::BAD_REQUEST).body(hyper::Body::from("Missing required body parameter body")).unwrap()),
            };
            let result = api_impl.update_pet(param_body, &context).await;
            let mut response = Response::builder();
            response.headers_mut().unwrap().typed_insert(XSpanId( (&context as &dyn Has<XSpanId>).get().0.to_string() ));
            let result = match result {
                Err(e) => {
                    // Application code returned an error. This should not happen, as the implementation should
                    // return a valid response.
                    response = response.status(StatusCode::INTERNAL_SERVER_ERROR);
                    return Ok(response.body(hyper::Body::from("An internal error occurred")).unwrap());
                }
                Ok(r) => r,
            };
        
            if !unused_elements.is_empty() {
                response.headers_mut().unwrap().typed_insert(Warning(format!("Ignoring unknown fields in body: {:?}", unused_elements)));
            }
        
            match result {
                UpdatePetResponse::InvalidIDSupplied
            
                => {
                    response = response.status(hyper::http::StatusCode::from_u16(400).unwrap());
            
                    Ok(response.body(Body::empty()).unwrap())
                },
                UpdatePetResponse::PetNotFound
            
                => {
                    response = response.status(hyper::http::StatusCode::from_u16(404).unwrap());
            
                    Ok(response.body(Body::empty()).unwrap())
                },
                UpdatePetResponse::ValidationException
            
                => {
                    response = response.status(hyper::http::StatusCode::from_u16(405).unwrap());
            
                    Ok(response.body(Body::empty()).unwrap())
                },
            }
        },
    
        // UpdatePetWithForm - POST /pet/{petId}
        &hyper::Method::POST if path.matched(paths::ID_PET_PETID) => {
            let authorization = match (&context as &dyn Has<Option<Authorization>>).get() {
                &Some(ref authorization) => authorization,
                &None => return Ok(Response::builder().status(StatusCode::FORBIDDEN).body(hyper::Body::from("Unauthenticated")).unwrap()),
            };

            // Authorization
            if let Scopes::Some(ref scopes) = authorization.scopes {
                let required_scopes: BTreeSet<String> = vec![
                "write:pets".to_string(), // modify pets in your account
                "read:pets".to_string(), // read your pets
                ].into_iter().collect();

                if !required_scopes.is_subset(scopes) {
                    let missing_scopes = required_scopes.difference(scopes);
                    return Ok(Response::builder()
                        .status(StatusCode::FORBIDDEN)
                        .body(hyper::Body::from(missing_scopes.fold(
                            "Insufficient authorization, missing scopes".to_string(),
                            |s, scope| format!("{} {}", s, scope)
                        ))).unwrap())
                    ;
                }
            }
            // Path parameters
            let path = uri.path().to_string();
            let path_params = paths::REGEX_PET_PETID
                .captures(&path)
                .unwrap_or_else(||
                    panic!("Path {} matched RE PET_PETID in set but failed match against \"{}\"", path, paths::REGEX_PET_PETID.as_str())
                );
            let param_pet_id = match percent_encoding::percent_decode(path_params["pet_id"].as_bytes()).decode_utf8() {
                Ok(param_pet_id) => match param_pet_id.parse::<i64>() {
                    Ok(param_pet_id) => param_pet_id,
                    Err(e) => return Ok(Response::builder().status(StatusCode::BAD_REQUEST).body(hyper::Body::from(format!("Couldn't parse path parameter petId: {:?}", e))).unwrap()),
                },
                Err(_) => return Ok(Response::builder().status(StatusCode::BAD_REQUEST).body(hyper::Body::from(format!("Couldn't percent-decode path parameter as UTF-8: {}", &path_params["petId"]))).unwrap())
            };
            // Form parameters
            let param_name = Some("name_example".to_string());
            let param_status = Some("status_example".to_string());
            let result = api_impl.update_pet_with_form(param_pet_id, param_name, param_status, &context).await;
            let mut response = Response::builder();
            response.headers_mut().unwrap().typed_insert(XSpanId( (&context as &dyn Has<XSpanId>).get().0.to_string() ));
            let result = match result {
                Err(e) => {
                    // Application code returned an error. This should not happen, as the implementation should
                    // return a valid response.
                    response = response.status(StatusCode::INTERNAL_SERVER_ERROR);
                    return Ok(response.body(hyper::Body::from("An internal error occurred")).unwrap());
                }
                Ok(r) => r,
            };
        
            match result {
                UpdatePetWithFormResponse::InvalidInput
            
                => {
                    response = response.status(hyper::http::StatusCode::from_u16(405).unwrap());
            
                    Ok(response.body(Body::empty()).unwrap())
                },
            }
        },
    
        // UploadFile - POST /pet/{petId}/uploadImage
        &hyper::Method::POST if path.matched(paths::ID_PET_PETID_UPLOADIMAGE) => {
            let authorization = match (&context as &dyn Has<Option<Authorization>>).get() {
                &Some(ref authorization) => authorization,
                &None => return Ok(Response::builder().status(StatusCode::FORBIDDEN).body(hyper::Body::from("Unauthenticated")).unwrap()),
            };

            // Authorization
            if let Scopes::Some(ref scopes) = authorization.scopes {
                let required_scopes: BTreeSet<String> = vec![
                "write:pets".to_string(), // modify pets in your account
                "read:pets".to_string(), // read your pets
                ].into_iter().collect();

                if !required_scopes.is_subset(scopes) {
                    let missing_scopes = required_scopes.difference(scopes);
                    return Ok(Response::builder()
                        .status(StatusCode::FORBIDDEN)
                        .body(hyper::Body::from(missing_scopes.fold(
                            "Insufficient authorization, missing scopes".to_string(),
                            |s, scope| format!("{} {}", s, scope)
                        ))).unwrap())
                    ;
                }
            }
            let boundary = match multipart_boundary(&headers) {
                Some(boundary) => boundary.to_string(),
                None => return Ok(Response::builder().status(StatusCode::BAD_REQUEST).body(hyper::Body::from("Couldn't find valid multipart body")).unwrap()),
            };
            // Path parameters
            let path = uri.path().to_string();
            let path_params = paths::REGEX_PET_PETID_UPLOADIMAGE
                .captures(&path)
                .unwrap_or_else(||
                    panic!("Path {} matched RE PET_PETID_UPLOADIMAGE in set but failed match against \"{}\"", path, paths::REGEX_PET_PETID_UPLOADIMAGE.as_str())
                );
            let param_pet_id = match percent_encoding::percent_decode(path_params["pet_id"].as_bytes()).decode_utf8() {
                Ok(param_pet_id) => match param_pet_id.parse::<i64>() {
                    Ok(param_pet_id) => param_pet_id,
                    Err(e) => return Ok(Response::builder().status(StatusCode::BAD_REQUEST).body(hyper::Body::from(format!("Couldn't parse path parameter petId: {:?}", e))).unwrap()),
                },
                Err(_) => return Ok(Response::builder().status(StatusCode::BAD_REQUEST).body(hyper::Body::from(format!("Couldn't percent-decode path parameter as UTF-8: {}", &path_params["petId"]))).unwrap())
            };
            // Form Body parameters (note that non-required body parameters will ignore garbage
            // values, rather than causing a 400 response). Produce warning header and logs for
            // any unused fields.
            let body = hyper::body::aggregate(body).await.map_err(|e| ApiError(format!("Error getting response: {}", e)));
            let body = match body {
                Err(e) => {
                    return Ok(
                        Response::builder()
                            .status(StatusCode::BAD_REQUEST)
                            .body(hyper::Body::from(format!("Couldn't receive body parameter body: {}", e)))
                            .unwrap()
                    );
                }
                Ok(v) => v,
            };
            // Read Form Parameters from body
            let mut entries = match Multipart::with_body(body.reader(), boundary).save().temp() {
                SaveResult::Full(entries) => {
                    entries
                },
                _ => {
                    return Ok(Response::builder().status(StatusCode::BAD_REQUEST).body(hyper::Body::from(format!("Unable to process all message parts"))).unwrap())
                },
            };
                        
                            
            let file_additional_metadata = entries.fields.remove("additional_metadata");

            let param_additional_metadata = match file_additional_metadata {
                Some(file) => {
                    let mut reader = file[0].data.readable().unwrap();
                    let mut additional_metadata_bytes = vec![];
                    reader.read_to_end(&mut additional_metadata_bytes).unwrap();
                    let additional_metadata_model: String = serde_json::from_slice(&additional_metadata_bytes.as_slice()).expect("Impossible to fail to serialize");
                    Some(additional_metadata_model)
                }
                None => None,
            };
                            

                            
            let file_file = entries.fields.remove("file");

            let param_file = match file_file {
                Some(file) => {
                    let mut reader = file[0].data.readable().unwrap();
                    let mut file_bytes = vec![];
                    reader.read_to_end(&mut file_bytes).unwrap();
                    let file_model: openapi_context::ByteArray = serde_json::from_slice(&file_bytes.as_slice()).expect("Impossible to fail to serialize");
                    Some(file_model)
                }
                None => None,
            };
                            
            let result = api_impl.upload_file(param_pet_id, param_additional_metadata, param_file, &context).await;
            let mut response = Response::builder();
            response.headers_mut().unwrap().typed_insert(XSpanId( (&context as &dyn Has<XSpanId>).get().0.to_string() ));
            let result = match result {
                Err(e) => {
                    // Application code returned an error. This should not happen, as the implementation should
                    // return a valid response.
                    response = response.status(StatusCode::INTERNAL_SERVER_ERROR);
                    return Ok(response.body(hyper::Body::from("An internal error occurred")).unwrap());
                }
                Ok(r) => r,
            };
        
            match result {
                UploadFileResponse::SuccessfulOperation(body)
            
                => {
                    response = response.status(hyper::http::StatusCode::from_u16(200).unwrap());
            
                    let header = headers::ContentType::from(mimetypes::responses::UPLOAD_FILE_SUCCESSFUL_OPERATION.clone());
                    response.headers_mut().unwrap().typed_insert(header);
            
                
                    let body = serde_json::to_string(&body).expect("impossible to fail to serialize");
                
                    Ok(response.body(Body::from(body)).unwrap())
                },
            }
        },
    
        // DeleteOrder - DELETE /store/order/{order_id}
        &hyper::Method::DELETE if path.matched(paths::ID_STORE_ORDER_ORDER_ID) => {
            // Path parameters
            let path = uri.path().to_string();
            let path_params = paths::REGEX_STORE_ORDER_ORDER_ID
                .captures(&path)
                .unwrap_or_else(||
                    panic!("Path {} matched RE STORE_ORDER_ORDER_ID in set but failed match against \"{}\"", path, paths::REGEX_STORE_ORDER_ORDER_ID.as_str())
                );
            let param_order_id = match percent_encoding::percent_decode(path_params["order_id"].as_bytes()).decode_utf8() {
                Ok(param_order_id) => match param_order_id.parse::<String>() {
                    Ok(param_order_id) => param_order_id,
                    Err(e) => return Ok(Response::builder().status(StatusCode::BAD_REQUEST).body(hyper::Body::from(format!("Couldn't parse path parameter order_id: {:?}", e))).unwrap()),
                },
                Err(_) => return Ok(Response::builder().status(StatusCode::BAD_REQUEST).body(hyper::Body::from(format!("Couldn't percent-decode path parameter as UTF-8: {}", &path_params["order_id"]))).unwrap())
            };
            let result = api_impl.delete_order(param_order_id, &context).await;
            let mut response = Response::builder();
            response.headers_mut().unwrap().typed_insert(XSpanId( (&context as &dyn Has<XSpanId>).get().0.to_string() ));
            let result = match result {
                Err(e) => {
                    // Application code returned an error. This should not happen, as the implementation should
                    // return a valid response.
                    response = response.status(StatusCode::INTERNAL_SERVER_ERROR);
                    return Ok(response.body(hyper::Body::from("An internal error occurred")).unwrap());
                }
                Ok(r) => r,
            };
        
            match result {
                DeleteOrderResponse::InvalidIDSupplied
            
                => {
                    response = response.status(hyper::http::StatusCode::from_u16(400).unwrap());
            
                    Ok(response.body(Body::empty()).unwrap())
                },
                DeleteOrderResponse::OrderNotFound
            
                => {
                    response = response.status(hyper::http::StatusCode::from_u16(404).unwrap());
            
                    Ok(response.body(Body::empty()).unwrap())
                },
            }
        },
    
        // GetInventory - GET /store/inventory
        &hyper::Method::GET if path.matched(paths::ID_STORE_INVENTORY) => {
            let authorization = match (&context as &dyn Has<Option<Authorization>>).get() {
                &Some(ref authorization) => authorization,
                &None => return Ok(Response::builder().status(StatusCode::FORBIDDEN).body(hyper::Body::from("Unauthenticated")).unwrap()),
            };

            let result = api_impl.get_inventory(&context).await;
            let mut response = Response::builder();
            response.headers_mut().unwrap().typed_insert(XSpanId( (&context as &dyn Has<XSpanId>).get().0.to_string() ));
            let result = match result {
                Err(e) => {
                    // Application code returned an error. This should not happen, as the implementation should
                    // return a valid response.
                    response = response.status(StatusCode::INTERNAL_SERVER_ERROR);
                    return Ok(response.body(hyper::Body::from("An internal error occurred")).unwrap());
                }
                Ok(r) => r,
            };
        
            match result {
                GetInventoryResponse::SuccessfulOperation(body)
            
                => {
                    response = response.status(hyper::http::StatusCode::from_u16(200).unwrap());
            
                    let header = headers::ContentType::from(mimetypes::responses::GET_INVENTORY_SUCCESSFUL_OPERATION.clone());
                    response.headers_mut().unwrap().typed_insert(header);
            
                
                    let body = serde_json::to_string(&body).expect("impossible to fail to serialize");
                
                    Ok(response.body(Body::from(body)).unwrap())
                },
            }
        },
    
        // GetOrderById - GET /store/order/{order_id}
        &hyper::Method::GET if path.matched(paths::ID_STORE_ORDER_ORDER_ID) => {
            // Path parameters
            let path = uri.path().to_string();
            let path_params = paths::REGEX_STORE_ORDER_ORDER_ID
                .captures(&path)
                .unwrap_or_else(||
                    panic!("Path {} matched RE STORE_ORDER_ORDER_ID in set but failed match against \"{}\"", path, paths::REGEX_STORE_ORDER_ORDER_ID.as_str())
                );
            let param_order_id = match percent_encoding::percent_decode(path_params["order_id"].as_bytes()).decode_utf8() {
                Ok(param_order_id) => match param_order_id.parse::<i64>() {
                    Ok(param_order_id) => param_order_id,
                    Err(e) => return Ok(Response::builder().status(StatusCode::BAD_REQUEST).body(hyper::Body::from(format!("Couldn't parse path parameter order_id: {:?}", e))).unwrap()),
                },
                Err(_) => return Ok(Response::builder().status(StatusCode::BAD_REQUEST).body(hyper::Body::from(format!("Couldn't percent-decode path parameter as UTF-8: {}", &path_params["order_id"]))).unwrap())
            };
            let result = api_impl.get_order_by_id(param_order_id, &context).await;
            let mut response = Response::builder();
            response.headers_mut().unwrap().typed_insert(XSpanId( (&context as &dyn Has<XSpanId>).get().0.to_string() ));
            let result = match result {
                Err(e) => {
                    // Application code returned an error. This should not happen, as the implementation should
                    // return a valid response.
                    response = response.status(StatusCode::INTERNAL_SERVER_ERROR);
                    return Ok(response.body(hyper::Body::from("An internal error occurred")).unwrap());
                }
                Ok(r) => r,
            };
        
            match result {
                GetOrderByIdResponse::SuccessfulOperation(body)
            
                => {
                    response = response.status(hyper::http::StatusCode::from_u16(200).unwrap());
            
                    let header = headers::ContentType::from(mimetypes::responses::GET_ORDER_BY_ID_SUCCESSFUL_OPERATION.clone());
                    response.headers_mut().unwrap().typed_insert(header);
            
                
                    let body = serde_xml_rs::to_string(&body).expect("impossible to fail to serialize");
                
                    Ok(response.body(Body::from(body)).unwrap())
                },
                GetOrderByIdResponse::InvalidIDSupplied
            
                => {
                    response = response.status(hyper::http::StatusCode::from_u16(400).unwrap());
            
                    Ok(response.body(Body::empty()).unwrap())
                },
                GetOrderByIdResponse::OrderNotFound
            
                => {
                    response = response.status(hyper::http::StatusCode::from_u16(404).unwrap());
            
                    Ok(response.body(Body::empty()).unwrap())
                },
            }
        },
    
        // PlaceOrder - POST /store/order
        &hyper::Method::POST if path.matched(paths::ID_STORE_ORDER) => {
            // Body parameters (note that non-required body parameters will ignore garbage
            // values, rather than causing a 400 response). Produce warning header and logs for
            // any unused fields

            let body = hyper::body::aggregate(body).await.map_err(|e| ApiError(format!("Error getting response: {}", e)));
            let body = match body {
                Err(e) => {
                    return Ok(
                        Response::builder()
                            .status(StatusCode::BAD_REQUEST)
                            .body(hyper::Body::from(format!("Couldn't receive body parameter body: {}", e)))
                            .unwrap()
                    );
                }
                Ok(v) => v,
            };
            let mut unused_elements = Vec::new();
            let param_body: Option<crate::models::Order> = if body.remaining() > 0 {
                let deserializer = &mut serde_json::Deserializer::from_reader(body.reader());
                match serde_ignored::deserialize(deserializer, |path| {
                    warn!("Ignoring unknown field in body: {}", path);
                    unused_elements.push(path.to_string());
                }) {
                    Ok(param_body) => param_body,
                    Err(e) => return Ok(Response::builder().status(StatusCode::BAD_REQUEST).body(hyper::Body::from(format!("Couldn't parse body parameter body - doesn't match schema: {}", e))).unwrap()),
                }
            } else {
                None
            };
            let param_body = match param_body {
                Some(param_body) => param_body,
                None => return Ok(Response::builder().status(StatusCode::BAD_REQUEST).body(hyper::Body::from("Missing required body parameter body")).unwrap()),
            };
            let result = api_impl.place_order(param_body, &context).await;
            let mut response = Response::builder();
            response.headers_mut().unwrap().typed_insert(XSpanId( (&context as &dyn Has<XSpanId>).get().0.to_string() ));
            let result = match result {
                Err(e) => {
                    // Application code returned an error. This should not happen, as the implementation should
                    // return a valid response.
                    response = response.status(StatusCode::INTERNAL_SERVER_ERROR);
                    return Ok(response.body(hyper::Body::from("An internal error occurred")).unwrap());
                }
                Ok(r) => r,
            };
        
            if !unused_elements.is_empty() {
                response.headers_mut().unwrap().typed_insert(Warning(format!("Ignoring unknown fields in body: {:?}", unused_elements)));
            }
        
            match result {
                PlaceOrderResponse::SuccessfulOperation(body)
            
                => {
                    response = response.status(hyper::http::StatusCode::from_u16(200).unwrap());
            
                    let header = headers::ContentType::from(mimetypes::responses::PLACE_ORDER_SUCCESSFUL_OPERATION.clone());
                    response.headers_mut().unwrap().typed_insert(header);
            
                
                    let body = serde_xml_rs::to_string(&body).expect("impossible to fail to serialize");
                
                    Ok(response.body(Body::from(body)).unwrap())
                },
                PlaceOrderResponse::InvalidOrder
            
                => {
                    response = response.status(hyper::http::StatusCode::from_u16(400).unwrap());
            
                    Ok(response.body(Body::empty()).unwrap())
                },
            }
        },
    
        // CreateUser - POST /user
        &hyper::Method::POST if path.matched(paths::ID_USER) => {
            // Body parameters (note that non-required body parameters will ignore garbage
            // values, rather than causing a 400 response). Produce warning header and logs for
            // any unused fields

            let body = hyper::body::aggregate(body).await.map_err(|e| ApiError(format!("Error getting response: {}", e)));
            let body = match body {
                Err(e) => {
                    return Ok(
                        Response::builder()
                            .status(StatusCode::BAD_REQUEST)
                            .body(hyper::Body::from(format!("Couldn't receive body parameter body: {}", e)))
                            .unwrap()
                    );
                }
                Ok(v) => v,
            };
            let mut unused_elements = Vec::new();
            let param_body: Option<crate::models::User> = if body.remaining() > 0 {
                let deserializer = &mut serde_json::Deserializer::from_reader(body.reader());
                match serde_ignored::deserialize(deserializer, |path| {
                    warn!("Ignoring unknown field in body: {}", path);
                    unused_elements.push(path.to_string());
                }) {
                    Ok(param_body) => param_body,
                    Err(e) => return Ok(Response::builder().status(StatusCode::BAD_REQUEST).body(hyper::Body::from(format!("Couldn't parse body parameter body - doesn't match schema: {}", e))).unwrap()),
                }
            } else {
                None
            };
            let param_body = match param_body {
                Some(param_body) => param_body,
                None => return Ok(Response::builder().status(StatusCode::BAD_REQUEST).body(hyper::Body::from("Missing required body parameter body")).unwrap()),
            };
            let result = api_impl.create_user(param_body, &context).await;
            let mut response = Response::builder();
            response.headers_mut().unwrap().typed_insert(XSpanId( (&context as &dyn Has<XSpanId>).get().0.to_string() ));
            let result = match result {
                Err(e) => {
                    // Application code returned an error. This should not happen, as the implementation should
                    // return a valid response.
                    response = response.status(StatusCode::INTERNAL_SERVER_ERROR);
                    return Ok(response.body(hyper::Body::from("An internal error occurred")).unwrap());
                }
                Ok(r) => r,
            };
        
            if !unused_elements.is_empty() {
                response.headers_mut().unwrap().typed_insert(Warning(format!("Ignoring unknown fields in body: {:?}", unused_elements)));
            }
        
            match result {
                CreateUserResponse::SuccessfulOperation
            
                => {
                    response = response.status(hyper::http::StatusCode::from_u16(0).unwrap());
            
                    Ok(response.body(Body::empty()).unwrap())
                },
            }
        },
    
        // CreateUsersWithArrayInput - POST /user/createWithArray
        &hyper::Method::POST if path.matched(paths::ID_USER_CREATEWITHARRAY) => {
            // Body parameters (note that non-required body parameters will ignore garbage
            // values, rather than causing a 400 response). Produce warning header and logs for
            // any unused fields

            let body = hyper::body::aggregate(body).await.map_err(|e| ApiError(format!("Error getting response: {}", e)));
            let body = match body {
                Err(e) => {
                    return Ok(
                        Response::builder()
                            .status(StatusCode::BAD_REQUEST)
                            .body(hyper::Body::from(format!("Couldn't receive body parameter body: {}", e)))
                            .unwrap()
                    );
                }
                Ok(v) => v,
            };
            let mut unused_elements = Vec::new();
            let param_body: Option<Vec<crate::models::User>> = if body.remaining() > 0 {
                let deserializer = &mut serde_json::Deserializer::from_reader(body.reader());
                match serde_ignored::deserialize(deserializer, |path| {
                    warn!("Ignoring unknown field in body: {}", path);
                    unused_elements.push(path.to_string());
                }) {
                    Ok(param_body) => param_body,
                    Err(e) => return Ok(Response::builder().status(StatusCode::BAD_REQUEST).body(hyper::Body::from(format!("Couldn't parse body parameter body - doesn't match schema: {}", e))).unwrap()),
                }
            } else {
                None
            };
            let param_body = match param_body {
                Some(param_body) => param_body,
                None => return Ok(Response::builder().status(StatusCode::BAD_REQUEST).body(hyper::Body::from("Missing required body parameter body")).unwrap()),
            };
            let result = api_impl.create_users_with_array_input(param_body.as_ref(), &context).await;
            let mut response = Response::builder();
            response.headers_mut().unwrap().typed_insert(XSpanId( (&context as &dyn Has<XSpanId>).get().0.to_string() ));
            let result = match result {
                Err(e) => {
                    // Application code returned an error. This should not happen, as the implementation should
                    // return a valid response.
                    response = response.status(StatusCode::INTERNAL_SERVER_ERROR);
                    return Ok(response.body(hyper::Body::from("An internal error occurred")).unwrap());
                }
                Ok(r) => r,
            };
        
            if !unused_elements.is_empty() {
                response.headers_mut().unwrap().typed_insert(Warning(format!("Ignoring unknown fields in body: {:?}", unused_elements)));
            }
        
            match result {
                CreateUsersWithArrayInputResponse::SuccessfulOperation
            
                => {
                    response = response.status(hyper::http::StatusCode::from_u16(0).unwrap());
            
                    Ok(response.body(Body::empty()).unwrap())
                },
            }
        },
    
        // CreateUsersWithListInput - POST /user/createWithList
        &hyper::Method::POST if path.matched(paths::ID_USER_CREATEWITHLIST) => {
            // Body parameters (note that non-required body parameters will ignore garbage
            // values, rather than causing a 400 response). Produce warning header and logs for
            // any unused fields

            let body = hyper::body::aggregate(body).await.map_err(|e| ApiError(format!("Error getting response: {}", e)));
            let body = match body {
                Err(e) => {
                    return Ok(
                        Response::builder()
                            .status(StatusCode::BAD_REQUEST)
                            .body(hyper::Body::from(format!("Couldn't receive body parameter body: {}", e)))
                            .unwrap()
                    );
                }
                Ok(v) => v,
            };
            let mut unused_elements = Vec::new();
            let param_body: Option<Vec<crate::models::User>> = if body.remaining() > 0 {
                let deserializer = &mut serde_json::Deserializer::from_reader(body.reader());
                match serde_ignored::deserialize(deserializer, |path| {
                    warn!("Ignoring unknown field in body: {}", path);
                    unused_elements.push(path.to_string());
                }) {
                    Ok(param_body) => param_body,
                    Err(e) => return Ok(Response::builder().status(StatusCode::BAD_REQUEST).body(hyper::Body::from(format!("Couldn't parse body parameter body - doesn't match schema: {}", e))).unwrap()),
                }
            } else {
                None
            };
            let param_body = match param_body {
                Some(param_body) => param_body,
                None => return Ok(Response::builder().status(StatusCode::BAD_REQUEST).body(hyper::Body::from("Missing required body parameter body")).unwrap()),
            };
            let result = api_impl.create_users_with_list_input(param_body.as_ref(), &context).await;
            let mut response = Response::builder();
            response.headers_mut().unwrap().typed_insert(XSpanId( (&context as &dyn Has<XSpanId>).get().0.to_string() ));
            let result = match result {
                Err(e) => {
                    // Application code returned an error. This should not happen, as the implementation should
                    // return a valid response.
                    response = response.status(StatusCode::INTERNAL_SERVER_ERROR);
                    return Ok(response.body(hyper::Body::from("An internal error occurred")).unwrap());
                }
                Ok(r) => r,
            };
        
            if !unused_elements.is_empty() {
                response.headers_mut().unwrap().typed_insert(Warning(format!("Ignoring unknown fields in body: {:?}", unused_elements)));
            }
        
            match result {
                CreateUsersWithListInputResponse::SuccessfulOperation
            
                => {
                    response = response.status(hyper::http::StatusCode::from_u16(0).unwrap());
            
                    Ok(response.body(Body::empty()).unwrap())
                },
            }
        },
    
        // DeleteUser - DELETE /user/{username}
        &hyper::Method::DELETE if path.matched(paths::ID_USER_USERNAME) => {
            // Path parameters
            let path = uri.path().to_string();
            let path_params = paths::REGEX_USER_USERNAME
                .captures(&path)
                .unwrap_or_else(||
                    panic!("Path {} matched RE USER_USERNAME in set but failed match against \"{}\"", path, paths::REGEX_USER_USERNAME.as_str())
                );
            let param_username = match percent_encoding::percent_decode(path_params["username"].as_bytes()).decode_utf8() {
                Ok(param_username) => match param_username.parse::<String>() {
                    Ok(param_username) => param_username,
                    Err(e) => return Ok(Response::builder().status(StatusCode::BAD_REQUEST).body(hyper::Body::from(format!("Couldn't parse path parameter username: {:?}", e))).unwrap()),
                },
                Err(_) => return Ok(Response::builder().status(StatusCode::BAD_REQUEST).body(hyper::Body::from(format!("Couldn't percent-decode path parameter as UTF-8: {}", &path_params["username"]))).unwrap())
            };
            let result = api_impl.delete_user(param_username, &context).await;
            let mut response = Response::builder();
            response.headers_mut().unwrap().typed_insert(XSpanId( (&context as &dyn Has<XSpanId>).get().0.to_string() ));
            let result = match result {
                Err(e) => {
                    // Application code returned an error. This should not happen, as the implementation should
                    // return a valid response.
                    response = response.status(StatusCode::INTERNAL_SERVER_ERROR);
                    return Ok(response.body(hyper::Body::from("An internal error occurred")).unwrap());
                }
                Ok(r) => r,
            };
        
            match result {
                DeleteUserResponse::InvalidUsernameSupplied
            
                => {
                    response = response.status(hyper::http::StatusCode::from_u16(400).unwrap());
            
                    Ok(response.body(Body::empty()).unwrap())
                },
                DeleteUserResponse::UserNotFound
            
                => {
                    response = response.status(hyper::http::StatusCode::from_u16(404).unwrap());
            
                    Ok(response.body(Body::empty()).unwrap())
                },
            }
        },
    
        // GetUserByName - GET /user/{username}
        &hyper::Method::GET if path.matched(paths::ID_USER_USERNAME) => {
            // Path parameters
            let path = uri.path().to_string();
            let path_params = paths::REGEX_USER_USERNAME
                .captures(&path)
                .unwrap_or_else(||
                    panic!("Path {} matched RE USER_USERNAME in set but failed match against \"{}\"", path, paths::REGEX_USER_USERNAME.as_str())
                );
            let param_username = match percent_encoding::percent_decode(path_params["username"].as_bytes()).decode_utf8() {
                Ok(param_username) => match param_username.parse::<String>() {
                    Ok(param_username) => param_username,
                    Err(e) => return Ok(Response::builder().status(StatusCode::BAD_REQUEST).body(hyper::Body::from(format!("Couldn't parse path parameter username: {:?}", e))).unwrap()),
                },
                Err(_) => return Ok(Response::builder().status(StatusCode::BAD_REQUEST).body(hyper::Body::from(format!("Couldn't percent-decode path parameter as UTF-8: {}", &path_params["username"]))).unwrap())
            };
            let result = api_impl.get_user_by_name(param_username, &context).await;
            let mut response = Response::builder();
            response.headers_mut().unwrap().typed_insert(XSpanId( (&context as &dyn Has<XSpanId>).get().0.to_string() ));
            let result = match result {
                Err(e) => {
                    // Application code returned an error. This should not happen, as the implementation should
                    // return a valid response.
                    response = response.status(StatusCode::INTERNAL_SERVER_ERROR);
                    return Ok(response.body(hyper::Body::from("An internal error occurred")).unwrap());
                }
                Ok(r) => r,
            };
        
            match result {
                GetUserByNameResponse::SuccessfulOperation(body)
            
                => {
                    response = response.status(hyper::http::StatusCode::from_u16(200).unwrap());
            
                    let header = headers::ContentType::from(mimetypes::responses::GET_USER_BY_NAME_SUCCESSFUL_OPERATION.clone());
                    response.headers_mut().unwrap().typed_insert(header);
            
                
                    let body = serde_xml_rs::to_string(&body).expect("impossible to fail to serialize");
                
                    Ok(response.body(Body::from(body)).unwrap())
                },
                GetUserByNameResponse::InvalidUsernameSupplied
            
                => {
                    response = response.status(hyper::http::StatusCode::from_u16(400).unwrap());
            
                    Ok(response.body(Body::empty()).unwrap())
                },
                GetUserByNameResponse::UserNotFound
            
                => {
                    response = response.status(hyper::http::StatusCode::from_u16(404).unwrap());
            
                    Ok(response.body(Body::empty()).unwrap())
                },
            }
        },
    
        // LoginUser - GET /user/login
        &hyper::Method::GET if path.matched(paths::ID_USER_LOGIN) => {
            // Query parameters (note that non-required or collection query parameters will ignore garbage values, rather than causing a 400 response)
            let query_params = form_urlencoded::parse(uri.query().unwrap_or_default().as_bytes()).collect::<Vec<_>>();
            let param_username = query_params.iter().filter(|e| e.0 == "username").map(|e| e.1.to_owned())
                .nth(0);
            let param_username = match param_username {
                Some(param_username) => match param_username.parse::<String>() {
                    Ok(param_username) => param_username,
                    Err(e) => return Ok(Response::builder().status(StatusCode::BAD_REQUEST).body(hyper::Body::from(format!("Couldn't parse query parameter username - doesn't match schema: {}", e))).unwrap()),
                },
                None => return Ok(Response::builder().status(StatusCode::BAD_REQUEST).body(hyper::Body::from("Missing required query parameter username")).unwrap()),
            };
            let param_password = query_params.iter().filter(|e| e.0 == "password").map(|e| e.1.to_owned())
                .nth(0);
            let param_password = match param_password {
                Some(param_password) => match param_password.parse::<String>() {
                    Ok(param_password) => param_password,
                    Err(e) => return Ok(Response::builder().status(StatusCode::BAD_REQUEST).body(hyper::Body::from(format!("Couldn't parse query parameter password - doesn't match schema: {}", e))).unwrap()),
                },
                None => return Ok(Response::builder().status(StatusCode::BAD_REQUEST).body(hyper::Body::from("Missing required query parameter password")).unwrap()),
            };
            let result = api_impl.login_user(param_username, param_password, &context).await;
            let mut response = Response::builder();
            response.headers_mut().unwrap().typed_insert(XSpanId( (&context as &dyn Has<XSpanId>).get().0.to_string() ));
            let result = match result {
                Err(e) => {
                    // Application code returned an error. This should not happen, as the implementation should
                    // return a valid response.
                    response = response.status(StatusCode::INTERNAL_SERVER_ERROR);
                    return Ok(response.body(hyper::Body::from("An internal error occurred")).unwrap());
                }
                Ok(r) => r,
            };
        
            match result {
                LoginUserResponse::SuccessfulOperation
                    {
                        body,
                        x_rate_limit,         
x_expires_after
                    }
        
            
                => {
                    response = response.status(hyper::http::StatusCode::from_u16(200).unwrap());
                    response.headers_mut().unwrap().typed_insert(ResponseXRateLimit(x_rate_limit));
                    response.headers_mut().unwrap().typed_insert(ResponseXExpiresAfter(x_expires_after));
            
                    let header = headers::ContentType::from(mimetypes::responses::LOGIN_USER_SUCCESSFUL_OPERATION.clone());
                    response.headers_mut().unwrap().typed_insert(header);
            
                
                    let body = serde_xml_rs::to_string(&body).expect("impossible to fail to serialize");
                
                    Ok(response.body(Body::from(body)).unwrap())
                },
                LoginUserResponse::InvalidUsername
            
                => {
                    response = response.status(hyper::http::StatusCode::from_u16(400).unwrap());
            
                    Ok(response.body(Body::empty()).unwrap())
                },
            }
        },
    
        // LogoutUser - GET /user/logout
        &hyper::Method::GET if path.matched(paths::ID_USER_LOGOUT) => {
            let result = api_impl.logout_user(&context).await;
            let mut response = Response::builder();
            response.headers_mut().unwrap().typed_insert(XSpanId( (&context as &dyn Has<XSpanId>).get().0.to_string() ));
            let result = match result {
                Err(e) => {
                    // Application code returned an error. This should not happen, as the implementation should
                    // return a valid response.
                    response = response.status(StatusCode::INTERNAL_SERVER_ERROR);
                    return Ok(response.body(hyper::Body::from("An internal error occurred")).unwrap());
                }
                Ok(r) => r,
            };
        
            match result {
                LogoutUserResponse::SuccessfulOperation
            
                => {
                    response = response.status(hyper::http::StatusCode::from_u16(0).unwrap());
            
                    Ok(response.body(Body::empty()).unwrap())
                },
            }
        },
    
        // UpdateUser - PUT /user/{username}
        &hyper::Method::PUT if path.matched(paths::ID_USER_USERNAME) => {
            // Path parameters
            let path = uri.path().to_string();
            let path_params = paths::REGEX_USER_USERNAME
                .captures(&path)
                .unwrap_or_else(||
                    panic!("Path {} matched RE USER_USERNAME in set but failed match against \"{}\"", path, paths::REGEX_USER_USERNAME.as_str())
                );
            let param_username = match percent_encoding::percent_decode(path_params["username"].as_bytes()).decode_utf8() {
                Ok(param_username) => match param_username.parse::<String>() {
                    Ok(param_username) => param_username,
                    Err(e) => return Ok(Response::builder().status(StatusCode::BAD_REQUEST).body(hyper::Body::from(format!("Couldn't parse path parameter username: {:?}", e))).unwrap()),
                },
                Err(_) => return Ok(Response::builder().status(StatusCode::BAD_REQUEST).body(hyper::Body::from(format!("Couldn't percent-decode path parameter as UTF-8: {}", &path_params["username"]))).unwrap())
            };
            // Body parameters (note that non-required body parameters will ignore garbage
            // values, rather than causing a 400 response). Produce warning header and logs for
            // any unused fields

            let body = hyper::body::aggregate(body).await.map_err(|e| ApiError(format!("Error getting response: {}", e)));
            let body = match body {
                Err(e) => {
                    return Ok(
                        Response::builder()
                            .status(StatusCode::BAD_REQUEST)
                            .body(hyper::Body::from(format!("Couldn't receive body parameter body: {}", e)))
                            .unwrap()
                    );
                }
                Ok(v) => v,
            };
            let mut unused_elements = Vec::new();
            let param_body: Option<crate::models::User> = if body.remaining() > 0 {
                let deserializer = &mut serde_json::Deserializer::from_reader(body.reader());
                match serde_ignored::deserialize(deserializer, |path| {
                    warn!("Ignoring unknown field in body: {}", path);
                    unused_elements.push(path.to_string());
                }) {
                    Ok(param_body) => param_body,
                    Err(e) => return Ok(Response::builder().status(StatusCode::BAD_REQUEST).body(hyper::Body::from(format!("Couldn't parse body parameter body - doesn't match schema: {}", e))).unwrap()),
                }
            } else {
                None
            };
            let param_body = match param_body {
                Some(param_body) => param_body,
                None => return Ok(Response::builder().status(StatusCode::BAD_REQUEST).body(hyper::Body::from("Missing required body parameter body")).unwrap()),
            };
            let result = api_impl.update_user(param_username, param_body, &context).await;
            let mut response = Response::builder();
            response.headers_mut().unwrap().typed_insert(XSpanId( (&context as &dyn Has<XSpanId>).get().0.to_string() ));
            let result = match result {
                Err(e) => {
                    // Application code returned an error. This should not happen, as the implementation should
                    // return a valid response.
                    response = response.status(StatusCode::INTERNAL_SERVER_ERROR);
                    return Ok(response.body(hyper::Body::from("An internal error occurred")).unwrap());
                }
                Ok(r) => r,
            };
        
            if !unused_elements.is_empty() {
                response.headers_mut().unwrap().typed_insert(Warning(format!("Ignoring unknown fields in body: {:?}", unused_elements)));
            }
        
            match result {
                UpdateUserResponse::InvalidUserSupplied
            
                => {
                    response = response.status(hyper::http::StatusCode::from_u16(400).unwrap());
            
                    Ok(response.body(Body::empty()).unwrap())
                },
                UpdateUserResponse::UserNotFound
            
                => {
                    response = response.status(hyper::http::StatusCode::from_u16(404).unwrap());
            
                    Ok(response.body(Body::empty()).unwrap())
                },
            }
        },
    
        _ => Ok(Response::builder().status(StatusCode::NOT_FOUND).body(Body::empty()).unwrap()),
    }
}

impl<T, C> Clone for Service<T, C> where T: Clone
{
    fn clone(&self) -> Self {
        Service {
            api_impl: self.api_impl.clone(),
            marker: self.marker.clone(),
        }
    }
}

/// Utility function to get the multipart boundary marker (if any) from the Headers.
fn multipart_boundary<'a>(headers: &'a HeaderMap) -> Option<String> {
    headers.typed_get::<ContentType>().and_then(|content_type| {
        let mime = Mime::from(content_type);
        if mime.type_() == mime::MULTIPART && mime.subtype() == mime::FORM_DATA {
            mime.get_param(mime::BOUNDARY).map(|x| x.as_str().to_owned())
        } else {
            None
        }
    })
}

/// Request parser for `Api`.
pub struct ApiRequestParser;
impl RequestParser<Body> for ApiRequestParser {
    fn parse_operation_id(request: &Request<Body>) -> Result<&'static str, ()> {
        let path = paths::GLOBAL_REGEX_SET.matches(request.uri().path());
        match request.method() {

            // TestSpecialTags - PATCH /another-fake/dummy
            &hyper::Method::PATCH if path.matched(paths::ID_ANOTHER_FAKE_DUMMY) => Ok("TestSpecialTags"),

            // FakeOuterBooleanSerialize - POST /fake/outer/boolean
            &hyper::Method::POST if path.matched(paths::ID_FAKE_OUTER_BOOLEAN) => Ok("FakeOuterBooleanSerialize"),

            // FakeOuterCompositeSerialize - POST /fake/outer/composite
            &hyper::Method::POST if path.matched(paths::ID_FAKE_OUTER_COMPOSITE) => Ok("FakeOuterCompositeSerialize"),

            // FakeOuterNumberSerialize - POST /fake/outer/number
            &hyper::Method::POST if path.matched(paths::ID_FAKE_OUTER_NUMBER) => Ok("FakeOuterNumberSerialize"),

            // FakeOuterStringSerialize - POST /fake/outer/string
            &hyper::Method::POST if path.matched(paths::ID_FAKE_OUTER_STRING) => Ok("FakeOuterStringSerialize"),

            // HyphenParam - GET /fake/hyphenParam/{hyphen-param}
            &hyper::Method::GET if path.matched(paths::ID_FAKE_HYPHENPARAM_HYPHEN_PARAM) => Ok("HyphenParam"),

            // TestBodyWithQueryParams - PUT /fake/body-with-query-params
            &hyper::Method::PUT if path.matched(paths::ID_FAKE_BODY_WITH_QUERY_PARAMS) => Ok("TestBodyWithQueryParams"),

            // TestClientModel - PATCH /fake
            &hyper::Method::PATCH if path.matched(paths::ID_FAKE) => Ok("TestClientModel"),

            // TestEndpointParameters - POST /fake
            &hyper::Method::POST if path.matched(paths::ID_FAKE) => Ok("TestEndpointParameters"),

            // TestEnumParameters - GET /fake
            &hyper::Method::GET if path.matched(paths::ID_FAKE) => Ok("TestEnumParameters"),

            // TestInlineAdditionalProperties - POST /fake/inline-additionalProperties
            &hyper::Method::POST if path.matched(paths::ID_FAKE_INLINE_ADDITIONALPROPERTIES) => Ok("TestInlineAdditionalProperties"),

            // TestJsonFormData - GET /fake/jsonFormData
            &hyper::Method::GET if path.matched(paths::ID_FAKE_JSONFORMDATA) => Ok("TestJsonFormData"),

            // TestClassname - PATCH /fake_classname_test
            &hyper::Method::PATCH if path.matched(paths::ID_FAKE_CLASSNAME_TEST) => Ok("TestClassname"),

            // AddPet - POST /pet
            &hyper::Method::POST if path.matched(paths::ID_PET) => Ok("AddPet"),

            // DeletePet - DELETE /pet/{petId}
            &hyper::Method::DELETE if path.matched(paths::ID_PET_PETID) => Ok("DeletePet"),

            // FindPetsByStatus - GET /pet/findByStatus
            &hyper::Method::GET if path.matched(paths::ID_PET_FINDBYSTATUS) => Ok("FindPetsByStatus"),

            // FindPetsByTags - GET /pet/findByTags
            &hyper::Method::GET if path.matched(paths::ID_PET_FINDBYTAGS) => Ok("FindPetsByTags"),

            // GetPetById - GET /pet/{petId}
            &hyper::Method::GET if path.matched(paths::ID_PET_PETID) => Ok("GetPetById"),

            // UpdatePet - PUT /pet
            &hyper::Method::PUT if path.matched(paths::ID_PET) => Ok("UpdatePet"),

            // UpdatePetWithForm - POST /pet/{petId}
            &hyper::Method::POST if path.matched(paths::ID_PET_PETID) => Ok("UpdatePetWithForm"),

            // UploadFile - POST /pet/{petId}/uploadImage
            &hyper::Method::POST if path.matched(paths::ID_PET_PETID_UPLOADIMAGE) => Ok("UploadFile"),

            // DeleteOrder - DELETE /store/order/{order_id}
            &hyper::Method::DELETE if path.matched(paths::ID_STORE_ORDER_ORDER_ID) => Ok("DeleteOrder"),

            // GetInventory - GET /store/inventory
            &hyper::Method::GET if path.matched(paths::ID_STORE_INVENTORY) => Ok("GetInventory"),

            // GetOrderById - GET /store/order/{order_id}
            &hyper::Method::GET if path.matched(paths::ID_STORE_ORDER_ORDER_ID) => Ok("GetOrderById"),

            // PlaceOrder - POST /store/order
            &hyper::Method::POST if path.matched(paths::ID_STORE_ORDER) => Ok("PlaceOrder"),

            // CreateUser - POST /user
            &hyper::Method::POST if path.matched(paths::ID_USER) => Ok("CreateUser"),

            // CreateUsersWithArrayInput - POST /user/createWithArray
            &hyper::Method::POST if path.matched(paths::ID_USER_CREATEWITHARRAY) => Ok("CreateUsersWithArrayInput"),

            // CreateUsersWithListInput - POST /user/createWithList
            &hyper::Method::POST if path.matched(paths::ID_USER_CREATEWITHLIST) => Ok("CreateUsersWithListInput"),

            // DeleteUser - DELETE /user/{username}
            &hyper::Method::DELETE if path.matched(paths::ID_USER_USERNAME) => Ok("DeleteUser"),

            // GetUserByName - GET /user/{username}
            &hyper::Method::GET if path.matched(paths::ID_USER_USERNAME) => Ok("GetUserByName"),

            // LoginUser - GET /user/login
            &hyper::Method::GET if path.matched(paths::ID_USER_LOGIN) => Ok("LoginUser"),

            // LogoutUser - GET /user/logout
            &hyper::Method::GET if path.matched(paths::ID_USER_LOGOUT) => Ok("LogoutUser"),

            // UpdateUser - PUT /user/{username}
            &hyper::Method::PUT if path.matched(paths::ID_USER_USERNAME) => Ok("UpdateUser"),
            _ => Err(()),
        }
    }
}
