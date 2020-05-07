use std::marker::PhantomData;
use futures::{Future, future, Stream, stream};
use hyper;
use hyper::{Request, Response, Error, StatusCode, Body, HeaderMap};
use hyper::header::{HeaderName, HeaderValue, CONTENT_TYPE};
use log::warn;
use serde_json;
use std::io;
use url::form_urlencoded;
#[allow(unused_imports)]
use swagger;
use swagger::{ApiError, XSpanIdString, Has, RequestParser};
pub use swagger::auth::Authorization;
use swagger::auth::Scopes;
use swagger::context::ContextualPayload;

#[allow(unused_imports)]
use crate::models;
use crate::header;

pub use crate::context;

use crate::{Api,
     AllOfGetResponse,
     DummyGetResponse,
     DummyPutResponse,
     FileResponseGetResponse,
     GetStructuredYamlResponse,
     HtmlPostResponse,
     PostYamlResponse,
     RawJsonGetResponse,
     SoloObjectPostResponse
};

mod paths {
    use lazy_static::lazy_static;

    lazy_static! {
        pub static ref GLOBAL_REGEX_SET: regex::RegexSet = regex::RegexSet::new(vec![
            r"^/allOf$",
            r"^/dummy$",
            r"^/file_response$",
            r"^/get-structured-yaml$",
            r"^/html$",
            r"^/post-yaml$",
            r"^/raw_json$",
            r"^/solo-object$"
        ])
        .expect("Unable to create global regex set");
    }
    pub(crate) static ID_ALLOF: usize = 0;
    pub(crate) static ID_DUMMY: usize = 1;
    pub(crate) static ID_FILE_RESPONSE: usize = 2;
    pub(crate) static ID_GET_STRUCTURED_YAML: usize = 3;
    pub(crate) static ID_HTML: usize = 4;
    pub(crate) static ID_POST_YAML: usize = 5;
    pub(crate) static ID_RAW_JSON: usize = 6;
    pub(crate) static ID_SOLO_OBJECT: usize = 7;
}

pub struct MakeService<T, RC> {
    api_impl: T,
    marker: PhantomData<RC>,
}

impl<T, RC> MakeService<T, RC>
where
    T: Api<RC> + Clone + Send + 'static,
    RC: Has<XSpanIdString>  + 'static
{
    pub fn new(api_impl: T) -> Self {
        MakeService {
            api_impl,
            marker: PhantomData
        }
    }
}

impl<'a, T, SC, RC> hyper::service::MakeService<&'a SC> for MakeService<T, RC>
where
    T: Api<RC> + Clone + Send + 'static,
    RC: Has<XSpanIdString>  + 'static + Send
{
    type ReqBody = ContextualPayload<Body, RC>;
    type ResBody = Body;
    type Error = Error;
    type Service = Service<T, RC>;
    type Future = future::FutureResult<Self::Service, Self::MakeError>;
    type MakeError = Error;

    fn make_service(&mut self, _ctx: &'a SC) -> Self::Future {
        future::FutureResult::from(Ok(Service::new(
            self.api_impl.clone(),
        )))
    }
}

type ServiceFuture = Box<dyn Future<Item = Response<Body>, Error = Error> + Send>;

fn method_not_allowed() -> ServiceFuture {
    Box::new(future::ok(
        Response::builder().status(StatusCode::METHOD_NOT_ALLOWED)
            .body(Body::empty())
            .expect("Unable to create Method Not Allowed response")
    ))
}

pub struct Service<T, RC> {
    api_impl: T,
    marker: PhantomData<RC>,
}

impl<T, RC> Service<T, RC>
where
    T: Api<RC> + Clone + Send + 'static,
    RC: Has<XSpanIdString>  + 'static {
    pub fn new(api_impl: T) -> Self {
        Service {
            api_impl: api_impl,
            marker: PhantomData
        }
    }
}

impl<T, C> hyper::service::Service for Service<T, C>
where
    T: Api<C> + Clone + Send + 'static,
    C: Has<XSpanIdString>  + 'static + Send
{
    type ReqBody = ContextualPayload<Body, C>;
    type ResBody = Body;
    type Error = Error;
    type Future = ServiceFuture;

    fn call(&mut self, req: Request<Self::ReqBody>) -> Self::Future {
        let api_impl = self.api_impl.clone();
        let (parts, body) = req.into_parts();
        let (method, uri, headers) = (parts.method, parts.uri, parts.headers);
        let path = paths::GLOBAL_REGEX_SET.matches(uri.path());
        let mut context = body.context;
        let body = body.inner;

        match &method {

            // AllOfGet - GET /allOf
            &hyper::Method::GET if path.matched(paths::ID_ALLOF) => {
                Box::new({
                        {{
                                Box::new(
                                    api_impl.all_of_get(
                                        &context
                                    ).then(move |result| {
                                        let mut response = Response::new(Body::empty());
                                        response.headers_mut().insert(
                                            HeaderName::from_static("x-span-id"),
                                            HeaderValue::from_str((&context as &dyn Has<XSpanIdString>).get().0.clone().to_string().as_str())
                                                .expect("Unable to create X-Span-ID header value"));

                                        match result {
                                            Ok(rsp) => match rsp {
                                                AllOfGetResponse::OK
                                                    (body)
                                                => {
                                                    *response.status_mut() = StatusCode::from_u16(200).expect("Unable to turn 200 into a StatusCode");
                                                    response.headers_mut().insert(
                                                        CONTENT_TYPE,
                                                        HeaderValue::from_str("*/*")
                                                            .expect("Unable to create Content-Type header for ALL_OF_GET_OK"));
                                                    let body = serde_json::to_string(&body).expect("impossible to fail to serialize");
                                                    *response.body_mut() = Body::from(body);
                                                },
                                            },
                                            Err(_) => {
                                                // Application code returned an error. This should not happen, as the implementation should
                                                // return a valid response.
                                                *response.status_mut() = StatusCode::INTERNAL_SERVER_ERROR;
                                                *response.body_mut() = Body::from("An internal error occurred");
                                            },
                                        }

                                        future::ok(response)
                                    }
                                ))
                        }}
                }) as Self::Future
            },

            // DummyGet - GET /dummy
            &hyper::Method::GET if path.matched(paths::ID_DUMMY) => {
                Box::new({
                        {{
                                Box::new(
                                    api_impl.dummy_get(
                                        &context
                                    ).then(move |result| {
                                        let mut response = Response::new(Body::empty());
                                        response.headers_mut().insert(
                                            HeaderName::from_static("x-span-id"),
                                            HeaderValue::from_str((&context as &dyn Has<XSpanIdString>).get().0.clone().to_string().as_str())
                                                .expect("Unable to create X-Span-ID header value"));

                                        match result {
                                            Ok(rsp) => match rsp {
                                                DummyGetResponse::Success
                                                => {
                                                    *response.status_mut() = StatusCode::from_u16(200).expect("Unable to turn 200 into a StatusCode");
                                                },
                                            },
                                            Err(_) => {
                                                // Application code returned an error. This should not happen, as the implementation should
                                                // return a valid response.
                                                *response.status_mut() = StatusCode::INTERNAL_SERVER_ERROR;
                                                *response.body_mut() = Body::from("An internal error occurred");
                                            },
                                        }

                                        future::ok(response)
                                    }
                                ))
                        }}
                }) as Self::Future
            },

            // DummyPut - PUT /dummy
            &hyper::Method::PUT if path.matched(paths::ID_DUMMY) => {
                // Body parameters (note that non-required body parameters will ignore garbage
                // values, rather than causing a 400 response). Produce warning header and logs for
                // any unused fields.
                Box::new(body.concat2()
                    .then(move |result| -> Self::Future {
                        match result {
                            Ok(body) => {
                                let mut unused_elements = Vec::new();
                                let param_nested_response: Option<models::InlineObject> = if !body.is_empty() {
                                    let deserializer = &mut serde_json::Deserializer::from_slice(&*body);
                                    match serde_ignored::deserialize(deserializer, |path| {
                                            warn!("Ignoring unknown field in body: {}", path);
                                            unused_elements.push(path.to_string());
                                    }) {
                                        Ok(param_nested_response) => param_nested_response,
                                        Err(e) => return Box::new(future::ok(Response::builder()
                                                        .status(StatusCode::BAD_REQUEST)
                                                        .body(Body::from(format!("Couldn't parse body parameter nested_response - doesn't match schema: {}", e)))
                                                        .expect("Unable to create Bad Request response for invalid body parameter nested_response due to schema"))),
                                    }
                                } else {
                                    None
                                };
                                let param_nested_response = match param_nested_response {
                                    Some(param_nested_response) => param_nested_response,
                                    None => return Box::new(future::ok(Response::builder()
                                                        .status(StatusCode::BAD_REQUEST)
                                                        .body(Body::from("Missing required body parameter nested_response"))
                                                        .expect("Unable to create Bad Request response for missing body parameter nested_response"))),
                                };

                                Box::new(
                                    api_impl.dummy_put(
                                            param_nested_response,
                                        &context
                                    ).then(move |result| {
                                        let mut response = Response::new(Body::empty());
                                        response.headers_mut().insert(
                                            HeaderName::from_static("x-span-id"),
                                            HeaderValue::from_str((&context as &dyn Has<XSpanIdString>).get().0.clone().to_string().as_str())
                                                .expect("Unable to create X-Span-ID header value"));

                                        if !unused_elements.is_empty() {
                                            response.headers_mut().insert(
                                                HeaderName::from_static("warning"),
                                                HeaderValue::from_str(format!("Ignoring unknown fields in body: {:?}", unused_elements).as_str())
                                                    .expect("Unable to create Warning header value"));
                                        }

                                        match result {
                                            Ok(rsp) => match rsp {
                                                DummyPutResponse::Success
                                                => {
                                                    *response.status_mut() = StatusCode::from_u16(200).expect("Unable to turn 200 into a StatusCode");
                                                },
                                            },
                                            Err(_) => {
                                                // Application code returned an error. This should not happen, as the implementation should
                                                // return a valid response.
                                                *response.status_mut() = StatusCode::INTERNAL_SERVER_ERROR;
                                                *response.body_mut() = Body::from("An internal error occurred");
                                            },
                                        }

                                        future::ok(response)
                                    }
                                ))
                            },
                            Err(e) => Box::new(future::ok(Response::builder()
                                                .status(StatusCode::BAD_REQUEST)
                                                .body(Body::from(format!("Couldn't read body parameter nested_response: {}", e)))
                                                .expect("Unable to create Bad Request response due to unable to read body parameter nested_response"))),
                        }
                    })
                ) as Self::Future
            },

            // FileResponseGet - GET /file_response
            &hyper::Method::GET if path.matched(paths::ID_FILE_RESPONSE) => {
                Box::new({
                        {{
                                Box::new(
                                    api_impl.file_response_get(
                                        &context
                                    ).then(move |result| {
                                        let mut response = Response::new(Body::empty());
                                        response.headers_mut().insert(
                                            HeaderName::from_static("x-span-id"),
                                            HeaderValue::from_str((&context as &dyn Has<XSpanIdString>).get().0.clone().to_string().as_str())
                                                .expect("Unable to create X-Span-ID header value"));

                                        match result {
                                            Ok(rsp) => match rsp {
                                                FileResponseGetResponse::Success
                                                    (body)
                                                => {
                                                    *response.status_mut() = StatusCode::from_u16(200).expect("Unable to turn 200 into a StatusCode");
                                                    response.headers_mut().insert(
                                                        CONTENT_TYPE,
                                                        HeaderValue::from_str("application/json")
                                                            .expect("Unable to create Content-Type header for FILE_RESPONSE_GET_SUCCESS"));
                                                    let body = serde_json::to_string(&body).expect("impossible to fail to serialize");
                                                    *response.body_mut() = Body::from(body);
                                                },
                                            },
                                            Err(_) => {
                                                // Application code returned an error. This should not happen, as the implementation should
                                                // return a valid response.
                                                *response.status_mut() = StatusCode::INTERNAL_SERVER_ERROR;
                                                *response.body_mut() = Body::from("An internal error occurred");
                                            },
                                        }

                                        future::ok(response)
                                    }
                                ))
                        }}
                }) as Self::Future
            },

            // GetStructuredYaml - GET /get-structured-yaml
            &hyper::Method::GET if path.matched(paths::ID_GET_STRUCTURED_YAML) => {
                Box::new({
                        {{
                                Box::new(
                                    api_impl.get_structured_yaml(
                                        &context
                                    ).then(move |result| {
                                        let mut response = Response::new(Body::empty());
                                        response.headers_mut().insert(
                                            HeaderName::from_static("x-span-id"),
                                            HeaderValue::from_str((&context as &dyn Has<XSpanIdString>).get().0.clone().to_string().as_str())
                                                .expect("Unable to create X-Span-ID header value"));

                                        match result {
                                            Ok(rsp) => match rsp {
                                                GetStructuredYamlResponse::OK
                                                    (body)
                                                => {
                                                    *response.status_mut() = StatusCode::from_u16(200).expect("Unable to turn 200 into a StatusCode");
                                                    response.headers_mut().insert(
                                                        CONTENT_TYPE,
                                                        HeaderValue::from_str("application/yaml")
                                                            .expect("Unable to create Content-Type header for GET_STRUCTURED_YAML_OK"));
                                                    let body = body;
                                                    *response.body_mut() = Body::from(body);
                                                },
                                            },
                                            Err(_) => {
                                                // Application code returned an error. This should not happen, as the implementation should
                                                // return a valid response.
                                                *response.status_mut() = StatusCode::INTERNAL_SERVER_ERROR;
                                                *response.body_mut() = Body::from("An internal error occurred");
                                            },
                                        }

                                        future::ok(response)
                                    }
                                ))
                        }}
                }) as Self::Future
            },

            // HtmlPost - POST /html
            &hyper::Method::POST if path.matched(paths::ID_HTML) => {
                // Body parameters (note that non-required body parameters will ignore garbage
                // values, rather than causing a 400 response). Produce warning header and logs for
                // any unused fields.
                Box::new(body.concat2()
                    .then(move |result| -> Self::Future {
                        match result {
                            Ok(body) => {
                                let param_body: Option<String> = if !body.is_empty() {
                                    match String::from_utf8(body.to_vec()) {
                                        Ok(param_body) => Some(param_body),
                                        Err(e) => return Box::new(future::ok(Response::builder()
                                                        .status(StatusCode::BAD_REQUEST)
                                                        .body(Body::from(format!("Couldn't parse body parameter body - not valid UTF-8: {}", e)))
                                                        .expect("Unable to create Bad Request response for invalid body parameter body due to UTF-8"))),
                                    }
                                } else {
                                    None
                                };
                                let param_body = match param_body {
                                    Some(param_body) => param_body,
                                    None => return Box::new(future::ok(Response::builder()
                                                        .status(StatusCode::BAD_REQUEST)
                                                        .body(Body::from("Missing required body parameter body"))
                                                        .expect("Unable to create Bad Request response for missing body parameter body"))),
                                };

                                Box::new(
                                    api_impl.html_post(
                                            param_body,
                                        &context
                                    ).then(move |result| {
                                        let mut response = Response::new(Body::empty());
                                        response.headers_mut().insert(
                                            HeaderName::from_static("x-span-id"),
                                            HeaderValue::from_str((&context as &dyn Has<XSpanIdString>).get().0.clone().to_string().as_str())
                                                .expect("Unable to create X-Span-ID header value"));

                                        match result {
                                            Ok(rsp) => match rsp {
                                                HtmlPostResponse::Success
                                                    (body)
                                                => {
                                                    *response.status_mut() = StatusCode::from_u16(200).expect("Unable to turn 200 into a StatusCode");
                                                    response.headers_mut().insert(
                                                        CONTENT_TYPE,
                                                        HeaderValue::from_str("text/html")
                                                            .expect("Unable to create Content-Type header for HTML_POST_SUCCESS"));
                                                    let body = body;
                                                    *response.body_mut() = Body::from(body);
                                                },
                                            },
                                            Err(_) => {
                                                // Application code returned an error. This should not happen, as the implementation should
                                                // return a valid response.
                                                *response.status_mut() = StatusCode::INTERNAL_SERVER_ERROR;
                                                *response.body_mut() = Body::from("An internal error occurred");
                                            },
                                        }

                                        future::ok(response)
                                    }
                                ))
                            },
                            Err(e) => Box::new(future::ok(Response::builder()
                                                .status(StatusCode::BAD_REQUEST)
                                                .body(Body::from(format!("Couldn't read body parameter body: {}", e)))
                                                .expect("Unable to create Bad Request response due to unable to read body parameter body"))),
                        }
                    })
                ) as Self::Future
            },

            // PostYaml - POST /post-yaml
            &hyper::Method::POST if path.matched(paths::ID_POST_YAML) => {
                // Body parameters (note that non-required body parameters will ignore garbage
                // values, rather than causing a 400 response). Produce warning header and logs for
                // any unused fields.
                Box::new(body.concat2()
                    .then(move |result| -> Self::Future {
                        match result {
                            Ok(body) => {
                                let param_value: Option<String> = if !body.is_empty() {
                                    match String::from_utf8(body.to_vec()) {
                                        Ok(param_value) => Some(param_value),
                                        Err(e) => return Box::new(future::ok(Response::builder()
                                                        .status(StatusCode::BAD_REQUEST)
                                                        .body(Body::from(format!("Couldn't parse body parameter value - not valid UTF-8: {}", e)))
                                                        .expect("Unable to create Bad Request response for invalid body parameter value due to UTF-8"))),
                                    }
                                } else {
                                    None
                                };
                                let param_value = match param_value {
                                    Some(param_value) => param_value,
                                    None => return Box::new(future::ok(Response::builder()
                                                        .status(StatusCode::BAD_REQUEST)
                                                        .body(Body::from("Missing required body parameter value"))
                                                        .expect("Unable to create Bad Request response for missing body parameter value"))),
                                };

                                Box::new(
                                    api_impl.post_yaml(
                                            param_value,
                                        &context
                                    ).then(move |result| {
                                        let mut response = Response::new(Body::empty());
                                        response.headers_mut().insert(
                                            HeaderName::from_static("x-span-id"),
                                            HeaderValue::from_str((&context as &dyn Has<XSpanIdString>).get().0.clone().to_string().as_str())
                                                .expect("Unable to create X-Span-ID header value"));

                                        match result {
                                            Ok(rsp) => match rsp {
                                                PostYamlResponse::OK
                                                => {
                                                    *response.status_mut() = StatusCode::from_u16(204).expect("Unable to turn 204 into a StatusCode");
                                                },
                                            },
                                            Err(_) => {
                                                // Application code returned an error. This should not happen, as the implementation should
                                                // return a valid response.
                                                *response.status_mut() = StatusCode::INTERNAL_SERVER_ERROR;
                                                *response.body_mut() = Body::from("An internal error occurred");
                                            },
                                        }

                                        future::ok(response)
                                    }
                                ))
                            },
                            Err(e) => Box::new(future::ok(Response::builder()
                                                .status(StatusCode::BAD_REQUEST)
                                                .body(Body::from(format!("Couldn't read body parameter value: {}", e)))
                                                .expect("Unable to create Bad Request response due to unable to read body parameter value"))),
                        }
                    })
                ) as Self::Future
            },

            // RawJsonGet - GET /raw_json
            &hyper::Method::GET if path.matched(paths::ID_RAW_JSON) => {
                Box::new({
                        {{
                                Box::new(
                                    api_impl.raw_json_get(
                                        &context
                                    ).then(move |result| {
                                        let mut response = Response::new(Body::empty());
                                        response.headers_mut().insert(
                                            HeaderName::from_static("x-span-id"),
                                            HeaderValue::from_str((&context as &dyn Has<XSpanIdString>).get().0.clone().to_string().as_str())
                                                .expect("Unable to create X-Span-ID header value"));

                                        match result {
                                            Ok(rsp) => match rsp {
                                                RawJsonGetResponse::Success
                                                    (body)
                                                => {
                                                    *response.status_mut() = StatusCode::from_u16(200).expect("Unable to turn 200 into a StatusCode");
                                                    response.headers_mut().insert(
                                                        CONTENT_TYPE,
                                                        HeaderValue::from_str("*/*")
                                                            .expect("Unable to create Content-Type header for RAW_JSON_GET_SUCCESS"));
                                                    let body = serde_json::to_string(&body).expect("impossible to fail to serialize");
                                                    *response.body_mut() = Body::from(body);
                                                },
                                            },
                                            Err(_) => {
                                                // Application code returned an error. This should not happen, as the implementation should
                                                // return a valid response.
                                                *response.status_mut() = StatusCode::INTERNAL_SERVER_ERROR;
                                                *response.body_mut() = Body::from("An internal error occurred");
                                            },
                                        }

                                        future::ok(response)
                                    }
                                ))
                        }}
                }) as Self::Future
            },

            // SoloObjectPost - POST /solo-object
            &hyper::Method::POST if path.matched(paths::ID_SOLO_OBJECT) => {
                // Body parameters (note that non-required body parameters will ignore garbage
                // values, rather than causing a 400 response). Produce warning header and logs for
                // any unused fields.
                Box::new(body.concat2()
                    .then(move |result| -> Self::Future {
                        match result {
                            Ok(body) => {
                                let mut unused_elements = Vec::new();
                                let param_value: Option<serde_json::Value> = if !body.is_empty() {
                                    let deserializer = &mut serde_json::Deserializer::from_slice(&*body);
                                    match serde_ignored::deserialize(deserializer, |path| {
                                            warn!("Ignoring unknown field in body: {}", path);
                                            unused_elements.push(path.to_string());
                                    }) {
                                        Ok(param_value) => param_value,
                                        Err(e) => return Box::new(future::ok(Response::builder()
                                                        .status(StatusCode::BAD_REQUEST)
                                                        .body(Body::from(format!("Couldn't parse body parameter value - doesn't match schema: {}", e)))
                                                        .expect("Unable to create Bad Request response for invalid body parameter value due to schema"))),
                                    }
                                } else {
                                    None
                                };
                                let param_value = match param_value {
                                    Some(param_value) => param_value,
                                    None => return Box::new(future::ok(Response::builder()
                                                        .status(StatusCode::BAD_REQUEST)
                                                        .body(Body::from("Missing required body parameter value"))
                                                        .expect("Unable to create Bad Request response for missing body parameter value"))),
                                };

                                Box::new(
                                    api_impl.solo_object_post(
                                            param_value,
                                        &context
                                    ).then(move |result| {
                                        let mut response = Response::new(Body::empty());
                                        response.headers_mut().insert(
                                            HeaderName::from_static("x-span-id"),
                                            HeaderValue::from_str((&context as &dyn Has<XSpanIdString>).get().0.clone().to_string().as_str())
                                                .expect("Unable to create X-Span-ID header value"));

                                        if !unused_elements.is_empty() {
                                            response.headers_mut().insert(
                                                HeaderName::from_static("warning"),
                                                HeaderValue::from_str(format!("Ignoring unknown fields in body: {:?}", unused_elements).as_str())
                                                    .expect("Unable to create Warning header value"));
                                        }

                                        match result {
                                            Ok(rsp) => match rsp {
                                                SoloObjectPostResponse::OK
                                                => {
                                                    *response.status_mut() = StatusCode::from_u16(204).expect("Unable to turn 204 into a StatusCode");
                                                },
                                            },
                                            Err(_) => {
                                                // Application code returned an error. This should not happen, as the implementation should
                                                // return a valid response.
                                                *response.status_mut() = StatusCode::INTERNAL_SERVER_ERROR;
                                                *response.body_mut() = Body::from("An internal error occurred");
                                            },
                                        }

                                        future::ok(response)
                                    }
                                ))
                            },
                            Err(e) => Box::new(future::ok(Response::builder()
                                                .status(StatusCode::BAD_REQUEST)
                                                .body(Body::from(format!("Couldn't read body parameter value: {}", e)))
                                                .expect("Unable to create Bad Request response due to unable to read body parameter value"))),
                        }
                    })
                ) as Self::Future
            },

            _ if path.matched(paths::ID_ALLOF) => method_not_allowed(),
            _ if path.matched(paths::ID_DUMMY) => method_not_allowed(),
            _ if path.matched(paths::ID_FILE_RESPONSE) => method_not_allowed(),
            _ if path.matched(paths::ID_GET_STRUCTURED_YAML) => method_not_allowed(),
            _ if path.matched(paths::ID_HTML) => method_not_allowed(),
            _ if path.matched(paths::ID_POST_YAML) => method_not_allowed(),
            _ if path.matched(paths::ID_RAW_JSON) => method_not_allowed(),
            _ if path.matched(paths::ID_SOLO_OBJECT) => method_not_allowed(),
            _ => Box::new(future::ok(
                Response::builder().status(StatusCode::NOT_FOUND)
                    .body(Body::empty())
                    .expect("Unable to create Not Found response")
            )) as Self::Future
        }
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

/// Request parser for `Api`.
pub struct ApiRequestParser;
impl<T> RequestParser<T> for ApiRequestParser {
    fn parse_operation_id(request: &Request<T>) -> Result<&'static str, ()> {
        let path = paths::GLOBAL_REGEX_SET.matches(request.uri().path());
        match request.method() {
            // AllOfGet - GET /allOf
            &hyper::Method::GET if path.matched(paths::ID_ALLOF) => Ok("AllOfGet"),
            // DummyGet - GET /dummy
            &hyper::Method::GET if path.matched(paths::ID_DUMMY) => Ok("DummyGet"),
            // DummyPut - PUT /dummy
            &hyper::Method::PUT if path.matched(paths::ID_DUMMY) => Ok("DummyPut"),
            // FileResponseGet - GET /file_response
            &hyper::Method::GET if path.matched(paths::ID_FILE_RESPONSE) => Ok("FileResponseGet"),
            // GetStructuredYaml - GET /get-structured-yaml
            &hyper::Method::GET if path.matched(paths::ID_GET_STRUCTURED_YAML) => Ok("GetStructuredYaml"),
            // HtmlPost - POST /html
            &hyper::Method::POST if path.matched(paths::ID_HTML) => Ok("HtmlPost"),
            // PostYaml - POST /post-yaml
            &hyper::Method::POST if path.matched(paths::ID_POST_YAML) => Ok("PostYaml"),
            // RawJsonGet - GET /raw_json
            &hyper::Method::GET if path.matched(paths::ID_RAW_JSON) => Ok("RawJsonGet"),
            // SoloObjectPost - POST /solo-object
            &hyper::Method::POST if path.matched(paths::ID_SOLO_OBJECT) => Ok("SoloObjectPost"),
            _ => Err(()),
        }
    }
}
