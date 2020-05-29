use std::marker::PhantomData;
use futures::{Future, future, Stream, stream};
use hyper;
use hyper::{Request, Response, Error, StatusCode, Body, HeaderMap};
use hyper::header::{HeaderName, HeaderValue, CONTENT_TYPE};
use log::warn;
use serde_json;
#[allow(unused_imports)]
use std::convert::{TryFrom, TryInto};
use std::io;
use url::form_urlencoded;
#[allow(unused_imports)]
use swagger;
use swagger::{ApiError, XSpanIdString, Has, RequestParser};
pub use swagger::auth::Authorization;
use swagger::auth::Scopes;
use swagger::context::ContextualPayload;
use uuid;
use serde_xml_rs;

#[allow(unused_imports)]
use crate::models;
use crate::header;

pub use crate::context;

use crate::CallbackApi as Api;
use crate::CallbackCallbackWithHeaderPostResponse;
use crate::CallbackCallbackPostResponse;

mod paths {
    use lazy_static::lazy_static;

    lazy_static! {
        pub static ref GLOBAL_REGEX_SET: regex::RegexSet = regex::RegexSet::new(vec![
            r"^/(?P<request_query_url>.*)/callback$",
            r"^/(?P<request_query_url>.*)/callback-with-header$"
        ])
        .expect("Unable to create global regex set");
    }
    pub(crate) static ID_REQUEST_QUERY_URL_CALLBACK: usize = 0;
    lazy_static! {
        pub static ref REGEX_REQUEST_QUERY_URL_CALLBACK: regex::Regex =
            regex::Regex::new(r"^/(?P<request_query_url>.*)/callback$")
                .expect("Unable to create regex for REQUEST_QUERY_URL_CALLBACK");
    }
    pub(crate) static ID_REQUEST_QUERY_URL_CALLBACK_WITH_HEADER: usize = 1;
    lazy_static! {
        pub static ref REGEX_REQUEST_QUERY_URL_CALLBACK_WITH_HEADER: regex::Regex =
            regex::Regex::new(r"^/(?P<request_query_url>.*)/callback-with-header$")
                .expect("Unable to create regex for REQUEST_QUERY_URL_CALLBACK_WITH_HEADER");
    }
}


pub struct MakeService<T, RC> {
    api_impl: T,
    marker: PhantomData<RC>,
}

impl<T, RC> MakeService<T, RC>
where
    T: Api<RC> + Clone + Send + 'static,
    RC: Has<XSpanIdString> + Has<Option<Authorization>> + 'static
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
    RC: Has<XSpanIdString> + Has<Option<Authorization>> + 'static + Send
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
    RC: Has<XSpanIdString> + Has<Option<Authorization>> + 'static {
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
    C: Has<XSpanIdString> + Has<Option<Authorization>> + 'static + Send
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

            // CallbackCallbackWithHeaderPost - POST /{$request.query.url}/callback-with-header
            &hyper::Method::POST if path.matched(paths::ID_REQUEST_QUERY_URL_CALLBACK_WITH_HEADER) => {
                // Path parameters
                let path: &str = &uri.path().to_string();
                let path_params =
                    paths::REGEX_REQUEST_QUERY_URL_CALLBACK_WITH_HEADER
                    .captures(&path)
                    .unwrap_or_else(||
                        panic!("Path {} matched RE REQUEST_QUERY_URL_CALLBACK_WITH_HEADER in set but failed match against \"{}\"", path, paths::REGEX_REQUEST_QUERY_URL_CALLBACK_WITH_HEADER.as_str())
                    );

                let callback_request_query_url = path_params["request_query_url"].to_string();
                // Header parameters
                let param_information = headers.get(HeaderName::from_static("information"));

                let param_information = match param_information {
                    Some(v) => match header::IntoHeaderValue::<String>::try_from((*v).clone()) {
                        Ok(result) =>
                            Some(result.0),
                        Err(err) => {
                            return Box::new(future::ok(Response::builder()
                                        .status(StatusCode::BAD_REQUEST)
                                        .body(Body::from(format!("Invalid header Information - {}", err)))
                                        .expect("Unable to create Bad Request response for invalid header Information")));

                        },
                    },
                    None => {
                        None
                    }
                };

                Box::new({
                        {{
                                Box::new(
                                    api_impl.callback_callback_with_header_post(
                                            callback_request_query_url,
                                            param_information,
                                        &context
                                    ).then(move |result| {
                                        let mut response = Response::new(Body::empty());
                                        response.headers_mut().insert(
                                            HeaderName::from_static("x-span-id"),
                                            HeaderValue::from_str((&context as &dyn Has<XSpanIdString>).get().0.clone().to_string().as_str())
                                                .expect("Unable to create X-Span-ID header value"));

                                        match result {
                                            Ok(rsp) => match rsp {
                                                CallbackCallbackWithHeaderPostResponse::OK
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
                        }}
                }) as Self::Future
            },

            // CallbackCallbackPost - POST /{$request.query.url}/callback
            &hyper::Method::POST if path.matched(paths::ID_REQUEST_QUERY_URL_CALLBACK) => {
                // Path parameters
                let path: &str = &uri.path().to_string();
                let path_params =
                    paths::REGEX_REQUEST_QUERY_URL_CALLBACK
                    .captures(&path)
                    .unwrap_or_else(||
                        panic!("Path {} matched RE REQUEST_QUERY_URL_CALLBACK in set but failed match against \"{}\"", path, paths::REGEX_REQUEST_QUERY_URL_CALLBACK.as_str())
                    );

                let callback_request_query_url = path_params["request_query_url"].to_string();
                Box::new({
                        {{
                                Box::new(
                                    api_impl.callback_callback_post(
                                            callback_request_query_url,
                                        &context
                                    ).then(move |result| {
                                        let mut response = Response::new(Body::empty());
                                        response.headers_mut().insert(
                                            HeaderName::from_static("x-span-id"),
                                            HeaderValue::from_str((&context as &dyn Has<XSpanIdString>).get().0.clone().to_string().as_str())
                                                .expect("Unable to create X-Span-ID header value"));

                                        match result {
                                            Ok(rsp) => match rsp {
                                                CallbackCallbackPostResponse::OK
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
                        }}
                }) as Self::Future
            },

            _ if path.matched(paths::ID_REQUEST_QUERY_URL_CALLBACK) => method_not_allowed(),
            _ if path.matched(paths::ID_REQUEST_QUERY_URL_CALLBACK_WITH_HEADER) => method_not_allowed(),
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
            // CallbackCallbackWithHeaderPost - POST /{$request.query.url}/callback-with-header
            &hyper::Method::POST if path.matched(paths::ID_REQUEST_QUERY_URL_CALLBACK_WITH_HEADER) => Ok("CallbackCallbackWithHeaderPost"),
            // CallbackCallbackPost - POST /{$request.query.url}/callback
            &hyper::Method::POST if path.matched(paths::ID_REQUEST_QUERY_URL_CALLBACK) => Ok("CallbackCallbackPost"),
            _ => Err(()),
        }
    }
}
