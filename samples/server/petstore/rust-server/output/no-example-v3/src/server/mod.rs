#[allow(unused_imports)]
use std::collections::{HashMap, BTreeMap, BTreeSet};
use std::marker::PhantomData;
use futures::{Future, future, Stream, stream};
use hyper;
use hyper::{Request, Response, Error, StatusCode, Body, HeaderMap};
use hyper::header::{HeaderName, HeaderValue, CONTENT_TYPE};
use url::form_urlencoded;
use serde_json;
use std::io;
#[allow(unused_imports)]
use swagger;
use swagger::{ApiError, XSpanIdString, Has, RequestParser};
pub use swagger::auth::Authorization;
use swagger::auth::Scopes;
use swagger::context::ContextualPayload;

#[allow(unused_imports)]
use models;
use header;

pub use crate::context;

use {Api,
     OpGetResponse
};

mod paths {
    extern crate regex;

    lazy_static! {
        pub static ref GLOBAL_REGEX_SET: regex::RegexSet = regex::RegexSet::new(vec![
            r"^/op$"
        ])
        .expect("Unable to create global regex set");
    }
    pub static ID_OP: usize = 0;
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

            // OpGet - GET /op
            &hyper::Method::GET if path.matched(paths::ID_OP) => {
                // Body parameters (note that non-required body parameters will ignore garbage
                // values, rather than causing a 400 response). Produce warning header and logs for
                // any unused fields.
                Box::new(body.concat2()
                    .then(move |result| -> Self::Future {
                        match result {
                            Ok(body) => {
                                let mut unused_elements = Vec::new();
                                let param_inline_object: Option<models::InlineObject> = if !body.is_empty() {
                                    let deserializer = &mut serde_json::Deserializer::from_slice(&*body);
                                    match serde_ignored::deserialize(deserializer, |path| {
                                            warn!("Ignoring unknown field in body: {}", path);
                                            unused_elements.push(path.to_string());
                                    }) {
                                        Ok(param_inline_object) => param_inline_object,
                                        Err(e) => return Box::new(future::ok(Response::builder()
                                                        .status(StatusCode::BAD_REQUEST)
                                                        .body(Body::from(format!("Couldn't parse body parameter InlineObject - doesn't match schema: {}", e)))
                                                        .expect("Unable to create Bad Request response for invalid body parameter InlineObject due to schema"))),
                                    }
                                } else {
                                    None
                                };
                                let param_inline_object = match param_inline_object {
                                    Some(param_inline_object) => param_inline_object,
                                    None => return Box::new(future::ok(Response::builder()
                                                        .status(StatusCode::BAD_REQUEST)
                                                        .body(Body::from("Missing required body parameter InlineObject"))
                                                        .expect("Unable to create Bad Request response for missing body parameter InlineObject"))),
                                };

                                Box::new(
                                    api_impl.op_get(
                                            param_inline_object,
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
                                                OpGetResponse::OK
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
                                                .body(Body::from(format!("Couldn't read body parameter InlineObject: {}", e)))
                                                .expect("Unable to create Bad Request response due to unable to read body parameter InlineObject"))),
                        }
                    })
                ) as Self::Future
            },

            _ if path.matched(paths::ID_OP) => method_not_allowed(),
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
            // OpGet - GET /op
            &hyper::Method::GET if path.matched(paths::ID_OP) => Ok("OpGet"),
            _ => Err(()),
        }
    }
}
