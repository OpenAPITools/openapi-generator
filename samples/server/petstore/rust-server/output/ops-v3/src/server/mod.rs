use futures::{future, future::BoxFuture, Stream, stream, future::FutureExt, stream::TryStreamExt};
use hyper::{Request, Response, StatusCode, Body, HeaderMap};
use hyper::header::{HeaderName, HeaderValue, CONTENT_TYPE};
use log::warn;
#[allow(unused_imports)]
use std::convert::{TryFrom, TryInto};
use std::error::Error;
use std::future::Future;
use std::marker::PhantomData;
use std::task::{Context, Poll};
use swagger::{ApiError, BodyExt, Has, RequestParser, XSpanIdString};
pub use swagger::auth::Authorization;
use swagger::auth::Scopes;
use url::form_urlencoded;

#[allow(unused_imports)]
use crate::models;
use crate::header;

pub use crate::context;

type ServiceFuture = BoxFuture<'static, Result<Response<Body>, crate::ServiceError>>;

use crate::{Api,
     Op10GetResponse,
     Op11GetResponse,
     Op12GetResponse,
     Op13GetResponse,
     Op14GetResponse,
     Op15GetResponse,
     Op16GetResponse,
     Op17GetResponse,
     Op18GetResponse,
     Op19GetResponse,
     Op1GetResponse,
     Op20GetResponse,
     Op21GetResponse,
     Op22GetResponse,
     Op23GetResponse,
     Op24GetResponse,
     Op25GetResponse,
     Op26GetResponse,
     Op27GetResponse,
     Op28GetResponse,
     Op29GetResponse,
     Op2GetResponse,
     Op30GetResponse,
     Op31GetResponse,
     Op32GetResponse,
     Op33GetResponse,
     Op34GetResponse,
     Op35GetResponse,
     Op36GetResponse,
     Op37GetResponse,
     Op3GetResponse,
     Op4GetResponse,
     Op5GetResponse,
     Op6GetResponse,
     Op7GetResponse,
     Op8GetResponse,
     Op9GetResponse
};

mod paths {
    use lazy_static::lazy_static;

    lazy_static! {
        pub static ref GLOBAL_REGEX_SET: regex::RegexSet = regex::RegexSet::new(vec![
            r"^/op1$",
            r"^/op10$",
            r"^/op11$",
            r"^/op12$",
            r"^/op13$",
            r"^/op14$",
            r"^/op15$",
            r"^/op16$",
            r"^/op17$",
            r"^/op18$",
            r"^/op19$",
            r"^/op2$",
            r"^/op20$",
            r"^/op21$",
            r"^/op22$",
            r"^/op23$",
            r"^/op24$",
            r"^/op25$",
            r"^/op26$",
            r"^/op27$",
            r"^/op28$",
            r"^/op29$",
            r"^/op3$",
            r"^/op30$",
            r"^/op31$",
            r"^/op32$",
            r"^/op33$",
            r"^/op34$",
            r"^/op35$",
            r"^/op36$",
            r"^/op37$",
            r"^/op4$",
            r"^/op5$",
            r"^/op6$",
            r"^/op7$",
            r"^/op8$",
            r"^/op9$"
        ])
        .expect("Unable to create global regex set");
    }
    pub(crate) static ID_OP1: usize = 0;
    pub(crate) static ID_OP10: usize = 1;
    pub(crate) static ID_OP11: usize = 2;
    pub(crate) static ID_OP12: usize = 3;
    pub(crate) static ID_OP13: usize = 4;
    pub(crate) static ID_OP14: usize = 5;
    pub(crate) static ID_OP15: usize = 6;
    pub(crate) static ID_OP16: usize = 7;
    pub(crate) static ID_OP17: usize = 8;
    pub(crate) static ID_OP18: usize = 9;
    pub(crate) static ID_OP19: usize = 10;
    pub(crate) static ID_OP2: usize = 11;
    pub(crate) static ID_OP20: usize = 12;
    pub(crate) static ID_OP21: usize = 13;
    pub(crate) static ID_OP22: usize = 14;
    pub(crate) static ID_OP23: usize = 15;
    pub(crate) static ID_OP24: usize = 16;
    pub(crate) static ID_OP25: usize = 17;
    pub(crate) static ID_OP26: usize = 18;
    pub(crate) static ID_OP27: usize = 19;
    pub(crate) static ID_OP28: usize = 20;
    pub(crate) static ID_OP29: usize = 21;
    pub(crate) static ID_OP3: usize = 22;
    pub(crate) static ID_OP30: usize = 23;
    pub(crate) static ID_OP31: usize = 24;
    pub(crate) static ID_OP32: usize = 25;
    pub(crate) static ID_OP33: usize = 26;
    pub(crate) static ID_OP34: usize = 27;
    pub(crate) static ID_OP35: usize = 28;
    pub(crate) static ID_OP36: usize = 29;
    pub(crate) static ID_OP37: usize = 30;
    pub(crate) static ID_OP4: usize = 31;
    pub(crate) static ID_OP5: usize = 32;
    pub(crate) static ID_OP6: usize = 33;
    pub(crate) static ID_OP7: usize = 34;
    pub(crate) static ID_OP8: usize = 35;
    pub(crate) static ID_OP9: usize = 36;
}

pub struct MakeService<T, C> where
    T: Api<C> + Clone + Send + 'static,
    C: Has<XSpanIdString>  + Send + Sync + 'static
{
    api_impl: T,
    marker: PhantomData<C>,
}

impl<T, C> MakeService<T, C> where
    T: Api<C> + Clone + Send + 'static,
    C: Has<XSpanIdString>  + Send + Sync + 'static
{
    pub fn new(api_impl: T) -> Self {
        MakeService {
            api_impl,
            marker: PhantomData
        }
    }
}

impl<T, C, Target> hyper::service::Service<Target> for MakeService<T, C> where
    T: Api<C> + Clone + Send + 'static,
    C: Has<XSpanIdString>  + Send + Sync + 'static
{
    type Response = Service<T, C>;
    type Error = crate::ServiceError;
    type Future = future::Ready<Result<Self::Response, Self::Error>>;

    fn poll_ready(&mut self, cx: &mut Context<'_>) -> Poll<Result<(), Self::Error>> {
        Poll::Ready(Ok(()))
    }

    fn call(&mut self, target: Target) -> Self::Future {
        futures::future::ok(Service::new(
            self.api_impl.clone(),
        ))
    }
}

fn method_not_allowed() -> Result<Response<Body>, crate::ServiceError> {
    Ok(
        Response::builder().status(StatusCode::METHOD_NOT_ALLOWED)
            .body(Body::empty())
            .expect("Unable to create Method Not Allowed response")
    )
}

pub struct Service<T, C> where
    T: Api<C> + Clone + Send + 'static,
    C: Has<XSpanIdString>  + Send + Sync + 'static
{
    api_impl: T,
    marker: PhantomData<C>,
}

impl<T, C> Service<T, C> where
    T: Api<C> + Clone + Send + 'static,
    C: Has<XSpanIdString>  + Send + Sync + 'static
{
    pub fn new(api_impl: T) -> Self {
        Service {
            api_impl,
            marker: PhantomData
        }
    }
}

impl<T, C> Clone for Service<T, C> where
    T: Api<C> + Clone + Send + 'static,
    C: Has<XSpanIdString>  + Send + Sync + 'static
{
    fn clone(&self) -> Self {
        Service {
            api_impl: self.api_impl.clone(),
            marker: self.marker,
        }
    }
}

impl<T, C> hyper::service::Service<(Request<Body>, C)> for Service<T, C> where
    T: Api<C> + Clone + Send + Sync + 'static,
    C: Has<XSpanIdString>  + Send + Sync + 'static
{
    type Response = Response<Body>;
    type Error = crate::ServiceError;
    type Future = ServiceFuture;

    fn poll_ready(&mut self, cx: &mut Context) -> Poll<Result<(), Self::Error>> {
        self.api_impl.poll_ready(cx)
    }

    fn call(&mut self, req: (Request<Body>, C)) -> Self::Future { async fn run<T, C>(mut api_impl: T, req: (Request<Body>, C)) -> Result<Response<Body>, crate::ServiceError> where
        T: Api<C> + Clone + Send + 'static,
        C: Has<XSpanIdString>  + Send + Sync + 'static
    {
        let (request, context) = req;
        let (parts, body) = request.into_parts();
        let (method, uri, headers) = (parts.method, parts.uri, parts.headers);
        let path = paths::GLOBAL_REGEX_SET.matches(uri.path());

        match method {

            // Op10Get - GET /op10
            hyper::Method::GET if path.matched(paths::ID_OP10) => {
                                let result = api_impl.op10_get(
                                        &context
                                    ).await;
                                let mut response = Response::new(Body::empty());
                                response.headers_mut().insert(
                                            HeaderName::from_static("x-span-id"),
                                            HeaderValue::from_str((&context as &dyn Has<XSpanIdString>).get().0.clone().as_str())
                                                .expect("Unable to create X-Span-ID header value"));

                                        match result {
                                            Ok(rsp) => match rsp {
                                                Op10GetResponse::OK
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

                                        Ok(response)
            },

            // Op11Get - GET /op11
            hyper::Method::GET if path.matched(paths::ID_OP11) => {
                                let result = api_impl.op11_get(
                                        &context
                                    ).await;
                                let mut response = Response::new(Body::empty());
                                response.headers_mut().insert(
                                            HeaderName::from_static("x-span-id"),
                                            HeaderValue::from_str((&context as &dyn Has<XSpanIdString>).get().0.clone().as_str())
                                                .expect("Unable to create X-Span-ID header value"));

                                        match result {
                                            Ok(rsp) => match rsp {
                                                Op11GetResponse::OK
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

                                        Ok(response)
            },

            // Op12Get - GET /op12
            hyper::Method::GET if path.matched(paths::ID_OP12) => {
                                let result = api_impl.op12_get(
                                        &context
                                    ).await;
                                let mut response = Response::new(Body::empty());
                                response.headers_mut().insert(
                                            HeaderName::from_static("x-span-id"),
                                            HeaderValue::from_str((&context as &dyn Has<XSpanIdString>).get().0.clone().as_str())
                                                .expect("Unable to create X-Span-ID header value"));

                                        match result {
                                            Ok(rsp) => match rsp {
                                                Op12GetResponse::OK
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

                                        Ok(response)
            },

            // Op13Get - GET /op13
            hyper::Method::GET if path.matched(paths::ID_OP13) => {
                                let result = api_impl.op13_get(
                                        &context
                                    ).await;
                                let mut response = Response::new(Body::empty());
                                response.headers_mut().insert(
                                            HeaderName::from_static("x-span-id"),
                                            HeaderValue::from_str((&context as &dyn Has<XSpanIdString>).get().0.clone().as_str())
                                                .expect("Unable to create X-Span-ID header value"));

                                        match result {
                                            Ok(rsp) => match rsp {
                                                Op13GetResponse::OK
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

                                        Ok(response)
            },

            // Op14Get - GET /op14
            hyper::Method::GET if path.matched(paths::ID_OP14) => {
                                let result = api_impl.op14_get(
                                        &context
                                    ).await;
                                let mut response = Response::new(Body::empty());
                                response.headers_mut().insert(
                                            HeaderName::from_static("x-span-id"),
                                            HeaderValue::from_str((&context as &dyn Has<XSpanIdString>).get().0.clone().as_str())
                                                .expect("Unable to create X-Span-ID header value"));

                                        match result {
                                            Ok(rsp) => match rsp {
                                                Op14GetResponse::OK
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

                                        Ok(response)
            },

            // Op15Get - GET /op15
            hyper::Method::GET if path.matched(paths::ID_OP15) => {
                                let result = api_impl.op15_get(
                                        &context
                                    ).await;
                                let mut response = Response::new(Body::empty());
                                response.headers_mut().insert(
                                            HeaderName::from_static("x-span-id"),
                                            HeaderValue::from_str((&context as &dyn Has<XSpanIdString>).get().0.clone().as_str())
                                                .expect("Unable to create X-Span-ID header value"));

                                        match result {
                                            Ok(rsp) => match rsp {
                                                Op15GetResponse::OK
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

                                        Ok(response)
            },

            // Op16Get - GET /op16
            hyper::Method::GET if path.matched(paths::ID_OP16) => {
                                let result = api_impl.op16_get(
                                        &context
                                    ).await;
                                let mut response = Response::new(Body::empty());
                                response.headers_mut().insert(
                                            HeaderName::from_static("x-span-id"),
                                            HeaderValue::from_str((&context as &dyn Has<XSpanIdString>).get().0.clone().as_str())
                                                .expect("Unable to create X-Span-ID header value"));

                                        match result {
                                            Ok(rsp) => match rsp {
                                                Op16GetResponse::OK
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

                                        Ok(response)
            },

            // Op17Get - GET /op17
            hyper::Method::GET if path.matched(paths::ID_OP17) => {
                                let result = api_impl.op17_get(
                                        &context
                                    ).await;
                                let mut response = Response::new(Body::empty());
                                response.headers_mut().insert(
                                            HeaderName::from_static("x-span-id"),
                                            HeaderValue::from_str((&context as &dyn Has<XSpanIdString>).get().0.clone().as_str())
                                                .expect("Unable to create X-Span-ID header value"));

                                        match result {
                                            Ok(rsp) => match rsp {
                                                Op17GetResponse::OK
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

                                        Ok(response)
            },

            // Op18Get - GET /op18
            hyper::Method::GET if path.matched(paths::ID_OP18) => {
                                let result = api_impl.op18_get(
                                        &context
                                    ).await;
                                let mut response = Response::new(Body::empty());
                                response.headers_mut().insert(
                                            HeaderName::from_static("x-span-id"),
                                            HeaderValue::from_str((&context as &dyn Has<XSpanIdString>).get().0.clone().as_str())
                                                .expect("Unable to create X-Span-ID header value"));

                                        match result {
                                            Ok(rsp) => match rsp {
                                                Op18GetResponse::OK
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

                                        Ok(response)
            },

            // Op19Get - GET /op19
            hyper::Method::GET if path.matched(paths::ID_OP19) => {
                                let result = api_impl.op19_get(
                                        &context
                                    ).await;
                                let mut response = Response::new(Body::empty());
                                response.headers_mut().insert(
                                            HeaderName::from_static("x-span-id"),
                                            HeaderValue::from_str((&context as &dyn Has<XSpanIdString>).get().0.clone().as_str())
                                                .expect("Unable to create X-Span-ID header value"));

                                        match result {
                                            Ok(rsp) => match rsp {
                                                Op19GetResponse::OK
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

                                        Ok(response)
            },

            // Op1Get - GET /op1
            hyper::Method::GET if path.matched(paths::ID_OP1) => {
                                let result = api_impl.op1_get(
                                        &context
                                    ).await;
                                let mut response = Response::new(Body::empty());
                                response.headers_mut().insert(
                                            HeaderName::from_static("x-span-id"),
                                            HeaderValue::from_str((&context as &dyn Has<XSpanIdString>).get().0.clone().as_str())
                                                .expect("Unable to create X-Span-ID header value"));

                                        match result {
                                            Ok(rsp) => match rsp {
                                                Op1GetResponse::OK
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

                                        Ok(response)
            },

            // Op20Get - GET /op20
            hyper::Method::GET if path.matched(paths::ID_OP20) => {
                                let result = api_impl.op20_get(
                                        &context
                                    ).await;
                                let mut response = Response::new(Body::empty());
                                response.headers_mut().insert(
                                            HeaderName::from_static("x-span-id"),
                                            HeaderValue::from_str((&context as &dyn Has<XSpanIdString>).get().0.clone().as_str())
                                                .expect("Unable to create X-Span-ID header value"));

                                        match result {
                                            Ok(rsp) => match rsp {
                                                Op20GetResponse::OK
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

                                        Ok(response)
            },

            // Op21Get - GET /op21
            hyper::Method::GET if path.matched(paths::ID_OP21) => {
                                let result = api_impl.op21_get(
                                        &context
                                    ).await;
                                let mut response = Response::new(Body::empty());
                                response.headers_mut().insert(
                                            HeaderName::from_static("x-span-id"),
                                            HeaderValue::from_str((&context as &dyn Has<XSpanIdString>).get().0.clone().as_str())
                                                .expect("Unable to create X-Span-ID header value"));

                                        match result {
                                            Ok(rsp) => match rsp {
                                                Op21GetResponse::OK
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

                                        Ok(response)
            },

            // Op22Get - GET /op22
            hyper::Method::GET if path.matched(paths::ID_OP22) => {
                                let result = api_impl.op22_get(
                                        &context
                                    ).await;
                                let mut response = Response::new(Body::empty());
                                response.headers_mut().insert(
                                            HeaderName::from_static("x-span-id"),
                                            HeaderValue::from_str((&context as &dyn Has<XSpanIdString>).get().0.clone().as_str())
                                                .expect("Unable to create X-Span-ID header value"));

                                        match result {
                                            Ok(rsp) => match rsp {
                                                Op22GetResponse::OK
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

                                        Ok(response)
            },

            // Op23Get - GET /op23
            hyper::Method::GET if path.matched(paths::ID_OP23) => {
                                let result = api_impl.op23_get(
                                        &context
                                    ).await;
                                let mut response = Response::new(Body::empty());
                                response.headers_mut().insert(
                                            HeaderName::from_static("x-span-id"),
                                            HeaderValue::from_str((&context as &dyn Has<XSpanIdString>).get().0.clone().as_str())
                                                .expect("Unable to create X-Span-ID header value"));

                                        match result {
                                            Ok(rsp) => match rsp {
                                                Op23GetResponse::OK
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

                                        Ok(response)
            },

            // Op24Get - GET /op24
            hyper::Method::GET if path.matched(paths::ID_OP24) => {
                                let result = api_impl.op24_get(
                                        &context
                                    ).await;
                                let mut response = Response::new(Body::empty());
                                response.headers_mut().insert(
                                            HeaderName::from_static("x-span-id"),
                                            HeaderValue::from_str((&context as &dyn Has<XSpanIdString>).get().0.clone().as_str())
                                                .expect("Unable to create X-Span-ID header value"));

                                        match result {
                                            Ok(rsp) => match rsp {
                                                Op24GetResponse::OK
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

                                        Ok(response)
            },

            // Op25Get - GET /op25
            hyper::Method::GET if path.matched(paths::ID_OP25) => {
                                let result = api_impl.op25_get(
                                        &context
                                    ).await;
                                let mut response = Response::new(Body::empty());
                                response.headers_mut().insert(
                                            HeaderName::from_static("x-span-id"),
                                            HeaderValue::from_str((&context as &dyn Has<XSpanIdString>).get().0.clone().as_str())
                                                .expect("Unable to create X-Span-ID header value"));

                                        match result {
                                            Ok(rsp) => match rsp {
                                                Op25GetResponse::OK
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

                                        Ok(response)
            },

            // Op26Get - GET /op26
            hyper::Method::GET if path.matched(paths::ID_OP26) => {
                                let result = api_impl.op26_get(
                                        &context
                                    ).await;
                                let mut response = Response::new(Body::empty());
                                response.headers_mut().insert(
                                            HeaderName::from_static("x-span-id"),
                                            HeaderValue::from_str((&context as &dyn Has<XSpanIdString>).get().0.clone().as_str())
                                                .expect("Unable to create X-Span-ID header value"));

                                        match result {
                                            Ok(rsp) => match rsp {
                                                Op26GetResponse::OK
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

                                        Ok(response)
            },

            // Op27Get - GET /op27
            hyper::Method::GET if path.matched(paths::ID_OP27) => {
                                let result = api_impl.op27_get(
                                        &context
                                    ).await;
                                let mut response = Response::new(Body::empty());
                                response.headers_mut().insert(
                                            HeaderName::from_static("x-span-id"),
                                            HeaderValue::from_str((&context as &dyn Has<XSpanIdString>).get().0.clone().as_str())
                                                .expect("Unable to create X-Span-ID header value"));

                                        match result {
                                            Ok(rsp) => match rsp {
                                                Op27GetResponse::OK
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

                                        Ok(response)
            },

            // Op28Get - GET /op28
            hyper::Method::GET if path.matched(paths::ID_OP28) => {
                                let result = api_impl.op28_get(
                                        &context
                                    ).await;
                                let mut response = Response::new(Body::empty());
                                response.headers_mut().insert(
                                            HeaderName::from_static("x-span-id"),
                                            HeaderValue::from_str((&context as &dyn Has<XSpanIdString>).get().0.clone().as_str())
                                                .expect("Unable to create X-Span-ID header value"));

                                        match result {
                                            Ok(rsp) => match rsp {
                                                Op28GetResponse::OK
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

                                        Ok(response)
            },

            // Op29Get - GET /op29
            hyper::Method::GET if path.matched(paths::ID_OP29) => {
                                let result = api_impl.op29_get(
                                        &context
                                    ).await;
                                let mut response = Response::new(Body::empty());
                                response.headers_mut().insert(
                                            HeaderName::from_static("x-span-id"),
                                            HeaderValue::from_str((&context as &dyn Has<XSpanIdString>).get().0.clone().as_str())
                                                .expect("Unable to create X-Span-ID header value"));

                                        match result {
                                            Ok(rsp) => match rsp {
                                                Op29GetResponse::OK
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

                                        Ok(response)
            },

            // Op2Get - GET /op2
            hyper::Method::GET if path.matched(paths::ID_OP2) => {
                                let result = api_impl.op2_get(
                                        &context
                                    ).await;
                                let mut response = Response::new(Body::empty());
                                response.headers_mut().insert(
                                            HeaderName::from_static("x-span-id"),
                                            HeaderValue::from_str((&context as &dyn Has<XSpanIdString>).get().0.clone().as_str())
                                                .expect("Unable to create X-Span-ID header value"));

                                        match result {
                                            Ok(rsp) => match rsp {
                                                Op2GetResponse::OK
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

                                        Ok(response)
            },

            // Op30Get - GET /op30
            hyper::Method::GET if path.matched(paths::ID_OP30) => {
                                let result = api_impl.op30_get(
                                        &context
                                    ).await;
                                let mut response = Response::new(Body::empty());
                                response.headers_mut().insert(
                                            HeaderName::from_static("x-span-id"),
                                            HeaderValue::from_str((&context as &dyn Has<XSpanIdString>).get().0.clone().as_str())
                                                .expect("Unable to create X-Span-ID header value"));

                                        match result {
                                            Ok(rsp) => match rsp {
                                                Op30GetResponse::OK
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

                                        Ok(response)
            },

            // Op31Get - GET /op31
            hyper::Method::GET if path.matched(paths::ID_OP31) => {
                                let result = api_impl.op31_get(
                                        &context
                                    ).await;
                                let mut response = Response::new(Body::empty());
                                response.headers_mut().insert(
                                            HeaderName::from_static("x-span-id"),
                                            HeaderValue::from_str((&context as &dyn Has<XSpanIdString>).get().0.clone().as_str())
                                                .expect("Unable to create X-Span-ID header value"));

                                        match result {
                                            Ok(rsp) => match rsp {
                                                Op31GetResponse::OK
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

                                        Ok(response)
            },

            // Op32Get - GET /op32
            hyper::Method::GET if path.matched(paths::ID_OP32) => {
                                let result = api_impl.op32_get(
                                        &context
                                    ).await;
                                let mut response = Response::new(Body::empty());
                                response.headers_mut().insert(
                                            HeaderName::from_static("x-span-id"),
                                            HeaderValue::from_str((&context as &dyn Has<XSpanIdString>).get().0.clone().as_str())
                                                .expect("Unable to create X-Span-ID header value"));

                                        match result {
                                            Ok(rsp) => match rsp {
                                                Op32GetResponse::OK
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

                                        Ok(response)
            },

            // Op33Get - GET /op33
            hyper::Method::GET if path.matched(paths::ID_OP33) => {
                                let result = api_impl.op33_get(
                                        &context
                                    ).await;
                                let mut response = Response::new(Body::empty());
                                response.headers_mut().insert(
                                            HeaderName::from_static("x-span-id"),
                                            HeaderValue::from_str((&context as &dyn Has<XSpanIdString>).get().0.clone().as_str())
                                                .expect("Unable to create X-Span-ID header value"));

                                        match result {
                                            Ok(rsp) => match rsp {
                                                Op33GetResponse::OK
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

                                        Ok(response)
            },

            // Op34Get - GET /op34
            hyper::Method::GET if path.matched(paths::ID_OP34) => {
                                let result = api_impl.op34_get(
                                        &context
                                    ).await;
                                let mut response = Response::new(Body::empty());
                                response.headers_mut().insert(
                                            HeaderName::from_static("x-span-id"),
                                            HeaderValue::from_str((&context as &dyn Has<XSpanIdString>).get().0.clone().as_str())
                                                .expect("Unable to create X-Span-ID header value"));

                                        match result {
                                            Ok(rsp) => match rsp {
                                                Op34GetResponse::OK
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

                                        Ok(response)
            },

            // Op35Get - GET /op35
            hyper::Method::GET if path.matched(paths::ID_OP35) => {
                                let result = api_impl.op35_get(
                                        &context
                                    ).await;
                                let mut response = Response::new(Body::empty());
                                response.headers_mut().insert(
                                            HeaderName::from_static("x-span-id"),
                                            HeaderValue::from_str((&context as &dyn Has<XSpanIdString>).get().0.clone().as_str())
                                                .expect("Unable to create X-Span-ID header value"));

                                        match result {
                                            Ok(rsp) => match rsp {
                                                Op35GetResponse::OK
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

                                        Ok(response)
            },

            // Op36Get - GET /op36
            hyper::Method::GET if path.matched(paths::ID_OP36) => {
                                let result = api_impl.op36_get(
                                        &context
                                    ).await;
                                let mut response = Response::new(Body::empty());
                                response.headers_mut().insert(
                                            HeaderName::from_static("x-span-id"),
                                            HeaderValue::from_str((&context as &dyn Has<XSpanIdString>).get().0.clone().as_str())
                                                .expect("Unable to create X-Span-ID header value"));

                                        match result {
                                            Ok(rsp) => match rsp {
                                                Op36GetResponse::OK
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

                                        Ok(response)
            },

            // Op37Get - GET /op37
            hyper::Method::GET if path.matched(paths::ID_OP37) => {
                                let result = api_impl.op37_get(
                                        &context
                                    ).await;
                                let mut response = Response::new(Body::empty());
                                response.headers_mut().insert(
                                            HeaderName::from_static("x-span-id"),
                                            HeaderValue::from_str((&context as &dyn Has<XSpanIdString>).get().0.clone().as_str())
                                                .expect("Unable to create X-Span-ID header value"));

                                        match result {
                                            Ok(rsp) => match rsp {
                                                Op37GetResponse::OK
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

                                        Ok(response)
            },

            // Op3Get - GET /op3
            hyper::Method::GET if path.matched(paths::ID_OP3) => {
                                let result = api_impl.op3_get(
                                        &context
                                    ).await;
                                let mut response = Response::new(Body::empty());
                                response.headers_mut().insert(
                                            HeaderName::from_static("x-span-id"),
                                            HeaderValue::from_str((&context as &dyn Has<XSpanIdString>).get().0.clone().as_str())
                                                .expect("Unable to create X-Span-ID header value"));

                                        match result {
                                            Ok(rsp) => match rsp {
                                                Op3GetResponse::OK
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

                                        Ok(response)
            },

            // Op4Get - GET /op4
            hyper::Method::GET if path.matched(paths::ID_OP4) => {
                                let result = api_impl.op4_get(
                                        &context
                                    ).await;
                                let mut response = Response::new(Body::empty());
                                response.headers_mut().insert(
                                            HeaderName::from_static("x-span-id"),
                                            HeaderValue::from_str((&context as &dyn Has<XSpanIdString>).get().0.clone().as_str())
                                                .expect("Unable to create X-Span-ID header value"));

                                        match result {
                                            Ok(rsp) => match rsp {
                                                Op4GetResponse::OK
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

                                        Ok(response)
            },

            // Op5Get - GET /op5
            hyper::Method::GET if path.matched(paths::ID_OP5) => {
                                let result = api_impl.op5_get(
                                        &context
                                    ).await;
                                let mut response = Response::new(Body::empty());
                                response.headers_mut().insert(
                                            HeaderName::from_static("x-span-id"),
                                            HeaderValue::from_str((&context as &dyn Has<XSpanIdString>).get().0.clone().as_str())
                                                .expect("Unable to create X-Span-ID header value"));

                                        match result {
                                            Ok(rsp) => match rsp {
                                                Op5GetResponse::OK
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

                                        Ok(response)
            },

            // Op6Get - GET /op6
            hyper::Method::GET if path.matched(paths::ID_OP6) => {
                                let result = api_impl.op6_get(
                                        &context
                                    ).await;
                                let mut response = Response::new(Body::empty());
                                response.headers_mut().insert(
                                            HeaderName::from_static("x-span-id"),
                                            HeaderValue::from_str((&context as &dyn Has<XSpanIdString>).get().0.clone().as_str())
                                                .expect("Unable to create X-Span-ID header value"));

                                        match result {
                                            Ok(rsp) => match rsp {
                                                Op6GetResponse::OK
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

                                        Ok(response)
            },

            // Op7Get - GET /op7
            hyper::Method::GET if path.matched(paths::ID_OP7) => {
                                let result = api_impl.op7_get(
                                        &context
                                    ).await;
                                let mut response = Response::new(Body::empty());
                                response.headers_mut().insert(
                                            HeaderName::from_static("x-span-id"),
                                            HeaderValue::from_str((&context as &dyn Has<XSpanIdString>).get().0.clone().as_str())
                                                .expect("Unable to create X-Span-ID header value"));

                                        match result {
                                            Ok(rsp) => match rsp {
                                                Op7GetResponse::OK
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

                                        Ok(response)
            },

            // Op8Get - GET /op8
            hyper::Method::GET if path.matched(paths::ID_OP8) => {
                                let result = api_impl.op8_get(
                                        &context
                                    ).await;
                                let mut response = Response::new(Body::empty());
                                response.headers_mut().insert(
                                            HeaderName::from_static("x-span-id"),
                                            HeaderValue::from_str((&context as &dyn Has<XSpanIdString>).get().0.clone().as_str())
                                                .expect("Unable to create X-Span-ID header value"));

                                        match result {
                                            Ok(rsp) => match rsp {
                                                Op8GetResponse::OK
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

                                        Ok(response)
            },

            // Op9Get - GET /op9
            hyper::Method::GET if path.matched(paths::ID_OP9) => {
                                let result = api_impl.op9_get(
                                        &context
                                    ).await;
                                let mut response = Response::new(Body::empty());
                                response.headers_mut().insert(
                                            HeaderName::from_static("x-span-id"),
                                            HeaderValue::from_str((&context as &dyn Has<XSpanIdString>).get().0.clone().as_str())
                                                .expect("Unable to create X-Span-ID header value"));

                                        match result {
                                            Ok(rsp) => match rsp {
                                                Op9GetResponse::OK
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

                                        Ok(response)
            },

            _ if path.matched(paths::ID_OP1) => method_not_allowed(),
            _ if path.matched(paths::ID_OP10) => method_not_allowed(),
            _ if path.matched(paths::ID_OP11) => method_not_allowed(),
            _ if path.matched(paths::ID_OP12) => method_not_allowed(),
            _ if path.matched(paths::ID_OP13) => method_not_allowed(),
            _ if path.matched(paths::ID_OP14) => method_not_allowed(),
            _ if path.matched(paths::ID_OP15) => method_not_allowed(),
            _ if path.matched(paths::ID_OP16) => method_not_allowed(),
            _ if path.matched(paths::ID_OP17) => method_not_allowed(),
            _ if path.matched(paths::ID_OP18) => method_not_allowed(),
            _ if path.matched(paths::ID_OP19) => method_not_allowed(),
            _ if path.matched(paths::ID_OP2) => method_not_allowed(),
            _ if path.matched(paths::ID_OP20) => method_not_allowed(),
            _ if path.matched(paths::ID_OP21) => method_not_allowed(),
            _ if path.matched(paths::ID_OP22) => method_not_allowed(),
            _ if path.matched(paths::ID_OP23) => method_not_allowed(),
            _ if path.matched(paths::ID_OP24) => method_not_allowed(),
            _ if path.matched(paths::ID_OP25) => method_not_allowed(),
            _ if path.matched(paths::ID_OP26) => method_not_allowed(),
            _ if path.matched(paths::ID_OP27) => method_not_allowed(),
            _ if path.matched(paths::ID_OP28) => method_not_allowed(),
            _ if path.matched(paths::ID_OP29) => method_not_allowed(),
            _ if path.matched(paths::ID_OP3) => method_not_allowed(),
            _ if path.matched(paths::ID_OP30) => method_not_allowed(),
            _ if path.matched(paths::ID_OP31) => method_not_allowed(),
            _ if path.matched(paths::ID_OP32) => method_not_allowed(),
            _ if path.matched(paths::ID_OP33) => method_not_allowed(),
            _ if path.matched(paths::ID_OP34) => method_not_allowed(),
            _ if path.matched(paths::ID_OP35) => method_not_allowed(),
            _ if path.matched(paths::ID_OP36) => method_not_allowed(),
            _ if path.matched(paths::ID_OP37) => method_not_allowed(),
            _ if path.matched(paths::ID_OP4) => method_not_allowed(),
            _ if path.matched(paths::ID_OP5) => method_not_allowed(),
            _ if path.matched(paths::ID_OP6) => method_not_allowed(),
            _ if path.matched(paths::ID_OP7) => method_not_allowed(),
            _ if path.matched(paths::ID_OP8) => method_not_allowed(),
            _ if path.matched(paths::ID_OP9) => method_not_allowed(),
            _ => Ok(Response::builder().status(StatusCode::NOT_FOUND)
                    .body(Body::empty())
                    .expect("Unable to create Not Found response"))
        }
    } Box::pin(run(self.api_impl.clone(), req)) }
}

/// Request parser for `Api`.
pub struct ApiRequestParser;
impl<T> RequestParser<T> for ApiRequestParser {
    fn parse_operation_id(request: &Request<T>) -> Option<&'static str> {
        let path = paths::GLOBAL_REGEX_SET.matches(request.uri().path());
        match *request.method() {
            // Op10Get - GET /op10
            hyper::Method::GET if path.matched(paths::ID_OP10) => Some("Op10Get"),
            // Op11Get - GET /op11
            hyper::Method::GET if path.matched(paths::ID_OP11) => Some("Op11Get"),
            // Op12Get - GET /op12
            hyper::Method::GET if path.matched(paths::ID_OP12) => Some("Op12Get"),
            // Op13Get - GET /op13
            hyper::Method::GET if path.matched(paths::ID_OP13) => Some("Op13Get"),
            // Op14Get - GET /op14
            hyper::Method::GET if path.matched(paths::ID_OP14) => Some("Op14Get"),
            // Op15Get - GET /op15
            hyper::Method::GET if path.matched(paths::ID_OP15) => Some("Op15Get"),
            // Op16Get - GET /op16
            hyper::Method::GET if path.matched(paths::ID_OP16) => Some("Op16Get"),
            // Op17Get - GET /op17
            hyper::Method::GET if path.matched(paths::ID_OP17) => Some("Op17Get"),
            // Op18Get - GET /op18
            hyper::Method::GET if path.matched(paths::ID_OP18) => Some("Op18Get"),
            // Op19Get - GET /op19
            hyper::Method::GET if path.matched(paths::ID_OP19) => Some("Op19Get"),
            // Op1Get - GET /op1
            hyper::Method::GET if path.matched(paths::ID_OP1) => Some("Op1Get"),
            // Op20Get - GET /op20
            hyper::Method::GET if path.matched(paths::ID_OP20) => Some("Op20Get"),
            // Op21Get - GET /op21
            hyper::Method::GET if path.matched(paths::ID_OP21) => Some("Op21Get"),
            // Op22Get - GET /op22
            hyper::Method::GET if path.matched(paths::ID_OP22) => Some("Op22Get"),
            // Op23Get - GET /op23
            hyper::Method::GET if path.matched(paths::ID_OP23) => Some("Op23Get"),
            // Op24Get - GET /op24
            hyper::Method::GET if path.matched(paths::ID_OP24) => Some("Op24Get"),
            // Op25Get - GET /op25
            hyper::Method::GET if path.matched(paths::ID_OP25) => Some("Op25Get"),
            // Op26Get - GET /op26
            hyper::Method::GET if path.matched(paths::ID_OP26) => Some("Op26Get"),
            // Op27Get - GET /op27
            hyper::Method::GET if path.matched(paths::ID_OP27) => Some("Op27Get"),
            // Op28Get - GET /op28
            hyper::Method::GET if path.matched(paths::ID_OP28) => Some("Op28Get"),
            // Op29Get - GET /op29
            hyper::Method::GET if path.matched(paths::ID_OP29) => Some("Op29Get"),
            // Op2Get - GET /op2
            hyper::Method::GET if path.matched(paths::ID_OP2) => Some("Op2Get"),
            // Op30Get - GET /op30
            hyper::Method::GET if path.matched(paths::ID_OP30) => Some("Op30Get"),
            // Op31Get - GET /op31
            hyper::Method::GET if path.matched(paths::ID_OP31) => Some("Op31Get"),
            // Op32Get - GET /op32
            hyper::Method::GET if path.matched(paths::ID_OP32) => Some("Op32Get"),
            // Op33Get - GET /op33
            hyper::Method::GET if path.matched(paths::ID_OP33) => Some("Op33Get"),
            // Op34Get - GET /op34
            hyper::Method::GET if path.matched(paths::ID_OP34) => Some("Op34Get"),
            // Op35Get - GET /op35
            hyper::Method::GET if path.matched(paths::ID_OP35) => Some("Op35Get"),
            // Op36Get - GET /op36
            hyper::Method::GET if path.matched(paths::ID_OP36) => Some("Op36Get"),
            // Op37Get - GET /op37
            hyper::Method::GET if path.matched(paths::ID_OP37) => Some("Op37Get"),
            // Op3Get - GET /op3
            hyper::Method::GET if path.matched(paths::ID_OP3) => Some("Op3Get"),
            // Op4Get - GET /op4
            hyper::Method::GET if path.matched(paths::ID_OP4) => Some("Op4Get"),
            // Op5Get - GET /op5
            hyper::Method::GET if path.matched(paths::ID_OP5) => Some("Op5Get"),
            // Op6Get - GET /op6
            hyper::Method::GET if path.matched(paths::ID_OP6) => Some("Op6Get"),
            // Op7Get - GET /op7
            hyper::Method::GET if path.matched(paths::ID_OP7) => Some("Op7Get"),
            // Op8Get - GET /op8
            hyper::Method::GET if path.matched(paths::ID_OP8) => Some("Op8Get"),
            // Op9Get - GET /op9
            hyper::Method::GET if path.matched(paths::ID_OP9) => Some("Op9Get"),
            _ => None,
        }
    }
}
