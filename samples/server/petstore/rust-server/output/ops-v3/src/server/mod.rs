#![allow(unused_extern_crates)]
extern crate serde_ignored;
extern crate tokio_core;
extern crate native_tls;
extern crate hyper_tls;
extern crate openssl;
extern crate mime;
extern crate chrono;
extern crate percent_encoding;
extern crate url;

use std::sync::Arc;
use std::marker::PhantomData;
use futures::{Future, future, Stream, stream};
use hyper;
use hyper::{Request, Response, Error, StatusCode};
use hyper::header::{Headers, ContentType};
use self::url::form_urlencoded;
use mimetypes;
use serde_json;

#[allow(unused_imports)]
use std::collections::{HashMap, BTreeMap};
#[allow(unused_imports)]
use swagger;
use std::io;

#[allow(unused_imports)]
use std::collections::BTreeSet;

pub use swagger::auth::Authorization;
use swagger::{ApiError, XSpanId, XSpanIdString, Has, RequestParser};
use swagger::auth::Scopes;

use {Api,
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
#[allow(unused_imports)]
use models;

pub mod context;

header! { (Warning, "Warning") => [String] }

mod paths {
    extern crate regex;

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
        ]).unwrap();
    }
    pub static ID_OP1: usize = 0;
    pub static ID_OP10: usize = 1;
    pub static ID_OP11: usize = 2;
    pub static ID_OP12: usize = 3;
    pub static ID_OP13: usize = 4;
    pub static ID_OP14: usize = 5;
    pub static ID_OP15: usize = 6;
    pub static ID_OP16: usize = 7;
    pub static ID_OP17: usize = 8;
    pub static ID_OP18: usize = 9;
    pub static ID_OP19: usize = 10;
    pub static ID_OP2: usize = 11;
    pub static ID_OP20: usize = 12;
    pub static ID_OP21: usize = 13;
    pub static ID_OP22: usize = 14;
    pub static ID_OP23: usize = 15;
    pub static ID_OP24: usize = 16;
    pub static ID_OP25: usize = 17;
    pub static ID_OP26: usize = 18;
    pub static ID_OP27: usize = 19;
    pub static ID_OP28: usize = 20;
    pub static ID_OP29: usize = 21;
    pub static ID_OP3: usize = 22;
    pub static ID_OP30: usize = 23;
    pub static ID_OP31: usize = 24;
    pub static ID_OP32: usize = 25;
    pub static ID_OP33: usize = 26;
    pub static ID_OP34: usize = 27;
    pub static ID_OP35: usize = 28;
    pub static ID_OP36: usize = 29;
    pub static ID_OP37: usize = 30;
    pub static ID_OP4: usize = 31;
    pub static ID_OP5: usize = 32;
    pub static ID_OP6: usize = 33;
    pub static ID_OP7: usize = 34;
    pub static ID_OP8: usize = 35;
    pub static ID_OP9: usize = 36;
}

pub struct NewService<T, C> {
    api_impl: Arc<T>,
    marker: PhantomData<C>,
}

impl<T, C> NewService<T, C>
where
    T: Api<C> + Clone + 'static,
    C: Has<XSpanIdString>  + 'static
{
    pub fn new<U: Into<Arc<T>>>(api_impl: U) -> NewService<T, C> {
        NewService{api_impl: api_impl.into(), marker: PhantomData}
    }
}

impl<T, C> hyper::server::NewService for NewService<T, C>
where
    T: Api<C> + Clone + 'static,
    C: Has<XSpanIdString>  + 'static
{
    type Request = (Request, C);
    type Response = Response;
    type Error = Error;
    type Instance = Service<T, C>;

    fn new_service(&self) -> Result<Self::Instance, io::Error> {
        Ok(Service::new(self.api_impl.clone()))
    }
}

pub struct Service<T, C> {
    api_impl: Arc<T>,
    marker: PhantomData<C>,
}

impl<T, C> Service<T, C>
where
    T: Api<C> + Clone + 'static,
    C: Has<XSpanIdString>  + 'static {
    pub fn new<U: Into<Arc<T>>>(api_impl: U) -> Service<T, C> {
        Service{api_impl: api_impl.into(), marker: PhantomData}
    }
}

impl<T, C> hyper::server::Service for Service<T, C>
where
    T: Api<C> + Clone + 'static,
    C: Has<XSpanIdString>  + 'static
{
    type Request = (Request, C);
    type Response = Response;
    type Error = Error;
    type Future = Box<Future<Item=Response, Error=Error>>;

    fn call(&self, (req, mut context): Self::Request) -> Self::Future {
        let api_impl = self.api_impl.clone();
        let (method, uri, _, headers, body) = req.deconstruct();
        let path = paths::GLOBAL_REGEX_SET.matches(uri.path());

        // This match statement is duplicated below in `parse_operation_id()`.
        // Please update both places if changing how this code is autogenerated.
        match &method {

            // Op10Get - GET /op10
            &hyper::Method::Get if path.matched(paths::ID_OP10) => {
                Box::new({
                        {{
                                Box::new(api_impl.op10_get(&context)
                                    .then(move |result| {
                                        let mut response = Response::new();
                                        response.headers_mut().set(XSpanId((&context as &Has<XSpanIdString>).get().0.to_string()));

                                        match result {
                                            Ok(rsp) => match rsp {
                                                Op10GetResponse::OK


                                                => {
                                                    response.set_status(StatusCode::try_from(200).unwrap());

                                                },
                                            },
                                            Err(_) => {
                                                // Application code returned an error. This should not happen, as the implementation should
                                                // return a valid response.
                                                response.set_status(StatusCode::InternalServerError);
                                                response.set_body("An internal error occurred");
                                            },
                                        }

                                        future::ok(response)
                                    }
                                ))
                        }}
                }) as Box<Future<Item=Response, Error=Error>>
            },

            // Op11Get - GET /op11
            &hyper::Method::Get if path.matched(paths::ID_OP11) => {
                Box::new({
                        {{
                                Box::new(api_impl.op11_get(&context)
                                    .then(move |result| {
                                        let mut response = Response::new();
                                        response.headers_mut().set(XSpanId((&context as &Has<XSpanIdString>).get().0.to_string()));

                                        match result {
                                            Ok(rsp) => match rsp {
                                                Op11GetResponse::OK


                                                => {
                                                    response.set_status(StatusCode::try_from(200).unwrap());

                                                },
                                            },
                                            Err(_) => {
                                                // Application code returned an error. This should not happen, as the implementation should
                                                // return a valid response.
                                                response.set_status(StatusCode::InternalServerError);
                                                response.set_body("An internal error occurred");
                                            },
                                        }

                                        future::ok(response)
                                    }
                                ))
                        }}
                }) as Box<Future<Item=Response, Error=Error>>
            },

            // Op12Get - GET /op12
            &hyper::Method::Get if path.matched(paths::ID_OP12) => {
                Box::new({
                        {{
                                Box::new(api_impl.op12_get(&context)
                                    .then(move |result| {
                                        let mut response = Response::new();
                                        response.headers_mut().set(XSpanId((&context as &Has<XSpanIdString>).get().0.to_string()));

                                        match result {
                                            Ok(rsp) => match rsp {
                                                Op12GetResponse::OK


                                                => {
                                                    response.set_status(StatusCode::try_from(200).unwrap());

                                                },
                                            },
                                            Err(_) => {
                                                // Application code returned an error. This should not happen, as the implementation should
                                                // return a valid response.
                                                response.set_status(StatusCode::InternalServerError);
                                                response.set_body("An internal error occurred");
                                            },
                                        }

                                        future::ok(response)
                                    }
                                ))
                        }}
                }) as Box<Future<Item=Response, Error=Error>>
            },

            // Op13Get - GET /op13
            &hyper::Method::Get if path.matched(paths::ID_OP13) => {
                Box::new({
                        {{
                                Box::new(api_impl.op13_get(&context)
                                    .then(move |result| {
                                        let mut response = Response::new();
                                        response.headers_mut().set(XSpanId((&context as &Has<XSpanIdString>).get().0.to_string()));

                                        match result {
                                            Ok(rsp) => match rsp {
                                                Op13GetResponse::OK


                                                => {
                                                    response.set_status(StatusCode::try_from(200).unwrap());

                                                },
                                            },
                                            Err(_) => {
                                                // Application code returned an error. This should not happen, as the implementation should
                                                // return a valid response.
                                                response.set_status(StatusCode::InternalServerError);
                                                response.set_body("An internal error occurred");
                                            },
                                        }

                                        future::ok(response)
                                    }
                                ))
                        }}
                }) as Box<Future<Item=Response, Error=Error>>
            },

            // Op14Get - GET /op14
            &hyper::Method::Get if path.matched(paths::ID_OP14) => {
                Box::new({
                        {{
                                Box::new(api_impl.op14_get(&context)
                                    .then(move |result| {
                                        let mut response = Response::new();
                                        response.headers_mut().set(XSpanId((&context as &Has<XSpanIdString>).get().0.to_string()));

                                        match result {
                                            Ok(rsp) => match rsp {
                                                Op14GetResponse::OK


                                                => {
                                                    response.set_status(StatusCode::try_from(200).unwrap());

                                                },
                                            },
                                            Err(_) => {
                                                // Application code returned an error. This should not happen, as the implementation should
                                                // return a valid response.
                                                response.set_status(StatusCode::InternalServerError);
                                                response.set_body("An internal error occurred");
                                            },
                                        }

                                        future::ok(response)
                                    }
                                ))
                        }}
                }) as Box<Future<Item=Response, Error=Error>>
            },

            // Op15Get - GET /op15
            &hyper::Method::Get if path.matched(paths::ID_OP15) => {
                Box::new({
                        {{
                                Box::new(api_impl.op15_get(&context)
                                    .then(move |result| {
                                        let mut response = Response::new();
                                        response.headers_mut().set(XSpanId((&context as &Has<XSpanIdString>).get().0.to_string()));

                                        match result {
                                            Ok(rsp) => match rsp {
                                                Op15GetResponse::OK


                                                => {
                                                    response.set_status(StatusCode::try_from(200).unwrap());

                                                },
                                            },
                                            Err(_) => {
                                                // Application code returned an error. This should not happen, as the implementation should
                                                // return a valid response.
                                                response.set_status(StatusCode::InternalServerError);
                                                response.set_body("An internal error occurred");
                                            },
                                        }

                                        future::ok(response)
                                    }
                                ))
                        }}
                }) as Box<Future<Item=Response, Error=Error>>
            },

            // Op16Get - GET /op16
            &hyper::Method::Get if path.matched(paths::ID_OP16) => {
                Box::new({
                        {{
                                Box::new(api_impl.op16_get(&context)
                                    .then(move |result| {
                                        let mut response = Response::new();
                                        response.headers_mut().set(XSpanId((&context as &Has<XSpanIdString>).get().0.to_string()));

                                        match result {
                                            Ok(rsp) => match rsp {
                                                Op16GetResponse::OK


                                                => {
                                                    response.set_status(StatusCode::try_from(200).unwrap());

                                                },
                                            },
                                            Err(_) => {
                                                // Application code returned an error. This should not happen, as the implementation should
                                                // return a valid response.
                                                response.set_status(StatusCode::InternalServerError);
                                                response.set_body("An internal error occurred");
                                            },
                                        }

                                        future::ok(response)
                                    }
                                ))
                        }}
                }) as Box<Future<Item=Response, Error=Error>>
            },

            // Op17Get - GET /op17
            &hyper::Method::Get if path.matched(paths::ID_OP17) => {
                Box::new({
                        {{
                                Box::new(api_impl.op17_get(&context)
                                    .then(move |result| {
                                        let mut response = Response::new();
                                        response.headers_mut().set(XSpanId((&context as &Has<XSpanIdString>).get().0.to_string()));

                                        match result {
                                            Ok(rsp) => match rsp {
                                                Op17GetResponse::OK


                                                => {
                                                    response.set_status(StatusCode::try_from(200).unwrap());

                                                },
                                            },
                                            Err(_) => {
                                                // Application code returned an error. This should not happen, as the implementation should
                                                // return a valid response.
                                                response.set_status(StatusCode::InternalServerError);
                                                response.set_body("An internal error occurred");
                                            },
                                        }

                                        future::ok(response)
                                    }
                                ))
                        }}
                }) as Box<Future<Item=Response, Error=Error>>
            },

            // Op18Get - GET /op18
            &hyper::Method::Get if path.matched(paths::ID_OP18) => {
                Box::new({
                        {{
                                Box::new(api_impl.op18_get(&context)
                                    .then(move |result| {
                                        let mut response = Response::new();
                                        response.headers_mut().set(XSpanId((&context as &Has<XSpanIdString>).get().0.to_string()));

                                        match result {
                                            Ok(rsp) => match rsp {
                                                Op18GetResponse::OK


                                                => {
                                                    response.set_status(StatusCode::try_from(200).unwrap());

                                                },
                                            },
                                            Err(_) => {
                                                // Application code returned an error. This should not happen, as the implementation should
                                                // return a valid response.
                                                response.set_status(StatusCode::InternalServerError);
                                                response.set_body("An internal error occurred");
                                            },
                                        }

                                        future::ok(response)
                                    }
                                ))
                        }}
                }) as Box<Future<Item=Response, Error=Error>>
            },

            // Op19Get - GET /op19
            &hyper::Method::Get if path.matched(paths::ID_OP19) => {
                Box::new({
                        {{
                                Box::new(api_impl.op19_get(&context)
                                    .then(move |result| {
                                        let mut response = Response::new();
                                        response.headers_mut().set(XSpanId((&context as &Has<XSpanIdString>).get().0.to_string()));

                                        match result {
                                            Ok(rsp) => match rsp {
                                                Op19GetResponse::OK


                                                => {
                                                    response.set_status(StatusCode::try_from(200).unwrap());

                                                },
                                            },
                                            Err(_) => {
                                                // Application code returned an error. This should not happen, as the implementation should
                                                // return a valid response.
                                                response.set_status(StatusCode::InternalServerError);
                                                response.set_body("An internal error occurred");
                                            },
                                        }

                                        future::ok(response)
                                    }
                                ))
                        }}
                }) as Box<Future<Item=Response, Error=Error>>
            },

            // Op1Get - GET /op1
            &hyper::Method::Get if path.matched(paths::ID_OP1) => {
                Box::new({
                        {{
                                Box::new(api_impl.op1_get(&context)
                                    .then(move |result| {
                                        let mut response = Response::new();
                                        response.headers_mut().set(XSpanId((&context as &Has<XSpanIdString>).get().0.to_string()));

                                        match result {
                                            Ok(rsp) => match rsp {
                                                Op1GetResponse::OK


                                                => {
                                                    response.set_status(StatusCode::try_from(200).unwrap());

                                                },
                                            },
                                            Err(_) => {
                                                // Application code returned an error. This should not happen, as the implementation should
                                                // return a valid response.
                                                response.set_status(StatusCode::InternalServerError);
                                                response.set_body("An internal error occurred");
                                            },
                                        }

                                        future::ok(response)
                                    }
                                ))
                        }}
                }) as Box<Future<Item=Response, Error=Error>>
            },

            // Op20Get - GET /op20
            &hyper::Method::Get if path.matched(paths::ID_OP20) => {
                Box::new({
                        {{
                                Box::new(api_impl.op20_get(&context)
                                    .then(move |result| {
                                        let mut response = Response::new();
                                        response.headers_mut().set(XSpanId((&context as &Has<XSpanIdString>).get().0.to_string()));

                                        match result {
                                            Ok(rsp) => match rsp {
                                                Op20GetResponse::OK


                                                => {
                                                    response.set_status(StatusCode::try_from(200).unwrap());

                                                },
                                            },
                                            Err(_) => {
                                                // Application code returned an error. This should not happen, as the implementation should
                                                // return a valid response.
                                                response.set_status(StatusCode::InternalServerError);
                                                response.set_body("An internal error occurred");
                                            },
                                        }

                                        future::ok(response)
                                    }
                                ))
                        }}
                }) as Box<Future<Item=Response, Error=Error>>
            },

            // Op21Get - GET /op21
            &hyper::Method::Get if path.matched(paths::ID_OP21) => {
                Box::new({
                        {{
                                Box::new(api_impl.op21_get(&context)
                                    .then(move |result| {
                                        let mut response = Response::new();
                                        response.headers_mut().set(XSpanId((&context as &Has<XSpanIdString>).get().0.to_string()));

                                        match result {
                                            Ok(rsp) => match rsp {
                                                Op21GetResponse::OK


                                                => {
                                                    response.set_status(StatusCode::try_from(200).unwrap());

                                                },
                                            },
                                            Err(_) => {
                                                // Application code returned an error. This should not happen, as the implementation should
                                                // return a valid response.
                                                response.set_status(StatusCode::InternalServerError);
                                                response.set_body("An internal error occurred");
                                            },
                                        }

                                        future::ok(response)
                                    }
                                ))
                        }}
                }) as Box<Future<Item=Response, Error=Error>>
            },

            // Op22Get - GET /op22
            &hyper::Method::Get if path.matched(paths::ID_OP22) => {
                Box::new({
                        {{
                                Box::new(api_impl.op22_get(&context)
                                    .then(move |result| {
                                        let mut response = Response::new();
                                        response.headers_mut().set(XSpanId((&context as &Has<XSpanIdString>).get().0.to_string()));

                                        match result {
                                            Ok(rsp) => match rsp {
                                                Op22GetResponse::OK


                                                => {
                                                    response.set_status(StatusCode::try_from(200).unwrap());

                                                },
                                            },
                                            Err(_) => {
                                                // Application code returned an error. This should not happen, as the implementation should
                                                // return a valid response.
                                                response.set_status(StatusCode::InternalServerError);
                                                response.set_body("An internal error occurred");
                                            },
                                        }

                                        future::ok(response)
                                    }
                                ))
                        }}
                }) as Box<Future<Item=Response, Error=Error>>
            },

            // Op23Get - GET /op23
            &hyper::Method::Get if path.matched(paths::ID_OP23) => {
                Box::new({
                        {{
                                Box::new(api_impl.op23_get(&context)
                                    .then(move |result| {
                                        let mut response = Response::new();
                                        response.headers_mut().set(XSpanId((&context as &Has<XSpanIdString>).get().0.to_string()));

                                        match result {
                                            Ok(rsp) => match rsp {
                                                Op23GetResponse::OK


                                                => {
                                                    response.set_status(StatusCode::try_from(200).unwrap());

                                                },
                                            },
                                            Err(_) => {
                                                // Application code returned an error. This should not happen, as the implementation should
                                                // return a valid response.
                                                response.set_status(StatusCode::InternalServerError);
                                                response.set_body("An internal error occurred");
                                            },
                                        }

                                        future::ok(response)
                                    }
                                ))
                        }}
                }) as Box<Future<Item=Response, Error=Error>>
            },

            // Op24Get - GET /op24
            &hyper::Method::Get if path.matched(paths::ID_OP24) => {
                Box::new({
                        {{
                                Box::new(api_impl.op24_get(&context)
                                    .then(move |result| {
                                        let mut response = Response::new();
                                        response.headers_mut().set(XSpanId((&context as &Has<XSpanIdString>).get().0.to_string()));

                                        match result {
                                            Ok(rsp) => match rsp {
                                                Op24GetResponse::OK


                                                => {
                                                    response.set_status(StatusCode::try_from(200).unwrap());

                                                },
                                            },
                                            Err(_) => {
                                                // Application code returned an error. This should not happen, as the implementation should
                                                // return a valid response.
                                                response.set_status(StatusCode::InternalServerError);
                                                response.set_body("An internal error occurred");
                                            },
                                        }

                                        future::ok(response)
                                    }
                                ))
                        }}
                }) as Box<Future<Item=Response, Error=Error>>
            },

            // Op25Get - GET /op25
            &hyper::Method::Get if path.matched(paths::ID_OP25) => {
                Box::new({
                        {{
                                Box::new(api_impl.op25_get(&context)
                                    .then(move |result| {
                                        let mut response = Response::new();
                                        response.headers_mut().set(XSpanId((&context as &Has<XSpanIdString>).get().0.to_string()));

                                        match result {
                                            Ok(rsp) => match rsp {
                                                Op25GetResponse::OK


                                                => {
                                                    response.set_status(StatusCode::try_from(200).unwrap());

                                                },
                                            },
                                            Err(_) => {
                                                // Application code returned an error. This should not happen, as the implementation should
                                                // return a valid response.
                                                response.set_status(StatusCode::InternalServerError);
                                                response.set_body("An internal error occurred");
                                            },
                                        }

                                        future::ok(response)
                                    }
                                ))
                        }}
                }) as Box<Future<Item=Response, Error=Error>>
            },

            // Op26Get - GET /op26
            &hyper::Method::Get if path.matched(paths::ID_OP26) => {
                Box::new({
                        {{
                                Box::new(api_impl.op26_get(&context)
                                    .then(move |result| {
                                        let mut response = Response::new();
                                        response.headers_mut().set(XSpanId((&context as &Has<XSpanIdString>).get().0.to_string()));

                                        match result {
                                            Ok(rsp) => match rsp {
                                                Op26GetResponse::OK


                                                => {
                                                    response.set_status(StatusCode::try_from(200).unwrap());

                                                },
                                            },
                                            Err(_) => {
                                                // Application code returned an error. This should not happen, as the implementation should
                                                // return a valid response.
                                                response.set_status(StatusCode::InternalServerError);
                                                response.set_body("An internal error occurred");
                                            },
                                        }

                                        future::ok(response)
                                    }
                                ))
                        }}
                }) as Box<Future<Item=Response, Error=Error>>
            },

            // Op27Get - GET /op27
            &hyper::Method::Get if path.matched(paths::ID_OP27) => {
                Box::new({
                        {{
                                Box::new(api_impl.op27_get(&context)
                                    .then(move |result| {
                                        let mut response = Response::new();
                                        response.headers_mut().set(XSpanId((&context as &Has<XSpanIdString>).get().0.to_string()));

                                        match result {
                                            Ok(rsp) => match rsp {
                                                Op27GetResponse::OK


                                                => {
                                                    response.set_status(StatusCode::try_from(200).unwrap());

                                                },
                                            },
                                            Err(_) => {
                                                // Application code returned an error. This should not happen, as the implementation should
                                                // return a valid response.
                                                response.set_status(StatusCode::InternalServerError);
                                                response.set_body("An internal error occurred");
                                            },
                                        }

                                        future::ok(response)
                                    }
                                ))
                        }}
                }) as Box<Future<Item=Response, Error=Error>>
            },

            // Op28Get - GET /op28
            &hyper::Method::Get if path.matched(paths::ID_OP28) => {
                Box::new({
                        {{
                                Box::new(api_impl.op28_get(&context)
                                    .then(move |result| {
                                        let mut response = Response::new();
                                        response.headers_mut().set(XSpanId((&context as &Has<XSpanIdString>).get().0.to_string()));

                                        match result {
                                            Ok(rsp) => match rsp {
                                                Op28GetResponse::OK


                                                => {
                                                    response.set_status(StatusCode::try_from(200).unwrap());

                                                },
                                            },
                                            Err(_) => {
                                                // Application code returned an error. This should not happen, as the implementation should
                                                // return a valid response.
                                                response.set_status(StatusCode::InternalServerError);
                                                response.set_body("An internal error occurred");
                                            },
                                        }

                                        future::ok(response)
                                    }
                                ))
                        }}
                }) as Box<Future<Item=Response, Error=Error>>
            },

            // Op29Get - GET /op29
            &hyper::Method::Get if path.matched(paths::ID_OP29) => {
                Box::new({
                        {{
                                Box::new(api_impl.op29_get(&context)
                                    .then(move |result| {
                                        let mut response = Response::new();
                                        response.headers_mut().set(XSpanId((&context as &Has<XSpanIdString>).get().0.to_string()));

                                        match result {
                                            Ok(rsp) => match rsp {
                                                Op29GetResponse::OK


                                                => {
                                                    response.set_status(StatusCode::try_from(200).unwrap());

                                                },
                                            },
                                            Err(_) => {
                                                // Application code returned an error. This should not happen, as the implementation should
                                                // return a valid response.
                                                response.set_status(StatusCode::InternalServerError);
                                                response.set_body("An internal error occurred");
                                            },
                                        }

                                        future::ok(response)
                                    }
                                ))
                        }}
                }) as Box<Future<Item=Response, Error=Error>>
            },

            // Op2Get - GET /op2
            &hyper::Method::Get if path.matched(paths::ID_OP2) => {
                Box::new({
                        {{
                                Box::new(api_impl.op2_get(&context)
                                    .then(move |result| {
                                        let mut response = Response::new();
                                        response.headers_mut().set(XSpanId((&context as &Has<XSpanIdString>).get().0.to_string()));

                                        match result {
                                            Ok(rsp) => match rsp {
                                                Op2GetResponse::OK


                                                => {
                                                    response.set_status(StatusCode::try_from(200).unwrap());

                                                },
                                            },
                                            Err(_) => {
                                                // Application code returned an error. This should not happen, as the implementation should
                                                // return a valid response.
                                                response.set_status(StatusCode::InternalServerError);
                                                response.set_body("An internal error occurred");
                                            },
                                        }

                                        future::ok(response)
                                    }
                                ))
                        }}
                }) as Box<Future<Item=Response, Error=Error>>
            },

            // Op30Get - GET /op30
            &hyper::Method::Get if path.matched(paths::ID_OP30) => {
                Box::new({
                        {{
                                Box::new(api_impl.op30_get(&context)
                                    .then(move |result| {
                                        let mut response = Response::new();
                                        response.headers_mut().set(XSpanId((&context as &Has<XSpanIdString>).get().0.to_string()));

                                        match result {
                                            Ok(rsp) => match rsp {
                                                Op30GetResponse::OK


                                                => {
                                                    response.set_status(StatusCode::try_from(200).unwrap());

                                                },
                                            },
                                            Err(_) => {
                                                // Application code returned an error. This should not happen, as the implementation should
                                                // return a valid response.
                                                response.set_status(StatusCode::InternalServerError);
                                                response.set_body("An internal error occurred");
                                            },
                                        }

                                        future::ok(response)
                                    }
                                ))
                        }}
                }) as Box<Future<Item=Response, Error=Error>>
            },

            // Op31Get - GET /op31
            &hyper::Method::Get if path.matched(paths::ID_OP31) => {
                Box::new({
                        {{
                                Box::new(api_impl.op31_get(&context)
                                    .then(move |result| {
                                        let mut response = Response::new();
                                        response.headers_mut().set(XSpanId((&context as &Has<XSpanIdString>).get().0.to_string()));

                                        match result {
                                            Ok(rsp) => match rsp {
                                                Op31GetResponse::OK


                                                => {
                                                    response.set_status(StatusCode::try_from(200).unwrap());

                                                },
                                            },
                                            Err(_) => {
                                                // Application code returned an error. This should not happen, as the implementation should
                                                // return a valid response.
                                                response.set_status(StatusCode::InternalServerError);
                                                response.set_body("An internal error occurred");
                                            },
                                        }

                                        future::ok(response)
                                    }
                                ))
                        }}
                }) as Box<Future<Item=Response, Error=Error>>
            },

            // Op32Get - GET /op32
            &hyper::Method::Get if path.matched(paths::ID_OP32) => {
                Box::new({
                        {{
                                Box::new(api_impl.op32_get(&context)
                                    .then(move |result| {
                                        let mut response = Response::new();
                                        response.headers_mut().set(XSpanId((&context as &Has<XSpanIdString>).get().0.to_string()));

                                        match result {
                                            Ok(rsp) => match rsp {
                                                Op32GetResponse::OK


                                                => {
                                                    response.set_status(StatusCode::try_from(200).unwrap());

                                                },
                                            },
                                            Err(_) => {
                                                // Application code returned an error. This should not happen, as the implementation should
                                                // return a valid response.
                                                response.set_status(StatusCode::InternalServerError);
                                                response.set_body("An internal error occurred");
                                            },
                                        }

                                        future::ok(response)
                                    }
                                ))
                        }}
                }) as Box<Future<Item=Response, Error=Error>>
            },

            // Op33Get - GET /op33
            &hyper::Method::Get if path.matched(paths::ID_OP33) => {
                Box::new({
                        {{
                                Box::new(api_impl.op33_get(&context)
                                    .then(move |result| {
                                        let mut response = Response::new();
                                        response.headers_mut().set(XSpanId((&context as &Has<XSpanIdString>).get().0.to_string()));

                                        match result {
                                            Ok(rsp) => match rsp {
                                                Op33GetResponse::OK


                                                => {
                                                    response.set_status(StatusCode::try_from(200).unwrap());

                                                },
                                            },
                                            Err(_) => {
                                                // Application code returned an error. This should not happen, as the implementation should
                                                // return a valid response.
                                                response.set_status(StatusCode::InternalServerError);
                                                response.set_body("An internal error occurred");
                                            },
                                        }

                                        future::ok(response)
                                    }
                                ))
                        }}
                }) as Box<Future<Item=Response, Error=Error>>
            },

            // Op34Get - GET /op34
            &hyper::Method::Get if path.matched(paths::ID_OP34) => {
                Box::new({
                        {{
                                Box::new(api_impl.op34_get(&context)
                                    .then(move |result| {
                                        let mut response = Response::new();
                                        response.headers_mut().set(XSpanId((&context as &Has<XSpanIdString>).get().0.to_string()));

                                        match result {
                                            Ok(rsp) => match rsp {
                                                Op34GetResponse::OK


                                                => {
                                                    response.set_status(StatusCode::try_from(200).unwrap());

                                                },
                                            },
                                            Err(_) => {
                                                // Application code returned an error. This should not happen, as the implementation should
                                                // return a valid response.
                                                response.set_status(StatusCode::InternalServerError);
                                                response.set_body("An internal error occurred");
                                            },
                                        }

                                        future::ok(response)
                                    }
                                ))
                        }}
                }) as Box<Future<Item=Response, Error=Error>>
            },

            // Op35Get - GET /op35
            &hyper::Method::Get if path.matched(paths::ID_OP35) => {
                Box::new({
                        {{
                                Box::new(api_impl.op35_get(&context)
                                    .then(move |result| {
                                        let mut response = Response::new();
                                        response.headers_mut().set(XSpanId((&context as &Has<XSpanIdString>).get().0.to_string()));

                                        match result {
                                            Ok(rsp) => match rsp {
                                                Op35GetResponse::OK


                                                => {
                                                    response.set_status(StatusCode::try_from(200).unwrap());

                                                },
                                            },
                                            Err(_) => {
                                                // Application code returned an error. This should not happen, as the implementation should
                                                // return a valid response.
                                                response.set_status(StatusCode::InternalServerError);
                                                response.set_body("An internal error occurred");
                                            },
                                        }

                                        future::ok(response)
                                    }
                                ))
                        }}
                }) as Box<Future<Item=Response, Error=Error>>
            },

            // Op36Get - GET /op36
            &hyper::Method::Get if path.matched(paths::ID_OP36) => {
                Box::new({
                        {{
                                Box::new(api_impl.op36_get(&context)
                                    .then(move |result| {
                                        let mut response = Response::new();
                                        response.headers_mut().set(XSpanId((&context as &Has<XSpanIdString>).get().0.to_string()));

                                        match result {
                                            Ok(rsp) => match rsp {
                                                Op36GetResponse::OK


                                                => {
                                                    response.set_status(StatusCode::try_from(200).unwrap());

                                                },
                                            },
                                            Err(_) => {
                                                // Application code returned an error. This should not happen, as the implementation should
                                                // return a valid response.
                                                response.set_status(StatusCode::InternalServerError);
                                                response.set_body("An internal error occurred");
                                            },
                                        }

                                        future::ok(response)
                                    }
                                ))
                        }}
                }) as Box<Future<Item=Response, Error=Error>>
            },

            // Op37Get - GET /op37
            &hyper::Method::Get if path.matched(paths::ID_OP37) => {
                Box::new({
                        {{
                                Box::new(api_impl.op37_get(&context)
                                    .then(move |result| {
                                        let mut response = Response::new();
                                        response.headers_mut().set(XSpanId((&context as &Has<XSpanIdString>).get().0.to_string()));

                                        match result {
                                            Ok(rsp) => match rsp {
                                                Op37GetResponse::OK


                                                => {
                                                    response.set_status(StatusCode::try_from(200).unwrap());

                                                },
                                            },
                                            Err(_) => {
                                                // Application code returned an error. This should not happen, as the implementation should
                                                // return a valid response.
                                                response.set_status(StatusCode::InternalServerError);
                                                response.set_body("An internal error occurred");
                                            },
                                        }

                                        future::ok(response)
                                    }
                                ))
                        }}
                }) as Box<Future<Item=Response, Error=Error>>
            },

            // Op3Get - GET /op3
            &hyper::Method::Get if path.matched(paths::ID_OP3) => {
                Box::new({
                        {{
                                Box::new(api_impl.op3_get(&context)
                                    .then(move |result| {
                                        let mut response = Response::new();
                                        response.headers_mut().set(XSpanId((&context as &Has<XSpanIdString>).get().0.to_string()));

                                        match result {
                                            Ok(rsp) => match rsp {
                                                Op3GetResponse::OK


                                                => {
                                                    response.set_status(StatusCode::try_from(200).unwrap());

                                                },
                                            },
                                            Err(_) => {
                                                // Application code returned an error. This should not happen, as the implementation should
                                                // return a valid response.
                                                response.set_status(StatusCode::InternalServerError);
                                                response.set_body("An internal error occurred");
                                            },
                                        }

                                        future::ok(response)
                                    }
                                ))
                        }}
                }) as Box<Future<Item=Response, Error=Error>>
            },

            // Op4Get - GET /op4
            &hyper::Method::Get if path.matched(paths::ID_OP4) => {
                Box::new({
                        {{
                                Box::new(api_impl.op4_get(&context)
                                    .then(move |result| {
                                        let mut response = Response::new();
                                        response.headers_mut().set(XSpanId((&context as &Has<XSpanIdString>).get().0.to_string()));

                                        match result {
                                            Ok(rsp) => match rsp {
                                                Op4GetResponse::OK


                                                => {
                                                    response.set_status(StatusCode::try_from(200).unwrap());

                                                },
                                            },
                                            Err(_) => {
                                                // Application code returned an error. This should not happen, as the implementation should
                                                // return a valid response.
                                                response.set_status(StatusCode::InternalServerError);
                                                response.set_body("An internal error occurred");
                                            },
                                        }

                                        future::ok(response)
                                    }
                                ))
                        }}
                }) as Box<Future<Item=Response, Error=Error>>
            },

            // Op5Get - GET /op5
            &hyper::Method::Get if path.matched(paths::ID_OP5) => {
                Box::new({
                        {{
                                Box::new(api_impl.op5_get(&context)
                                    .then(move |result| {
                                        let mut response = Response::new();
                                        response.headers_mut().set(XSpanId((&context as &Has<XSpanIdString>).get().0.to_string()));

                                        match result {
                                            Ok(rsp) => match rsp {
                                                Op5GetResponse::OK


                                                => {
                                                    response.set_status(StatusCode::try_from(200).unwrap());

                                                },
                                            },
                                            Err(_) => {
                                                // Application code returned an error. This should not happen, as the implementation should
                                                // return a valid response.
                                                response.set_status(StatusCode::InternalServerError);
                                                response.set_body("An internal error occurred");
                                            },
                                        }

                                        future::ok(response)
                                    }
                                ))
                        }}
                }) as Box<Future<Item=Response, Error=Error>>
            },

            // Op6Get - GET /op6
            &hyper::Method::Get if path.matched(paths::ID_OP6) => {
                Box::new({
                        {{
                                Box::new(api_impl.op6_get(&context)
                                    .then(move |result| {
                                        let mut response = Response::new();
                                        response.headers_mut().set(XSpanId((&context as &Has<XSpanIdString>).get().0.to_string()));

                                        match result {
                                            Ok(rsp) => match rsp {
                                                Op6GetResponse::OK


                                                => {
                                                    response.set_status(StatusCode::try_from(200).unwrap());

                                                },
                                            },
                                            Err(_) => {
                                                // Application code returned an error. This should not happen, as the implementation should
                                                // return a valid response.
                                                response.set_status(StatusCode::InternalServerError);
                                                response.set_body("An internal error occurred");
                                            },
                                        }

                                        future::ok(response)
                                    }
                                ))
                        }}
                }) as Box<Future<Item=Response, Error=Error>>
            },

            // Op7Get - GET /op7
            &hyper::Method::Get if path.matched(paths::ID_OP7) => {
                Box::new({
                        {{
                                Box::new(api_impl.op7_get(&context)
                                    .then(move |result| {
                                        let mut response = Response::new();
                                        response.headers_mut().set(XSpanId((&context as &Has<XSpanIdString>).get().0.to_string()));

                                        match result {
                                            Ok(rsp) => match rsp {
                                                Op7GetResponse::OK


                                                => {
                                                    response.set_status(StatusCode::try_from(200).unwrap());

                                                },
                                            },
                                            Err(_) => {
                                                // Application code returned an error. This should not happen, as the implementation should
                                                // return a valid response.
                                                response.set_status(StatusCode::InternalServerError);
                                                response.set_body("An internal error occurred");
                                            },
                                        }

                                        future::ok(response)
                                    }
                                ))
                        }}
                }) as Box<Future<Item=Response, Error=Error>>
            },

            // Op8Get - GET /op8
            &hyper::Method::Get if path.matched(paths::ID_OP8) => {
                Box::new({
                        {{
                                Box::new(api_impl.op8_get(&context)
                                    .then(move |result| {
                                        let mut response = Response::new();
                                        response.headers_mut().set(XSpanId((&context as &Has<XSpanIdString>).get().0.to_string()));

                                        match result {
                                            Ok(rsp) => match rsp {
                                                Op8GetResponse::OK


                                                => {
                                                    response.set_status(StatusCode::try_from(200).unwrap());

                                                },
                                            },
                                            Err(_) => {
                                                // Application code returned an error. This should not happen, as the implementation should
                                                // return a valid response.
                                                response.set_status(StatusCode::InternalServerError);
                                                response.set_body("An internal error occurred");
                                            },
                                        }

                                        future::ok(response)
                                    }
                                ))
                        }}
                }) as Box<Future<Item=Response, Error=Error>>
            },

            // Op9Get - GET /op9
            &hyper::Method::Get if path.matched(paths::ID_OP9) => {
                Box::new({
                        {{
                                Box::new(api_impl.op9_get(&context)
                                    .then(move |result| {
                                        let mut response = Response::new();
                                        response.headers_mut().set(XSpanId((&context as &Has<XSpanIdString>).get().0.to_string()));

                                        match result {
                                            Ok(rsp) => match rsp {
                                                Op9GetResponse::OK


                                                => {
                                                    response.set_status(StatusCode::try_from(200).unwrap());

                                                },
                                            },
                                            Err(_) => {
                                                // Application code returned an error. This should not happen, as the implementation should
                                                // return a valid response.
                                                response.set_status(StatusCode::InternalServerError);
                                                response.set_body("An internal error occurred");
                                            },
                                        }

                                        future::ok(response)
                                    }
                                ))
                        }}
                }) as Box<Future<Item=Response, Error=Error>>
            },

            _ => Box::new(future::ok(Response::new().with_status(StatusCode::NotFound))) as Box<Future<Item=Response, Error=Error>>,
        }
    }
}

impl<T, C> Clone for Service<T, C>
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
impl RequestParser for ApiRequestParser {
    fn parse_operation_id(request: &Request) -> Result<&'static str, ()> {
        let path = paths::GLOBAL_REGEX_SET.matches(request.uri().path());
        match request.method() {

            // Op10Get - GET /op10
            &hyper::Method::Get if path.matched(paths::ID_OP10) => Ok("Op10Get"),

            // Op11Get - GET /op11
            &hyper::Method::Get if path.matched(paths::ID_OP11) => Ok("Op11Get"),

            // Op12Get - GET /op12
            &hyper::Method::Get if path.matched(paths::ID_OP12) => Ok("Op12Get"),

            // Op13Get - GET /op13
            &hyper::Method::Get if path.matched(paths::ID_OP13) => Ok("Op13Get"),

            // Op14Get - GET /op14
            &hyper::Method::Get if path.matched(paths::ID_OP14) => Ok("Op14Get"),

            // Op15Get - GET /op15
            &hyper::Method::Get if path.matched(paths::ID_OP15) => Ok("Op15Get"),

            // Op16Get - GET /op16
            &hyper::Method::Get if path.matched(paths::ID_OP16) => Ok("Op16Get"),

            // Op17Get - GET /op17
            &hyper::Method::Get if path.matched(paths::ID_OP17) => Ok("Op17Get"),

            // Op18Get - GET /op18
            &hyper::Method::Get if path.matched(paths::ID_OP18) => Ok("Op18Get"),

            // Op19Get - GET /op19
            &hyper::Method::Get if path.matched(paths::ID_OP19) => Ok("Op19Get"),

            // Op1Get - GET /op1
            &hyper::Method::Get if path.matched(paths::ID_OP1) => Ok("Op1Get"),

            // Op20Get - GET /op20
            &hyper::Method::Get if path.matched(paths::ID_OP20) => Ok("Op20Get"),

            // Op21Get - GET /op21
            &hyper::Method::Get if path.matched(paths::ID_OP21) => Ok("Op21Get"),

            // Op22Get - GET /op22
            &hyper::Method::Get if path.matched(paths::ID_OP22) => Ok("Op22Get"),

            // Op23Get - GET /op23
            &hyper::Method::Get if path.matched(paths::ID_OP23) => Ok("Op23Get"),

            // Op24Get - GET /op24
            &hyper::Method::Get if path.matched(paths::ID_OP24) => Ok("Op24Get"),

            // Op25Get - GET /op25
            &hyper::Method::Get if path.matched(paths::ID_OP25) => Ok("Op25Get"),

            // Op26Get - GET /op26
            &hyper::Method::Get if path.matched(paths::ID_OP26) => Ok("Op26Get"),

            // Op27Get - GET /op27
            &hyper::Method::Get if path.matched(paths::ID_OP27) => Ok("Op27Get"),

            // Op28Get - GET /op28
            &hyper::Method::Get if path.matched(paths::ID_OP28) => Ok("Op28Get"),

            // Op29Get - GET /op29
            &hyper::Method::Get if path.matched(paths::ID_OP29) => Ok("Op29Get"),

            // Op2Get - GET /op2
            &hyper::Method::Get if path.matched(paths::ID_OP2) => Ok("Op2Get"),

            // Op30Get - GET /op30
            &hyper::Method::Get if path.matched(paths::ID_OP30) => Ok("Op30Get"),

            // Op31Get - GET /op31
            &hyper::Method::Get if path.matched(paths::ID_OP31) => Ok("Op31Get"),

            // Op32Get - GET /op32
            &hyper::Method::Get if path.matched(paths::ID_OP32) => Ok("Op32Get"),

            // Op33Get - GET /op33
            &hyper::Method::Get if path.matched(paths::ID_OP33) => Ok("Op33Get"),

            // Op34Get - GET /op34
            &hyper::Method::Get if path.matched(paths::ID_OP34) => Ok("Op34Get"),

            // Op35Get - GET /op35
            &hyper::Method::Get if path.matched(paths::ID_OP35) => Ok("Op35Get"),

            // Op36Get - GET /op36
            &hyper::Method::Get if path.matched(paths::ID_OP36) => Ok("Op36Get"),

            // Op37Get - GET /op37
            &hyper::Method::Get if path.matched(paths::ID_OP37) => Ok("Op37Get"),

            // Op3Get - GET /op3
            &hyper::Method::Get if path.matched(paths::ID_OP3) => Ok("Op3Get"),

            // Op4Get - GET /op4
            &hyper::Method::Get if path.matched(paths::ID_OP4) => Ok("Op4Get"),

            // Op5Get - GET /op5
            &hyper::Method::Get if path.matched(paths::ID_OP5) => Ok("Op5Get"),

            // Op6Get - GET /op6
            &hyper::Method::Get if path.matched(paths::ID_OP6) => Ok("Op6Get"),

            // Op7Get - GET /op7
            &hyper::Method::Get if path.matched(paths::ID_OP7) => Ok("Op7Get"),

            // Op8Get - GET /op8
            &hyper::Method::Get if path.matched(paths::ID_OP8) => Ok("Op8Get"),

            // Op9Get - GET /op9
            &hyper::Method::Get if path.matched(paths::ID_OP9) => Ok("Op9Get"),
            _ => Err(()),
        }
    }
}
