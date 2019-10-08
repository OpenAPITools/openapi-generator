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
use serde_json;

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
use crate::models;

pub mod context;
pub mod tls;

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
    C: Has<XSpanId>  + Send + Sync + 'static
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
    C: Has<XSpanId>  + Send + Sync + 'static
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
    
        // Op10Get - GET /op10
        &hyper::Method::GET if path.matched(paths::ID_OP10) => {
            let result = api_impl.op10_get(&context).await;
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
                Op10GetResponse::OK
            
                => {
                    response = response.status(hyper::http::StatusCode::from_u16(200).unwrap());
            
                    Ok(response.body(Body::empty()).unwrap())
                },
            }
        },
    
        // Op11Get - GET /op11
        &hyper::Method::GET if path.matched(paths::ID_OP11) => {
            let result = api_impl.op11_get(&context).await;
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
                Op11GetResponse::OK
            
                => {
                    response = response.status(hyper::http::StatusCode::from_u16(200).unwrap());
            
                    Ok(response.body(Body::empty()).unwrap())
                },
            }
        },
    
        // Op12Get - GET /op12
        &hyper::Method::GET if path.matched(paths::ID_OP12) => {
            let result = api_impl.op12_get(&context).await;
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
                Op12GetResponse::OK
            
                => {
                    response = response.status(hyper::http::StatusCode::from_u16(200).unwrap());
            
                    Ok(response.body(Body::empty()).unwrap())
                },
            }
        },
    
        // Op13Get - GET /op13
        &hyper::Method::GET if path.matched(paths::ID_OP13) => {
            let result = api_impl.op13_get(&context).await;
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
                Op13GetResponse::OK
            
                => {
                    response = response.status(hyper::http::StatusCode::from_u16(200).unwrap());
            
                    Ok(response.body(Body::empty()).unwrap())
                },
            }
        },
    
        // Op14Get - GET /op14
        &hyper::Method::GET if path.matched(paths::ID_OP14) => {
            let result = api_impl.op14_get(&context).await;
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
                Op14GetResponse::OK
            
                => {
                    response = response.status(hyper::http::StatusCode::from_u16(200).unwrap());
            
                    Ok(response.body(Body::empty()).unwrap())
                },
            }
        },
    
        // Op15Get - GET /op15
        &hyper::Method::GET if path.matched(paths::ID_OP15) => {
            let result = api_impl.op15_get(&context).await;
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
                Op15GetResponse::OK
            
                => {
                    response = response.status(hyper::http::StatusCode::from_u16(200).unwrap());
            
                    Ok(response.body(Body::empty()).unwrap())
                },
            }
        },
    
        // Op16Get - GET /op16
        &hyper::Method::GET if path.matched(paths::ID_OP16) => {
            let result = api_impl.op16_get(&context).await;
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
                Op16GetResponse::OK
            
                => {
                    response = response.status(hyper::http::StatusCode::from_u16(200).unwrap());
            
                    Ok(response.body(Body::empty()).unwrap())
                },
            }
        },
    
        // Op17Get - GET /op17
        &hyper::Method::GET if path.matched(paths::ID_OP17) => {
            let result = api_impl.op17_get(&context).await;
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
                Op17GetResponse::OK
            
                => {
                    response = response.status(hyper::http::StatusCode::from_u16(200).unwrap());
            
                    Ok(response.body(Body::empty()).unwrap())
                },
            }
        },
    
        // Op18Get - GET /op18
        &hyper::Method::GET if path.matched(paths::ID_OP18) => {
            let result = api_impl.op18_get(&context).await;
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
                Op18GetResponse::OK
            
                => {
                    response = response.status(hyper::http::StatusCode::from_u16(200).unwrap());
            
                    Ok(response.body(Body::empty()).unwrap())
                },
            }
        },
    
        // Op19Get - GET /op19
        &hyper::Method::GET if path.matched(paths::ID_OP19) => {
            let result = api_impl.op19_get(&context).await;
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
                Op19GetResponse::OK
            
                => {
                    response = response.status(hyper::http::StatusCode::from_u16(200).unwrap());
            
                    Ok(response.body(Body::empty()).unwrap())
                },
            }
        },
    
        // Op1Get - GET /op1
        &hyper::Method::GET if path.matched(paths::ID_OP1) => {
            let result = api_impl.op1_get(&context).await;
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
                Op1GetResponse::OK
            
                => {
                    response = response.status(hyper::http::StatusCode::from_u16(200).unwrap());
            
                    Ok(response.body(Body::empty()).unwrap())
                },
            }
        },
    
        // Op20Get - GET /op20
        &hyper::Method::GET if path.matched(paths::ID_OP20) => {
            let result = api_impl.op20_get(&context).await;
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
                Op20GetResponse::OK
            
                => {
                    response = response.status(hyper::http::StatusCode::from_u16(200).unwrap());
            
                    Ok(response.body(Body::empty()).unwrap())
                },
            }
        },
    
        // Op21Get - GET /op21
        &hyper::Method::GET if path.matched(paths::ID_OP21) => {
            let result = api_impl.op21_get(&context).await;
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
                Op21GetResponse::OK
            
                => {
                    response = response.status(hyper::http::StatusCode::from_u16(200).unwrap());
            
                    Ok(response.body(Body::empty()).unwrap())
                },
            }
        },
    
        // Op22Get - GET /op22
        &hyper::Method::GET if path.matched(paths::ID_OP22) => {
            let result = api_impl.op22_get(&context).await;
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
                Op22GetResponse::OK
            
                => {
                    response = response.status(hyper::http::StatusCode::from_u16(200).unwrap());
            
                    Ok(response.body(Body::empty()).unwrap())
                },
            }
        },
    
        // Op23Get - GET /op23
        &hyper::Method::GET if path.matched(paths::ID_OP23) => {
            let result = api_impl.op23_get(&context).await;
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
                Op23GetResponse::OK
            
                => {
                    response = response.status(hyper::http::StatusCode::from_u16(200).unwrap());
            
                    Ok(response.body(Body::empty()).unwrap())
                },
            }
        },
    
        // Op24Get - GET /op24
        &hyper::Method::GET if path.matched(paths::ID_OP24) => {
            let result = api_impl.op24_get(&context).await;
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
                Op24GetResponse::OK
            
                => {
                    response = response.status(hyper::http::StatusCode::from_u16(200).unwrap());
            
                    Ok(response.body(Body::empty()).unwrap())
                },
            }
        },
    
        // Op25Get - GET /op25
        &hyper::Method::GET if path.matched(paths::ID_OP25) => {
            let result = api_impl.op25_get(&context).await;
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
                Op25GetResponse::OK
            
                => {
                    response = response.status(hyper::http::StatusCode::from_u16(200).unwrap());
            
                    Ok(response.body(Body::empty()).unwrap())
                },
            }
        },
    
        // Op26Get - GET /op26
        &hyper::Method::GET if path.matched(paths::ID_OP26) => {
            let result = api_impl.op26_get(&context).await;
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
                Op26GetResponse::OK
            
                => {
                    response = response.status(hyper::http::StatusCode::from_u16(200).unwrap());
            
                    Ok(response.body(Body::empty()).unwrap())
                },
            }
        },
    
        // Op27Get - GET /op27
        &hyper::Method::GET if path.matched(paths::ID_OP27) => {
            let result = api_impl.op27_get(&context).await;
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
                Op27GetResponse::OK
            
                => {
                    response = response.status(hyper::http::StatusCode::from_u16(200).unwrap());
            
                    Ok(response.body(Body::empty()).unwrap())
                },
            }
        },
    
        // Op28Get - GET /op28
        &hyper::Method::GET if path.matched(paths::ID_OP28) => {
            let result = api_impl.op28_get(&context).await;
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
                Op28GetResponse::OK
            
                => {
                    response = response.status(hyper::http::StatusCode::from_u16(200).unwrap());
            
                    Ok(response.body(Body::empty()).unwrap())
                },
            }
        },
    
        // Op29Get - GET /op29
        &hyper::Method::GET if path.matched(paths::ID_OP29) => {
            let result = api_impl.op29_get(&context).await;
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
                Op29GetResponse::OK
            
                => {
                    response = response.status(hyper::http::StatusCode::from_u16(200).unwrap());
            
                    Ok(response.body(Body::empty()).unwrap())
                },
            }
        },
    
        // Op2Get - GET /op2
        &hyper::Method::GET if path.matched(paths::ID_OP2) => {
            let result = api_impl.op2_get(&context).await;
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
                Op2GetResponse::OK
            
                => {
                    response = response.status(hyper::http::StatusCode::from_u16(200).unwrap());
            
                    Ok(response.body(Body::empty()).unwrap())
                },
            }
        },
    
        // Op30Get - GET /op30
        &hyper::Method::GET if path.matched(paths::ID_OP30) => {
            let result = api_impl.op30_get(&context).await;
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
                Op30GetResponse::OK
            
                => {
                    response = response.status(hyper::http::StatusCode::from_u16(200).unwrap());
            
                    Ok(response.body(Body::empty()).unwrap())
                },
            }
        },
    
        // Op31Get - GET /op31
        &hyper::Method::GET if path.matched(paths::ID_OP31) => {
            let result = api_impl.op31_get(&context).await;
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
                Op31GetResponse::OK
            
                => {
                    response = response.status(hyper::http::StatusCode::from_u16(200).unwrap());
            
                    Ok(response.body(Body::empty()).unwrap())
                },
            }
        },
    
        // Op32Get - GET /op32
        &hyper::Method::GET if path.matched(paths::ID_OP32) => {
            let result = api_impl.op32_get(&context).await;
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
                Op32GetResponse::OK
            
                => {
                    response = response.status(hyper::http::StatusCode::from_u16(200).unwrap());
            
                    Ok(response.body(Body::empty()).unwrap())
                },
            }
        },
    
        // Op33Get - GET /op33
        &hyper::Method::GET if path.matched(paths::ID_OP33) => {
            let result = api_impl.op33_get(&context).await;
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
                Op33GetResponse::OK
            
                => {
                    response = response.status(hyper::http::StatusCode::from_u16(200).unwrap());
            
                    Ok(response.body(Body::empty()).unwrap())
                },
            }
        },
    
        // Op34Get - GET /op34
        &hyper::Method::GET if path.matched(paths::ID_OP34) => {
            let result = api_impl.op34_get(&context).await;
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
                Op34GetResponse::OK
            
                => {
                    response = response.status(hyper::http::StatusCode::from_u16(200).unwrap());
            
                    Ok(response.body(Body::empty()).unwrap())
                },
            }
        },
    
        // Op35Get - GET /op35
        &hyper::Method::GET if path.matched(paths::ID_OP35) => {
            let result = api_impl.op35_get(&context).await;
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
                Op35GetResponse::OK
            
                => {
                    response = response.status(hyper::http::StatusCode::from_u16(200).unwrap());
            
                    Ok(response.body(Body::empty()).unwrap())
                },
            }
        },
    
        // Op36Get - GET /op36
        &hyper::Method::GET if path.matched(paths::ID_OP36) => {
            let result = api_impl.op36_get(&context).await;
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
                Op36GetResponse::OK
            
                => {
                    response = response.status(hyper::http::StatusCode::from_u16(200).unwrap());
            
                    Ok(response.body(Body::empty()).unwrap())
                },
            }
        },
    
        // Op37Get - GET /op37
        &hyper::Method::GET if path.matched(paths::ID_OP37) => {
            let result = api_impl.op37_get(&context).await;
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
                Op37GetResponse::OK
            
                => {
                    response = response.status(hyper::http::StatusCode::from_u16(200).unwrap());
            
                    Ok(response.body(Body::empty()).unwrap())
                },
            }
        },
    
        // Op3Get - GET /op3
        &hyper::Method::GET if path.matched(paths::ID_OP3) => {
            let result = api_impl.op3_get(&context).await;
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
                Op3GetResponse::OK
            
                => {
                    response = response.status(hyper::http::StatusCode::from_u16(200).unwrap());
            
                    Ok(response.body(Body::empty()).unwrap())
                },
            }
        },
    
        // Op4Get - GET /op4
        &hyper::Method::GET if path.matched(paths::ID_OP4) => {
            let result = api_impl.op4_get(&context).await;
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
                Op4GetResponse::OK
            
                => {
                    response = response.status(hyper::http::StatusCode::from_u16(200).unwrap());
            
                    Ok(response.body(Body::empty()).unwrap())
                },
            }
        },
    
        // Op5Get - GET /op5
        &hyper::Method::GET if path.matched(paths::ID_OP5) => {
            let result = api_impl.op5_get(&context).await;
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
                Op5GetResponse::OK
            
                => {
                    response = response.status(hyper::http::StatusCode::from_u16(200).unwrap());
            
                    Ok(response.body(Body::empty()).unwrap())
                },
            }
        },
    
        // Op6Get - GET /op6
        &hyper::Method::GET if path.matched(paths::ID_OP6) => {
            let result = api_impl.op6_get(&context).await;
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
                Op6GetResponse::OK
            
                => {
                    response = response.status(hyper::http::StatusCode::from_u16(200).unwrap());
            
                    Ok(response.body(Body::empty()).unwrap())
                },
            }
        },
    
        // Op7Get - GET /op7
        &hyper::Method::GET if path.matched(paths::ID_OP7) => {
            let result = api_impl.op7_get(&context).await;
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
                Op7GetResponse::OK
            
                => {
                    response = response.status(hyper::http::StatusCode::from_u16(200).unwrap());
            
                    Ok(response.body(Body::empty()).unwrap())
                },
            }
        },
    
        // Op8Get - GET /op8
        &hyper::Method::GET if path.matched(paths::ID_OP8) => {
            let result = api_impl.op8_get(&context).await;
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
                Op8GetResponse::OK
            
                => {
                    response = response.status(hyper::http::StatusCode::from_u16(200).unwrap());
            
                    Ok(response.body(Body::empty()).unwrap())
                },
            }
        },
    
        // Op9Get - GET /op9
        &hyper::Method::GET if path.matched(paths::ID_OP9) => {
            let result = api_impl.op9_get(&context).await;
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
                Op9GetResponse::OK
            
                => {
                    response = response.status(hyper::http::StatusCode::from_u16(200).unwrap());
            
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


/// Request parser for `Api`.
pub struct ApiRequestParser;
impl RequestParser<Body> for ApiRequestParser {
    fn parse_operation_id(request: &Request<Body>) -> Result<&'static str, ()> {
        let path = paths::GLOBAL_REGEX_SET.matches(request.uri().path());
        match request.method() {

            // Op10Get - GET /op10
            &hyper::Method::GET if path.matched(paths::ID_OP10) => Ok("Op10Get"),

            // Op11Get - GET /op11
            &hyper::Method::GET if path.matched(paths::ID_OP11) => Ok("Op11Get"),

            // Op12Get - GET /op12
            &hyper::Method::GET if path.matched(paths::ID_OP12) => Ok("Op12Get"),

            // Op13Get - GET /op13
            &hyper::Method::GET if path.matched(paths::ID_OP13) => Ok("Op13Get"),

            // Op14Get - GET /op14
            &hyper::Method::GET if path.matched(paths::ID_OP14) => Ok("Op14Get"),

            // Op15Get - GET /op15
            &hyper::Method::GET if path.matched(paths::ID_OP15) => Ok("Op15Get"),

            // Op16Get - GET /op16
            &hyper::Method::GET if path.matched(paths::ID_OP16) => Ok("Op16Get"),

            // Op17Get - GET /op17
            &hyper::Method::GET if path.matched(paths::ID_OP17) => Ok("Op17Get"),

            // Op18Get - GET /op18
            &hyper::Method::GET if path.matched(paths::ID_OP18) => Ok("Op18Get"),

            // Op19Get - GET /op19
            &hyper::Method::GET if path.matched(paths::ID_OP19) => Ok("Op19Get"),

            // Op1Get - GET /op1
            &hyper::Method::GET if path.matched(paths::ID_OP1) => Ok("Op1Get"),

            // Op20Get - GET /op20
            &hyper::Method::GET if path.matched(paths::ID_OP20) => Ok("Op20Get"),

            // Op21Get - GET /op21
            &hyper::Method::GET if path.matched(paths::ID_OP21) => Ok("Op21Get"),

            // Op22Get - GET /op22
            &hyper::Method::GET if path.matched(paths::ID_OP22) => Ok("Op22Get"),

            // Op23Get - GET /op23
            &hyper::Method::GET if path.matched(paths::ID_OP23) => Ok("Op23Get"),

            // Op24Get - GET /op24
            &hyper::Method::GET if path.matched(paths::ID_OP24) => Ok("Op24Get"),

            // Op25Get - GET /op25
            &hyper::Method::GET if path.matched(paths::ID_OP25) => Ok("Op25Get"),

            // Op26Get - GET /op26
            &hyper::Method::GET if path.matched(paths::ID_OP26) => Ok("Op26Get"),

            // Op27Get - GET /op27
            &hyper::Method::GET if path.matched(paths::ID_OP27) => Ok("Op27Get"),

            // Op28Get - GET /op28
            &hyper::Method::GET if path.matched(paths::ID_OP28) => Ok("Op28Get"),

            // Op29Get - GET /op29
            &hyper::Method::GET if path.matched(paths::ID_OP29) => Ok("Op29Get"),

            // Op2Get - GET /op2
            &hyper::Method::GET if path.matched(paths::ID_OP2) => Ok("Op2Get"),

            // Op30Get - GET /op30
            &hyper::Method::GET if path.matched(paths::ID_OP30) => Ok("Op30Get"),

            // Op31Get - GET /op31
            &hyper::Method::GET if path.matched(paths::ID_OP31) => Ok("Op31Get"),

            // Op32Get - GET /op32
            &hyper::Method::GET if path.matched(paths::ID_OP32) => Ok("Op32Get"),

            // Op33Get - GET /op33
            &hyper::Method::GET if path.matched(paths::ID_OP33) => Ok("Op33Get"),

            // Op34Get - GET /op34
            &hyper::Method::GET if path.matched(paths::ID_OP34) => Ok("Op34Get"),

            // Op35Get - GET /op35
            &hyper::Method::GET if path.matched(paths::ID_OP35) => Ok("Op35Get"),

            // Op36Get - GET /op36
            &hyper::Method::GET if path.matched(paths::ID_OP36) => Ok("Op36Get"),

            // Op37Get - GET /op37
            &hyper::Method::GET if path.matched(paths::ID_OP37) => Ok("Op37Get"),

            // Op3Get - GET /op3
            &hyper::Method::GET if path.matched(paths::ID_OP3) => Ok("Op3Get"),

            // Op4Get - GET /op4
            &hyper::Method::GET if path.matched(paths::ID_OP4) => Ok("Op4Get"),

            // Op5Get - GET /op5
            &hyper::Method::GET if path.matched(paths::ID_OP5) => Ok("Op5Get"),

            // Op6Get - GET /op6
            &hyper::Method::GET if path.matched(paths::ID_OP6) => Ok("Op6Get"),

            // Op7Get - GET /op7
            &hyper::Method::GET if path.matched(paths::ID_OP7) => Ok("Op7Get"),

            // Op8Get - GET /op8
            &hyper::Method::GET if path.matched(paths::ID_OP8) => Ok("Op8Get"),

            // Op9Get - GET /op9
            &hyper::Method::GET if path.matched(paths::ID_OP9) => Ok("Op9Get"),
            _ => Err(()),
        }
    }
}
