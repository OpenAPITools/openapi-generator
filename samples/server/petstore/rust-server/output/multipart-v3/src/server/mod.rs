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
use hyper_0_10::header::{Headers, ContentType};
header! { (ContentId, "Content-ID") => [String] }
use mime_0_2::{TopLevel, SubLevel, Mime as Mime2};
use mime_multipart::{read_multipart_body, Node, Part};
use multipart::server::Multipart;
use multipart::server::save::SaveResult;

#[allow(unused_imports)]
use models;
use header;

pub use crate::context;

use {Api,
     MultipartRelatedRequestPostResponse,
     MultipartRequestPostResponse
};

mod paths {
    extern crate regex;

    lazy_static! {
        pub static ref GLOBAL_REGEX_SET: regex::RegexSet = regex::RegexSet::new(vec![
            r"^/multipart_related_request$",
            r"^/multipart_request$"
        ])
        .expect("Unable to create global regex set");
    }
    pub static ID_MULTIPART_RELATED_REQUEST: usize = 0;
    pub static ID_MULTIPART_REQUEST: usize = 1;
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
    type Future = Box<dyn Future<Item = Response<Self::ResBody>, Error = Self::Error> + Send>;

    fn call(&mut self, req: Request<Self::ReqBody>) -> Self::Future {
        let api_impl = self.api_impl.clone();
        let (parts, body) = req.into_parts();
        let (method, uri, headers) = (parts.method, parts.uri, parts.headers);
        let path = paths::GLOBAL_REGEX_SET.matches(uri.path());
        let mut context = body.context;
        let body = body.inner;

        match &method {

            // MultipartRelatedRequestPost - POST /multipart_related_request
            &hyper::Method::POST if path.matched(paths::ID_MULTIPART_RELATED_REQUEST) => {
                // Body parameters (note that non-required body parameters will ignore garbage
                // values, rather than causing a 400 response). Produce warning header and logs for
                // any unused fields.
                Box::new(body.concat2()
                    .then(move |result| -> Self::Future {
                        match result {
                            Ok(body) => {
                                let mut unused_elements: Vec<String> = vec![];

                                // Get multipart chunks.

                                // Extract the top-level content type header.
                                let content_type_mime = headers
                                    .get(CONTENT_TYPE)
                                    .ok_or("Missing content-type header".to_string())
                                    .and_then(|v| v.to_str().map_err(|e| format!("Couldn't read content-type header value for MultipartRelatedRequestPost: {}", e)))
                                    .and_then(|v| v.parse::<Mime2>().map_err(|_e| format!("Couldn't parse content-type header value for MultipartRelatedRequestPost")));

                                // Insert top-level content type header into a Headers object.
                                let mut multi_part_headers = Headers::new();
                                match content_type_mime {
                                    Ok(content_type_mime) => {
                                        multi_part_headers.set(ContentType(content_type_mime));
                                    },
                                    Err(e) => {
                                        return Box::new(future::ok(Response::builder()
                                                .status(StatusCode::BAD_REQUEST)
                                                .body(Body::from(e))
                                                .expect("Unable to create Bad Request response due to unable to read content-type header for MultipartRelatedRequestPost")));
                                    }
                                }

                                // &*body expresses the body as a byteslice, &mut provides a
                                // mutable reference to that byteslice.
                                let nodes = match read_multipart_body(&mut&*body, &multi_part_headers, false) {
                                    Ok(nodes) => nodes,
                                    Err(e) => {
                                        return Box::new(future::ok(Response::builder()
                                                .status(StatusCode::BAD_REQUEST)
                                                .body(Body::from(format!("Could not read multipart body for MultipartRelatedRequestPost: {}", e)))
                                                .expect("Unable to create Bad Request response due to unable to read multipart body for MultipartRelatedRequestPost")));
                                    }
                                };

                                let mut param_object_field = None;
                                let mut param_optional_binary_field = None;
                                let mut param_required_binary_field = None;

                                for node in nodes {
                                    if let Node::Part(part) = node {
                                        let content_type = part.content_type().map(|x| format!("{}",x));
                                        match content_type.as_ref().map(|x| x.as_str()) {
                                            Some("application/json") => {
                                                // Extract JSON part.
                                                let deserializer = &mut serde_json::Deserializer::from_slice(part.body.as_slice());
                                                let json_data: models::MultipartRequestObjectField = match serde_ignored::deserialize(deserializer, |path| {
                                                    warn!("Ignoring unknown field in JSON part: {}", path);
                                                    unused_elements.push(path.to_string());
                                                }) {
                                                    Ok(json_data) => json_data,
                                                    Err(e) => return Box::new(future::ok(Response::builder()
                                                                    .status(StatusCode::BAD_REQUEST)
                                                                    .body(Body::from(format!("Couldn't parse body parameter models::MultipartRequestObjectField - doesn't match schema: {}", e)))
                                                                    .expect("Unable to create Bad Request response for invalid body parameter models::MultipartRequestObjectField due to schema")))
                                                };
                                                // Push JSON part to return object.
                                                param_object_field.get_or_insert(json_data);
                                            },
                                            Some("application/zip") => {
                                                param_optional_binary_field.get_or_insert(swagger::ByteArray(part.body));
                                            },
                                            Some("image/png") => {
                                                param_required_binary_field.get_or_insert(swagger::ByteArray(part.body));
                                            },
                                            Some(content_type) => {
                                                warn!("Ignoring unknown content type: {}", content_type);
                                                unused_elements.push(content_type.to_string());
                                            },
                                            None => {
                                                warn!("Missing content type");
                                            },
                                        }
                                    } else {
                                        unimplemented!("No support for handling unexpected parts");
                                        // unused_elements.push();
                                    }
                                }

                                // Check that the required multipart chunks are present.
                                let param_required_binary_field = match param_required_binary_field {
                                    Some(x) => x,
                                    None =>  return Box::new(future::ok(Response::builder()
                                                                    .status(StatusCode::BAD_REQUEST)
                                                                    .body(Body::from(format!("Missing required multipart/related parameter required_binary_field")))
                                                                    .expect("Unable to create Bad Request response for missing multipart/related parameter required_binary_field due to schema")))
                                };

                                Box::new(
                                    api_impl.multipart_related_request_post(
                                            param_required_binary_field,
                                            param_object_field,
                                            param_optional_binary_field,
                                        &context
                                    ).then(move |result| {
                                        let mut response = Response::new(Body::empty());
                                        response.headers_mut().insert(
                                            HeaderName::from_static("x-span-id"),
                                            HeaderValue::from_str((&context as &dyn Has<XSpanIdString>).get().0.clone().to_string().as_str())
                                                .expect("Unable to create X-Span-ID header value"));

                                        match result {
                                            Ok(rsp) => match rsp {
                                                MultipartRelatedRequestPostResponse::OK
                                                => {
                                                    *response.status_mut() = StatusCode::from_u16(201).expect("Unable to turn 201 into a StatusCode");
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
                                                .body(Body::from(format!("Couldn't read body parameter Default: {}", e)))
                                                .expect("Unable to create Bad Request response due to unable to read body parameter Default"))),
                        }
                    })
                ) as Self::Future
            },

            // MultipartRequestPost - POST /multipart_request
            &hyper::Method::POST if path.matched(paths::ID_MULTIPART_REQUEST) => {
                let boundary = match swagger::multipart::boundary(&headers) {
                    Some(boundary) => boundary.to_string(),
                    None => return Box::new(future::ok(Response::builder()
                                .status(StatusCode::BAD_REQUEST)
                                .body(Body::from("Couldn't find valid multipart body".to_string()))
                                .expect("Unable to create Bad Request response for incorrect boundary"))),
                };

                // Form Body parameters (note that non-required body parameters will ignore garbage
                // values, rather than causing a 400 response). Produce warning header and logs for
                // any unused fields.
                Box::new(body.concat2()
                    .then(move |result| -> Self::Future {
                        match result {
                            Ok(body) => {
                                use std::io::Read;

                                // Read Form Parameters from body
                                let mut entries = match Multipart::with_body(&body.to_vec()[..], boundary).save().temp() {
                                    SaveResult::Full(entries) => {
                                        entries
                                    },
                                    _ => {
                                        return Box::new(future::ok(Response::builder()
                                                        .status(StatusCode::BAD_REQUEST)
                                                        .body(Body::from(format!("Unable to process all message parts")))
                                                        .expect("Unable to create Bad Request response due to failure to process all message")))
                                    },
                                };
                                let field_string_field = entries.fields.remove("string_field");
                                let param_string_field = match field_string_field {
                                    Some(field) => {
                                        let mut reader = field[0].data.readable().expect("Unable to read field for string_field");
                                        let mut data = String::new();
                                        reader.read_to_string(&mut data).expect("Reading saved String should never fail");
                                        let string_field_model: String = match serde_json::from_str(&data) {
                                            Ok(model) => model,
                                            Err(e) => {
                                                return Box::new(future::ok(
                                                    Response::builder()
                                                    .status(StatusCode::BAD_REQUEST)
                                                    .body(Body::from(format!("string_field data does not match API definition : {}", e)))
                                                    .expect("Unable to create Bad Request due to missing required form parameter string_field")))
                                            }
                                        };
                                        string_field_model
                                    },
                                    None => {
                                        return Box::new(future::ok(
                                            Response::builder()
                                            .status(StatusCode::BAD_REQUEST)
                                            .body(Body::from(format!("Missing required form parameter string_field")))
                                            .expect("Unable to create Bad Request due to missing required form parameter string_field")))
                                    }
                                };
                                let field_optional_string_field = entries.fields.remove("optional_string_field");
                                let param_optional_string_field = match field_optional_string_field {
                                    Some(field) => {
                                        let mut reader = field[0].data.readable().expect("Unable to read field for optional_string_field");
                                    Some({
                                        let mut data = String::new();
                                        reader.read_to_string(&mut data).expect("Reading saved String should never fail");
                                        let optional_string_field_model: String = match serde_json::from_str(&data) {
                                            Ok(model) => model,
                                            Err(e) => {
                                                return Box::new(future::ok(
                                                    Response::builder()
                                                    .status(StatusCode::BAD_REQUEST)
                                                    .body(Body::from(format!("optional_string_field data does not match API definition : {}", e)))
                                                    .expect("Unable to create Bad Request due to missing required form parameter optional_string_field")))
                                            }
                                        };
                                        optional_string_field_model
                                    })
                                    },
                                    None => {
                                            None
                                    }
                                };
                                let field_object_field = entries.fields.remove("object_field");
                                let param_object_field = match field_object_field {
                                    Some(field) => {
                                        let mut reader = field[0].data.readable().expect("Unable to read field for object_field");
                                    Some({
                                        let mut data = String::new();
                                        reader.read_to_string(&mut data).expect("Reading saved String should never fail");
                                        let object_field_model: models::MultipartRequestObjectField = match serde_json::from_str(&data) {
                                            Ok(model) => model,
                                            Err(e) => {
                                                return Box::new(future::ok(
                                                    Response::builder()
                                                    .status(StatusCode::BAD_REQUEST)
                                                    .body(Body::from(format!("object_field data does not match API definition : {}", e)))
                                                    .expect("Unable to create Bad Request due to missing required form parameter object_field")))
                                            }
                                        };
                                        object_field_model
                                    })
                                    },
                                    None => {
                                            None
                                    }
                                };
                                let field_binary_field = entries.fields.remove("binary_field");
                                let param_binary_field = match field_binary_field {
                                    Some(field) => {
                                        let mut reader = field[0].data.readable().expect("Unable to read field for binary_field");
                                        let mut data = vec![];
                                        reader.read_to_end(&mut data).expect("Reading saved binary data should never fail");
                                        swagger::ByteArray(data)
                                    },
                                    None => {
                                        return Box::new(future::ok(
                                            Response::builder()
                                            .status(StatusCode::BAD_REQUEST)
                                            .body(Body::from(format!("Missing required form parameter binary_field")))
                                            .expect("Unable to create Bad Request due to missing required form parameter binary_field")))
                                    }
                                };
                                Box::new(
                                    api_impl.multipart_request_post(
                                            param_string_field,
                                            param_binary_field,
                                            param_optional_string_field,
                                            param_object_field,
                                        &context
                                    ).then(move |result| {
                                        let mut response = Response::new(Body::empty());
                                        response.headers_mut().insert(
                                            HeaderName::from_static("x-span-id"),
                                            HeaderValue::from_str((&context as &dyn Has<XSpanIdString>).get().0.clone().to_string().as_str())
                                                .expect("Unable to create X-Span-ID header value"));

                                        match result {
                                            Ok(rsp) => match rsp {
                                                MultipartRequestPostResponse::OK
                                                => {
                                                    *response.status_mut() = StatusCode::from_u16(201).expect("Unable to turn 201 into a StatusCode");
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
                                as Self::Future
                            },
                            Err(e) => Box::new(future::ok(Response::builder()
                                                .status(StatusCode::BAD_REQUEST)
                                                .body(Body::from(format!("Couldn't read multipart body")))
                                                .expect("Unable to create Bad Request response due to unable read multipart body"))),
                        }
                    })
                )
            },

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
            // MultipartRelatedRequestPost - POST /multipart_related_request
            &hyper::Method::POST if path.matched(paths::ID_MULTIPART_RELATED_REQUEST) => Ok("MultipartRelatedRequestPost"),
            // MultipartRequestPost - POST /multipart_request
            &hyper::Method::POST if path.matched(paths::ID_MULTIPART_REQUEST) => Ok("MultipartRequestPost"),
            _ => Err(()),
        }
    }
}
