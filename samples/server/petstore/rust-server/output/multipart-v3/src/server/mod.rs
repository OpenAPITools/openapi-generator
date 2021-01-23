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
use hyper_0_10::header::{Headers, ContentType};
use mime_0_2::{TopLevel, SubLevel, Mime as Mime2};
use mime_multipart::{read_multipart_body, Node, Part};
use multipart::server::Multipart;
use multipart::server::save::SaveResult;

#[allow(unused_imports)]
use crate::models;
use crate::header;

pub use crate::context;

type ServiceFuture = BoxFuture<'static, Result<Response<Body>, crate::ServiceError>>;

use crate::{Api,
     MultipartRelatedRequestPostResponse,
     MultipartRequestPostResponse,
     MultipleIdenticalMimeTypesPostResponse
};

mod paths {
    use lazy_static::lazy_static;

    lazy_static! {
        pub static ref GLOBAL_REGEX_SET: regex::RegexSet = regex::RegexSet::new(vec![
            r"^/multipart_related_request$",
            r"^/multipart_request$",
            r"^/multiple-identical-mime-types$"
        ])
        .expect("Unable to create global regex set");
    }
    pub(crate) static ID_MULTIPART_RELATED_REQUEST: usize = 0;
    pub(crate) static ID_MULTIPART_REQUEST: usize = 1;
    pub(crate) static ID_MULTIPLE_IDENTICAL_MIME_TYPES: usize = 2;
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
            api_impl: api_impl,
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
            marker: self.marker.clone(),
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

        match &method {

            // MultipartRelatedRequestPost - POST /multipart_related_request
            &hyper::Method::POST if path.matched(paths::ID_MULTIPART_RELATED_REQUEST) => {
                // Body parameters (note that non-required body parameters will ignore garbage
                // values, rather than causing a 400 response). Produce warning header and logs for
                // any unused fields.
                let result = body.to_raw();
                match result.await {
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
                                        return Ok(Response::builder()
                                                .status(StatusCode::BAD_REQUEST)
                                                .body(Body::from(e))
                                                .expect("Unable to create Bad Request response due to unable to read content-type header for MultipartRelatedRequestPost"));
                                    }
                                }

                                // &*body expresses the body as a byteslice, &mut provides a
                                // mutable reference to that byteslice.
                                let nodes = match read_multipart_body(&mut&*body, &multi_part_headers, false) {
                                    Ok(nodes) => nodes,
                                    Err(e) => {
                                        return Ok(Response::builder()
                                                .status(StatusCode::BAD_REQUEST)
                                                .body(Body::from(format!("Could not read multipart body for MultipartRelatedRequestPost: {}", e)))
                                                .expect("Unable to create Bad Request response due to unable to read multipart body for MultipartRelatedRequestPost"));
                                    }
                                };

                                let mut param_object_field = None;
                                let mut param_optional_binary_field = None;
                                let mut param_required_binary_field = None;

                                for node in nodes {
                                    if let Node::Part(part) = node {
                                        let content_type = part.content_type().map(|x| format!("{}",x));
                                        match content_type.as_ref().map(|x| x.as_str()) {
                                            Some("application/json") if param_object_field.is_none() => {
                                                // Extract JSON part.
                                                let deserializer = &mut serde_json::Deserializer::from_slice(part.body.as_slice());
                                                let json_data: models::MultipartRequestObjectField = match serde_ignored::deserialize(deserializer, |path| {
                                                    warn!("Ignoring unknown field in JSON part: {}", path);
                                                    unused_elements.push(path.to_string());
                                                }) {
                                                    Ok(json_data) => json_data,
                                                    Err(e) => return Ok(Response::builder()
                                                                    .status(StatusCode::BAD_REQUEST)
                                                                    .body(Body::from(format!("Couldn't parse body parameter models::MultipartRequestObjectField - doesn't match schema: {}", e)))
                                                                    .expect("Unable to create Bad Request response for invalid body parameter models::MultipartRequestObjectField due to schema"))
                                                };
                                                // Push JSON part to return object.
                                                param_object_field.get_or_insert(json_data);
                                            },
                                            Some("application/zip") if param_optional_binary_field.is_none() => {
                                                param_optional_binary_field.get_or_insert(swagger::ByteArray(part.body));
                                            },
                                            Some("image/png") if param_required_binary_field.is_none() => {
                                                param_required_binary_field.get_or_insert(swagger::ByteArray(part.body));
                                            },
                                            Some(content_type) => {
                                                warn!("Ignoring unexpected content type: {}", content_type);
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
                                    None =>  return Ok(Response::builder()
                                                                    .status(StatusCode::BAD_REQUEST)
                                                                    .body(Body::from(format!("Missing required multipart/related parameter required_binary_field")))
                                                                    .expect("Unable to create Bad Request response for missing multipart/related parameter required_binary_field due to schema"))
                                };

                                let result = api_impl.multipart_related_request_post(
                                            param_required_binary_field,
                                            param_object_field,
                                            param_optional_binary_field,
                                        &context
                                    ).await;
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

                                        Ok(response)
                            },
                            Err(e) => Ok(Response::builder()
                                                .status(StatusCode::BAD_REQUEST)
                                                .body(Body::from(format!("Couldn't read body parameter Default: {}", e)))
                                                .expect("Unable to create Bad Request response due to unable to read body parameter Default")),
                        }
            },

            // MultipartRequestPost - POST /multipart_request
            &hyper::Method::POST if path.matched(paths::ID_MULTIPART_REQUEST) => {
                let boundary = match swagger::multipart::form::boundary(&headers) {
                    Some(boundary) => boundary.to_string(),
                    None => return Ok(Response::builder()
                                .status(StatusCode::BAD_REQUEST)
                                .body(Body::from("Couldn't find valid multipart body".to_string()))
                                .expect("Unable to create Bad Request response for incorrect boundary")),
                };

                // Form Body parameters (note that non-required body parameters will ignore garbage
                // values, rather than causing a 400 response). Produce warning header and logs for
                // any unused fields.
                let result = body.to_raw();
                match result.await {
                            Ok(body) => {
                                use std::io::Read;

                                // Read Form Parameters from body
                                let mut entries = match Multipart::with_body(&body.to_vec()[..], boundary).save().temp() {
                                    SaveResult::Full(entries) => {
                                        entries
                                    },
                                    _ => {
                                        return Ok(Response::builder()
                                                        .status(StatusCode::BAD_REQUEST)
                                                        .body(Body::from(format!("Unable to process all message parts")))
                                                        .expect("Unable to create Bad Request response due to failure to process all message"))
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
                                                return Ok(
                                                    Response::builder()
                                                    .status(StatusCode::BAD_REQUEST)
                                                    .body(Body::from(format!("string_field data does not match API definition : {}", e)))
                                                    .expect("Unable to create Bad Request due to missing required form parameter string_field"))
                                            }
                                        };
                                        string_field_model
                                    },
                                    None => {
                                        return Ok(
                                            Response::builder()
                                            .status(StatusCode::BAD_REQUEST)
                                            .body(Body::from(format!("Missing required form parameter string_field")))
                                            .expect("Unable to create Bad Request due to missing required form parameter string_field"))
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
                                                return Ok(
                                                    Response::builder()
                                                    .status(StatusCode::BAD_REQUEST)
                                                    .body(Body::from(format!("optional_string_field data does not match API definition : {}", e)))
                                                    .expect("Unable to create Bad Request due to missing required form parameter optional_string_field"))
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
                                                return Ok(
                                                    Response::builder()
                                                    .status(StatusCode::BAD_REQUEST)
                                                    .body(Body::from(format!("object_field data does not match API definition : {}", e)))
                                                    .expect("Unable to create Bad Request due to missing required form parameter object_field"))
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
                                        return Ok(
                                            Response::builder()
                                            .status(StatusCode::BAD_REQUEST)
                                            .body(Body::from(format!("Missing required form parameter binary_field")))
                                            .expect("Unable to create Bad Request due to missing required form parameter binary_field"))
                                    }
                                };
                                let result = api_impl.multipart_request_post(
                                            param_string_field,
                                            param_binary_field,
                                            param_optional_string_field,
                                            param_object_field,
                                        &context
                                    ).await;
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

                                        Ok(response)
                            },
                            Err(e) => Ok(Response::builder()
                                                .status(StatusCode::BAD_REQUEST)
                                                .body(Body::from(format!("Couldn't read multipart body")))
                                                .expect("Unable to create Bad Request response due to unable read multipart body")),
                        }
            },

            // MultipleIdenticalMimeTypesPost - POST /multiple-identical-mime-types
            &hyper::Method::POST if path.matched(paths::ID_MULTIPLE_IDENTICAL_MIME_TYPES) => {
                // Body parameters (note that non-required body parameters will ignore garbage
                // values, rather than causing a 400 response). Produce warning header and logs for
                // any unused fields.
                let result = body.to_raw();
                match result.await {
                            Ok(body) => {
                                let mut unused_elements: Vec<String> = vec![];

                                // Get multipart chunks.

                                // Extract the top-level content type header.
                                let content_type_mime = headers
                                    .get(CONTENT_TYPE)
                                    .ok_or("Missing content-type header".to_string())
                                    .and_then(|v| v.to_str().map_err(|e| format!("Couldn't read content-type header value for MultipleIdenticalMimeTypesPost: {}", e)))
                                    .and_then(|v| v.parse::<Mime2>().map_err(|_e| format!("Couldn't parse content-type header value for MultipleIdenticalMimeTypesPost")));

                                // Insert top-level content type header into a Headers object.
                                let mut multi_part_headers = Headers::new();
                                match content_type_mime {
                                    Ok(content_type_mime) => {
                                        multi_part_headers.set(ContentType(content_type_mime));
                                    },
                                    Err(e) => {
                                        return Ok(Response::builder()
                                                .status(StatusCode::BAD_REQUEST)
                                                .body(Body::from(e))
                                                .expect("Unable to create Bad Request response due to unable to read content-type header for MultipleIdenticalMimeTypesPost"));
                                    }
                                }

                                // &*body expresses the body as a byteslice, &mut provides a
                                // mutable reference to that byteslice.
                                let nodes = match read_multipart_body(&mut&*body, &multi_part_headers, false) {
                                    Ok(nodes) => nodes,
                                    Err(e) => {
                                        return Ok(Response::builder()
                                                .status(StatusCode::BAD_REQUEST)
                                                .body(Body::from(format!("Could not read multipart body for MultipleIdenticalMimeTypesPost: {}", e)))
                                                .expect("Unable to create Bad Request response due to unable to read multipart body for MultipleIdenticalMimeTypesPost"));
                                    }
                                };

                                let mut param_binary1 = None;
                                let mut param_binary2 = None;

                                for node in nodes {
                                    if let Node::Part(part) = node {
                                        let content_type = part.content_type().map(|x| format!("{}",x));
                                        match content_type.as_ref().map(|x| x.as_str()) {
                                            Some("application/octet-stream") if param_binary1.is_none() => {
                                                param_binary1.get_or_insert(swagger::ByteArray(part.body));
                                            },
                                            Some("application/octet-stream") if param_binary2.is_none() => {
                                                param_binary2.get_or_insert(swagger::ByteArray(part.body));
                                            },
                                            Some(content_type) => {
                                                warn!("Ignoring unexpected content type: {}", content_type);
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

                                let result = api_impl.multiple_identical_mime_types_post(
                                            param_binary1,
                                            param_binary2,
                                        &context
                                    ).await;
                                let mut response = Response::new(Body::empty());
                                response.headers_mut().insert(
                                            HeaderName::from_static("x-span-id"),
                                            HeaderValue::from_str((&context as &dyn Has<XSpanIdString>).get().0.clone().to_string().as_str())
                                                .expect("Unable to create X-Span-ID header value"));

                                        match result {
                                            Ok(rsp) => match rsp {
                                                MultipleIdenticalMimeTypesPostResponse::OK
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
                            Err(e) => Ok(Response::builder()
                                                .status(StatusCode::BAD_REQUEST)
                                                .body(Body::from(format!("Couldn't read body parameter Default: {}", e)))
                                                .expect("Unable to create Bad Request response due to unable to read body parameter Default")),
                        }
            },

            _ if path.matched(paths::ID_MULTIPART_RELATED_REQUEST) => method_not_allowed(),
            _ if path.matched(paths::ID_MULTIPART_REQUEST) => method_not_allowed(),
            _ if path.matched(paths::ID_MULTIPLE_IDENTICAL_MIME_TYPES) => method_not_allowed(),
            _ => Ok(Response::builder().status(StatusCode::NOT_FOUND)
                    .body(Body::empty())
                    .expect("Unable to create Not Found response"))
        }
    } Box::pin(run(self.api_impl.clone(), req)) }
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
            // MultipleIdenticalMimeTypesPost - POST /multiple-identical-mime-types
            &hyper::Method::POST if path.matched(paths::ID_MULTIPLE_IDENTICAL_MIME_TYPES) => Ok("MultipleIdenticalMimeTypesPost"),
            _ => Err(()),
        }
    }
}
