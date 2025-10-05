use bytes::Bytes;
use futures::{future, future::BoxFuture, Stream, stream, future::FutureExt, stream::TryStreamExt};
use http_body_util::{combinators::BoxBody, Full};
use hyper::{body::{Body, Incoming}, HeaderMap, Request, Response, StatusCode};
use hyper::header::{HeaderName, HeaderValue, CONTENT_TYPE};
use log::warn;
#[allow(unused_imports)]
use std::convert::{TryFrom, TryInto};
use std::{convert::Infallible, error::Error};
use std::future::Future;
use std::marker::PhantomData;
use std::task::{Context, Poll};
use swagger::{ApiError, BodyExt, Has, RequestParser, XSpanIdString};
pub use swagger::auth::Authorization;
use swagger::auth::Scopes;
use url::form_urlencoded;
use mime_multipart::{read_multipart_body, Node, Part};
use multipart::server::Multipart;
use multipart::server::save::{PartialReason, SaveResult};

#[allow(unused_imports)]
use crate::{models, header, AuthenticationApi};

pub use crate::context;

type ServiceFuture = BoxFuture<'static, Result<Response<BoxBody<Bytes, Infallible>>, crate::ServiceError>>;

use crate::{Api,
     MultipartRelatedRequestPostResponse,
     MultipartRequestPostResponse,
     MultipleIdenticalMimeTypesPostResponse
};

mod server_auth;

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


pub struct MakeService<T, C>
where
    T: Api<C> + Clone + Send + 'static,
    C: Has<XSpanIdString>  + Send + Sync + 'static
{
    api_impl: T,
    multipart_form_size_limit: Option<u64>,
    marker: PhantomData<C>,
}

impl<T, C> MakeService<T, C>
where
    T: Api<C> + Clone + Send + 'static,
    C: Has<XSpanIdString>  + Send + Sync + 'static
{
    pub fn new(api_impl: T) -> Self {
        MakeService {
            api_impl,
            multipart_form_size_limit: Some(8 * 1024 * 1024),
            marker: PhantomData
        }
    }

    /// Configure size limit when inspecting a multipart/form body.
    ///
    /// Default is 8 MiB.
    ///
    /// Set to None for no size limit, which presents a Denial of Service attack risk.
    pub fn multipart_form_size_limit(mut self, multipart_form_size_limit: Option<u64>) -> Self {
        self.multipart_form_size_limit = multipart_form_size_limit;
        self
    }
}

impl<T, C> Clone for MakeService<T, C>
where
    T: Api<C> + Clone + Send + 'static,
    C: Has<XSpanIdString>   + Send + Sync + 'static
{
    fn clone(&self) -> Self {
        Self {
            api_impl: self.api_impl.clone(),
            multipart_form_size_limit: Some(8 * 1024 * 1024),
            marker: PhantomData,
        }
    }
}

impl<T, C, Target> hyper::service::Service<Target> for MakeService<T, C>
where
    T: Api<C> + Clone + Send + 'static,
    C: Has<XSpanIdString>  + Send + Sync + 'static
{
    type Response = Service<T, C>;
    type Error = crate::ServiceError;
    type Future = future::Ready<Result<Self::Response, Self::Error>>;

    fn call(&self, target: Target) -> Self::Future {
        let service = Service::new(self.api_impl.clone())
            .multipart_form_size_limit(self.multipart_form_size_limit);

        future::ok(service)
    }
}

fn method_not_allowed() -> Result<Response<BoxBody<Bytes, Infallible>>, crate::ServiceError> {
    Ok(
        Response::builder().status(StatusCode::METHOD_NOT_ALLOWED)
            .body(BoxBody::new(http_body_util::Empty::new()))
            .expect("Unable to create Method Not Allowed response")
    )
}

pub struct Service<T, C> where
    T: Api<C> + Clone + Send + 'static,
    C: Has<XSpanIdString>  + Send + Sync + 'static
{
    api_impl: T,
    multipart_form_size_limit: Option<u64>,
    marker: PhantomData<C>,
}

impl<T, C> Service<T, C> where
    T: Api<C> + Clone + Send + 'static,
    C: Has<XSpanIdString>  + Send + Sync + 'static
{
    pub fn new(api_impl: T) -> Self {
        Service {
            api_impl,
            multipart_form_size_limit: Some(8 * 1024 * 1024),
            marker: PhantomData
        }
    }

    /// Configure size limit when extracting a multipart/form body.
    ///
    /// Default is 8 MiB.
    ///
    /// Set to None for no size limit, which presents a Denial of Service attack risk.
    pub fn multipart_form_size_limit(mut self, multipart_form_size_limit: Option<u64>) -> Self {
        self.multipart_form_size_limit = multipart_form_size_limit;
        self
    }
}

impl<T, C> Clone for Service<T, C> where
    T: Api<C> + Clone + Send + 'static,
    C: Has<XSpanIdString>  + Send + Sync + 'static
{
    fn clone(&self) -> Self {
        Service {
            api_impl: self.api_impl.clone(),
            multipart_form_size_limit: Some(8 * 1024 * 1024),
            marker: self.marker,
        }
    }
}

#[allow(dead_code)]
fn body_from_string(s: String) -> BoxBody<Bytes, Infallible> {
    BoxBody::new(Full::new(Bytes::from(s)))
}

fn body_from_str(s: &str) -> BoxBody<Bytes, Infallible> {
    BoxBody::new(Full::new(Bytes::copy_from_slice(s.as_bytes())))
}

impl<T, C, ReqBody> hyper::service::Service<(Request<ReqBody>, C)> for Service<T, C> where
    T: Api<C> + Clone + Send + Sync + 'static,
    C: Has<XSpanIdString>  + Send + Sync + 'static,
    ReqBody: Body + Send + 'static,
    ReqBody::Error: Into<Box<dyn Error + Send + Sync>> + Send,
    ReqBody::Data: Send,
{
    type Response = Response<BoxBody<Bytes, Infallible>>;
    type Error = crate::ServiceError;
    type Future = ServiceFuture;

    fn call(&self, req: (Request<ReqBody>, C)) -> Self::Future {
        async fn run<T, C, ReqBody>(
            mut api_impl: T,
            req: (Request<ReqBody>, C),
            multipart_form_size_limit: Option<u64>,
        ) -> Result<Response<BoxBody<Bytes, Infallible>>, crate::ServiceError>
        where
            T: Api<C> + Clone + Send + 'static,
            C: Has<XSpanIdString>  + Send + Sync + 'static,
            ReqBody: Body + Send + 'static,
            ReqBody::Error: Into<Box<dyn Error + Send + Sync>> + Send,
            ReqBody::Data: Send,
        {
            let (request, context) = req;
            let (parts, body) = request.into_parts();
            let (method, uri, headers) = (parts.method, parts.uri, parts.headers);
            let path = paths::GLOBAL_REGEX_SET.matches(uri.path());

            match method {

            // MultipartRelatedRequestPost - POST /multipart_related_request
            hyper::Method::POST if path.matched(paths::ID_MULTIPART_RELATED_REQUEST) => {
                // Handle body parameters (note that non-required body parameters will ignore garbage
                // values, rather than causing a 400 response). Produce warning header and logs for
                // any unused fields.
                let result = http_body_util::BodyExt::collect(body).await.map(|f| f.to_bytes().to_vec());
                match result {
                     Ok(body) => {
                                let mut unused_elements : Vec<String> = vec![];
                                // Get multipart chunks.

                                // Create headers from top-level content type header.
                                let multipart_headers = match swagger::multipart::related::create_multipart_headers(headers.get(CONTENT_TYPE)) {
                                    Ok(headers) => headers,
                                    Err(e) => {
                                        return Ok(Response::builder()
                                                .status(StatusCode::BAD_REQUEST)
                                                .body(BoxBody::new(e.to_string()))
                                                .expect("Unable to create Bad Request response due to unable to read content-type header for MultipartRelatedRequestPost"));
                                    }
                                };

                                // &*body expresses the body as a byteslice, &mut provides a
                                // mutable reference to that byteslice.
                                let nodes = match read_multipart_body(&mut&*body, &multipart_headers, false) {
                                    Ok(nodes) => nodes,
                                    Err(e) => {
                                        return Ok(Response::builder()
                                                .status(StatusCode::BAD_REQUEST)
                                                .body(BoxBody::new(format!("Could not read multipart body for MultipartRelatedRequestPost: {e}")))
                                                .expect("Unable to create Bad Request response due to unable to read multipart body for MultipartRelatedRequestPost"));
                                    }
                                };

                                let mut param_object_field = None;
                                let mut param_optional_binary_field = None;
                                let mut param_required_binary_field = None;

                                for node in nodes {
                                    if let Node::Part(part) = node {
                                        let content_type = part.content_type().map(|x| format!("{x}"));
                                        match content_type.as_deref() {
                                            Some("application/json") if param_object_field.is_none() => {
                                                // Extract JSON part.
                                                let deserializer = &mut serde_json::Deserializer::from_slice(part.body.as_slice());
                                                let json_data: models::MultipartRequestObjectField = match serde_ignored::deserialize(deserializer, |path| {
                                                    warn!("Ignoring unknown field in JSON part: {path}");
                                                    unused_elements.push(path.to_string());
                                                }) {
                                                    Ok(json_data) => json_data,
                                                    Err(e) => return Ok(Response::builder()
                                                        .status(StatusCode::BAD_REQUEST)
                                                        .body(BoxBody::new(format!("Couldn't parse body parameter models::MultipartRequestObjectField - doesn't match schema: {e}")))
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
                                                warn!("Ignoring unexpected content type: {content_type}");
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
                                        .body(BoxBody::new("Missing required multipart/related parameter required_binary_field".to_string()))
                                        .expect("Unable to create Bad Request response for missing multipart/related parameter required_binary_field due to schema"))
                                };


                                let result = api_impl.multipart_related_request_post(
                                            param_required_binary_field,
                                            param_object_field,
                                            param_optional_binary_field,
                                        &context
                                    ).await;
                                let mut response = Response::new(BoxBody::new(http_body_util::Empty::new()));
                                response.headers_mut().insert(
                                            HeaderName::from_static("x-span-id"),
                                            HeaderValue::from_str((&context as &dyn Has<XSpanIdString>).get().0.clone().as_str())
                                                .expect("Unable to create X-Span-ID header value"));

                                        if !unused_elements.is_empty() {
                                            response.headers_mut().insert(
                                                HeaderName::from_static("warning"),
                                                HeaderValue::from_str(format!("Ignoring unknown fields in body: {unused_elements:?}").as_str())
                                                    .expect("Unable to create Warning header value"));
                                        }
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
                                                *response.body_mut() = body_from_str("An internal error occurred");
                                            },
                                        }

                                        Ok(response)
                            },
                            Err(e) => Ok(Response::builder()
                                                .status(StatusCode::BAD_REQUEST)
                                                .body(body_from_string(format!("Unable to read body: {}", e.into())))
                                                .expect("Unable to create Bad Request response due to unable to read body")),
                        }
            },

            // MultipartRequestPost - POST /multipart_request
            hyper::Method::POST if path.matched(paths::ID_MULTIPART_REQUEST) => {
                // Handle body parameters (note that non-required body parameters will ignore garbage
                // values, rather than causing a 400 response). Produce warning header and logs for
                // any unused fields.
                let result = http_body_util::BodyExt::collect(body).await.map(|f| f.to_bytes().to_vec());
                match result {
                     Ok(body) => {
                                let boundary = match swagger::multipart::form::boundary(&headers) {
                                    Some(boundary) => boundary.to_string(),
                                    None => return Ok(Response::builder()
                                                .status(StatusCode::BAD_REQUEST)
                                                .body(BoxBody::new("Couldn't find valid multipart body".to_string()))
                                                .expect("Unable to create Bad Request response for incorrect boundary")),
                                };

                                use std::io::Read;

                                // Read Form Parameters from body
                                let mut entries = match Multipart::with_body(&body.to_vec()[..], boundary)
                                    .save()
                                    .size_limit(multipart_form_size_limit)
                                    .temp()
                                {
                                    SaveResult::Full(entries) => {
                                        entries
                                    },
                                    SaveResult::Partial(_, PartialReason::CountLimit) => {
                                        return Ok(Response::builder()
                                                        .status(StatusCode::BAD_REQUEST)
                                                        .body(BoxBody::new("Unable to process message part due to excessive parts".to_string()))
                                                        .expect("Unable to create Bad Request response due to excessive parts"))
                                    },
                                    SaveResult::Partial(_, PartialReason::SizeLimit) => {
                                        return Ok(Response::builder()
                                                        .status(StatusCode::BAD_REQUEST)
                                                        .body(BoxBody::new("Unable to process message part due to excessive data".to_string()))
                                                        .expect("Unable to create Bad Request response due to excessive data"))
                                    },
                                    SaveResult::Partial(_, PartialReason::Utf8Error(_)) => {
                                        return Ok(Response::builder()
                                                        .status(StatusCode::BAD_REQUEST)
                                                        .body(BoxBody::new("Unable to process message part due to invalid data".to_string()))
                                                        .expect("Unable to create Bad Request response due to invalid data"))
                                    },
                                    SaveResult::Partial(_, PartialReason::IoError(_)) => {
                                        return Ok(Response::builder()
                                                        .status(StatusCode::INTERNAL_SERVER_ERROR)
                                                        .body(BoxBody::new("Failed to process message part due an internal error".to_string()))
                                                        .expect("Unable to create Internal Server Error response due to an internal error"))
                                    },
                                    SaveResult::Error(e) => {
                                        return Ok(Response::builder()
                                                        .status(StatusCode::INTERNAL_SERVER_ERROR)
                                                        .body(BoxBody::new("Failed to process all message parts due to an internal error".to_string()))
                                                        .expect("Unable to create Internal Server Error response due to an internal error"))
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
                                                    .body(BoxBody::new(format!("string_field data does not match API definition : {e}")))
                                                    .expect("Unable to create Bad Request due to missing required form parameter string_field"))
                                            }
                                        };
                                        string_field_model
                                    },
                                    None => {
                                        return Ok(
                                            Response::builder()
                                            .status(StatusCode::BAD_REQUEST)
                                            .body(BoxBody::new("Missing required form parameter string_field".to_string()))
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
                                                    .body(BoxBody::new(format!("optional_string_field data does not match API definition : {e}")))
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
                                                    .body(BoxBody::new(format!("object_field data does not match API definition : {e}")))
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
                                            .body(BoxBody::new("Missing required form parameter binary_field".to_string()))
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
                                let mut response = Response::new(BoxBody::new(http_body_util::Empty::new()));
                                response.headers_mut().insert(
                                            HeaderName::from_static("x-span-id"),
                                            HeaderValue::from_str((&context as &dyn Has<XSpanIdString>).get().0.clone().as_str())
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
                                                *response.body_mut() = body_from_str("An internal error occurred");
                                            },
                                        }

                                        Ok(response)
                            },
                            Err(e) => Ok(Response::builder()
                                                .status(StatusCode::BAD_REQUEST)
                                                .body(body_from_string(format!("Unable to read body: {}", e.into())))
                                                .expect("Unable to create Bad Request response due to unable to read body")),
                        }
            },

            // MultipleIdenticalMimeTypesPost - POST /multiple-identical-mime-types
            hyper::Method::POST if path.matched(paths::ID_MULTIPLE_IDENTICAL_MIME_TYPES) => {
                // Handle body parameters (note that non-required body parameters will ignore garbage
                // values, rather than causing a 400 response). Produce warning header and logs for
                // any unused fields.
                let result = http_body_util::BodyExt::collect(body).await.map(|f| f.to_bytes().to_vec());
                match result {
                     Ok(body) => {
                                let mut unused_elements : Vec<String> = vec![];
                                // Get multipart chunks.

                                // Create headers from top-level content type header.
                                let multipart_headers = match swagger::multipart::related::create_multipart_headers(headers.get(CONTENT_TYPE)) {
                                    Ok(headers) => headers,
                                    Err(e) => {
                                        return Ok(Response::builder()
                                                .status(StatusCode::BAD_REQUEST)
                                                .body(BoxBody::new(e.to_string()))
                                                .expect("Unable to create Bad Request response due to unable to read content-type header for MultipleIdenticalMimeTypesPost"));
                                    }
                                };

                                // &*body expresses the body as a byteslice, &mut provides a
                                // mutable reference to that byteslice.
                                let nodes = match read_multipart_body(&mut&*body, &multipart_headers, false) {
                                    Ok(nodes) => nodes,
                                    Err(e) => {
                                        return Ok(Response::builder()
                                                .status(StatusCode::BAD_REQUEST)
                                                .body(BoxBody::new(format!("Could not read multipart body for MultipleIdenticalMimeTypesPost: {e}")))
                                                .expect("Unable to create Bad Request response due to unable to read multipart body for MultipleIdenticalMimeTypesPost"));
                                    }
                                };

                                let mut param_binary1 = None;
                                let mut param_binary2 = None;

                                for node in nodes {
                                    if let Node::Part(part) = node {
                                        let content_type = part.content_type().map(|x| format!("{x}"));
                                        match content_type.as_deref() {
                                            Some("application/octet-stream") if param_binary1.is_none() => {
                                                param_binary1.get_or_insert(swagger::ByteArray(part.body));
                                            },
                                            Some("application/octet-stream") if param_binary2.is_none() => {
                                                param_binary2.get_or_insert(swagger::ByteArray(part.body));
                                            },
                                            Some(content_type) => {
                                                warn!("Ignoring unexpected content type: {content_type}");
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
                                let mut response = Response::new(BoxBody::new(http_body_util::Empty::new()));
                                response.headers_mut().insert(
                                            HeaderName::from_static("x-span-id"),
                                            HeaderValue::from_str((&context as &dyn Has<XSpanIdString>).get().0.clone().as_str())
                                                .expect("Unable to create X-Span-ID header value"));

                                        if !unused_elements.is_empty() {
                                            response.headers_mut().insert(
                                                HeaderName::from_static("warning"),
                                                HeaderValue::from_str(format!("Ignoring unknown fields in body: {unused_elements:?}").as_str())
                                                    .expect("Unable to create Warning header value"));
                                        }
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
                                                *response.body_mut() = body_from_str("An internal error occurred");
                                            },
                                        }

                                        Ok(response)
                            },
                            Err(e) => Ok(Response::builder()
                                                .status(StatusCode::BAD_REQUEST)
                                                .body(body_from_string(format!("Unable to read body: {}", e.into())))
                                                .expect("Unable to create Bad Request response due to unable to read body")),
                        }
            },

            _ if path.matched(paths::ID_MULTIPART_RELATED_REQUEST) => method_not_allowed(),
            _ if path.matched(paths::ID_MULTIPART_REQUEST) => method_not_allowed(),
            _ if path.matched(paths::ID_MULTIPLE_IDENTICAL_MIME_TYPES) => method_not_allowed(),
                _ => Ok(Response::builder().status(StatusCode::NOT_FOUND)
                        .body(BoxBody::new(http_body_util::Empty::new()))
                        .expect("Unable to create Not Found response"))
            }
        }
        Box::pin(run(
            self.api_impl.clone(),
            req,
            self.multipart_form_size_limit,
        ))
    }
}

/// Request parser for `Api`.
pub struct ApiRequestParser;
impl<T> RequestParser<T> for ApiRequestParser {
    fn parse_operation_id(request: &Request<T>) -> Option<&'static str> {
        let path = paths::GLOBAL_REGEX_SET.matches(request.uri().path());
        match *request.method() {
            // MultipartRelatedRequestPost - POST /multipart_related_request
            hyper::Method::POST if path.matched(paths::ID_MULTIPART_RELATED_REQUEST) => Some("MultipartRelatedRequestPost"),
            // MultipartRequestPost - POST /multipart_request
            hyper::Method::POST if path.matched(paths::ID_MULTIPART_REQUEST) => Some("MultipartRequestPost"),
            // MultipleIdenticalMimeTypesPost - POST /multiple-identical-mime-types
            hyper::Method::POST if path.matched(paths::ID_MULTIPLE_IDENTICAL_MIME_TYPES) => Some("MultipleIdenticalMimeTypesPost"),
            _ => None,
        }
    }
}
