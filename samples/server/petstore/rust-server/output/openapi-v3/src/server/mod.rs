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
     AnyOfGetResponse,
     CallbackWithHeaderPostResponse,
     ComplexQueryParamGetResponse,
     EnumInPathPathParamGetResponse,
     JsonComplexQueryParamGetResponse,
     MandatoryRequestHeaderGetResponse,
     MergePatchJsonGetResponse,
     MultigetGetResponse,
     MultipleAuthSchemeGetResponse,
     OneOfGetResponse,
     OverrideServerGetResponse,
     ParamgetGetResponse,
     ReadonlyAuthSchemeGetResponse,
     RegisterCallbackPostResponse,
     RequiredOctetStreamPutResponse,
     ResponsesWithHeadersGetResponse,
     Rfc7807GetResponse,
     UntypedPropertyGetResponse,
     UuidGetResponse,
     XmlExtraPostResponse,
     XmlOtherPostResponse,
     XmlOtherPutResponse,
     XmlPostResponse,
     XmlPutResponse,
     CreateRepoResponse,
     GetRepoInfoResponse
};

pub mod callbacks;

mod paths {
    use lazy_static::lazy_static;

    lazy_static! {
        pub static ref GLOBAL_REGEX_SET: regex::RegexSet = regex::RegexSet::new(vec![
            r"^/any-of$",
            r"^/callback-with-header$",
            r"^/complex-query-param$",
            r"^/enum_in_path/(?P<path_param>[^/?#]*)$",
            r"^/json-complex-query-param$",
            r"^/mandatory-request-header$",
            r"^/merge-patch-json$",
            r"^/multiget$",
            r"^/multiple_auth_scheme$",
            r"^/one-of$",
            r"^/override-server$",
            r"^/paramget$",
            r"^/readonly_auth_scheme$",
            r"^/register-callback$",
            r"^/repos$",
            r"^/repos/(?P<repoId>[^/?#]*)$",
            r"^/required_octet_stream$",
            r"^/responses_with_headers$",
            r"^/rfc7807$",
            r"^/untyped_property$",
            r"^/uuid$",
            r"^/xml$",
            r"^/xml_extra$",
            r"^/xml_other$"
        ])
        .expect("Unable to create global regex set");
    }
    pub(crate) static ID_ANY_OF: usize = 0;
    pub(crate) static ID_CALLBACK_WITH_HEADER: usize = 1;
    pub(crate) static ID_COMPLEX_QUERY_PARAM: usize = 2;
    pub(crate) static ID_ENUM_IN_PATH_PATH_PARAM: usize = 3;
    lazy_static! {
        pub static ref REGEX_ENUM_IN_PATH_PATH_PARAM: regex::Regex =
            regex::Regex::new(r"^/enum_in_path/(?P<path_param>[^/?#]*)$")
                .expect("Unable to create regex for ENUM_IN_PATH_PATH_PARAM");
    }
    pub(crate) static ID_JSON_COMPLEX_QUERY_PARAM: usize = 4;
    pub(crate) static ID_MANDATORY_REQUEST_HEADER: usize = 5;
    pub(crate) static ID_MERGE_PATCH_JSON: usize = 6;
    pub(crate) static ID_MULTIGET: usize = 7;
    pub(crate) static ID_MULTIPLE_AUTH_SCHEME: usize = 8;
    pub(crate) static ID_ONE_OF: usize = 9;
    pub(crate) static ID_OVERRIDE_SERVER: usize = 10;
    pub(crate) static ID_PARAMGET: usize = 11;
    pub(crate) static ID_READONLY_AUTH_SCHEME: usize = 12;
    pub(crate) static ID_REGISTER_CALLBACK: usize = 13;
    pub(crate) static ID_REPOS: usize = 14;
    pub(crate) static ID_REPOS_REPOID: usize = 15;
    lazy_static! {
        pub static ref REGEX_REPOS_REPOID: regex::Regex =
            regex::Regex::new(r"^/repos/(?P<repoId>[^/?#]*)$")
                .expect("Unable to create regex for REPOS_REPOID");
    }
    pub(crate) static ID_REQUIRED_OCTET_STREAM: usize = 16;
    pub(crate) static ID_RESPONSES_WITH_HEADERS: usize = 17;
    pub(crate) static ID_RFC7807: usize = 18;
    pub(crate) static ID_UNTYPED_PROPERTY: usize = 19;
    pub(crate) static ID_UUID: usize = 20;
    pub(crate) static ID_XML: usize = 21;
    pub(crate) static ID_XML_EXTRA: usize = 22;
    pub(crate) static ID_XML_OTHER: usize = 23;
}

pub struct MakeService<T, C> where
    T: Api<C> + Clone + Send + 'static,
    C: Has<XSpanIdString> + Has<Option<Authorization>> + Send + Sync + 'static
{
    api_impl: T,
    marker: PhantomData<C>,
}

impl<T, C> MakeService<T, C> where
    T: Api<C> + Clone + Send + 'static,
    C: Has<XSpanIdString> + Has<Option<Authorization>> + Send + Sync + 'static
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
    C: Has<XSpanIdString> + Has<Option<Authorization>> + Send + Sync + 'static
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
    C: Has<XSpanIdString> + Has<Option<Authorization>> + Send + Sync + 'static
{
    api_impl: T,
    marker: PhantomData<C>,
}

impl<T, C> Service<T, C> where
    T: Api<C> + Clone + Send + 'static,
    C: Has<XSpanIdString> + Has<Option<Authorization>> + Send + Sync + 'static
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
    C: Has<XSpanIdString> + Has<Option<Authorization>> + Send + Sync + 'static
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
    C: Has<XSpanIdString> + Has<Option<Authorization>> + Send + Sync + 'static
{
    type Response = Response<Body>;
    type Error = crate::ServiceError;
    type Future = ServiceFuture;

    fn poll_ready(&mut self, cx: &mut Context) -> Poll<Result<(), Self::Error>> {
        self.api_impl.poll_ready(cx)
    }

    fn call(&mut self, req: (Request<Body>, C)) -> Self::Future { async fn run<T, C>(mut api_impl: T, req: (Request<Body>, C)) -> Result<Response<Body>, crate::ServiceError> where
        T: Api<C> + Clone + Send + 'static,
        C: Has<XSpanIdString> + Has<Option<Authorization>> + Send + Sync + 'static
    {
        let (request, context) = req;
        let (parts, body) = request.into_parts();
        let (method, uri, headers) = (parts.method, parts.uri, parts.headers);
        let path = paths::GLOBAL_REGEX_SET.matches(uri.path());

        match &method {

            // AnyOfGet - GET /any-of
            &hyper::Method::GET if path.matched(paths::ID_ANY_OF) => {
                // Query parameters (note that non-required or collection query parameters will ignore garbage values, rather than causing a 400 response)
                let query_params = form_urlencoded::parse(uri.query().unwrap_or_default().as_bytes()).collect::<Vec<_>>();
                let param_any_of = query_params.iter().filter(|e| e.0 == "any-of").map(|e| e.1.to_owned())
                    .filter_map(|param_any_of| param_any_of.parse().ok())
                    .collect::<Vec<_>>();
                let param_any_of = if !param_any_of.is_empty() {
                    Some(param_any_of)
                } else {
                    None
                };

                                let result = api_impl.any_of_get(
                                            param_any_of.as_ref(),
                                        &context
                                    ).await;
                                let mut response = Response::new(Body::empty());
                                response.headers_mut().insert(
                                            HeaderName::from_static("x-span-id"),
                                            HeaderValue::from_str((&context as &dyn Has<XSpanIdString>).get().0.clone().to_string().as_str())
                                                .expect("Unable to create X-Span-ID header value"));

                                        match result {
                                            Ok(rsp) => match rsp {
                                                AnyOfGetResponse::Success
                                                    (body)
                                                => {
                                                    *response.status_mut() = StatusCode::from_u16(200).expect("Unable to turn 200 into a StatusCode");
                                                    response.headers_mut().insert(
                                                        CONTENT_TYPE,
                                                        HeaderValue::from_str("application/json")
                                                            .expect("Unable to create Content-Type header for ANY_OF_GET_SUCCESS"));
                                                    let body = serde_json::to_string(&body).expect("impossible to fail to serialize");
                                                    *response.body_mut() = Body::from(body);
                                                },
                                                AnyOfGetResponse::AlternateSuccess
                                                    (body)
                                                => {
                                                    *response.status_mut() = StatusCode::from_u16(201).expect("Unable to turn 201 into a StatusCode");
                                                    response.headers_mut().insert(
                                                        CONTENT_TYPE,
                                                        HeaderValue::from_str("application/json")
                                                            .expect("Unable to create Content-Type header for ANY_OF_GET_ALTERNATE_SUCCESS"));
                                                    let body = serde_json::to_string(&body).expect("impossible to fail to serialize");
                                                    *response.body_mut() = Body::from(body);
                                                },
                                                AnyOfGetResponse::AnyOfSuccess
                                                    (body)
                                                => {
                                                    *response.status_mut() = StatusCode::from_u16(202).expect("Unable to turn 202 into a StatusCode");
                                                    response.headers_mut().insert(
                                                        CONTENT_TYPE,
                                                        HeaderValue::from_str("application/json")
                                                            .expect("Unable to create Content-Type header for ANY_OF_GET_ANY_OF_SUCCESS"));
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

                                        Ok(response)
            },

            // CallbackWithHeaderPost - POST /callback-with-header
            &hyper::Method::POST if path.matched(paths::ID_CALLBACK_WITH_HEADER) => {
                // Query parameters (note that non-required or collection query parameters will ignore garbage values, rather than causing a 400 response)
                let query_params = form_urlencoded::parse(uri.query().unwrap_or_default().as_bytes()).collect::<Vec<_>>();
                let param_url = query_params.iter().filter(|e| e.0 == "url").map(|e| e.1.to_owned())
                    .nth(0);
                let param_url = match param_url {
                    Some(param_url) => {
                        let param_url =
                            <String as std::str::FromStr>::from_str
                                (&param_url);
                        match param_url {
                            Ok(param_url) => Some(param_url),
                            Err(e) => return Ok(Response::builder()
                                .status(StatusCode::BAD_REQUEST)
                                .body(Body::from(format!("Couldn't parse query parameter url - doesn't match schema: {}", e)))
                                .expect("Unable to create Bad Request response for invalid query parameter url")),
                        }
                    },
                    None => None,
                };
                let param_url = match param_url {
                    Some(param_url) => param_url,
                    None => return Ok(Response::builder()
                        .status(StatusCode::BAD_REQUEST)
                        .body(Body::from("Missing required query parameter url"))
                        .expect("Unable to create Bad Request response for missing query parameter url")),
                };

                                let result = api_impl.callback_with_header_post(
                                            param_url,
                                        &context
                                    ).await;
                                let mut response = Response::new(Body::empty());
                                response.headers_mut().insert(
                                            HeaderName::from_static("x-span-id"),
                                            HeaderValue::from_str((&context as &dyn Has<XSpanIdString>).get().0.clone().to_string().as_str())
                                                .expect("Unable to create X-Span-ID header value"));

                                        match result {
                                            Ok(rsp) => match rsp {
                                                CallbackWithHeaderPostResponse::OK
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

                                        Ok(response)
            },

            // ComplexQueryParamGet - GET /complex-query-param
            &hyper::Method::GET if path.matched(paths::ID_COMPLEX_QUERY_PARAM) => {
                // Query parameters (note that non-required or collection query parameters will ignore garbage values, rather than causing a 400 response)
                let query_params = form_urlencoded::parse(uri.query().unwrap_or_default().as_bytes()).collect::<Vec<_>>();
                let param_list_of_strings = query_params.iter().filter(|e| e.0 == "list-of-strings").map(|e| e.1.to_owned())
                    .filter_map(|param_list_of_strings| param_list_of_strings.parse().ok())
                    .collect::<Vec<_>>();
                let param_list_of_strings = if !param_list_of_strings.is_empty() {
                    Some(param_list_of_strings)
                } else {
                    None
                };

                                let result = api_impl.complex_query_param_get(
                                            param_list_of_strings.as_ref(),
                                        &context
                                    ).await;
                                let mut response = Response::new(Body::empty());
                                response.headers_mut().insert(
                                            HeaderName::from_static("x-span-id"),
                                            HeaderValue::from_str((&context as &dyn Has<XSpanIdString>).get().0.clone().to_string().as_str())
                                                .expect("Unable to create X-Span-ID header value"));

                                        match result {
                                            Ok(rsp) => match rsp {
                                                ComplexQueryParamGetResponse::Success
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

            // EnumInPathPathParamGet - GET /enum_in_path/{path_param}
            &hyper::Method::GET if path.matched(paths::ID_ENUM_IN_PATH_PATH_PARAM) => {
                // Path parameters
                let path: &str = &uri.path().to_string();
                let path_params =
                    paths::REGEX_ENUM_IN_PATH_PATH_PARAM
                    .captures(&path)
                    .unwrap_or_else(||
                        panic!("Path {} matched RE ENUM_IN_PATH_PATH_PARAM in set but failed match against \"{}\"", path, paths::REGEX_ENUM_IN_PATH_PATH_PARAM.as_str())
                    );

                let param_path_param = match percent_encoding::percent_decode(path_params["path_param"].as_bytes()).decode_utf8() {
                    Ok(param_path_param) => match param_path_param.parse::<models::StringEnum>() {
                        Ok(param_path_param) => param_path_param,
                        Err(e) => return Ok(Response::builder()
                                        .status(StatusCode::BAD_REQUEST)
                                        .body(Body::from(format!("Couldn't parse path parameter path_param: {}", e)))
                                        .expect("Unable to create Bad Request response for invalid path parameter")),
                    },
                    Err(_) => return Ok(Response::builder()
                                        .status(StatusCode::BAD_REQUEST)
                                        .body(Body::from(format!("Couldn't percent-decode path parameter as UTF-8: {}", &path_params["path_param"])))
                                        .expect("Unable to create Bad Request response for invalid percent decode"))
                };

                                let result = api_impl.enum_in_path_path_param_get(
                                            param_path_param,
                                        &context
                                    ).await;
                                let mut response = Response::new(Body::empty());
                                response.headers_mut().insert(
                                            HeaderName::from_static("x-span-id"),
                                            HeaderValue::from_str((&context as &dyn Has<XSpanIdString>).get().0.clone().to_string().as_str())
                                                .expect("Unable to create X-Span-ID header value"));

                                        match result {
                                            Ok(rsp) => match rsp {
                                                EnumInPathPathParamGetResponse::Success
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

            // JsonComplexQueryParamGet - GET /json-complex-query-param
            &hyper::Method::GET if path.matched(paths::ID_JSON_COMPLEX_QUERY_PARAM) => {
                // Query parameters (note that non-required or collection query parameters will ignore garbage values, rather than causing a 400 response)
                let query_params = form_urlencoded::parse(uri.query().unwrap_or_default().as_bytes()).collect::<Vec<_>>();
                let param_list_of_strings = query_params.iter().filter(|e| e.0 == "list-of-strings").map(|e| e.1.to_owned())
                    .nth(0);
                let param_list_of_strings = match param_list_of_strings {
                    Some(param_list_of_strings) => {
                        let param_list_of_strings =
                            serde_json::from_str::<Vec<models::StringObject>>
                                (&param_list_of_strings);
                        match param_list_of_strings {
                            Ok(param_list_of_strings) => Some(param_list_of_strings),
                            Err(e) => return Ok(Response::builder()
                                .status(StatusCode::BAD_REQUEST)
                                .body(Body::from(format!("Couldn't parse query parameter list-of-strings - doesn't match schema: {}", e)))
                                .expect("Unable to create Bad Request response for invalid query parameter list-of-strings")),
                        }
                    },
                    None => None,
                };

                                let result = api_impl.json_complex_query_param_get(
                                            param_list_of_strings.as_ref(),
                                        &context
                                    ).await;
                                let mut response = Response::new(Body::empty());
                                response.headers_mut().insert(
                                            HeaderName::from_static("x-span-id"),
                                            HeaderValue::from_str((&context as &dyn Has<XSpanIdString>).get().0.clone().to_string().as_str())
                                                .expect("Unable to create X-Span-ID header value"));

                                        match result {
                                            Ok(rsp) => match rsp {
                                                JsonComplexQueryParamGetResponse::Success
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

            // MandatoryRequestHeaderGet - GET /mandatory-request-header
            &hyper::Method::GET if path.matched(paths::ID_MANDATORY_REQUEST_HEADER) => {
                // Header parameters
                let param_x_header = headers.get(HeaderName::from_static("x-header"));

                let param_x_header = match param_x_header {
                    Some(v) => match header::IntoHeaderValue::<String>::try_from((*v).clone()) {
                        Ok(result) =>
                            result.0,
                        Err(err) => {
                            return Ok(Response::builder()
                                        .status(StatusCode::BAD_REQUEST)
                                        .body(Body::from(format!("Invalid header X-Header - {}", err)))
                                        .expect("Unable to create Bad Request response for invalid header X-Header"));

                        },
                    },
                    None => {
                        return Ok(Response::builder()
                                        .status(StatusCode::BAD_REQUEST)
                                        .body(Body::from("Missing required header X-Header"))
                                        .expect("Unable to create Bad Request response for missing required header X-Header"));
                    }
                };

                                let result = api_impl.mandatory_request_header_get(
                                            param_x_header,
                                        &context
                                    ).await;
                                let mut response = Response::new(Body::empty());
                                response.headers_mut().insert(
                                            HeaderName::from_static("x-span-id"),
                                            HeaderValue::from_str((&context as &dyn Has<XSpanIdString>).get().0.clone().to_string().as_str())
                                                .expect("Unable to create X-Span-ID header value"));

                                        match result {
                                            Ok(rsp) => match rsp {
                                                MandatoryRequestHeaderGetResponse::Success
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

            // MergePatchJsonGet - GET /merge-patch-json
            &hyper::Method::GET if path.matched(paths::ID_MERGE_PATCH_JSON) => {
                                let result = api_impl.merge_patch_json_get(
                                        &context
                                    ).await;
                                let mut response = Response::new(Body::empty());
                                response.headers_mut().insert(
                                            HeaderName::from_static("x-span-id"),
                                            HeaderValue::from_str((&context as &dyn Has<XSpanIdString>).get().0.clone().to_string().as_str())
                                                .expect("Unable to create X-Span-ID header value"));

                                        match result {
                                            Ok(rsp) => match rsp {
                                                MergePatchJsonGetResponse::Merge
                                                    (body)
                                                => {
                                                    *response.status_mut() = StatusCode::from_u16(200).expect("Unable to turn 200 into a StatusCode");
                                                    response.headers_mut().insert(
                                                        CONTENT_TYPE,
                                                        HeaderValue::from_str("application/merge-patch+json")
                                                            .expect("Unable to create Content-Type header for MERGE_PATCH_JSON_GET_MERGE"));
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

                                        Ok(response)
            },

            // MultigetGet - GET /multiget
            &hyper::Method::GET if path.matched(paths::ID_MULTIGET) => {
                                let result = api_impl.multiget_get(
                                        &context
                                    ).await;
                                let mut response = Response::new(Body::empty());
                                response.headers_mut().insert(
                                            HeaderName::from_static("x-span-id"),
                                            HeaderValue::from_str((&context as &dyn Has<XSpanIdString>).get().0.clone().to_string().as_str())
                                                .expect("Unable to create X-Span-ID header value"));

                                        match result {
                                            Ok(rsp) => match rsp {
                                                MultigetGetResponse::JSONRsp
                                                    (body)
                                                => {
                                                    *response.status_mut() = StatusCode::from_u16(200).expect("Unable to turn 200 into a StatusCode");
                                                    response.headers_mut().insert(
                                                        CONTENT_TYPE,
                                                        HeaderValue::from_str("application/json")
                                                            .expect("Unable to create Content-Type header for MULTIGET_GET_JSON_RSP"));
                                                    let body = serde_json::to_string(&body).expect("impossible to fail to serialize");
                                                    *response.body_mut() = Body::from(body);
                                                },
                                                MultigetGetResponse::XMLRsp
                                                    (body)
                                                => {
                                                    *response.status_mut() = StatusCode::from_u16(201).expect("Unable to turn 201 into a StatusCode");
                                                    response.headers_mut().insert(
                                                        CONTENT_TYPE,
                                                        HeaderValue::from_str("application/xml")
                                                            .expect("Unable to create Content-Type header for MULTIGET_GET_XML_RSP"));
                                                    let body = serde_xml_rs::to_string(&body).expect("impossible to fail to serialize");
                                                    *response.body_mut() = Body::from(body);
                                                },
                                                MultigetGetResponse::OctetRsp
                                                    (body)
                                                => {
                                                    *response.status_mut() = StatusCode::from_u16(202).expect("Unable to turn 202 into a StatusCode");
                                                    response.headers_mut().insert(
                                                        CONTENT_TYPE,
                                                        HeaderValue::from_str("application/octet-stream")
                                                            .expect("Unable to create Content-Type header for MULTIGET_GET_OCTET_RSP"));
                                                    let body = body.0;
                                                    *response.body_mut() = Body::from(body);
                                                },
                                                MultigetGetResponse::StringRsp
                                                    (body)
                                                => {
                                                    *response.status_mut() = StatusCode::from_u16(203).expect("Unable to turn 203 into a StatusCode");
                                                    response.headers_mut().insert(
                                                        CONTENT_TYPE,
                                                        HeaderValue::from_str("text/plain")
                                                            .expect("Unable to create Content-Type header for MULTIGET_GET_STRING_RSP"));
                                                    let body = body;
                                                    *response.body_mut() = Body::from(body);
                                                },
                                                MultigetGetResponse::DuplicateResponseLongText
                                                    (body)
                                                => {
                                                    *response.status_mut() = StatusCode::from_u16(204).expect("Unable to turn 204 into a StatusCode");
                                                    response.headers_mut().insert(
                                                        CONTENT_TYPE,
                                                        HeaderValue::from_str("application/json")
                                                            .expect("Unable to create Content-Type header for MULTIGET_GET_DUPLICATE_RESPONSE_LONG_TEXT"));
                                                    let body = serde_json::to_string(&body).expect("impossible to fail to serialize");
                                                    *response.body_mut() = Body::from(body);
                                                },
                                                MultigetGetResponse::DuplicateResponseLongText_2
                                                    (body)
                                                => {
                                                    *response.status_mut() = StatusCode::from_u16(205).expect("Unable to turn 205 into a StatusCode");
                                                    response.headers_mut().insert(
                                                        CONTENT_TYPE,
                                                        HeaderValue::from_str("application/json")
                                                            .expect("Unable to create Content-Type header for MULTIGET_GET_DUPLICATE_RESPONSE_LONG_TEXT_2"));
                                                    let body = serde_json::to_string(&body).expect("impossible to fail to serialize");
                                                    *response.body_mut() = Body::from(body);
                                                },
                                                MultigetGetResponse::DuplicateResponseLongText_3
                                                    (body)
                                                => {
                                                    *response.status_mut() = StatusCode::from_u16(206).expect("Unable to turn 206 into a StatusCode");
                                                    response.headers_mut().insert(
                                                        CONTENT_TYPE,
                                                        HeaderValue::from_str("application/json")
                                                            .expect("Unable to create Content-Type header for MULTIGET_GET_DUPLICATE_RESPONSE_LONG_TEXT_3"));
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

                                        Ok(response)
            },

            // MultipleAuthSchemeGet - GET /multiple_auth_scheme
            &hyper::Method::GET if path.matched(paths::ID_MULTIPLE_AUTH_SCHEME) => {
                {
                    let authorization = match (&context as &dyn Has<Option<Authorization>>).get() {
                        &Some(ref authorization) => authorization,
                        &None => return Ok(Response::builder()
                                                .status(StatusCode::FORBIDDEN)
                                                .body(Body::from("Unauthenticated"))
                                                .expect("Unable to create Authentication Forbidden response")),
                    };

                    // Authorization
                    if let Scopes::Some(ref scopes) = authorization.scopes {
                        let required_scopes: std::collections::BTreeSet<String> = vec![
                            "test.read".to_string(), // Allowed to read state.
                            "test.write".to_string(), // Allowed to change state.
                        ].into_iter().collect();

                        if !required_scopes.is_subset(scopes) {
                            let missing_scopes = required_scopes.difference(scopes);
                            return Ok(Response::builder()
                                .status(StatusCode::FORBIDDEN)
                                .body(Body::from(missing_scopes.fold(
                                    "Insufficient authorization, missing scopes".to_string(),
                                    |s, scope| format!("{} {}", s, scope))
                                ))
                                .expect("Unable to create Authentication Insufficient response")
                            );
                        }
                    }
                }

                                let result = api_impl.multiple_auth_scheme_get(
                                        &context
                                    ).await;
                                let mut response = Response::new(Body::empty());
                                response.headers_mut().insert(
                                            HeaderName::from_static("x-span-id"),
                                            HeaderValue::from_str((&context as &dyn Has<XSpanIdString>).get().0.clone().to_string().as_str())
                                                .expect("Unable to create X-Span-ID header value"));

                                        match result {
                                            Ok(rsp) => match rsp {
                                                MultipleAuthSchemeGetResponse::CheckThatLimitingToMultipleRequiredAuthSchemesWorks
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

            // OneOfGet - GET /one-of
            &hyper::Method::GET if path.matched(paths::ID_ONE_OF) => {
                                let result = api_impl.one_of_get(
                                        &context
                                    ).await;
                                let mut response = Response::new(Body::empty());
                                response.headers_mut().insert(
                                            HeaderName::from_static("x-span-id"),
                                            HeaderValue::from_str((&context as &dyn Has<XSpanIdString>).get().0.clone().to_string().as_str())
                                                .expect("Unable to create X-Span-ID header value"));

                                        match result {
                                            Ok(rsp) => match rsp {
                                                OneOfGetResponse::Success
                                                    (body)
                                                => {
                                                    *response.status_mut() = StatusCode::from_u16(200).expect("Unable to turn 200 into a StatusCode");
                                                    response.headers_mut().insert(
                                                        CONTENT_TYPE,
                                                        HeaderValue::from_str("application/json")
                                                            .expect("Unable to create Content-Type header for ONE_OF_GET_SUCCESS"));
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

                                        Ok(response)
            },

            // OverrideServerGet - GET /override-server
            &hyper::Method::GET if path.matched(paths::ID_OVERRIDE_SERVER) => {
                                let result = api_impl.override_server_get(
                                        &context
                                    ).await;
                                let mut response = Response::new(Body::empty());
                                response.headers_mut().insert(
                                            HeaderName::from_static("x-span-id"),
                                            HeaderValue::from_str((&context as &dyn Has<XSpanIdString>).get().0.clone().to_string().as_str())
                                                .expect("Unable to create X-Span-ID header value"));

                                        match result {
                                            Ok(rsp) => match rsp {
                                                OverrideServerGetResponse::Success
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

                                        Ok(response)
            },

            // ParamgetGet - GET /paramget
            &hyper::Method::GET if path.matched(paths::ID_PARAMGET) => {
                // Query parameters (note that non-required or collection query parameters will ignore garbage values, rather than causing a 400 response)
                let query_params = form_urlencoded::parse(uri.query().unwrap_or_default().as_bytes()).collect::<Vec<_>>();
                let param_uuid = query_params.iter().filter(|e| e.0 == "uuid").map(|e| e.1.to_owned())
                    .nth(0);
                let param_uuid = match param_uuid {
                    Some(param_uuid) => {
                        let param_uuid =
                            <uuid::Uuid as std::str::FromStr>::from_str
                                (&param_uuid);
                        match param_uuid {
                            Ok(param_uuid) => Some(param_uuid),
                            Err(e) => return Ok(Response::builder()
                                .status(StatusCode::BAD_REQUEST)
                                .body(Body::from(format!("Couldn't parse query parameter uuid - doesn't match schema: {}", e)))
                                .expect("Unable to create Bad Request response for invalid query parameter uuid")),
                        }
                    },
                    None => None,
                };
                let param_some_object = query_params.iter().filter(|e| e.0 == "someObject").map(|e| e.1.to_owned())
                    .nth(0);
                let param_some_object = match param_some_object {
                    Some(param_some_object) => {
                        let param_some_object =
                            <models::ObjectParam as std::str::FromStr>::from_str
                                (&param_some_object);
                        match param_some_object {
                            Ok(param_some_object) => Some(param_some_object),
                            Err(e) => return Ok(Response::builder()
                                .status(StatusCode::BAD_REQUEST)
                                .body(Body::from(format!("Couldn't parse query parameter someObject - doesn't match schema: {}", e)))
                                .expect("Unable to create Bad Request response for invalid query parameter someObject")),
                        }
                    },
                    None => None,
                };
                let param_some_list = query_params.iter().filter(|e| e.0 == "someList").map(|e| e.1.to_owned())
                    .nth(0);
                let param_some_list = match param_some_list {
                    Some(param_some_list) => {
                        let param_some_list =
                            <models::MyIdList as std::str::FromStr>::from_str
                                (&param_some_list);
                        match param_some_list {
                            Ok(param_some_list) => Some(param_some_list),
                            Err(e) => return Ok(Response::builder()
                                .status(StatusCode::BAD_REQUEST)
                                .body(Body::from(format!("Couldn't parse query parameter someList - doesn't match schema: {}", e)))
                                .expect("Unable to create Bad Request response for invalid query parameter someList")),
                        }
                    },
                    None => None,
                };

                                let result = api_impl.paramget_get(
                                            param_uuid,
                                            param_some_object,
                                            param_some_list,
                                        &context
                                    ).await;
                                let mut response = Response::new(Body::empty());
                                response.headers_mut().insert(
                                            HeaderName::from_static("x-span-id"),
                                            HeaderValue::from_str((&context as &dyn Has<XSpanIdString>).get().0.clone().to_string().as_str())
                                                .expect("Unable to create X-Span-ID header value"));

                                        match result {
                                            Ok(rsp) => match rsp {
                                                ParamgetGetResponse::JSONRsp
                                                    (body)
                                                => {
                                                    *response.status_mut() = StatusCode::from_u16(200).expect("Unable to turn 200 into a StatusCode");
                                                    response.headers_mut().insert(
                                                        CONTENT_TYPE,
                                                        HeaderValue::from_str("application/json")
                                                            .expect("Unable to create Content-Type header for PARAMGET_GET_JSON_RSP"));
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

                                        Ok(response)
            },

            // ReadonlyAuthSchemeGet - GET /readonly_auth_scheme
            &hyper::Method::GET if path.matched(paths::ID_READONLY_AUTH_SCHEME) => {
                {
                    let authorization = match (&context as &dyn Has<Option<Authorization>>).get() {
                        &Some(ref authorization) => authorization,
                        &None => return Ok(Response::builder()
                                                .status(StatusCode::FORBIDDEN)
                                                .body(Body::from("Unauthenticated"))
                                                .expect("Unable to create Authentication Forbidden response")),
                    };

                    // Authorization
                    if let Scopes::Some(ref scopes) = authorization.scopes {
                        let required_scopes: std::collections::BTreeSet<String> = vec![
                            "test.read".to_string(), // Allowed to read state.
                        ].into_iter().collect();

                        if !required_scopes.is_subset(scopes) {
                            let missing_scopes = required_scopes.difference(scopes);
                            return Ok(Response::builder()
                                .status(StatusCode::FORBIDDEN)
                                .body(Body::from(missing_scopes.fold(
                                    "Insufficient authorization, missing scopes".to_string(),
                                    |s, scope| format!("{} {}", s, scope))
                                ))
                                .expect("Unable to create Authentication Insufficient response")
                            );
                        }
                    }
                }

                                let result = api_impl.readonly_auth_scheme_get(
                                        &context
                                    ).await;
                                let mut response = Response::new(Body::empty());
                                response.headers_mut().insert(
                                            HeaderName::from_static("x-span-id"),
                                            HeaderValue::from_str((&context as &dyn Has<XSpanIdString>).get().0.clone().to_string().as_str())
                                                .expect("Unable to create X-Span-ID header value"));

                                        match result {
                                            Ok(rsp) => match rsp {
                                                ReadonlyAuthSchemeGetResponse::CheckThatLimitingToASingleRequiredAuthSchemeWorks
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

            // RegisterCallbackPost - POST /register-callback
            &hyper::Method::POST if path.matched(paths::ID_REGISTER_CALLBACK) => {
                // Query parameters (note that non-required or collection query parameters will ignore garbage values, rather than causing a 400 response)
                let query_params = form_urlencoded::parse(uri.query().unwrap_or_default().as_bytes()).collect::<Vec<_>>();
                let param_url = query_params.iter().filter(|e| e.0 == "url").map(|e| e.1.to_owned())
                    .nth(0);
                let param_url = match param_url {
                    Some(param_url) => {
                        let param_url =
                            <String as std::str::FromStr>::from_str
                                (&param_url);
                        match param_url {
                            Ok(param_url) => Some(param_url),
                            Err(e) => return Ok(Response::builder()
                                .status(StatusCode::BAD_REQUEST)
                                .body(Body::from(format!("Couldn't parse query parameter url - doesn't match schema: {}", e)))
                                .expect("Unable to create Bad Request response for invalid query parameter url")),
                        }
                    },
                    None => None,
                };
                let param_url = match param_url {
                    Some(param_url) => param_url,
                    None => return Ok(Response::builder()
                        .status(StatusCode::BAD_REQUEST)
                        .body(Body::from("Missing required query parameter url"))
                        .expect("Unable to create Bad Request response for missing query parameter url")),
                };

                                let result = api_impl.register_callback_post(
                                            param_url,
                                        &context
                                    ).await;
                                let mut response = Response::new(Body::empty());
                                response.headers_mut().insert(
                                            HeaderName::from_static("x-span-id"),
                                            HeaderValue::from_str((&context as &dyn Has<XSpanIdString>).get().0.clone().to_string().as_str())
                                                .expect("Unable to create X-Span-ID header value"));

                                        match result {
                                            Ok(rsp) => match rsp {
                                                RegisterCallbackPostResponse::OK
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

                                        Ok(response)
            },

            // RequiredOctetStreamPut - PUT /required_octet_stream
            &hyper::Method::PUT if path.matched(paths::ID_REQUIRED_OCTET_STREAM) => {
                // Body parameters (note that non-required body parameters will ignore garbage
                // values, rather than causing a 400 response). Produce warning header and logs for
                // any unused fields.
                let result = body.to_raw().await;
                match result {
                            Ok(body) => {
                                let param_body: Option<swagger::ByteArray> = if !body.is_empty() {
                                    Some(swagger::ByteArray(body.to_vec()))
                                } else {
                                    None
                                };
                                let param_body = match param_body {
                                    Some(param_body) => param_body,
                                    None => return Ok(Response::builder()
                                                        .status(StatusCode::BAD_REQUEST)
                                                        .body(Body::from("Missing required body parameter body"))
                                                        .expect("Unable to create Bad Request response for missing body parameter body")),
                                };

                                let result = api_impl.required_octet_stream_put(
                                            param_body,
                                        &context
                                    ).await;
                                let mut response = Response::new(Body::empty());
                                response.headers_mut().insert(
                                            HeaderName::from_static("x-span-id"),
                                            HeaderValue::from_str((&context as &dyn Has<XSpanIdString>).get().0.clone().to_string().as_str())
                                                .expect("Unable to create X-Span-ID header value"));

                                        match result {
                                            Ok(rsp) => match rsp {
                                                RequiredOctetStreamPutResponse::OK
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
                                                .body(Body::from(format!("Couldn't read body parameter body: {}", e)))
                                                .expect("Unable to create Bad Request response due to unable to read body parameter body")),
                        }
            },

            // ResponsesWithHeadersGet - GET /responses_with_headers
            &hyper::Method::GET if path.matched(paths::ID_RESPONSES_WITH_HEADERS) => {
                                let result = api_impl.responses_with_headers_get(
                                        &context
                                    ).await;
                                let mut response = Response::new(Body::empty());
                                response.headers_mut().insert(
                                            HeaderName::from_static("x-span-id"),
                                            HeaderValue::from_str((&context as &dyn Has<XSpanIdString>).get().0.clone().to_string().as_str())
                                                .expect("Unable to create X-Span-ID header value"));

                                        match result {
                                            Ok(rsp) => match rsp {
                                                ResponsesWithHeadersGetResponse::Success
                                                    {
                                                        body,
                                                        success_info,
                                                        bool_header,
                                                        object_header
                                                    }
                                                => {
                                                    let success_info = match header::IntoHeaderValue(success_info).try_into() {
                                                        Ok(val) => val,
                                                        Err(e) => {
                                                            return Ok(Response::builder()
                                                                    .status(StatusCode::INTERNAL_SERVER_ERROR)
                                                                    .body(Body::from(format!("An internal server error occurred handling success_info header - {}", e)))
                                                                    .expect("Unable to create Internal Server Error for invalid response header"))
                                                        }
                                                    };

                                                    response.headers_mut().insert(
                                                        HeaderName::from_static("success-info"),
                                                        success_info
                                                    );
                                                    if let Some(bool_header) = bool_header {
                                                    let bool_header = match header::IntoHeaderValue(bool_header).try_into() {
                                                        Ok(val) => val,
                                                        Err(e) => {
                                                            return Ok(Response::builder()
                                                                    .status(StatusCode::INTERNAL_SERVER_ERROR)
                                                                    .body(Body::from(format!("An internal server error occurred handling bool_header header - {}", e)))
                                                                    .expect("Unable to create Internal Server Error for invalid response header"))
                                                        }
                                                    };

                                                    response.headers_mut().insert(
                                                        HeaderName::from_static("bool-header"),
                                                        bool_header
                                                    );
                                                    }
                                                    if let Some(object_header) = object_header {
                                                    let object_header = match header::IntoHeaderValue(object_header).try_into() {
                                                        Ok(val) => val,
                                                        Err(e) => {
                                                            return Ok(Response::builder()
                                                                    .status(StatusCode::INTERNAL_SERVER_ERROR)
                                                                    .body(Body::from(format!("An internal server error occurred handling object_header header - {}", e)))
                                                                    .expect("Unable to create Internal Server Error for invalid response header"))
                                                        }
                                                    };

                                                    response.headers_mut().insert(
                                                        HeaderName::from_static("object-header"),
                                                        object_header
                                                    );
                                                    }
                                                    *response.status_mut() = StatusCode::from_u16(200).expect("Unable to turn 200 into a StatusCode");
                                                    response.headers_mut().insert(
                                                        CONTENT_TYPE,
                                                        HeaderValue::from_str("application/json")
                                                            .expect("Unable to create Content-Type header for RESPONSES_WITH_HEADERS_GET_SUCCESS"));
                                                    let body = serde_json::to_string(&body).expect("impossible to fail to serialize");
                                                    *response.body_mut() = Body::from(body);
                                                },
                                                ResponsesWithHeadersGetResponse::PreconditionFailed
                                                    {
                                                        further_info,
                                                        failure_info
                                                    }
                                                => {
                                                    if let Some(further_info) = further_info {
                                                    let further_info = match header::IntoHeaderValue(further_info).try_into() {
                                                        Ok(val) => val,
                                                        Err(e) => {
                                                            return Ok(Response::builder()
                                                                    .status(StatusCode::INTERNAL_SERVER_ERROR)
                                                                    .body(Body::from(format!("An internal server error occurred handling further_info header - {}", e)))
                                                                    .expect("Unable to create Internal Server Error for invalid response header"))
                                                        }
                                                    };

                                                    response.headers_mut().insert(
                                                        HeaderName::from_static("further-info"),
                                                        further_info
                                                    );
                                                    }
                                                    if let Some(failure_info) = failure_info {
                                                    let failure_info = match header::IntoHeaderValue(failure_info).try_into() {
                                                        Ok(val) => val,
                                                        Err(e) => {
                                                            return Ok(Response::builder()
                                                                    .status(StatusCode::INTERNAL_SERVER_ERROR)
                                                                    .body(Body::from(format!("An internal server error occurred handling failure_info header - {}", e)))
                                                                    .expect("Unable to create Internal Server Error for invalid response header"))
                                                        }
                                                    };

                                                    response.headers_mut().insert(
                                                        HeaderName::from_static("failure-info"),
                                                        failure_info
                                                    );
                                                    }
                                                    *response.status_mut() = StatusCode::from_u16(412).expect("Unable to turn 412 into a StatusCode");
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

            // Rfc7807Get - GET /rfc7807
            &hyper::Method::GET if path.matched(paths::ID_RFC7807) => {
                                let result = api_impl.rfc7807_get(
                                        &context
                                    ).await;
                                let mut response = Response::new(Body::empty());
                                response.headers_mut().insert(
                                            HeaderName::from_static("x-span-id"),
                                            HeaderValue::from_str((&context as &dyn Has<XSpanIdString>).get().0.clone().to_string().as_str())
                                                .expect("Unable to create X-Span-ID header value"));

                                        match result {
                                            Ok(rsp) => match rsp {
                                                Rfc7807GetResponse::OK
                                                    (body)
                                                => {
                                                    *response.status_mut() = StatusCode::from_u16(204).expect("Unable to turn 204 into a StatusCode");
                                                    response.headers_mut().insert(
                                                        CONTENT_TYPE,
                                                        HeaderValue::from_str("application/json")
                                                            .expect("Unable to create Content-Type header for RFC7807_GET_OK"));
                                                    let body = serde_json::to_string(&body).expect("impossible to fail to serialize");
                                                    *response.body_mut() = Body::from(body);
                                                },
                                                Rfc7807GetResponse::NotFound
                                                    (body)
                                                => {
                                                    *response.status_mut() = StatusCode::from_u16(404).expect("Unable to turn 404 into a StatusCode");
                                                    response.headers_mut().insert(
                                                        CONTENT_TYPE,
                                                        HeaderValue::from_str("application/problem+json")
                                                            .expect("Unable to create Content-Type header for RFC7807_GET_NOT_FOUND"));
                                                    let body = serde_json::to_string(&body).expect("impossible to fail to serialize");
                                                    *response.body_mut() = Body::from(body);
                                                },
                                                Rfc7807GetResponse::NotAcceptable
                                                    (body)
                                                => {
                                                    *response.status_mut() = StatusCode::from_u16(406).expect("Unable to turn 406 into a StatusCode");
                                                    response.headers_mut().insert(
                                                        CONTENT_TYPE,
                                                        HeaderValue::from_str("application/problem+xml")
                                                            .expect("Unable to create Content-Type header for RFC7807_GET_NOT_ACCEPTABLE"));
                                                    let body = serde_xml_rs::to_string(&body).expect("impossible to fail to serialize");
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

                                        Ok(response)
            },

            // UntypedPropertyGet - GET /untyped_property
            &hyper::Method::GET if path.matched(paths::ID_UNTYPED_PROPERTY) => {
                // Body parameters (note that non-required body parameters will ignore garbage
                // values, rather than causing a 400 response). Produce warning header and logs for
                // any unused fields.
                let result = body.to_raw().await;
                match result {
                            Ok(body) => {
                                let mut unused_elements = Vec::new();
                                let param_object_untyped_props: Option<models::ObjectUntypedProps> = if !body.is_empty() {
                                    let deserializer = &mut serde_json::Deserializer::from_slice(&*body);
                                    match serde_ignored::deserialize(deserializer, |path| {
                                            warn!("Ignoring unknown field in body: {}", path);
                                            unused_elements.push(path.to_string());
                                    }) {
                                        Ok(param_object_untyped_props) => param_object_untyped_props,
                                        Err(_) => None,
                                    }
                                } else {
                                    None
                                };

                                let result = api_impl.untyped_property_get(
                                            param_object_untyped_props,
                                        &context
                                    ).await;
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
                                                UntypedPropertyGetResponse::CheckThatUntypedPropertiesWorks
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
                                                .body(Body::from(format!("Couldn't read body parameter ObjectUntypedProps: {}", e)))
                                                .expect("Unable to create Bad Request response due to unable to read body parameter ObjectUntypedProps")),
                        }
            },

            // UuidGet - GET /uuid
            &hyper::Method::GET if path.matched(paths::ID_UUID) => {
                                let result = api_impl.uuid_get(
                                        &context
                                    ).await;
                                let mut response = Response::new(Body::empty());
                                response.headers_mut().insert(
                                            HeaderName::from_static("x-span-id"),
                                            HeaderValue::from_str((&context as &dyn Has<XSpanIdString>).get().0.clone().to_string().as_str())
                                                .expect("Unable to create X-Span-ID header value"));

                                        match result {
                                            Ok(rsp) => match rsp {
                                                UuidGetResponse::DuplicateResponseLongText
                                                    (body)
                                                => {
                                                    *response.status_mut() = StatusCode::from_u16(200).expect("Unable to turn 200 into a StatusCode");
                                                    response.headers_mut().insert(
                                                        CONTENT_TYPE,
                                                        HeaderValue::from_str("application/json")
                                                            .expect("Unable to create Content-Type header for UUID_GET_DUPLICATE_RESPONSE_LONG_TEXT"));
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

                                        Ok(response)
            },

            // XmlExtraPost - POST /xml_extra
            &hyper::Method::POST if path.matched(paths::ID_XML_EXTRA) => {
                // Body parameters (note that non-required body parameters will ignore garbage
                // values, rather than causing a 400 response). Produce warning header and logs for
                // any unused fields.
                let result = body.to_raw().await;
                match result {
                            Ok(body) => {
                                let mut unused_elements = Vec::new();
                                let param_duplicate_xml_object: Option<models::DuplicateXmlObject> = if !body.is_empty() {
                                    let deserializer = &mut serde_xml_rs::de::Deserializer::new_from_reader(&*body);
                                    match serde_ignored::deserialize(deserializer, |path| {
                                            warn!("Ignoring unknown field in body: {}", path);
                                            unused_elements.push(path.to_string());
                                    }) {
                                        Ok(param_duplicate_xml_object) => param_duplicate_xml_object,
                                        Err(_) => None,
                                    }
                                } else {
                                    None
                                };

                                let result = api_impl.xml_extra_post(
                                            param_duplicate_xml_object,
                                        &context
                                    ).await;
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
                                                XmlExtraPostResponse::OK
                                                => {
                                                    *response.status_mut() = StatusCode::from_u16(201).expect("Unable to turn 201 into a StatusCode");
                                                },
                                                XmlExtraPostResponse::BadRequest
                                                => {
                                                    *response.status_mut() = StatusCode::from_u16(400).expect("Unable to turn 400 into a StatusCode");
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
                                                .body(Body::from(format!("Couldn't read body parameter DuplicateXmlObject: {}", e)))
                                                .expect("Unable to create Bad Request response due to unable to read body parameter DuplicateXmlObject")),
                        }
            },

            // XmlOtherPost - POST /xml_other
            &hyper::Method::POST if path.matched(paths::ID_XML_OTHER) => {
                // Body parameters (note that non-required body parameters will ignore garbage
                // values, rather than causing a 400 response). Produce warning header and logs for
                // any unused fields.
                let result = body.to_raw().await;
                match result {
                            Ok(body) => {
                                let mut unused_elements = Vec::new();
                                let param_another_xml_object: Option<models::AnotherXmlObject> = if !body.is_empty() {
                                    let deserializer = &mut serde_xml_rs::de::Deserializer::new_from_reader(&*body);
                                    match serde_ignored::deserialize(deserializer, |path| {
                                            warn!("Ignoring unknown field in body: {}", path);
                                            unused_elements.push(path.to_string());
                                    }) {
                                        Ok(param_another_xml_object) => param_another_xml_object,
                                        Err(_) => None,
                                    }
                                } else {
                                    None
                                };

                                let result = api_impl.xml_other_post(
                                            param_another_xml_object,
                                        &context
                                    ).await;
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
                                                XmlOtherPostResponse::OK
                                                    (body)
                                                => {
                                                    *response.status_mut() = StatusCode::from_u16(201).expect("Unable to turn 201 into a StatusCode");
                                                    response.headers_mut().insert(
                                                        CONTENT_TYPE,
                                                        HeaderValue::from_str("text/xml")
                                                            .expect("Unable to create Content-Type header for XML_OTHER_POST_OK"));
                                                    let mut namespaces = std::collections::BTreeMap::new();

                                                    // An empty string is used to indicate a global namespace in xmltree.
                                                    namespaces.insert("".to_string(), models::AnotherXmlObject::NAMESPACE.to_string());
                                                    let body = serde_xml_rs::to_string_with_namespaces(&body, namespaces).expect("impossible to fail to serialize");
                                                    *response.body_mut() = Body::from(body);
                                                },
                                                XmlOtherPostResponse::BadRequest
                                                => {
                                                    *response.status_mut() = StatusCode::from_u16(400).expect("Unable to turn 400 into a StatusCode");
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
                                                .body(Body::from(format!("Couldn't read body parameter AnotherXmlObject: {}", e)))
                                                .expect("Unable to create Bad Request response due to unable to read body parameter AnotherXmlObject")),
                        }
            },

            // XmlOtherPut - PUT /xml_other
            &hyper::Method::PUT if path.matched(paths::ID_XML_OTHER) => {
                // Body parameters (note that non-required body parameters will ignore garbage
                // values, rather than causing a 400 response). Produce warning header and logs for
                // any unused fields.
                let result = body.to_raw().await;
                match result {
                            Ok(body) => {
                                let mut unused_elements = Vec::new();
                                let param_another_xml_array: Option<models::AnotherXmlArray> = if !body.is_empty() {
                                    let deserializer = &mut serde_xml_rs::de::Deserializer::new_from_reader(&*body);
                                    match serde_ignored::deserialize(deserializer, |path| {
                                            warn!("Ignoring unknown field in body: {}", path);
                                            unused_elements.push(path.to_string());
                                    }) {
                                        Ok(param_another_xml_array) => param_another_xml_array,
                                        Err(_) => None,
                                    }
                                } else {
                                    None
                                };

                                let result = api_impl.xml_other_put(
                                            param_another_xml_array,
                                        &context
                                    ).await;
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
                                                XmlOtherPutResponse::OK
                                                => {
                                                    *response.status_mut() = StatusCode::from_u16(201).expect("Unable to turn 201 into a StatusCode");
                                                },
                                                XmlOtherPutResponse::BadRequest
                                                => {
                                                    *response.status_mut() = StatusCode::from_u16(400).expect("Unable to turn 400 into a StatusCode");
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
                                                .body(Body::from(format!("Couldn't read body parameter AnotherXmlArray: {}", e)))
                                                .expect("Unable to create Bad Request response due to unable to read body parameter AnotherXmlArray")),
                        }
            },

            // XmlPost - POST /xml
            &hyper::Method::POST if path.matched(paths::ID_XML) => {
                // Body parameters (note that non-required body parameters will ignore garbage
                // values, rather than causing a 400 response). Produce warning header and logs for
                // any unused fields.
                let result = body.to_raw().await;
                match result {
                            Ok(body) => {
                                let mut unused_elements = Vec::new();
                                let param_xml_array: Option<models::XmlArray> = if !body.is_empty() {
                                    let deserializer = &mut serde_xml_rs::de::Deserializer::new_from_reader(&*body);
                                    match serde_ignored::deserialize(deserializer, |path| {
                                            warn!("Ignoring unknown field in body: {}", path);
                                            unused_elements.push(path.to_string());
                                    }) {
                                        Ok(param_xml_array) => param_xml_array,
                                        Err(_) => None,
                                    }
                                } else {
                                    None
                                };

                                let result = api_impl.xml_post(
                                            param_xml_array,
                                        &context
                                    ).await;
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
                                                XmlPostResponse::OK
                                                => {
                                                    *response.status_mut() = StatusCode::from_u16(201).expect("Unable to turn 201 into a StatusCode");
                                                },
                                                XmlPostResponse::BadRequest
                                                => {
                                                    *response.status_mut() = StatusCode::from_u16(400).expect("Unable to turn 400 into a StatusCode");
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
                                                .body(Body::from(format!("Couldn't read body parameter XmlArray: {}", e)))
                                                .expect("Unable to create Bad Request response due to unable to read body parameter XmlArray")),
                        }
            },

            // XmlPut - PUT /xml
            &hyper::Method::PUT if path.matched(paths::ID_XML) => {
                // Body parameters (note that non-required body parameters will ignore garbage
                // values, rather than causing a 400 response). Produce warning header and logs for
                // any unused fields.
                let result = body.to_raw().await;
                match result {
                            Ok(body) => {
                                let mut unused_elements = Vec::new();
                                let param_xml_object: Option<models::XmlObject> = if !body.is_empty() {
                                    let deserializer = &mut serde_xml_rs::de::Deserializer::new_from_reader(&*body);
                                    match serde_ignored::deserialize(deserializer, |path| {
                                            warn!("Ignoring unknown field in body: {}", path);
                                            unused_elements.push(path.to_string());
                                    }) {
                                        Ok(param_xml_object) => param_xml_object,
                                        Err(_) => None,
                                    }
                                } else {
                                    None
                                };

                                let result = api_impl.xml_put(
                                            param_xml_object,
                                        &context
                                    ).await;
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
                                                XmlPutResponse::OK
                                                => {
                                                    *response.status_mut() = StatusCode::from_u16(201).expect("Unable to turn 201 into a StatusCode");
                                                },
                                                XmlPutResponse::BadRequest
                                                => {
                                                    *response.status_mut() = StatusCode::from_u16(400).expect("Unable to turn 400 into a StatusCode");
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
                                                .body(Body::from(format!("Couldn't read body parameter XmlObject: {}", e)))
                                                .expect("Unable to create Bad Request response due to unable to read body parameter XmlObject")),
                        }
            },

            // CreateRepo - POST /repos
            &hyper::Method::POST if path.matched(paths::ID_REPOS) => {
                // Body parameters (note that non-required body parameters will ignore garbage
                // values, rather than causing a 400 response). Produce warning header and logs for
                // any unused fields.
                let result = body.to_raw().await;
                match result {
                            Ok(body) => {
                                let mut unused_elements = Vec::new();
                                let param_object_param: Option<models::ObjectParam> = if !body.is_empty() {
                                    let deserializer = &mut serde_json::Deserializer::from_slice(&*body);
                                    match serde_ignored::deserialize(deserializer, |path| {
                                            warn!("Ignoring unknown field in body: {}", path);
                                            unused_elements.push(path.to_string());
                                    }) {
                                        Ok(param_object_param) => param_object_param,
                                        Err(e) => return Ok(Response::builder()
                                                        .status(StatusCode::BAD_REQUEST)
                                                        .body(Body::from(format!("Couldn't parse body parameter ObjectParam - doesn't match schema: {}", e)))
                                                        .expect("Unable to create Bad Request response for invalid body parameter ObjectParam due to schema")),
                                    }
                                } else {
                                    None
                                };
                                let param_object_param = match param_object_param {
                                    Some(param_object_param) => param_object_param,
                                    None => return Ok(Response::builder()
                                                        .status(StatusCode::BAD_REQUEST)
                                                        .body(Body::from("Missing required body parameter ObjectParam"))
                                                        .expect("Unable to create Bad Request response for missing body parameter ObjectParam")),
                                };

                                let result = api_impl.create_repo(
                                            param_object_param,
                                        &context
                                    ).await;
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
                                                CreateRepoResponse::Success
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
                                                .body(Body::from(format!("Couldn't read body parameter ObjectParam: {}", e)))
                                                .expect("Unable to create Bad Request response due to unable to read body parameter ObjectParam")),
                        }
            },

            // GetRepoInfo - GET /repos/{repoId}
            &hyper::Method::GET if path.matched(paths::ID_REPOS_REPOID) => {
                // Path parameters
                let path: &str = &uri.path().to_string();
                let path_params =
                    paths::REGEX_REPOS_REPOID
                    .captures(&path)
                    .unwrap_or_else(||
                        panic!("Path {} matched RE REPOS_REPOID in set but failed match against \"{}\"", path, paths::REGEX_REPOS_REPOID.as_str())
                    );

                let param_repo_id = match percent_encoding::percent_decode(path_params["repoId"].as_bytes()).decode_utf8() {
                    Ok(param_repo_id) => match param_repo_id.parse::<String>() {
                        Ok(param_repo_id) => param_repo_id,
                        Err(e) => return Ok(Response::builder()
                                        .status(StatusCode::BAD_REQUEST)
                                        .body(Body::from(format!("Couldn't parse path parameter repoId: {}", e)))
                                        .expect("Unable to create Bad Request response for invalid path parameter")),
                    },
                    Err(_) => return Ok(Response::builder()
                                        .status(StatusCode::BAD_REQUEST)
                                        .body(Body::from(format!("Couldn't percent-decode path parameter as UTF-8: {}", &path_params["repoId"])))
                                        .expect("Unable to create Bad Request response for invalid percent decode"))
                };

                                let result = api_impl.get_repo_info(
                                            param_repo_id,
                                        &context
                                    ).await;
                                let mut response = Response::new(Body::empty());
                                response.headers_mut().insert(
                                            HeaderName::from_static("x-span-id"),
                                            HeaderValue::from_str((&context as &dyn Has<XSpanIdString>).get().0.clone().to_string().as_str())
                                                .expect("Unable to create X-Span-ID header value"));

                                        match result {
                                            Ok(rsp) => match rsp {
                                                GetRepoInfoResponse::OK
                                                    (body)
                                                => {
                                                    *response.status_mut() = StatusCode::from_u16(200).expect("Unable to turn 200 into a StatusCode");
                                                    response.headers_mut().insert(
                                                        CONTENT_TYPE,
                                                        HeaderValue::from_str("application/json")
                                                            .expect("Unable to create Content-Type header for GET_REPO_INFO_OK"));
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

                                        Ok(response)
            },

            _ if path.matched(paths::ID_ANY_OF) => method_not_allowed(),
            _ if path.matched(paths::ID_CALLBACK_WITH_HEADER) => method_not_allowed(),
            _ if path.matched(paths::ID_COMPLEX_QUERY_PARAM) => method_not_allowed(),
            _ if path.matched(paths::ID_ENUM_IN_PATH_PATH_PARAM) => method_not_allowed(),
            _ if path.matched(paths::ID_JSON_COMPLEX_QUERY_PARAM) => method_not_allowed(),
            _ if path.matched(paths::ID_MANDATORY_REQUEST_HEADER) => method_not_allowed(),
            _ if path.matched(paths::ID_MERGE_PATCH_JSON) => method_not_allowed(),
            _ if path.matched(paths::ID_MULTIGET) => method_not_allowed(),
            _ if path.matched(paths::ID_MULTIPLE_AUTH_SCHEME) => method_not_allowed(),
            _ if path.matched(paths::ID_ONE_OF) => method_not_allowed(),
            _ if path.matched(paths::ID_OVERRIDE_SERVER) => method_not_allowed(),
            _ if path.matched(paths::ID_PARAMGET) => method_not_allowed(),
            _ if path.matched(paths::ID_READONLY_AUTH_SCHEME) => method_not_allowed(),
            _ if path.matched(paths::ID_REGISTER_CALLBACK) => method_not_allowed(),
            _ if path.matched(paths::ID_REPOS) => method_not_allowed(),
            _ if path.matched(paths::ID_REPOS_REPOID) => method_not_allowed(),
            _ if path.matched(paths::ID_REQUIRED_OCTET_STREAM) => method_not_allowed(),
            _ if path.matched(paths::ID_RESPONSES_WITH_HEADERS) => method_not_allowed(),
            _ if path.matched(paths::ID_RFC7807) => method_not_allowed(),
            _ if path.matched(paths::ID_UNTYPED_PROPERTY) => method_not_allowed(),
            _ if path.matched(paths::ID_UUID) => method_not_allowed(),
            _ if path.matched(paths::ID_XML) => method_not_allowed(),
            _ if path.matched(paths::ID_XML_EXTRA) => method_not_allowed(),
            _ if path.matched(paths::ID_XML_OTHER) => method_not_allowed(),
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
            // AnyOfGet - GET /any-of
            &hyper::Method::GET if path.matched(paths::ID_ANY_OF) => Ok("AnyOfGet"),
            // CallbackWithHeaderPost - POST /callback-with-header
            &hyper::Method::POST if path.matched(paths::ID_CALLBACK_WITH_HEADER) => Ok("CallbackWithHeaderPost"),
            // ComplexQueryParamGet - GET /complex-query-param
            &hyper::Method::GET if path.matched(paths::ID_COMPLEX_QUERY_PARAM) => Ok("ComplexQueryParamGet"),
            // EnumInPathPathParamGet - GET /enum_in_path/{path_param}
            &hyper::Method::GET if path.matched(paths::ID_ENUM_IN_PATH_PATH_PARAM) => Ok("EnumInPathPathParamGet"),
            // JsonComplexQueryParamGet - GET /json-complex-query-param
            &hyper::Method::GET if path.matched(paths::ID_JSON_COMPLEX_QUERY_PARAM) => Ok("JsonComplexQueryParamGet"),
            // MandatoryRequestHeaderGet - GET /mandatory-request-header
            &hyper::Method::GET if path.matched(paths::ID_MANDATORY_REQUEST_HEADER) => Ok("MandatoryRequestHeaderGet"),
            // MergePatchJsonGet - GET /merge-patch-json
            &hyper::Method::GET if path.matched(paths::ID_MERGE_PATCH_JSON) => Ok("MergePatchJsonGet"),
            // MultigetGet - GET /multiget
            &hyper::Method::GET if path.matched(paths::ID_MULTIGET) => Ok("MultigetGet"),
            // MultipleAuthSchemeGet - GET /multiple_auth_scheme
            &hyper::Method::GET if path.matched(paths::ID_MULTIPLE_AUTH_SCHEME) => Ok("MultipleAuthSchemeGet"),
            // OneOfGet - GET /one-of
            &hyper::Method::GET if path.matched(paths::ID_ONE_OF) => Ok("OneOfGet"),
            // OverrideServerGet - GET /override-server
            &hyper::Method::GET if path.matched(paths::ID_OVERRIDE_SERVER) => Ok("OverrideServerGet"),
            // ParamgetGet - GET /paramget
            &hyper::Method::GET if path.matched(paths::ID_PARAMGET) => Ok("ParamgetGet"),
            // ReadonlyAuthSchemeGet - GET /readonly_auth_scheme
            &hyper::Method::GET if path.matched(paths::ID_READONLY_AUTH_SCHEME) => Ok("ReadonlyAuthSchemeGet"),
            // RegisterCallbackPost - POST /register-callback
            &hyper::Method::POST if path.matched(paths::ID_REGISTER_CALLBACK) => Ok("RegisterCallbackPost"),
            // RequiredOctetStreamPut - PUT /required_octet_stream
            &hyper::Method::PUT if path.matched(paths::ID_REQUIRED_OCTET_STREAM) => Ok("RequiredOctetStreamPut"),
            // ResponsesWithHeadersGet - GET /responses_with_headers
            &hyper::Method::GET if path.matched(paths::ID_RESPONSES_WITH_HEADERS) => Ok("ResponsesWithHeadersGet"),
            // Rfc7807Get - GET /rfc7807
            &hyper::Method::GET if path.matched(paths::ID_RFC7807) => Ok("Rfc7807Get"),
            // UntypedPropertyGet - GET /untyped_property
            &hyper::Method::GET if path.matched(paths::ID_UNTYPED_PROPERTY) => Ok("UntypedPropertyGet"),
            // UuidGet - GET /uuid
            &hyper::Method::GET if path.matched(paths::ID_UUID) => Ok("UuidGet"),
            // XmlExtraPost - POST /xml_extra
            &hyper::Method::POST if path.matched(paths::ID_XML_EXTRA) => Ok("XmlExtraPost"),
            // XmlOtherPost - POST /xml_other
            &hyper::Method::POST if path.matched(paths::ID_XML_OTHER) => Ok("XmlOtherPost"),
            // XmlOtherPut - PUT /xml_other
            &hyper::Method::PUT if path.matched(paths::ID_XML_OTHER) => Ok("XmlOtherPut"),
            // XmlPost - POST /xml
            &hyper::Method::POST if path.matched(paths::ID_XML) => Ok("XmlPost"),
            // XmlPut - PUT /xml
            &hyper::Method::PUT if path.matched(paths::ID_XML) => Ok("XmlPut"),
            // CreateRepo - POST /repos
            &hyper::Method::POST if path.matched(paths::ID_REPOS) => Ok("CreateRepo"),
            // GetRepoInfo - GET /repos/{repoId}
            &hyper::Method::GET if path.matched(paths::ID_REPOS_REPOID) => Ok("GetRepoInfo"),
            _ => Err(()),
        }
    }
}
