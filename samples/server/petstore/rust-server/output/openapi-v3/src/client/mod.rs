use hyper;
use hyper::client::HttpConnector;
use hyper::header::{HeaderName, HeaderValue, CONTENT_TYPE};
use hyper::{Body, Uri, Response};
use hyper_tls::HttpsConnector;

use url::form_urlencoded;
use url::percent_encoding::{utf8_percent_encode, PATH_SEGMENT_ENCODE_SET, QUERY_ENCODE_SET};
use futures;
use futures::{Future, Stream};
use futures::{future, stream};
use serde_json;
use std::borrow::Cow;
#[allow(unused_imports)]
use std::collections::{HashMap, BTreeMap};
use std::io::{Read, Error, ErrorKind};
use std::error;
use std::fmt;
use std::path::Path;
use std::sync::Arc;
use std::str;
use std::str::FromStr;
use std::string::ToString;
use swagger;
use swagger::{ApiError, XSpanIdString, Has, AuthData};
use swagger::client::Service;

use uuid;
use serde_xml_rs;

use {Api,
     MultigetGetResponse,
     MultipleAuthSchemeGetResponse,
     ReadonlyAuthSchemeGetResponse,
     RequiredOctetStreamPutResponse,
     ResponsesWithHeadersGetResponse,
     UuidGetResponse,
     XmlExtraPostResponse,
     XmlOtherPostResponse,
     XmlOtherPutResponse,
     XmlPostResponse,
     XmlPutResponse
     };

use mimetypes;
use models;

define_encode_set! {
    /// This encode set is used for object IDs
    ///
    /// Aside from the special characters defined in the `PATH_SEGMENT_ENCODE_SET`,
    /// the vertical bar (|) is encoded.
    pub ID_ENCODE_SET = [PATH_SEGMENT_ENCODE_SET] | {'|'}
}

/// Convert input into a base path, e.g. "http://example:123". Also checks the scheme as it goes.
fn into_base_path(input: &str, correct_scheme: Option<&'static str>) -> Result<String, ClientInitError> {
    // First convert to Uri, since a base path is a subset of Uri.
    let uri = Uri::from_str(input)?;

    let scheme = uri.scheme_part().ok_or(ClientInitError::InvalidScheme)?;

    // Check the scheme if necessary
    if let Some(correct_scheme) = correct_scheme {
        if scheme != correct_scheme {
            return Err(ClientInitError::InvalidScheme);
        }
    }

    let host = uri.host().ok_or_else(|| ClientInitError::MissingHost)?;
    let port = uri.port_part().map(|x| format!(":{}", x)).unwrap_or_default();
    Ok(format!("{}://{}{}", scheme, host, port))
}

/// A client that implements the API by making HTTP calls out to a server.
pub struct Client<F>
{
    client_service: Arc<Box<dyn Service<ReqBody=Body, Future=F> + Send + Sync>>,
    base_path: String,
}

impl<F> fmt::Debug for Client<F>
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Client {{ base_path: {} }}", self.base_path)
    }
}

impl<F> Clone for Client<F>
{
    fn clone(&self) -> Self {
        Client {
            client_service: self.client_service.clone(),
            base_path: self.base_path.clone(),
        }
    }
}

impl Client<hyper::client::ResponseFuture>
{
    /// Create a client with a custom implementation of hyper::client::Connect.
    ///
    /// Intended for use with custom implementations of connect for e.g. protocol logging
    /// or similar functionality which requires wrapping the transport layer. When wrapping a TCP connection,
    /// this function should be used in conjunction with
    /// `swagger::{http_connector, https_connector, https_mutual_connector}`.
    ///
    /// For ordinary tcp connections, prefer the use of `try_new_http`, `try_new_https`
    /// and `try_new_https_mutual`, to avoid introducing a dependency on the underlying transport layer.
    ///
    /// # Arguments
    ///
    /// * `base_path` - base path of the client API, i.e. "www.my-api-implementation.com"
    /// * `protocol` - Which protocol to use when constructing the request url, e.g. `Some("http")`
    /// * `connector_fn` - Function which returns an implementation of `hyper::client::Connect`
    pub fn try_new_with_connector<C>(
        base_path: &str,
        protocol: Option<&'static str>,
        connector_fn: Box<dyn Fn() -> C + Send + Sync>,
    ) -> Result<Self, ClientInitError> where
      C: hyper::client::connect::Connect + 'static,
      C::Transport: 'static,
      C::Future: 'static,
    {
        let connector = connector_fn();

        let client_service = Box::new(hyper::client::Client::builder().build(connector));

        Ok(Client {
            client_service: Arc::new(client_service),
            base_path: into_base_path(base_path, protocol)?,
        })
    }

    /// Create an HTTP client.
    ///
    /// # Arguments
    /// * `base_path` - base path of the client API, i.e. "www.my-api-implementation.com"
    pub fn try_new_http(
        base_path: &str,
    ) -> Result<Self, ClientInitError> {
        let http_connector = swagger::http_connector();

        Self::try_new_with_connector(base_path, Some("http"), http_connector)
    }

    /// Create a client with a TLS connection to the server.
    ///
    /// # Arguments
    /// * `base_path` - base path of the client API, i.e. "www.my-api-implementation.com"
    /// * `ca_certificate` - Path to CA certificate used to authenticate the server
    pub fn try_new_https<CA>(
        base_path: &str,
        ca_certificate: CA,
    ) -> Result<Self, ClientInitError>
    where
        CA: AsRef<Path>,
    {
        let https_connector = swagger::https_connector(ca_certificate);
        Self::try_new_with_connector(base_path, Some("https"), https_connector)
    }

    /// Create a client with a mutually authenticated TLS connection to the server.
    ///
    /// # Arguments
    /// * `base_path` - base path of the client API, i.e. "www.my-api-implementation.com"
    /// * `ca_certificate` - Path to CA certificate used to authenticate the server
    /// * `client_key` - Path to the client private key
    /// * `client_certificate` - Path to the client's public certificate associated with the private key
    pub fn try_new_https_mutual<CA, K, D>(
        base_path: &str,
        ca_certificate: CA,
        client_key: K,
        client_certificate: D,
    ) -> Result<Self, ClientInitError>
    where
        CA: AsRef<Path>,
        K: AsRef<Path>,
        D: AsRef<Path>,
    {
        let https_connector =
            swagger::https_mutual_connector(ca_certificate, client_key, client_certificate);
        Self::try_new_with_connector(base_path, Some("https"), https_connector)
    }
}

impl<F> Client<F>
{
    /// Constructor for creating a `Client` by passing in a pre-made `swagger::client::Service`
    ///
    /// This allows adding custom wrappers around the underlying transport, for example for logging.
    pub fn try_new_with_client_service(
        client_service: Arc<Box<dyn Service<ReqBody=Body, Future=F> + Send + Sync>>,
        base_path: &str,
    ) -> Result<Self, ClientInitError> {
        Ok(Client {
            client_service: client_service,
            base_path: into_base_path(base_path, None)?,
        })
    }
}

impl<C, F> Api<C> for Client<F> where
    C: Has<XSpanIdString> + Has<Option<AuthData>>,
    F: Future<Item=Response<Body>, Error=hyper::Error> + Send + 'static
{

    fn multiget_get(&self, context: &C) -> Box<dyn Future<Item=MultigetGetResponse, Error=ApiError> + Send> {
        let mut uri = format!(
            "{}/multiget",
            self.base_path
        );

        // Query parameters
        let mut query_string = url::form_urlencoded::Serializer::new("".to_owned());
        let query_string_str = query_string.finish();
        if !query_string_str.is_empty() {
            uri += "?";
            uri += &query_string_str;
        }

        let uri = match Uri::from_str(&uri) {
            Ok(uri) => uri,
            Err(err) => return Box::new(future::err(ApiError(format!("Unable to build URI: {}", err)))),
        };

        let mut request = match hyper::Request::builder()
            .method("GET")
            .uri(uri)
            .body(Body::empty()) {
                Ok(req) => req,
                Err(e) => return Box::new(future::err(ApiError(format!("Unable to create request: {}", e))))
        };


        let header = HeaderValue::from_str((context as &dyn Has<XSpanIdString>).get().0.clone().to_string().as_str());
        request.headers_mut().insert(HeaderName::from_static("x-span-id"), match header {
            Ok(h) => h,
            Err(e) => return Box::new(future::err(ApiError(format!("Unable to create X-Span ID header value: {}", e))))
        });


        Box::new(self.client_service.request(request)
                             .map_err(|e| ApiError(format!("No response received: {}", e)))
                             .and_then(|mut response| {
            match response.status().as_u16() {
                200 => {
                    let body = response.into_body();
                    Box::new(
                        body
                        .concat2()
                        .map_err(|e| ApiError(format!("Failed to read response: {}", e)))
                        .and_then(|body|
                            str::from_utf8(&body)
                            .map_err(|e| ApiError(format!("Response was not valid UTF8: {}", e)))
                            .and_then(|body|
                                serde_json::from_str::<models::AnotherXmlObject>(body)
                                .map_err(|e| e.into())
                            )
                        )
                        .map(move |body| {
                            MultigetGetResponse::JSONRsp
                            (body)
                        })
                    ) as Box<dyn Future<Item=_, Error=_> + Send>
                },
                201 => {
                    let body = response.into_body();
                    Box::new(
                        body
                        .concat2()
                        .map_err(|e| ApiError(format!("Failed to read response: {}", e)))
                        .and_then(|body|
                            str::from_utf8(&body)
                            .map_err(|e| ApiError(format!("Response was not valid UTF8: {}", e)))
                            .and_then(|body|
                                // ToDo: this will move to swagger-rs and become a standard From conversion trait
                                // once https://github.com/RReverser/serde-xml-rs/pull/45 is accepted upstream
                                serde_xml_rs::from_str::<models::InlineResponse201>(body)
                                .map_err(|e| ApiError(format!("Response body did not match the schema: {}", e)))
                            )
                        )
                        .map(move |body| {
                            MultigetGetResponse::XMLRsp
                            (body)
                        })
                    ) as Box<dyn Future<Item=_, Error=_> + Send>
                },
                202 => {
                    let body = response.into_body();
                    Box::new(
                        body
                        .concat2()
                        .map_err(|e| ApiError(format!("Failed to read response: {}", e)))
                        .and_then(|body|
                            Ok(swagger::ByteArray(body.to_vec()))
                        )
                        .map(move |body| {
                            MultigetGetResponse::OctetRsp
                            (body)
                        })
                    ) as Box<dyn Future<Item=_, Error=_> + Send>
                },
                203 => {
                    let body = response.into_body();
                    Box::new(
                        body
                        .concat2()
                        .map_err(|e| ApiError(format!("Failed to read response: {}", e)))
                        .and_then(|body|
                            str::from_utf8(&body)
                            .map_err(|e| ApiError(format!("Response was not valid UTF8: {}", e)))
                            .and_then(|body|
                                Ok(body.to_string())
                            )
                        )
                        .map(move |body| {
                            MultigetGetResponse::StringRsp
                            (body)
                        })
                    ) as Box<dyn Future<Item=_, Error=_> + Send>
                },
                204 => {
                    let body = response.into_body();
                    Box::new(
                        body
                        .concat2()
                        .map_err(|e| ApiError(format!("Failed to read response: {}", e)))
                        .and_then(|body|
                            str::from_utf8(&body)
                            .map_err(|e| ApiError(format!("Response was not valid UTF8: {}", e)))
                            .and_then(|body|
                                serde_json::from_str::<models::AnotherXmlObject>(body)
                                .map_err(|e| e.into())
                            )
                        )
                        .map(move |body| {
                            MultigetGetResponse::DuplicateResponseLongText
                            (body)
                        })
                    ) as Box<dyn Future<Item=_, Error=_> + Send>
                },
                205 => {
                    let body = response.into_body();
                    Box::new(
                        body
                        .concat2()
                        .map_err(|e| ApiError(format!("Failed to read response: {}", e)))
                        .and_then(|body|
                            str::from_utf8(&body)
                            .map_err(|e| ApiError(format!("Response was not valid UTF8: {}", e)))
                            .and_then(|body|
                                serde_json::from_str::<models::AnotherXmlObject>(body)
                                .map_err(|e| e.into())
                            )
                        )
                        .map(move |body| {
                            MultigetGetResponse::DuplicateResponseLongText_2
                            (body)
                        })
                    ) as Box<dyn Future<Item=_, Error=_> + Send>
                },
                206 => {
                    let body = response.into_body();
                    Box::new(
                        body
                        .concat2()
                        .map_err(|e| ApiError(format!("Failed to read response: {}", e)))
                        .and_then(|body|
                            str::from_utf8(&body)
                            .map_err(|e| ApiError(format!("Response was not valid UTF8: {}", e)))
                            .and_then(|body|
                                serde_json::from_str::<models::AnotherXmlObject>(body)
                                .map_err(|e| e.into())
                            )
                        )
                        .map(move |body| {
                            MultigetGetResponse::DuplicateResponseLongText_3
                            (body)
                        })
                    ) as Box<dyn Future<Item=_, Error=_> + Send>
                },
                code => {
                    let headers = response.headers().clone();
                    Box::new(response.into_body()
                            .take(100)
                            .concat2()
                            .then(move |body|
                                future::err(ApiError(format!("Unexpected response code {}:\n{:?}\n\n{}",
                                    code,
                                    headers,
                                    match body {
                                        Ok(ref body) => match str::from_utf8(body) {
                                            Ok(body) => Cow::from(body),
                                            Err(e) => Cow::from(format!("<Body was not UTF8: {:?}>", e)),
                                        },
                                        Err(e) => Cow::from(format!("<Failed to read body: {}>", e)),
                                    })))
                            )
                    ) as Box<dyn Future<Item=_, Error=_> + Send>
                }
            }
        }))

    }

    fn multiple_auth_scheme_get(&self, context: &C) -> Box<dyn Future<Item=MultipleAuthSchemeGetResponse, Error=ApiError> + Send> {
        let mut uri = format!(
            "{}/multiple_auth_scheme",
            self.base_path
        );

        // Query parameters
        let mut query_string = url::form_urlencoded::Serializer::new("".to_owned());
        let query_string_str = query_string.finish();
        if !query_string_str.is_empty() {
            uri += "?";
            uri += &query_string_str;
        }

        let uri = match Uri::from_str(&uri) {
            Ok(uri) => uri,
            Err(err) => return Box::new(future::err(ApiError(format!("Unable to build URI: {}", err)))),
        };

        let mut request = match hyper::Request::builder()
            .method("GET")
            .uri(uri)
            .body(Body::empty()) {
                Ok(req) => req,
                Err(e) => return Box::new(future::err(ApiError(format!("Unable to create request: {}", e))))
        };


        let header = HeaderValue::from_str((context as &dyn Has<XSpanIdString>).get().0.clone().to_string().as_str());
        request.headers_mut().insert(HeaderName::from_static("x-span-id"), match header {
            Ok(h) => h,
            Err(e) => return Box::new(future::err(ApiError(format!("Unable to create X-Span ID header value: {}", e))))
        });

        if let Some(auth_data) = (context as &dyn Has<Option<AuthData>>).get().as_ref() {
            // Currently only authentication with Basic, API Key, and Bearer are supported
            match auth_data {
                &AuthData::Bearer(ref bearer_header) => {
                    let auth = swagger::auth::Header(bearer_header.clone());
                    let header = match HeaderValue::from_str(&format!("{}", auth)) {
                        Ok(h) => h,
                        Err(e) => return Box::new(future::err(ApiError(format!("Unable to create Authorization header: {}", e))))
                    };
                    request.headers_mut().insert(
                        hyper::header::AUTHORIZATION,
                        header);
                },
                _ => {}
            }
        }

        Box::new(self.client_service.request(request)
                             .map_err(|e| ApiError(format!("No response received: {}", e)))
                             .and_then(|mut response| {
            match response.status().as_u16() {
                200 => {
                    let body = response.into_body();
                    Box::new(
                        future::ok(
                            MultipleAuthSchemeGetResponse::CheckThatLimitingToMultipleRequiredAuthSchemesWorks
                        )
                    ) as Box<dyn Future<Item=_, Error=_> + Send>
                },
                code => {
                    let headers = response.headers().clone();
                    Box::new(response.into_body()
                            .take(100)
                            .concat2()
                            .then(move |body|
                                future::err(ApiError(format!("Unexpected response code {}:\n{:?}\n\n{}",
                                    code,
                                    headers,
                                    match body {
                                        Ok(ref body) => match str::from_utf8(body) {
                                            Ok(body) => Cow::from(body),
                                            Err(e) => Cow::from(format!("<Body was not UTF8: {:?}>", e)),
                                        },
                                        Err(e) => Cow::from(format!("<Failed to read body: {}>", e)),
                                    })))
                            )
                    ) as Box<dyn Future<Item=_, Error=_> + Send>
                }
            }
        }))

    }

    fn readonly_auth_scheme_get(&self, context: &C) -> Box<dyn Future<Item=ReadonlyAuthSchemeGetResponse, Error=ApiError> + Send> {
        let mut uri = format!(
            "{}/readonly_auth_scheme",
            self.base_path
        );

        // Query parameters
        let mut query_string = url::form_urlencoded::Serializer::new("".to_owned());
        let query_string_str = query_string.finish();
        if !query_string_str.is_empty() {
            uri += "?";
            uri += &query_string_str;
        }

        let uri = match Uri::from_str(&uri) {
            Ok(uri) => uri,
            Err(err) => return Box::new(future::err(ApiError(format!("Unable to build URI: {}", err)))),
        };

        let mut request = match hyper::Request::builder()
            .method("GET")
            .uri(uri)
            .body(Body::empty()) {
                Ok(req) => req,
                Err(e) => return Box::new(future::err(ApiError(format!("Unable to create request: {}", e))))
        };


        let header = HeaderValue::from_str((context as &dyn Has<XSpanIdString>).get().0.clone().to_string().as_str());
        request.headers_mut().insert(HeaderName::from_static("x-span-id"), match header {
            Ok(h) => h,
            Err(e) => return Box::new(future::err(ApiError(format!("Unable to create X-Span ID header value: {}", e))))
        });

        if let Some(auth_data) = (context as &dyn Has<Option<AuthData>>).get().as_ref() {
            // Currently only authentication with Basic, API Key, and Bearer are supported
            match auth_data {
                &AuthData::Bearer(ref bearer_header) => {
                    let auth = swagger::auth::Header(bearer_header.clone());
                    let header = match HeaderValue::from_str(&format!("{}", auth)) {
                        Ok(h) => h,
                        Err(e) => return Box::new(future::err(ApiError(format!("Unable to create Authorization header: {}", e))))
                    };
                    request.headers_mut().insert(
                        hyper::header::AUTHORIZATION,
                        header);
                },
                _ => {}
            }
        }

        Box::new(self.client_service.request(request)
                             .map_err(|e| ApiError(format!("No response received: {}", e)))
                             .and_then(|mut response| {
            match response.status().as_u16() {
                200 => {
                    let body = response.into_body();
                    Box::new(
                        future::ok(
                            ReadonlyAuthSchemeGetResponse::CheckThatLimitingToASingleRequiredAuthSchemeWorks
                        )
                    ) as Box<dyn Future<Item=_, Error=_> + Send>
                },
                code => {
                    let headers = response.headers().clone();
                    Box::new(response.into_body()
                            .take(100)
                            .concat2()
                            .then(move |body|
                                future::err(ApiError(format!("Unexpected response code {}:\n{:?}\n\n{}",
                                    code,
                                    headers,
                                    match body {
                                        Ok(ref body) => match str::from_utf8(body) {
                                            Ok(body) => Cow::from(body),
                                            Err(e) => Cow::from(format!("<Body was not UTF8: {:?}>", e)),
                                        },
                                        Err(e) => Cow::from(format!("<Failed to read body: {}>", e)),
                                    })))
                            )
                    ) as Box<dyn Future<Item=_, Error=_> + Send>
                }
            }
        }))

    }

    fn required_octet_stream_put(&self, param_body: swagger::ByteArray, context: &C) -> Box<dyn Future<Item=RequiredOctetStreamPutResponse, Error=ApiError> + Send> {
        let mut uri = format!(
            "{}/required_octet_stream",
            self.base_path
        );

        // Query parameters
        let mut query_string = url::form_urlencoded::Serializer::new("".to_owned());
        let query_string_str = query_string.finish();
        if !query_string_str.is_empty() {
            uri += "?";
            uri += &query_string_str;
        }

        let uri = match Uri::from_str(&uri) {
            Ok(uri) => uri,
            Err(err) => return Box::new(future::err(ApiError(format!("Unable to build URI: {}", err)))),
        };

        let mut request = match hyper::Request::builder()
            .method("PUT")
            .uri(uri)
            .body(Body::empty()) {
                Ok(req) => req,
                Err(e) => return Box::new(future::err(ApiError(format!("Unable to create request: {}", e))))
        };

        let body = param_body.0;
                *request.body_mut() = Body::from(body);

        let header = &mimetypes::requests::REQUIRED_OCTET_STREAM_PUT;
        request.headers_mut().insert(CONTENT_TYPE, match HeaderValue::from_str(header) {
            Ok(h) => h,
            Err(e) => return Box::new(future::err(ApiError(format!("Unable to create header: {} - {}", header, e))))
        });

        let header = HeaderValue::from_str((context as &dyn Has<XSpanIdString>).get().0.clone().to_string().as_str());
        request.headers_mut().insert(HeaderName::from_static("x-span-id"), match header {
            Ok(h) => h,
            Err(e) => return Box::new(future::err(ApiError(format!("Unable to create X-Span ID header value: {}", e))))
        });


        Box::new(self.client_service.request(request)
                             .map_err(|e| ApiError(format!("No response received: {}", e)))
                             .and_then(|mut response| {
            match response.status().as_u16() {
                200 => {
                    let body = response.into_body();
                    Box::new(
                        future::ok(
                            RequiredOctetStreamPutResponse::OK
                        )
                    ) as Box<dyn Future<Item=_, Error=_> + Send>
                },
                code => {
                    let headers = response.headers().clone();
                    Box::new(response.into_body()
                            .take(100)
                            .concat2()
                            .then(move |body|
                                future::err(ApiError(format!("Unexpected response code {}:\n{:?}\n\n{}",
                                    code,
                                    headers,
                                    match body {
                                        Ok(ref body) => match str::from_utf8(body) {
                                            Ok(body) => Cow::from(body),
                                            Err(e) => Cow::from(format!("<Body was not UTF8: {:?}>", e)),
                                        },
                                        Err(e) => Cow::from(format!("<Failed to read body: {}>", e)),
                                    })))
                            )
                    ) as Box<dyn Future<Item=_, Error=_> + Send>
                }
            }
        }))

    }

    fn responses_with_headers_get(&self, context: &C) -> Box<dyn Future<Item=ResponsesWithHeadersGetResponse, Error=ApiError> + Send> {
        let mut uri = format!(
            "{}/responses_with_headers",
            self.base_path
        );

        // Query parameters
        let mut query_string = url::form_urlencoded::Serializer::new("".to_owned());
        let query_string_str = query_string.finish();
        if !query_string_str.is_empty() {
            uri += "?";
            uri += &query_string_str;
        }

        let uri = match Uri::from_str(&uri) {
            Ok(uri) => uri,
            Err(err) => return Box::new(future::err(ApiError(format!("Unable to build URI: {}", err)))),
        };

        let mut request = match hyper::Request::builder()
            .method("GET")
            .uri(uri)
            .body(Body::empty()) {
                Ok(req) => req,
                Err(e) => return Box::new(future::err(ApiError(format!("Unable to create request: {}", e))))
        };


        let header = HeaderValue::from_str((context as &dyn Has<XSpanIdString>).get().0.clone().to_string().as_str());
        request.headers_mut().insert(HeaderName::from_static("x-span-id"), match header {
            Ok(h) => h,
            Err(e) => return Box::new(future::err(ApiError(format!("Unable to create X-Span ID header value: {}", e))))
        });


        Box::new(self.client_service.request(request)
                             .map_err(|e| ApiError(format!("No response received: {}", e)))
                             .and_then(|mut response| {
            match response.status().as_u16() {
                200 => {
                    let response_success_info = match response.headers().get(HeaderName::from_static("success-info")) {
                        Some(response_success_info) => response_success_info.clone(),
                        None => return Box::new(future::err(ApiError(String::from("Required response header Success-Info for response 200 was not found.")))) as Box<dyn Future<Item=_, Error=_> + Send>,
                    };
                    let body = response.into_body();
                    Box::new(
                        body
                        .concat2()
                        .map_err(|e| ApiError(format!("Failed to read response: {}", e)))
                        .and_then(|body|
                            str::from_utf8(&body)
                            .map_err(|e| ApiError(format!("Response was not valid UTF8: {}", e)))
                            .and_then(|body|
                                serde_json::from_str::<String>(body)
                                .map_err(|e| e.into())
                            )
                        )
                        .map(move |body| {
                            ResponsesWithHeadersGetResponse::Success
                            {
                               body: body,
                               success_info: (*Into::<swagger::IntoHeaderValue<String>>::into(response_success_info)).clone(),
                            }
                        })
                    ) as Box<dyn Future<Item=_, Error=_> + Send>
                },
                412 => {
                    let response_further_info = match response.headers().get(HeaderName::from_static("further-info")) {
                        Some(response_further_info) => response_further_info.clone(),
                        None => return Box::new(future::err(ApiError(String::from("Required response header Further-Info for response 412 was not found.")))) as Box<dyn Future<Item=_, Error=_> + Send>,
                    };
                    let response_failure_info = match response.headers().get(HeaderName::from_static("failure-info")) {
                        Some(response_failure_info) => response_failure_info.clone(),
                        None => return Box::new(future::err(ApiError(String::from("Required response header Failure-Info for response 412 was not found.")))) as Box<dyn Future<Item=_, Error=_> + Send>,
                    };
                    let body = response.into_body();
                    Box::new(
                        future::ok(
                            ResponsesWithHeadersGetResponse::PreconditionFailed
                            {
                              further_info: (*Into::<swagger::IntoHeaderValue<String>>::into(response_further_info)).clone(),
                              failure_info: (*Into::<swagger::IntoHeaderValue<String>>::into(response_failure_info)).clone(),
                            }
                        )
                    ) as Box<dyn Future<Item=_, Error=_> + Send>
                },
                code => {
                    let headers = response.headers().clone();
                    Box::new(response.into_body()
                            .take(100)
                            .concat2()
                            .then(move |body|
                                future::err(ApiError(format!("Unexpected response code {}:\n{:?}\n\n{}",
                                    code,
                                    headers,
                                    match body {
                                        Ok(ref body) => match str::from_utf8(body) {
                                            Ok(body) => Cow::from(body),
                                            Err(e) => Cow::from(format!("<Body was not UTF8: {:?}>", e)),
                                        },
                                        Err(e) => Cow::from(format!("<Failed to read body: {}>", e)),
                                    })))
                            )
                    ) as Box<dyn Future<Item=_, Error=_> + Send>
                }
            }
        }))

    }

    fn uuid_get(&self, context: &C) -> Box<dyn Future<Item=UuidGetResponse, Error=ApiError> + Send> {
        let mut uri = format!(
            "{}/uuid",
            self.base_path
        );

        // Query parameters
        let mut query_string = url::form_urlencoded::Serializer::new("".to_owned());
        let query_string_str = query_string.finish();
        if !query_string_str.is_empty() {
            uri += "?";
            uri += &query_string_str;
        }

        let uri = match Uri::from_str(&uri) {
            Ok(uri) => uri,
            Err(err) => return Box::new(future::err(ApiError(format!("Unable to build URI: {}", err)))),
        };

        let mut request = match hyper::Request::builder()
            .method("GET")
            .uri(uri)
            .body(Body::empty()) {
                Ok(req) => req,
                Err(e) => return Box::new(future::err(ApiError(format!("Unable to create request: {}", e))))
        };


        let header = HeaderValue::from_str((context as &dyn Has<XSpanIdString>).get().0.clone().to_string().as_str());
        request.headers_mut().insert(HeaderName::from_static("x-span-id"), match header {
            Ok(h) => h,
            Err(e) => return Box::new(future::err(ApiError(format!("Unable to create X-Span ID header value: {}", e))))
        });


        Box::new(self.client_service.request(request)
                             .map_err(|e| ApiError(format!("No response received: {}", e)))
                             .and_then(|mut response| {
            match response.status().as_u16() {
                200 => {
                    let body = response.into_body();
                    Box::new(
                        body
                        .concat2()
                        .map_err(|e| ApiError(format!("Failed to read response: {}", e)))
                        .and_then(|body|
                            str::from_utf8(&body)
                            .map_err(|e| ApiError(format!("Response was not valid UTF8: {}", e)))
                            .and_then(|body|
                                serde_json::from_str::<uuid::Uuid>(body)
                                .map_err(|e| e.into())
                            )
                        )
                        .map(move |body| {
                            UuidGetResponse::DuplicateResponseLongText
                            (body)
                        })
                    ) as Box<dyn Future<Item=_, Error=_> + Send>
                },
                code => {
                    let headers = response.headers().clone();
                    Box::new(response.into_body()
                            .take(100)
                            .concat2()
                            .then(move |body|
                                future::err(ApiError(format!("Unexpected response code {}:\n{:?}\n\n{}",
                                    code,
                                    headers,
                                    match body {
                                        Ok(ref body) => match str::from_utf8(body) {
                                            Ok(body) => Cow::from(body),
                                            Err(e) => Cow::from(format!("<Body was not UTF8: {:?}>", e)),
                                        },
                                        Err(e) => Cow::from(format!("<Failed to read body: {}>", e)),
                                    })))
                            )
                    ) as Box<dyn Future<Item=_, Error=_> + Send>
                }
            }
        }))

    }

    fn xml_extra_post(&self, param_duplicate_xml_object: Option<models::DuplicateXmlObject>, context: &C) -> Box<dyn Future<Item=XmlExtraPostResponse, Error=ApiError> + Send> {
        let mut uri = format!(
            "{}/xml_extra",
            self.base_path
        );

        // Query parameters
        let mut query_string = url::form_urlencoded::Serializer::new("".to_owned());
        let query_string_str = query_string.finish();
        if !query_string_str.is_empty() {
            uri += "?";
            uri += &query_string_str;
        }

        let uri = match Uri::from_str(&uri) {
            Ok(uri) => uri,
            Err(err) => return Box::new(future::err(ApiError(format!("Unable to build URI: {}", err)))),
        };

        let mut request = match hyper::Request::builder()
            .method("POST")
            .uri(uri)
            .body(Body::empty()) {
                Ok(req) => req,
                Err(e) => return Box::new(future::err(ApiError(format!("Unable to create request: {}", e))))
        };

        let body = param_duplicate_xml_object.map(|ref body| {
            body.to_xml()
        });
        if let Some(body) = body {
                *request.body_mut() = Body::from(body);
        }

        let header = &mimetypes::requests::XML_EXTRA_POST;
        request.headers_mut().insert(CONTENT_TYPE, match HeaderValue::from_str(header) {
            Ok(h) => h,
            Err(e) => return Box::new(future::err(ApiError(format!("Unable to create header: {} - {}", header, e))))
        });

        let header = HeaderValue::from_str((context as &dyn Has<XSpanIdString>).get().0.clone().to_string().as_str());
        request.headers_mut().insert(HeaderName::from_static("x-span-id"), match header {
            Ok(h) => h,
            Err(e) => return Box::new(future::err(ApiError(format!("Unable to create X-Span ID header value: {}", e))))
        });


        Box::new(self.client_service.request(request)
                             .map_err(|e| ApiError(format!("No response received: {}", e)))
                             .and_then(|mut response| {
            match response.status().as_u16() {
                201 => {
                    let body = response.into_body();
                    Box::new(
                        future::ok(
                            XmlExtraPostResponse::OK
                        )
                    ) as Box<dyn Future<Item=_, Error=_> + Send>
                },
                400 => {
                    let body = response.into_body();
                    Box::new(
                        future::ok(
                            XmlExtraPostResponse::BadRequest
                        )
                    ) as Box<dyn Future<Item=_, Error=_> + Send>
                },
                code => {
                    let headers = response.headers().clone();
                    Box::new(response.into_body()
                            .take(100)
                            .concat2()
                            .then(move |body|
                                future::err(ApiError(format!("Unexpected response code {}:\n{:?}\n\n{}",
                                    code,
                                    headers,
                                    match body {
                                        Ok(ref body) => match str::from_utf8(body) {
                                            Ok(body) => Cow::from(body),
                                            Err(e) => Cow::from(format!("<Body was not UTF8: {:?}>", e)),
                                        },
                                        Err(e) => Cow::from(format!("<Failed to read body: {}>", e)),
                                    })))
                            )
                    ) as Box<dyn Future<Item=_, Error=_> + Send>
                }
            }
        }))

    }

    fn xml_other_post(&self, param_another_xml_object: Option<models::AnotherXmlObject>, context: &C) -> Box<dyn Future<Item=XmlOtherPostResponse, Error=ApiError> + Send> {
        let mut uri = format!(
            "{}/xml_other",
            self.base_path
        );

        // Query parameters
        let mut query_string = url::form_urlencoded::Serializer::new("".to_owned());
        let query_string_str = query_string.finish();
        if !query_string_str.is_empty() {
            uri += "?";
            uri += &query_string_str;
        }

        let uri = match Uri::from_str(&uri) {
            Ok(uri) => uri,
            Err(err) => return Box::new(future::err(ApiError(format!("Unable to build URI: {}", err)))),
        };

        let mut request = match hyper::Request::builder()
            .method("POST")
            .uri(uri)
            .body(Body::empty()) {
                Ok(req) => req,
                Err(e) => return Box::new(future::err(ApiError(format!("Unable to create request: {}", e))))
        };

        let body = param_another_xml_object.map(|ref body| {
            body.to_xml()
        });
        if let Some(body) = body {
                *request.body_mut() = Body::from(body);
        }

        let header = &mimetypes::requests::XML_OTHER_POST;
        request.headers_mut().insert(CONTENT_TYPE, match HeaderValue::from_str(header) {
            Ok(h) => h,
            Err(e) => return Box::new(future::err(ApiError(format!("Unable to create header: {} - {}", header, e))))
        });

        let header = HeaderValue::from_str((context as &dyn Has<XSpanIdString>).get().0.clone().to_string().as_str());
        request.headers_mut().insert(HeaderName::from_static("x-span-id"), match header {
            Ok(h) => h,
            Err(e) => return Box::new(future::err(ApiError(format!("Unable to create X-Span ID header value: {}", e))))
        });


        Box::new(self.client_service.request(request)
                             .map_err(|e| ApiError(format!("No response received: {}", e)))
                             .and_then(|mut response| {
            match response.status().as_u16() {
                201 => {
                    let body = response.into_body();
                    Box::new(
                        future::ok(
                            XmlOtherPostResponse::OK
                        )
                    ) as Box<dyn Future<Item=_, Error=_> + Send>
                },
                400 => {
                    let body = response.into_body();
                    Box::new(
                        future::ok(
                            XmlOtherPostResponse::BadRequest
                        )
                    ) as Box<dyn Future<Item=_, Error=_> + Send>
                },
                code => {
                    let headers = response.headers().clone();
                    Box::new(response.into_body()
                            .take(100)
                            .concat2()
                            .then(move |body|
                                future::err(ApiError(format!("Unexpected response code {}:\n{:?}\n\n{}",
                                    code,
                                    headers,
                                    match body {
                                        Ok(ref body) => match str::from_utf8(body) {
                                            Ok(body) => Cow::from(body),
                                            Err(e) => Cow::from(format!("<Body was not UTF8: {:?}>", e)),
                                        },
                                        Err(e) => Cow::from(format!("<Failed to read body: {}>", e)),
                                    })))
                            )
                    ) as Box<dyn Future<Item=_, Error=_> + Send>
                }
            }
        }))

    }

    fn xml_other_put(&self, param_string: Option<models::AnotherXmlArray>, context: &C) -> Box<dyn Future<Item=XmlOtherPutResponse, Error=ApiError> + Send> {
        let mut uri = format!(
            "{}/xml_other",
            self.base_path
        );

        // Query parameters
        let mut query_string = url::form_urlencoded::Serializer::new("".to_owned());
        let query_string_str = query_string.finish();
        if !query_string_str.is_empty() {
            uri += "?";
            uri += &query_string_str;
        }

        let uri = match Uri::from_str(&uri) {
            Ok(uri) => uri,
            Err(err) => return Box::new(future::err(ApiError(format!("Unable to build URI: {}", err)))),
        };

        let mut request = match hyper::Request::builder()
            .method("PUT")
            .uri(uri)
            .body(Body::empty()) {
                Ok(req) => req,
                Err(e) => return Box::new(future::err(ApiError(format!("Unable to create request: {}", e))))
        };

        let body = param_string.map(|ref body| {
            body.to_xml()
        });
        if let Some(body) = body {
                *request.body_mut() = Body::from(body);
        }

        let header = &mimetypes::requests::XML_OTHER_PUT;
        request.headers_mut().insert(CONTENT_TYPE, match HeaderValue::from_str(header) {
            Ok(h) => h,
            Err(e) => return Box::new(future::err(ApiError(format!("Unable to create header: {} - {}", header, e))))
        });

        let header = HeaderValue::from_str((context as &dyn Has<XSpanIdString>).get().0.clone().to_string().as_str());
        request.headers_mut().insert(HeaderName::from_static("x-span-id"), match header {
            Ok(h) => h,
            Err(e) => return Box::new(future::err(ApiError(format!("Unable to create X-Span ID header value: {}", e))))
        });


        Box::new(self.client_service.request(request)
                             .map_err(|e| ApiError(format!("No response received: {}", e)))
                             .and_then(|mut response| {
            match response.status().as_u16() {
                201 => {
                    let body = response.into_body();
                    Box::new(
                        future::ok(
                            XmlOtherPutResponse::OK
                        )
                    ) as Box<dyn Future<Item=_, Error=_> + Send>
                },
                400 => {
                    let body = response.into_body();
                    Box::new(
                        future::ok(
                            XmlOtherPutResponse::BadRequest
                        )
                    ) as Box<dyn Future<Item=_, Error=_> + Send>
                },
                code => {
                    let headers = response.headers().clone();
                    Box::new(response.into_body()
                            .take(100)
                            .concat2()
                            .then(move |body|
                                future::err(ApiError(format!("Unexpected response code {}:\n{:?}\n\n{}",
                                    code,
                                    headers,
                                    match body {
                                        Ok(ref body) => match str::from_utf8(body) {
                                            Ok(body) => Cow::from(body),
                                            Err(e) => Cow::from(format!("<Body was not UTF8: {:?}>", e)),
                                        },
                                        Err(e) => Cow::from(format!("<Failed to read body: {}>", e)),
                                    })))
                            )
                    ) as Box<dyn Future<Item=_, Error=_> + Send>
                }
            }
        }))

    }

    fn xml_post(&self, param_string: Option<models::XmlArray>, context: &C) -> Box<dyn Future<Item=XmlPostResponse, Error=ApiError> + Send> {
        let mut uri = format!(
            "{}/xml",
            self.base_path
        );

        // Query parameters
        let mut query_string = url::form_urlencoded::Serializer::new("".to_owned());
        let query_string_str = query_string.finish();
        if !query_string_str.is_empty() {
            uri += "?";
            uri += &query_string_str;
        }

        let uri = match Uri::from_str(&uri) {
            Ok(uri) => uri,
            Err(err) => return Box::new(future::err(ApiError(format!("Unable to build URI: {}", err)))),
        };

        let mut request = match hyper::Request::builder()
            .method("POST")
            .uri(uri)
            .body(Body::empty()) {
                Ok(req) => req,
                Err(e) => return Box::new(future::err(ApiError(format!("Unable to create request: {}", e))))
        };

        let body = param_string.map(|ref body| {
            body.to_xml()
        });
        if let Some(body) = body {
                *request.body_mut() = Body::from(body);
        }

        let header = &mimetypes::requests::XML_POST;
        request.headers_mut().insert(CONTENT_TYPE, match HeaderValue::from_str(header) {
            Ok(h) => h,
            Err(e) => return Box::new(future::err(ApiError(format!("Unable to create header: {} - {}", header, e))))
        });

        let header = HeaderValue::from_str((context as &dyn Has<XSpanIdString>).get().0.clone().to_string().as_str());
        request.headers_mut().insert(HeaderName::from_static("x-span-id"), match header {
            Ok(h) => h,
            Err(e) => return Box::new(future::err(ApiError(format!("Unable to create X-Span ID header value: {}", e))))
        });


        Box::new(self.client_service.request(request)
                             .map_err(|e| ApiError(format!("No response received: {}", e)))
                             .and_then(|mut response| {
            match response.status().as_u16() {
                201 => {
                    let body = response.into_body();
                    Box::new(
                        future::ok(
                            XmlPostResponse::OK
                        )
                    ) as Box<dyn Future<Item=_, Error=_> + Send>
                },
                400 => {
                    let body = response.into_body();
                    Box::new(
                        future::ok(
                            XmlPostResponse::BadRequest
                        )
                    ) as Box<dyn Future<Item=_, Error=_> + Send>
                },
                code => {
                    let headers = response.headers().clone();
                    Box::new(response.into_body()
                            .take(100)
                            .concat2()
                            .then(move |body|
                                future::err(ApiError(format!("Unexpected response code {}:\n{:?}\n\n{}",
                                    code,
                                    headers,
                                    match body {
                                        Ok(ref body) => match str::from_utf8(body) {
                                            Ok(body) => Cow::from(body),
                                            Err(e) => Cow::from(format!("<Body was not UTF8: {:?}>", e)),
                                        },
                                        Err(e) => Cow::from(format!("<Failed to read body: {}>", e)),
                                    })))
                            )
                    ) as Box<dyn Future<Item=_, Error=_> + Send>
                }
            }
        }))

    }

    fn xml_put(&self, param_xml_object: Option<models::XmlObject>, context: &C) -> Box<dyn Future<Item=XmlPutResponse, Error=ApiError> + Send> {
        let mut uri = format!(
            "{}/xml",
            self.base_path
        );

        // Query parameters
        let mut query_string = url::form_urlencoded::Serializer::new("".to_owned());
        let query_string_str = query_string.finish();
        if !query_string_str.is_empty() {
            uri += "?";
            uri += &query_string_str;
        }

        let uri = match Uri::from_str(&uri) {
            Ok(uri) => uri,
            Err(err) => return Box::new(future::err(ApiError(format!("Unable to build URI: {}", err)))),
        };

        let mut request = match hyper::Request::builder()
            .method("PUT")
            .uri(uri)
            .body(Body::empty()) {
                Ok(req) => req,
                Err(e) => return Box::new(future::err(ApiError(format!("Unable to create request: {}", e))))
        };

        let body = param_xml_object.map(|ref body| {
            body.to_xml()
        });
        if let Some(body) = body {
                *request.body_mut() = Body::from(body);
        }

        let header = &mimetypes::requests::XML_PUT;
        request.headers_mut().insert(CONTENT_TYPE, match HeaderValue::from_str(header) {
            Ok(h) => h,
            Err(e) => return Box::new(future::err(ApiError(format!("Unable to create header: {} - {}", header, e))))
        });

        let header = HeaderValue::from_str((context as &dyn Has<XSpanIdString>).get().0.clone().to_string().as_str());
        request.headers_mut().insert(HeaderName::from_static("x-span-id"), match header {
            Ok(h) => h,
            Err(e) => return Box::new(future::err(ApiError(format!("Unable to create X-Span ID header value: {}", e))))
        });


        Box::new(self.client_service.request(request)
                             .map_err(|e| ApiError(format!("No response received: {}", e)))
                             .and_then(|mut response| {
            match response.status().as_u16() {
                201 => {
                    let body = response.into_body();
                    Box::new(
                        future::ok(
                            XmlPutResponse::OK
                        )
                    ) as Box<dyn Future<Item=_, Error=_> + Send>
                },
                400 => {
                    let body = response.into_body();
                    Box::new(
                        future::ok(
                            XmlPutResponse::BadRequest
                        )
                    ) as Box<dyn Future<Item=_, Error=_> + Send>
                },
                code => {
                    let headers = response.headers().clone();
                    Box::new(response.into_body()
                            .take(100)
                            .concat2()
                            .then(move |body|
                                future::err(ApiError(format!("Unexpected response code {}:\n{:?}\n\n{}",
                                    code,
                                    headers,
                                    match body {
                                        Ok(ref body) => match str::from_utf8(body) {
                                            Ok(body) => Cow::from(body),
                                            Err(e) => Cow::from(format!("<Body was not UTF8: {:?}>", e)),
                                        },
                                        Err(e) => Cow::from(format!("<Failed to read body: {}>", e)),
                                    })))
                            )
                    ) as Box<dyn Future<Item=_, Error=_> + Send>
                }
            }
        }))

    }

}

#[derive(Debug)]
pub enum ClientInitError {
    InvalidScheme,
    InvalidUri(hyper::http::uri::InvalidUri),
    MissingHost,
    SslError(openssl::error::ErrorStack)
}

impl From<hyper::http::uri::InvalidUri> for ClientInitError {
    fn from(err: hyper::http::uri::InvalidUri) -> ClientInitError {
        ClientInitError::InvalidUri(err)
    }
}

impl From<openssl::error::ErrorStack> for ClientInitError {
    fn from(err: openssl::error::ErrorStack) -> ClientInitError {
        ClientInitError::SslError(err)
    }
}

impl fmt::Display for ClientInitError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        (self as &dyn fmt::Debug).fmt(f)
    }
}

impl error::Error for ClientInitError {
    fn description(&self) -> &str {
        "Failed to produce a hyper client."
    }
}
