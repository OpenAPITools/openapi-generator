use futures;
use futures::{Future, Stream, future, stream};
use hyper;
use hyper::client::HttpConnector;
use hyper::header::{HeaderName, HeaderValue, CONTENT_TYPE};
use hyper::{Body, Uri, Response};
#[cfg(not(any(target_os = "macos", target_os = "windows", target_os = "ios")))]
use hyper_openssl::HttpsConnector;
use serde_json;
use std::borrow::Cow;
use std::convert::TryInto;
use std::io::{Read, Error, ErrorKind};
use std::error;
use std::fmt;
use std::path::Path;
use std::sync::Arc;
use std::str;
use std::str::FromStr;
use std::string::ToString;
use swagger;
use swagger::{ApiError, Connector, client::Service, XSpanIdString, Has, AuthData};
use url::form_urlencoded;
use url::percent_encoding::{utf8_percent_encode, PATH_SEGMENT_ENCODE_SET, QUERY_ENCODE_SET};

use crate::models;
use crate::header;

url::define_encode_set! {
    /// This encode set is used for object IDs
    ///
    /// Aside from the special characters defined in the `PATH_SEGMENT_ENCODE_SET`,
    /// the vertical bar (|) is encoded.
    pub ID_ENCODE_SET = [PATH_SEGMENT_ENCODE_SET] | {'|'}
}

use crate::{Api,
     AllOfGetResponse,
     DummyGetResponse,
     DummyPutResponse,
     FileResponseGetResponse,
     GetStructuredYamlResponse,
     HtmlPostResponse,
     PostYamlResponse,
     RawJsonGetResponse,
     SoloObjectPostResponse
     };

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
    Ok(format!("{}://{}{}{}", scheme, host, port, uri.path().trim_end_matches('/')))
}

/// A client that implements the API by making HTTP calls out to a server.
pub struct Client<F>
{
    /// Inner service
    client_service: Arc<Box<dyn Service<ReqBody=Body, Future=F> + Send + Sync>>,

    /// Base path of the API
    base_path: String,
}

impl<F> fmt::Debug for Client<F>
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
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
    /// this function should be used in conjunction with `swagger::Connector::builder()`.
    ///
    /// For ordinary tcp connections, prefer the use of `try_new_http`, `try_new_https`
    /// and `try_new_https_mutual`, to avoid introducing a dependency on the underlying transport layer.
    ///
    /// # Arguments
    ///
    /// * `base_path` - base path of the client API, i.e. "www.my-api-implementation.com"
    /// * `protocol` - Which protocol to use when constructing the request url, e.g. `Some("http")`
    /// * `connector` - Implementation of `hyper::client::Connect` to use for the client
    pub fn try_new_with_connector<C>(
        base_path: &str,
        protocol: Option<&'static str>,
        connector: C,
    ) -> Result<Self, ClientInitError> where
      C: hyper::client::connect::Connect + 'static,
      C::Transport: 'static,
      C::Future: 'static,
    {
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
        let http_connector = Connector::builder().build();

        Self::try_new_with_connector(base_path, Some("http"), http_connector)
    }

    /// Create a client with a TLS connection to the server
    ///
    /// # Arguments
    /// * `base_path` - base path of the client API, i.e. "www.my-api-implementation.com"
    pub fn try_new_https(base_path: &str) -> Result<Self, ClientInitError>
    {
        let https_connector = Connector::builder()
            .https()
            .build()
            .map_err(|e| ClientInitError::SslError(e))?;
        Self::try_new_with_connector(base_path, Some("https"), https_connector)
    }

    /// Create a client with a TLS connection to the server using a pinned certificate
    ///
    /// # Arguments
    /// * `base_path` - base path of the client API, i.e. "www.my-api-implementation.com"
    /// * `ca_certificate` - Path to CA certificate used to authenticate the server
    #[cfg(not(any(target_os = "macos", target_os = "windows", target_os = "ios")))]
    pub fn try_new_https_pinned<CA>(
        base_path: &str,
        ca_certificate: CA,
    ) -> Result<Self, ClientInitError>
    where
        CA: AsRef<Path>,
    {
        let https_connector = Connector::builder()
            .https()
            .pin_server_certificate(ca_certificate)
            .build()
            .map_err(|e| ClientInitError::SslError(e))?;
        Self::try_new_with_connector(base_path, Some("https"), https_connector)
    }

    /// Create a client with a mutually authenticated TLS connection to the server.
    ///
    /// # Arguments
    /// * `base_path` - base path of the client API, i.e. "www.my-api-implementation.com"
    /// * `ca_certificate` - Path to CA certificate used to authenticate the server
    /// * `client_key` - Path to the client private key
    /// * `client_certificate` - Path to the client's public certificate associated with the private key
    #[cfg(not(any(target_os = "macos", target_os = "windows", target_os = "ios")))]
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
        let https_connector = Connector::builder()
            .https()
            .pin_server_certificate(ca_certificate)
            .client_authentication(client_key, client_certificate)
            .build()
            .map_err(|e| ClientInitError::SslError(e))?;
        Self::try_new_with_connector(base_path, Some("https"), https_connector)
    }
}

impl<F> Client<F>
{
    /// Constructor for creating a `Client` by passing in a pre-made `swagger::Service`
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

/// Error type failing to create a Client
#[derive(Debug)]
pub enum ClientInitError {
    /// Invalid URL Scheme
    InvalidScheme,

    /// Invalid URI
    InvalidUri(hyper::http::uri::InvalidUri),

    /// Missing Hostname
    MissingHost,

    /// SSL Connection Error
    #[cfg(any(target_os = "macos", target_os = "windows", target_os = "ios"))]
    SslError(native_tls::Error),

    /// SSL Connection Error
    #[cfg(not(any(target_os = "macos", target_os = "windows", target_os = "ios")))]
    SslError(openssl::error::ErrorStack),
}

impl From<hyper::http::uri::InvalidUri> for ClientInitError {
    fn from(err: hyper::http::uri::InvalidUri) -> ClientInitError {
        ClientInitError::InvalidUri(err)
    }
}

impl fmt::Display for ClientInitError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let s: &dyn fmt::Debug = self;
        s.fmt(f)
    }
}

impl error::Error for ClientInitError {
    fn description(&self) -> &str {
        "Failed to produce a hyper client."
    }
}

impl<C, F> Api<C> for Client<F> where
    C: Has<XSpanIdString> ,
    F: Future<Item=Response<Body>, Error=hyper::Error> + Send + 'static
{
    fn all_of_get(
        &self,
        context: &C) -> Box<dyn Future<Item=AllOfGetResponse, Error=ApiError> + Send>
    {
        let mut uri = format!(
            "{}/allOf",
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
                                                 serde_json::from_str::<models::AllOfObject>(body)
                                                     .map_err(|e| e.into())
                                             )
                                 )
                        .map(move |body| {
                            AllOfGetResponse::OK
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

    fn dummy_get(
        &self,
        context: &C) -> Box<dyn Future<Item=DummyGetResponse, Error=ApiError> + Send>
    {
        let mut uri = format!(
            "{}/dummy",
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
                        future::ok(
                            DummyGetResponse::Success
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

    fn dummy_put(
        &self,
        param_nested_response: models::DummyPutBody,
        context: &C) -> Box<dyn Future<Item=DummyPutResponse, Error=ApiError> + Send>
    {
        let mut uri = format!(
            "{}/dummy",
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

        let body = serde_json::to_string(&param_nested_response).expect("impossible to fail to serialize");
                *request.body_mut() = Body::from(body);

        let header = "application/json";
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
                            DummyPutResponse::Success
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

    fn file_response_get(
        &self,
        context: &C) -> Box<dyn Future<Item=FileResponseGetResponse, Error=ApiError> + Send>
    {
        let mut uri = format!(
            "{}/file_response",
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
                                                 serde_json::from_str::<swagger::ByteArray>(body)
                                                     .map_err(|e| e.into())
                                             )
                                 )
                        .map(move |body| {
                            FileResponseGetResponse::Success
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

    fn get_structured_yaml(
        &self,
        context: &C) -> Box<dyn Future<Item=GetStructuredYamlResponse, Error=ApiError> + Send>
    {
        let mut uri = format!(
            "{}/get-structured-yaml",
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
                                                 Ok(body.to_string())
                                             )
                                 )
                        .map(move |body| {
                            GetStructuredYamlResponse::OK
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

    fn html_post(
        &self,
        param_body: String,
        context: &C) -> Box<dyn Future<Item=HtmlPostResponse, Error=ApiError> + Send>
    {
        let mut uri = format!(
            "{}/html",
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

        let body = param_body;
                *request.body_mut() = Body::from(body);

        let header = "text/html";
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
                            HtmlPostResponse::Success
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

    fn post_yaml(
        &self,
        param_value: String,
        context: &C) -> Box<dyn Future<Item=PostYamlResponse, Error=ApiError> + Send>
    {
        let mut uri = format!(
            "{}/post-yaml",
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

        let body = param_value;
                *request.body_mut() = Body::from(body);

        let header = "application/yaml";
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
                204 => {
                    let body = response.into_body();
                    Box::new(
                        future::ok(
                            PostYamlResponse::OK
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

    fn raw_json_get(
        &self,
        context: &C) -> Box<dyn Future<Item=RawJsonGetResponse, Error=ApiError> + Send>
    {
        let mut uri = format!(
            "{}/raw_json",
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
                                                 serde_json::from_str::<serde_json::Value>(body)
                                                     .map_err(|e| e.into())
                                             )
                                 )
                        .map(move |body| {
                            RawJsonGetResponse::Success
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

    fn solo_object_post(
        &self,
        param_value: serde_json::Value,
        context: &C) -> Box<dyn Future<Item=SoloObjectPostResponse, Error=ApiError> + Send>
    {
        let mut uri = format!(
            "{}/solo-object",
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

        let body = serde_json::to_string(&param_value).expect("impossible to fail to serialize");

                *request.body_mut() = Body::from(body);

        let header = "application/json";
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
                204 => {
                    let body = response.into_body();
                    Box::new(
                        future::ok(
                            SoloObjectPostResponse::OK
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
