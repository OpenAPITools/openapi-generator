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
    C: Has<XSpanIdString> ,
    F: Future<Item=Response<Body>, Error=hyper::Error> + Send + 'static
{

    fn op10_get(&self, context: &C) -> Box<dyn Future<Item=Op10GetResponse, Error=ApiError> + Send> {
        let mut uri = format!(
            "{}/op10",
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
                            Op10GetResponse::OK
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

    fn op11_get(&self, context: &C) -> Box<dyn Future<Item=Op11GetResponse, Error=ApiError> + Send> {
        let mut uri = format!(
            "{}/op11",
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
                            Op11GetResponse::OK
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

    fn op12_get(&self, context: &C) -> Box<dyn Future<Item=Op12GetResponse, Error=ApiError> + Send> {
        let mut uri = format!(
            "{}/op12",
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
                            Op12GetResponse::OK
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

    fn op13_get(&self, context: &C) -> Box<dyn Future<Item=Op13GetResponse, Error=ApiError> + Send> {
        let mut uri = format!(
            "{}/op13",
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
                            Op13GetResponse::OK
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

    fn op14_get(&self, context: &C) -> Box<dyn Future<Item=Op14GetResponse, Error=ApiError> + Send> {
        let mut uri = format!(
            "{}/op14",
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
                            Op14GetResponse::OK
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

    fn op15_get(&self, context: &C) -> Box<dyn Future<Item=Op15GetResponse, Error=ApiError> + Send> {
        let mut uri = format!(
            "{}/op15",
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
                            Op15GetResponse::OK
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

    fn op16_get(&self, context: &C) -> Box<dyn Future<Item=Op16GetResponse, Error=ApiError> + Send> {
        let mut uri = format!(
            "{}/op16",
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
                            Op16GetResponse::OK
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

    fn op17_get(&self, context: &C) -> Box<dyn Future<Item=Op17GetResponse, Error=ApiError> + Send> {
        let mut uri = format!(
            "{}/op17",
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
                            Op17GetResponse::OK
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

    fn op18_get(&self, context: &C) -> Box<dyn Future<Item=Op18GetResponse, Error=ApiError> + Send> {
        let mut uri = format!(
            "{}/op18",
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
                            Op18GetResponse::OK
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

    fn op19_get(&self, context: &C) -> Box<dyn Future<Item=Op19GetResponse, Error=ApiError> + Send> {
        let mut uri = format!(
            "{}/op19",
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
                            Op19GetResponse::OK
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

    fn op1_get(&self, context: &C) -> Box<dyn Future<Item=Op1GetResponse, Error=ApiError> + Send> {
        let mut uri = format!(
            "{}/op1",
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
                            Op1GetResponse::OK
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

    fn op20_get(&self, context: &C) -> Box<dyn Future<Item=Op20GetResponse, Error=ApiError> + Send> {
        let mut uri = format!(
            "{}/op20",
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
                            Op20GetResponse::OK
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

    fn op21_get(&self, context: &C) -> Box<dyn Future<Item=Op21GetResponse, Error=ApiError> + Send> {
        let mut uri = format!(
            "{}/op21",
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
                            Op21GetResponse::OK
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

    fn op22_get(&self, context: &C) -> Box<dyn Future<Item=Op22GetResponse, Error=ApiError> + Send> {
        let mut uri = format!(
            "{}/op22",
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
                            Op22GetResponse::OK
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

    fn op23_get(&self, context: &C) -> Box<dyn Future<Item=Op23GetResponse, Error=ApiError> + Send> {
        let mut uri = format!(
            "{}/op23",
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
                            Op23GetResponse::OK
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

    fn op24_get(&self, context: &C) -> Box<dyn Future<Item=Op24GetResponse, Error=ApiError> + Send> {
        let mut uri = format!(
            "{}/op24",
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
                            Op24GetResponse::OK
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

    fn op25_get(&self, context: &C) -> Box<dyn Future<Item=Op25GetResponse, Error=ApiError> + Send> {
        let mut uri = format!(
            "{}/op25",
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
                            Op25GetResponse::OK
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

    fn op26_get(&self, context: &C) -> Box<dyn Future<Item=Op26GetResponse, Error=ApiError> + Send> {
        let mut uri = format!(
            "{}/op26",
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
                            Op26GetResponse::OK
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

    fn op27_get(&self, context: &C) -> Box<dyn Future<Item=Op27GetResponse, Error=ApiError> + Send> {
        let mut uri = format!(
            "{}/op27",
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
                            Op27GetResponse::OK
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

    fn op28_get(&self, context: &C) -> Box<dyn Future<Item=Op28GetResponse, Error=ApiError> + Send> {
        let mut uri = format!(
            "{}/op28",
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
                            Op28GetResponse::OK
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

    fn op29_get(&self, context: &C) -> Box<dyn Future<Item=Op29GetResponse, Error=ApiError> + Send> {
        let mut uri = format!(
            "{}/op29",
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
                            Op29GetResponse::OK
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

    fn op2_get(&self, context: &C) -> Box<dyn Future<Item=Op2GetResponse, Error=ApiError> + Send> {
        let mut uri = format!(
            "{}/op2",
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
                            Op2GetResponse::OK
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

    fn op30_get(&self, context: &C) -> Box<dyn Future<Item=Op30GetResponse, Error=ApiError> + Send> {
        let mut uri = format!(
            "{}/op30",
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
                            Op30GetResponse::OK
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

    fn op31_get(&self, context: &C) -> Box<dyn Future<Item=Op31GetResponse, Error=ApiError> + Send> {
        let mut uri = format!(
            "{}/op31",
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
                            Op31GetResponse::OK
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

    fn op32_get(&self, context: &C) -> Box<dyn Future<Item=Op32GetResponse, Error=ApiError> + Send> {
        let mut uri = format!(
            "{}/op32",
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
                            Op32GetResponse::OK
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

    fn op33_get(&self, context: &C) -> Box<dyn Future<Item=Op33GetResponse, Error=ApiError> + Send> {
        let mut uri = format!(
            "{}/op33",
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
                            Op33GetResponse::OK
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

    fn op34_get(&self, context: &C) -> Box<dyn Future<Item=Op34GetResponse, Error=ApiError> + Send> {
        let mut uri = format!(
            "{}/op34",
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
                            Op34GetResponse::OK
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

    fn op35_get(&self, context: &C) -> Box<dyn Future<Item=Op35GetResponse, Error=ApiError> + Send> {
        let mut uri = format!(
            "{}/op35",
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
                            Op35GetResponse::OK
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

    fn op36_get(&self, context: &C) -> Box<dyn Future<Item=Op36GetResponse, Error=ApiError> + Send> {
        let mut uri = format!(
            "{}/op36",
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
                            Op36GetResponse::OK
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

    fn op37_get(&self, context: &C) -> Box<dyn Future<Item=Op37GetResponse, Error=ApiError> + Send> {
        let mut uri = format!(
            "{}/op37",
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
                            Op37GetResponse::OK
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

    fn op3_get(&self, context: &C) -> Box<dyn Future<Item=Op3GetResponse, Error=ApiError> + Send> {
        let mut uri = format!(
            "{}/op3",
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
                            Op3GetResponse::OK
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

    fn op4_get(&self, context: &C) -> Box<dyn Future<Item=Op4GetResponse, Error=ApiError> + Send> {
        let mut uri = format!(
            "{}/op4",
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
                            Op4GetResponse::OK
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

    fn op5_get(&self, context: &C) -> Box<dyn Future<Item=Op5GetResponse, Error=ApiError> + Send> {
        let mut uri = format!(
            "{}/op5",
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
                            Op5GetResponse::OK
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

    fn op6_get(&self, context: &C) -> Box<dyn Future<Item=Op6GetResponse, Error=ApiError> + Send> {
        let mut uri = format!(
            "{}/op6",
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
                            Op6GetResponse::OK
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

    fn op7_get(&self, context: &C) -> Box<dyn Future<Item=Op7GetResponse, Error=ApiError> + Send> {
        let mut uri = format!(
            "{}/op7",
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
                            Op7GetResponse::OK
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

    fn op8_get(&self, context: &C) -> Box<dyn Future<Item=Op8GetResponse, Error=ApiError> + Send> {
        let mut uri = format!(
            "{}/op8",
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
                            Op8GetResponse::OK
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

    fn op9_get(&self, context: &C) -> Box<dyn Future<Item=Op9GetResponse, Error=ApiError> + Send> {
        let mut uri = format!(
            "{}/op9",
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
                            Op9GetResponse::OK
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
