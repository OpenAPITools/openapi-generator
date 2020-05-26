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
use uuid;
use serde_xml_rs;

use crate::models;
use crate::header;

url::define_encode_set! {
    /// This encode set is used for object IDs
    ///
    /// Aside from the special characters defined in the `PATH_SEGMENT_ENCODE_SET`,
    /// the vertical bar (|) is encoded.
    pub ID_ENCODE_SET = [PATH_SEGMENT_ENCODE_SET] | {'|'}
}

use crate::CallbackApi;
use crate::CallbackCallbackWithHeaderPostResponse;
use crate::CallbackCallbackPostResponse;

/// A client that implements the API by making HTTP calls out to a server.
pub struct Client<F>
{
    /// Inner service
    client_service: Arc<Box<dyn Service<ReqBody=Body, Future=F> + Send + Sync>>,
}

impl<F> fmt::Debug for Client<F>
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Client")
    }
}

impl<F> Clone for Client<F>
{
    fn clone(&self) -> Self {
        Client {
            client_service: self.client_service.clone(),
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
    /// For ordinary tcp connections, prefer the use of `new_http`, `new_https`
    /// and `new_https_mutual`, to avoid introducing a dependency on the underlying transport layer.
    ///
    /// # Arguments
    ///
    /// * `connector` - Implementation of `hyper::client::Connect` to use for the client
    pub fn new_with_connector<C>(
        connector: C,
    ) -> Self where
      C: hyper::client::connect::Connect + 'static,
      C::Transport: 'static,
      C::Future: 'static,
    {
        let client_service = Box::new(hyper::client::Client::builder().build(connector));

        Client {
            client_service: Arc::new(client_service),
        }
    }

    /// Create an HTTP client.
    pub fn new_http() -> Self {
        let http_connector = Connector::builder().build();
        Self::new_with_connector(http_connector)
    }

    /// Create a client with a TLS connection to the server.
    #[cfg(any(target_os = "macos", target_os = "windows", target_os = "ios"))]
    pub fn new_https()  -> Result<Self, native_tls::Error>
    {
        let https_connector = Connector::builder().https().build()?;
        Ok(Self::new_with_connector(https_connector))
    }

    /// Create a client with a TLS connection to the server.
    #[cfg(not(any(target_os = "macos", target_os = "windows", target_os = "ios")))]
    pub fn new_https() -> Result<Self, openssl::error::ErrorStack>
    {
        let https_connector = Connector::builder().https().build()?;
        Ok(Self::new_with_connector(https_connector))
    }

    /// Create a client with a TLS connection to the server, pinning the certificate
    ///
    /// # Arguments
    /// * `ca_certificate` - Path to CA certificate used to authenticate the server
    #[cfg(not(any(target_os = "macos", target_os = "windows", target_os = "ios")))]
    pub fn new_https_pinned<CA>(
        ca_certificate: CA,
    ) -> Result<Self, openssl::error::ErrorStack> where
        CA: AsRef<Path>,
    {
        let https_connector = Connector::builder()
            .https()
            .pin_server_certificate(ca_certificate)
            .build()?;
        Ok(Self::new_with_connector(https_connector))
    }

    /// Create a client with a mutually authenticated TLS connection to the server.
    ///
    /// # Arguments
    /// * `ca_certificate` - Path to CA certificate used to authenticate the server
    /// * `client_key` - Path to the client private key
    /// * `client_certificate` - Path to the client's public certificate associated with the private key
    #[cfg(not(any(target_os = "macos", target_os = "windows", target_os = "ios")))]
    pub fn new_https_mutual<CA, K, D>(
        ca_certificate: CA,
        client_key: K,
        client_certificate: D,
    ) -> Result<Self, openssl::error::ErrorStack>
    where
        CA: AsRef<Path>,
        K: AsRef<Path>,
        D: AsRef<Path>,
    {
        let https_connector = Connector::builder()
            .https()
            .pin_server_certificate(ca_certificate)
            .client_authentication(client_key, client_certificate)
            .build()?;
        Ok(Self::new_with_connector(https_connector))
    }
}

impl<F> Client<F>
{
    /// Constructor for creating a `Client` by passing in a pre-made `swagger::Service`
    ///
    /// This allows adding custom wrappers around the underlying transport, for example for logging.
    pub fn new_with_client_service(
        client_service: Arc<Box<dyn Service<ReqBody=Body, Future=F> + Send + Sync>>,
    ) -> Self {
        Client {
            client_service: client_service,
        }
    }
}

impl<C, F> CallbackApi<C> for Client<F> where
    C: Has<XSpanIdString> + Has<Option<AuthData>>,
    F: Future<Item=Response<Body>, Error=hyper::Error> + Send + 'static
{
    fn callback_callback_with_header_post(
        &self,
        callback_request_query_url: String,
        param_information: Option<String>,
        context: &C) -> Box<dyn Future<Item=CallbackCallbackWithHeaderPostResponse, Error=ApiError> + Send>
    {
        let mut uri = format!(
            "{request_query_url}/callback-with-header"
            ,request_query_url=callback_request_query_url
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

        let header = HeaderValue::from_str((context as &dyn Has<XSpanIdString>).get().0.clone().to_string().as_str());
        request.headers_mut().insert(HeaderName::from_static("x-span-id"), match header {
            Ok(h) => h,
            Err(e) => return Box::new(future::err(ApiError(format!("Unable to create X-Span ID header value: {}", e))))
        });

        // Header parameters
        match param_information {
            Some(param_information) => {
        request.headers_mut().append(
            HeaderName::from_static("information"),
            match header::IntoHeaderValue(param_information.clone()).try_into() {
                Ok(header) => header,
                Err(e) => {
                    return Box::new(future::err(ApiError(format!(
                        "Invalid header information - {}", e)))) as Box<dyn Future<Item=_, Error=_> + Send>;
                },
            });
            },
            None => {}
        }

        Box::new(self.client_service.request(request)
                             .map_err(|e| ApiError(format!("No response received: {}", e)))
                             .and_then(|mut response| {
            match response.status().as_u16() {
                204 => {
                    let body = response.into_body();
                    Box::new(
                        future::ok(
                            CallbackCallbackWithHeaderPostResponse::OK
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

    fn callback_callback_post(
        &self,
        callback_request_query_url: String,
        context: &C) -> Box<dyn Future<Item=CallbackCallbackPostResponse, Error=ApiError> + Send>
    {
        let mut uri = format!(
            "{request_query_url}/callback"
            ,request_query_url=callback_request_query_url
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
                            CallbackCallbackPostResponse::OK
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
