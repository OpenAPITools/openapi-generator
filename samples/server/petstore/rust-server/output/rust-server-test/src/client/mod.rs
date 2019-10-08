use crate::{models, headers::*};
use async_trait::async_trait;
use bytes::Buf;
use headers::HeaderMapExt;
use hyper;
use hyper::Uri;
use hyper::service::Service;
use percent_encoding::{utf8_percent_encode, AsciiSet, CONTROLS};
use futures::{Stream, StreamExt, TryFutureExt, TryStreamExt};
use std::borrow::Cow;
use std::io::{BufReader, Read, Error, ErrorKind};
use std::error;
use std::fmt;
use std::fs::File;
use std::future::Future;
use std::path::Path;
use std::sync::Arc;
use std::str;
use std::str::FromStr;
use std::string::ToString;

use crate::mimetypes;
use serde_json;

#[allow(unused_imports)]
use std::collections::{HashMap, BTreeMap};

use openapi_context::{ApiError, XSpanId, Has, AuthData};

use crate::{
    Api
    ,
    DummyGetResponse,
    DummyPutResponse,
    FileResponseGetResponse,
    HtmlPostResponse,
    RawJsonGetResponse
 };

const ID_ENCODE_SET: &AsciiSet = &CONTROLS.add(b'|');
//define_encode_set! {
//    /// This encode set is used for object IDs
//    ///
//    /// Aside from the special characters defined in the `PATH_SEGMENT_ENCODE_SET`,
//    /// the vertical bar (|) is encoded.
//    pub ID_ENCODE_SET = [PATH_SEGMENT_ENCODE_SET] | {'|'}
//}

/// Convert input into a base path, e.g. "http://example:123". Also checks the scheme as it goes.
fn into_base_path(input: &str, correct_scheme: Option<&'static str>) -> Result<String, ClientInitError> {
    // First convert to Uri, since a base path is a subset of Uri.
    let uri = Uri::from_str(input).map_err(ClientInitError::InvalidUri)?;

    let scheme = uri.scheme_str().ok_or(ClientInitError::InvalidScheme)?;

    // Check the scheme if necessary
    if let Some(correct_scheme) = correct_scheme {
        if scheme != correct_scheme {
            return Err(ClientInitError::InvalidScheme);
        }
    }

    let host = uri.host().ok_or_else(|| ClientInitError::MissingHost)?;
    let port = uri.port_u16().map(|x| format!(":{}", x)).unwrap_or_default();
    Ok(format!("{}://{}{}", scheme, host, port))
}

/// A client that implements the API by making HTTP calls out to a server.
pub struct Client<C> {
    client_service: hyper::Client<C, hyper::Body>,
    base_path: String,
}

impl<C> fmt::Debug for Client<C> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Client {{ base_path: {} }}", self.base_path)
    }
}

impl<C> Clone for Client<C> where C: Clone {
    fn clone(&self) -> Self {
        Client {
            client_service: self.client_service.clone(),
            base_path: self.base_path.clone()
        }
    }
}

fn as_reader<CA: AsRef<Path> + fmt::Display>(path: &CA) -> Result<BufReader<File>, ClientInitError> {
    let f = File::open(path.as_ref())
        .map_err(|e| ClientInitError::InvalidFile(format!("failed to open {}: {}", path, e)))?;
    Ok(BufReader::new(f))
}

impl<C> Client<C> where C: hyper::client::connect::Connect + Clone + Send + Sync + 'static {
    /// Create an HTTP client.
    ///
    /// # Arguments
    /// * `base_path` - base path of the client API, i.e. "www.my-api-implementation.com"
    pub fn try_new_http(base_path: &str) -> Result<Client<hyper::client::HttpConnector>, ClientInitError> {
        let http_connector = hyper::client::HttpConnector::new();
        try_new_with_connector(
            base_path,
            Some("http"),
            http_connector,
        )
    }

    /// Create a client with a TLS connection to the server.
    ///
    /// # Arguments
    /// * `base_path` - base path of the client API, i.e. "www.my-api-implementation.com"
    /// * `ca_certificate_path` - Path to CA certificate used to authenticate the server
    pub fn try_new_https<CA: AsRef<Path> + fmt::Display>(
        base_path: &str,
        ca_certificate_path: CA,
    ) -> Result<Client<hyper_rustls::HttpsConnector<hyper::client::HttpConnector>>, ClientInitError> {
        // Build an HTTP connector which supports HTTPS too.
        let mut http = hyper::client::HttpConnector::new();
        http.enforce_http(false);
        // Build a TLS client, using the custom CA store for lookups.
        let mut tls = rustls::ClientConfig::new();
        let mut rd = as_reader(&ca_certificate_path)?;
        tls.root_store
            .add_pem_file(&mut rd)
            .map_err(|_| ClientInitError::InvalidFile(format!("failed to load custom CA store {}", ca_certificate_path)))?;
        // Join the above part into an HTTPS connector.
        let connector = hyper_rustls::HttpsConnector::from((http, tls));

        try_new_with_connector(
            base_path,
            Some("https"),
            connector,
        )
    }

    /// Create a client with a mutually authenticated TLS connection to the server.
    ///
    /// # Arguments
    /// * `base_path` - base path of the client API, i.e. "www.my-api-implementation.com"
    /// * `ca_certificate_path` - Path to CA certificate used to authenticate the server
    /// * `client_key_path` - Path to the client private key
    /// * `client_certificate_path` - Path to the client's public certificate associated with the private key
    pub fn try_new_https_mutual<CA: AsRef<Path> + fmt::Display>(
        base_path: &str,
        ca_certificate_path: CA,
        client_key_path: CA,
        client_certificate_path: CA,
    ) -> Result<Client<hyper_rustls::HttpsConnector<hyper::client::HttpConnector>>, ClientInitError> {
        // Build an HTTP connector which supports HTTPS too.
        let mut http = hyper::client::HttpConnector::new();
        http.enforce_http(false);

        // Build TLS config
        let mut tls = rustls::ClientConfig::new();

        // Use custom CA
        let mut rd = as_reader(&ca_certificate_path)?;
        tls.root_store
            .add_pem_file(&mut rd)
            .map_err(|_| ClientInitError::InvalidFile(format!("failed to load custom CA store {}", ca_certificate_path)))?;

        // Use custom key and cert
        let mut key_rd = as_reader(&client_key_path)?;
        let mut keys = rustls::internal::pemfile::rsa_private_keys(&mut key_rd)
            .map_err(|_| ClientInitError::InvalidFile(format!("failed to load private key {}", client_key_path)))?;
        let key = keys.pop().ok_or(ClientInitError::InvalidFile(format!("multiple private keys {}", client_key_path)))?;
        let mut cert_rd = as_reader(&client_certificate_path)?;
        let certs = rustls::internal::pemfile::certs(&mut cert_rd)
            .map_err(|_| ClientInitError::InvalidFile(format!("failed to load client cert {}", client_certificate_path)))?;
        tls.set_single_client_cert(certs, key);

        // Join the above part into an HTTPS connector.
        let connector = hyper_rustls::HttpsConnector::from((http, tls));

        try_new_with_connector(
            base_path,
            Some("https"),
            connector,
        )
    }

    /// Create a client with a custom implementation of hyper::client::connect::Connect.
    ///
    /// Intended for use with custom implementations of connect for e.g. protocol logging
    /// or similar functionality which requires wrapping the transport layer. When wrapping a TCP connection,
    /// this function should be used in conjunction with
    /// `hyper::{http_connector, https_connector, https_mutual_connector}`.
    ///
    /// For ordinary tcp connections, prefer the use of `try_new_http`, `try_new_https`
    /// and `try_new_https_mutual`, to avoid introducing a dependency on the underlying transport layer.
    ///
    /// # Arguments
    ///
    /// * `handle` - tokio reactor handle to use for execution
    /// * `base_path` - base path of the client API, i.e. "www.my-api-implementation.com"
    /// * `protocol` - Which protocol to use when constructing the request url, e.g. `Some("http")`
    /// * `connector` - Implementation of `hyper::client::connect::Connect`
    pub fn try_new_with_connector(
        base_path: &str,
        protocol: Option<&'static str>,
        connector: C,
    ) -> Result<Client<C>, ClientInitError> {
        try_new_with_connector(base_path, protocol, connector)
    }

    /// Constructor for creating a `Client` by passing in a pre-made `hyper` client Service.
    ///
    /// This allows adding custom wrappers around the underlying transport, for example for logging.
    pub fn try_new_with_client_service(
        client_service: hyper::Client<C, hyper::Body>,
        base_path: &str
    ) -> Result<Client<C>, ClientInitError> {
        try_new_with_client_service(client_service, base_path)
    }
}

pub fn try_new_with_connector<C>(
    base_path: &str,
    protocol: Option<&'static str>,
    connector: C,
) -> Result<Client<C>, ClientInitError>
     where C: hyper::client::connect::Connect + Clone + Send + Sync + 'static
{
    let client_service = hyper::Client::builder().build(connector);

    try_new_with_client_service(client_service, base_path)
}

fn try_new_with_client_service<C>(
    client_service: hyper::Client<C, hyper::Body>,
    base_path: &str
) -> Result<Client<C>, ClientInitError>
     where C: hyper::client::connect::Connect + Clone + Send + Sync + 'static
{
    Ok(Client {
        client_service: client_service,
        base_path: into_base_path(base_path, None)?,
    })
}

#[async_trait]
impl<Ctx, Conn> Api<Ctx> for Client<Conn> where
    Ctx: Has<XSpanId>  + Send + Sync + 'static,
    Conn: hyper::client::connect::Connect + Clone + Send + Sync + 'static,
{

    async fn dummy_get(&mut self, context: &Ctx) -> Result<DummyGetResponse, ApiError> {
        let mut uri = format!(
            "{}/dummy",
            self.base_path
        );

        {
            let mut query_string = url::form_urlencoded::Serializer::new("".to_owned());


            let query_string = query_string.finish();
            if !query_string.is_empty() {
                uri += "?";
                uri += &query_string;
            }
        }

        let uri = match Uri::from_str(&uri) {
            Ok(uri) => uri,
            Err(err) => return Err(ApiError(format!("Unable to build URI: {}", err))),
        };

        let mut request = hyper::Request::builder();
        request.headers_mut().unwrap().typed_insert(XSpanId( (context as &dyn Has<XSpanId>).get().0.to_string() ));
        request = request.method(hyper::Method::GET);
        request = request.uri(uri);



        let body = hyper::Body::empty();
        let body = hyper::Body::empty();

        request.headers_mut().unwrap().typed_insert(XSpanId( (context as &dyn Has<XSpanId>).get().0.clone() ));
        let request = request.body(body).map_err(|e| ApiError(format!("Failed to build request: {}", e)))?;
        let mut response = self.client_service.call(request)
                             .map_err(|e| ApiError(format!("No response received: {}", e))).await?;
        match response.status().as_u16() {
            200 => {
                let body = hyper::body::aggregate(response.into_body()).await.map_err(|e| ApiError(format!("Error getting response: {}", e)))?;

                Ok(
                    DummyGetResponse::Success
                )
            },
            code => {
                let headers = response.headers().clone();
                let body = hyper::body::aggregate(response.into_body()).await.map_err(|e| ApiError(format!("Error getting response: {}", e)));
                Err(ApiError(format!("Unexpected response code {}:\n{:?}\n\n{}",
                    code,
                    headers,
                    match body {
                        Ok(body) => {
                            match str::from_utf8(body.bytes()) {
                                Ok(body) => body.to_owned(),
                                Err(e) => format!("<Body was not UTF8: {:?}>", e),
                            }
                        },
                        Err(e) => format!("<Failed to read body: {}>", e),
                    })))
            }
        }
    }

    async fn dummy_put(&mut self, param_nested_response: crate::models::InlineObject, context: &Ctx) -> Result<DummyPutResponse, ApiError> {
        let mut uri = format!(
            "{}/dummy",
            self.base_path
        );

        {
            let mut query_string = url::form_urlencoded::Serializer::new("".to_owned());


            let query_string = query_string.finish();
            if !query_string.is_empty() {
                uri += "?";
                uri += &query_string;
            }
        }

        let uri = match Uri::from_str(&uri) {
            Ok(uri) => uri,
            Err(err) => return Err(ApiError(format!("Unable to build URI: {}", err))),
        };

        let mut request = hyper::Request::builder();
        request.headers_mut().unwrap().typed_insert(XSpanId( (context as &dyn Has<XSpanId>).get().0.to_string() ));
        request = request.method(hyper::Method::PUT);
        request = request.uri(uri);



        let body = hyper::Body::empty();
        let body = serde_json::to_string(&param_nested_response).expect("impossible to fail to serialize");
        let body = hyper::Body::from(body);

        request.headers_mut().unwrap().typed_insert(XSpanId( (context as &dyn Has<XSpanId>).get().0.clone() ));
        let request = request.body(body).map_err(|e| ApiError(format!("Failed to build request: {}", e)))?;
        let mut response = self.client_service.call(request)
                             .map_err(|e| ApiError(format!("No response received: {}", e))).await?;
        match response.status().as_u16() {
            200 => {
                let body = hyper::body::aggregate(response.into_body()).await.map_err(|e| ApiError(format!("Error getting response: {}", e)))?;

                Ok(
                    DummyPutResponse::Success
                )
            },
            code => {
                let headers = response.headers().clone();
                let body = hyper::body::aggregate(response.into_body()).await.map_err(|e| ApiError(format!("Error getting response: {}", e)));
                Err(ApiError(format!("Unexpected response code {}:\n{:?}\n\n{}",
                    code,
                    headers,
                    match body {
                        Ok(body) => {
                            match str::from_utf8(body.bytes()) {
                                Ok(body) => body.to_owned(),
                                Err(e) => format!("<Body was not UTF8: {:?}>", e),
                            }
                        },
                        Err(e) => format!("<Failed to read body: {}>", e),
                    })))
            }
        }
    }

    async fn file_response_get(&mut self, context: &Ctx) -> Result<FileResponseGetResponse, ApiError> {
        let mut uri = format!(
            "{}/file_response",
            self.base_path
        );

        {
            let mut query_string = url::form_urlencoded::Serializer::new("".to_owned());


            let query_string = query_string.finish();
            if !query_string.is_empty() {
                uri += "?";
                uri += &query_string;
            }
        }

        let uri = match Uri::from_str(&uri) {
            Ok(uri) => uri,
            Err(err) => return Err(ApiError(format!("Unable to build URI: {}", err))),
        };

        let mut request = hyper::Request::builder();
        request.headers_mut().unwrap().typed_insert(XSpanId( (context as &dyn Has<XSpanId>).get().0.to_string() ));
        request = request.method(hyper::Method::GET);
        request = request.uri(uri);



        let body = hyper::Body::empty();
        let body = hyper::Body::empty();

        request.headers_mut().unwrap().typed_insert(XSpanId( (context as &dyn Has<XSpanId>).get().0.clone() ));
        let request = request.body(body).map_err(|e| ApiError(format!("Failed to build request: {}", e)))?;
        let mut response = self.client_service.call(request)
                             .map_err(|e| ApiError(format!("No response received: {}", e))).await?;
        match response.status().as_u16() {
            200 => {
                let body = hyper::body::aggregate(response.into_body()).await.map_err(|e| ApiError(format!("Error getting response: {}", e)))?;
                let body = str::from_utf8(body.bytes())
                    .map_err(|e| ApiError(format!("Response was not valid UTF8: {}", e)))?;
                let body = serde_json::from_str::<openapi_context::ByteArray>(body)
                    .map_err(ApiError::from)?;

                        Ok(FileResponseGetResponse::Success(body))
            },
            code => {
                let headers = response.headers().clone();
                let body = hyper::body::aggregate(response.into_body()).await.map_err(|e| ApiError(format!("Error getting response: {}", e)));
                Err(ApiError(format!("Unexpected response code {}:\n{:?}\n\n{}",
                    code,
                    headers,
                    match body {
                        Ok(body) => {
                            match str::from_utf8(body.bytes()) {
                                Ok(body) => body.to_owned(),
                                Err(e) => format!("<Body was not UTF8: {:?}>", e),
                            }
                        },
                        Err(e) => format!("<Failed to read body: {}>", e),
                    })))
            }
        }
    }

    async fn html_post(&mut self, param_body: String, context: &Ctx) -> Result<HtmlPostResponse, ApiError> {
        let mut uri = format!(
            "{}/html",
            self.base_path
        );

        {
            let mut query_string = url::form_urlencoded::Serializer::new("".to_owned());


            let query_string = query_string.finish();
            if !query_string.is_empty() {
                uri += "?";
                uri += &query_string;
            }
        }

        let uri = match Uri::from_str(&uri) {
            Ok(uri) => uri,
            Err(err) => return Err(ApiError(format!("Unable to build URI: {}", err))),
        };

        let mut request = hyper::Request::builder();
        request.headers_mut().unwrap().typed_insert(XSpanId( (context as &dyn Has<XSpanId>).get().0.to_string() ));
        request = request.method(hyper::Method::POST);
        request = request.uri(uri);



        let body = hyper::Body::empty();
        let body = param_body;
        let body = hyper::Body::from(body);

        request.headers_mut().unwrap().typed_insert(XSpanId( (context as &dyn Has<XSpanId>).get().0.clone() ));
        let request = request.body(body).map_err(|e| ApiError(format!("Failed to build request: {}", e)))?;
        let mut response = self.client_service.call(request)
                             .map_err(|e| ApiError(format!("No response received: {}", e))).await?;
        match response.status().as_u16() {
            200 => {
                let body = hyper::body::aggregate(response.into_body()).await.map_err(|e| ApiError(format!("Error getting response: {}", e)))?;
                let body = str::from_utf8(body.bytes())
                    .map_err(|e| ApiError(format!("Response was not valid UTF8: {}", e)))?;

                        Ok(HtmlPostResponse::Success(body))
            },
            code => {
                let headers = response.headers().clone();
                let body = hyper::body::aggregate(response.into_body()).await.map_err(|e| ApiError(format!("Error getting response: {}", e)));
                Err(ApiError(format!("Unexpected response code {}:\n{:?}\n\n{}",
                    code,
                    headers,
                    match body {
                        Ok(body) => {
                            match str::from_utf8(body.bytes()) {
                                Ok(body) => body.to_owned(),
                                Err(e) => format!("<Body was not UTF8: {:?}>", e),
                            }
                        },
                        Err(e) => format!("<Failed to read body: {}>", e),
                    })))
            }
        }
    }

    async fn raw_json_get(&mut self, context: &Ctx) -> Result<RawJsonGetResponse, ApiError> {
        let mut uri = format!(
            "{}/raw_json",
            self.base_path
        );

        {
            let mut query_string = url::form_urlencoded::Serializer::new("".to_owned());


            let query_string = query_string.finish();
            if !query_string.is_empty() {
                uri += "?";
                uri += &query_string;
            }
        }

        let uri = match Uri::from_str(&uri) {
            Ok(uri) => uri,
            Err(err) => return Err(ApiError(format!("Unable to build URI: {}", err))),
        };

        let mut request = hyper::Request::builder();
        request.headers_mut().unwrap().typed_insert(XSpanId( (context as &dyn Has<XSpanId>).get().0.to_string() ));
        request = request.method(hyper::Method::GET);
        request = request.uri(uri);



        let body = hyper::Body::empty();
        let body = hyper::Body::empty();

        request.headers_mut().unwrap().typed_insert(XSpanId( (context as &dyn Has<XSpanId>).get().0.clone() ));
        let request = request.body(body).map_err(|e| ApiError(format!("Failed to build request: {}", e)))?;
        let mut response = self.client_service.call(request)
                             .map_err(|e| ApiError(format!("No response received: {}", e))).await?;
        match response.status().as_u16() {
            200 => {
                let body = hyper::body::aggregate(response.into_body()).await.map_err(|e| ApiError(format!("Error getting response: {}", e)))?;
                let body = str::from_utf8(body.bytes())
                    .map_err(|e| ApiError(format!("Response was not valid UTF8: {}", e)))?;
                let body = serde_json::from_str::<serde_json::Value>(body)
                    .map_err(ApiError::from)?;

                        Ok(RawJsonGetResponse::Success(body))
            },
            code => {
                let headers = response.headers().clone();
                let body = hyper::body::aggregate(response.into_body()).await.map_err(|e| ApiError(format!("Error getting response: {}", e)));
                Err(ApiError(format!("Unexpected response code {}:\n{:?}\n\n{}",
                    code,
                    headers,
                    match body {
                        Ok(body) => {
                            match str::from_utf8(body.bytes()) {
                                Ok(body) => body.to_owned(),
                                Err(e) => format!("<Body was not UTF8: {:?}>", e),
                            }
                        },
                        Err(e) => format!("<Failed to read body: {}>", e),
                    })))
            }
        }
    }

}

#[derive(Debug)]
pub enum ClientInitError {
    InvalidScheme,
    InvalidUri(http::uri::InvalidUri),
    MissingHost,
    InvalidCertificate(String),
    InvalidFile(String),
}

impl From<http::uri::InvalidUri> for ClientInitError {
    fn from(err: http::uri::InvalidUri) -> ClientInitError {
        ClientInitError::InvalidUri(err)
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
