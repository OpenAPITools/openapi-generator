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

use mime::Mime;
use std::io::Cursor;
use multipart::client::lazy::Multipart;

use {Api,
     MultipartRequestPostResponse
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

    fn multipart_request_post(&self, param_string_field: String, param_binary_field: swagger::ByteArray, param_optional_string_field: Option<String>, param_object_field: Option<models::MultipartRequestObjectField>, context: &C) -> Box<dyn Future<Item=MultipartRequestPostResponse, Error=ApiError> + Send> {
        let mut uri = format!(
            "{}/multipart_request",
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

        let mut multipart = Multipart::new(); 

        // For each parameter, encode as appropriate and add to the multipart body as a stream.

        let string_field_str = match serde_json::to_string(&param_string_field) {
            Ok(str) => str,
            Err(e) => return Box::new(future::err(ApiError(format!("Unable to parse string_field to string: {}", e)))),
        };

        let string_field_vec = string_field_str.as_bytes().to_vec();

        let string_field_mime = mime_0_2::Mime::from_str("application/json").expect("impossible to fail to parse");

        let string_field_cursor = Cursor::new(string_field_vec);

        multipart.add_stream("string_field",  string_field_cursor,  None as Option<&str>, Some(string_field_mime));  

        let optional_string_field_str = match serde_json::to_string(&param_optional_string_field) {
            Ok(str) => str,
            Err(e) => return Box::new(future::err(ApiError(format!("Unable to parse optional_string_field to string: {}", e)))),
        };

        let optional_string_field_vec = optional_string_field_str.as_bytes().to_vec();

        let optional_string_field_mime = mime_0_2::Mime::from_str("application/json").expect("impossible to fail to parse");

        let optional_string_field_cursor = Cursor::new(optional_string_field_vec);

        multipart.add_stream("optional_string_field",  optional_string_field_cursor,  None as Option<&str>, Some(optional_string_field_mime));  

        let object_field_str = match serde_json::to_string(&param_object_field) {
            Ok(str) => str,
            Err(e) => return Box::new(future::err(ApiError(format!("Unable to parse object_field to string: {}", e)))),
        };

        let object_field_vec = object_field_str.as_bytes().to_vec();

        let object_field_mime = mime_0_2::Mime::from_str("application/json").expect("impossible to fail to parse");

        let object_field_cursor = Cursor::new(object_field_vec);

        multipart.add_stream("object_field",  object_field_cursor,  None as Option<&str>, Some(object_field_mime));  

        let binary_field_vec = param_binary_field.to_vec();

        let binary_field_mime = match mime_0_2::Mime::from_str("application/octet-stream") {
            Ok(mime) => mime,
            Err(err) => return Box::new(future::err(ApiError(format!("Unable to get mime type: {:?}", err)))),
        };

        let binary_field_cursor = Cursor::new(binary_field_vec);

        let filename = None as Option<&str> ;
        multipart.add_stream("binary_field",  binary_field_cursor,  filename, Some(binary_field_mime));

        let mut fields = match multipart.prepare() {
            Ok(fields) => fields,
            Err(err) => return Box::new(future::err(ApiError(format!("Unable to build request: {}", err)))),
        };

        let mut body_string = String::new();
        match fields.read_to_string(&mut body_string) {
            Ok(_) => (),
            Err(err) => return Box::new(future::err(ApiError(format!("Unable to build body: {}", err)))),
        }
        let boundary = fields.boundary();

        let multipart_header = format!("multipart/form-data;boundary={}", boundary);

        *request.body_mut() = Body::from(body_string);
        request.headers_mut().insert(CONTENT_TYPE, match HeaderValue::from_str(&multipart_header) {
            Ok(h) => h,
            Err(e) => return Box::new(future::err(ApiError(format!("Unable to create header: {} - {}", multipart_header, e))))
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
                            MultipartRequestPostResponse::OK
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
