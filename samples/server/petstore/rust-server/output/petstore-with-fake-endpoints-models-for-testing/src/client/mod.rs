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
use uuid;
use serde_xml_rs;

use {Api,
     TestSpecialTagsResponse,
     FakeOuterBooleanSerializeResponse,
     FakeOuterCompositeSerializeResponse,
     FakeOuterNumberSerializeResponse,
     FakeOuterStringSerializeResponse,
     TestBodyWithQueryParamsResponse,
     TestClientModelResponse,
     TestEndpointParametersResponse,
     TestEnumParametersResponse,
     TestInlineAdditionalPropertiesResponse,
     TestJsonFormDataResponse,
     TestClassnameResponse,
     AddPetResponse,
     DeletePetResponse,
     FindPetsByStatusResponse,
     FindPetsByTagsResponse,
     GetPetByIdResponse,
     UpdatePetResponse,
     UpdatePetWithFormResponse,
     UploadFileResponse,
     DeleteOrderResponse,
     GetInventoryResponse,
     GetOrderByIdResponse,
     PlaceOrderResponse,
     CreateUserResponse,
     CreateUsersWithArrayInputResponse,
     CreateUsersWithListInputResponse,
     DeleteUserResponse,
     GetUserByNameResponse,
     LoginUserResponse,
     LogoutUserResponse,
     UpdateUserResponse
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

    fn test_special_tags(&self, param_body: models::Client, context: &C) -> Box<dyn Future<Item=TestSpecialTagsResponse, Error=ApiError> + Send> {
        let mut uri = format!(
            "{}/v2/another-fake/dummy",
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
            .method("PATCH")
            .uri(uri)
            .body(Body::empty()) {
                Ok(req) => req,
                Err(e) => return Box::new(future::err(ApiError(format!("Unable to create request: {}", e))))
        };

        // Body parameter
        let body = serde_json::to_string(&param_body).expect("impossible to fail to serialize");
                *request.body_mut() = Body::from(body);

        let header = &mimetypes::requests::TEST_SPECIAL_TAGS;
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
                                serde_json::from_str::<models::Client>(body)
                                .map_err(|e| e.into())
                            )
                        )
                        .map(move |body| {
                            TestSpecialTagsResponse::SuccessfulOperation
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

    fn fake_outer_boolean_serialize(&self, param_body: Option<models::OuterBoolean>, context: &C) -> Box<dyn Future<Item=FakeOuterBooleanSerializeResponse, Error=ApiError> + Send> {
        let mut uri = format!(
            "{}/v2/fake/outer/boolean",
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

        // Body parameter
        let body = param_body.map(|ref body| {
            serde_json::to_string(body).expect("impossible to fail to serialize")
        });
        if let Some(body) = body {
                *request.body_mut() = Body::from(body);
        }

        let header = &mimetypes::requests::FAKE_OUTER_BOOLEAN_SERIALIZE;
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
                                serde_json::from_str::<bool>(body)
                                .map_err(|e| e.into())
                            )
                        )
                        .map(move |body| {
                            FakeOuterBooleanSerializeResponse::OutputBoolean
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

    fn fake_outer_composite_serialize(&self, param_body: Option<models::OuterComposite>, context: &C) -> Box<dyn Future<Item=FakeOuterCompositeSerializeResponse, Error=ApiError> + Send> {
        let mut uri = format!(
            "{}/v2/fake/outer/composite",
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

        let body = param_body.map(|ref body| {
            serde_json::to_string(body).expect("impossible to fail to serialize")
        });
        if let Some(body) = body {
                *request.body_mut() = Body::from(body);
        }

        let header = &mimetypes::requests::FAKE_OUTER_COMPOSITE_SERIALIZE;
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
                                serde_json::from_str::<models::OuterComposite>(body)
                                .map_err(|e| e.into())
                            )
                        )
                        .map(move |body| {
                            FakeOuterCompositeSerializeResponse::OutputComposite
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

    fn fake_outer_number_serialize(&self, param_body: Option<models::OuterNumber>, context: &C) -> Box<dyn Future<Item=FakeOuterNumberSerializeResponse, Error=ApiError> + Send> {
        let mut uri = format!(
            "{}/v2/fake/outer/number",
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

        let body = param_body.map(|ref body| {
            serde_json::to_string(body).expect("impossible to fail to serialize")
        });
        if let Some(body) = body {
                *request.body_mut() = Body::from(body);
        }

        let header = &mimetypes::requests::FAKE_OUTER_NUMBER_SERIALIZE;
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
                                serde_json::from_str::<f64>(body)
                                .map_err(|e| e.into())
                            )
                        )
                        .map(move |body| {
                            FakeOuterNumberSerializeResponse::OutputNumber
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

    fn fake_outer_string_serialize(&self, param_body: Option<models::OuterString>, context: &C) -> Box<dyn Future<Item=FakeOuterStringSerializeResponse, Error=ApiError> + Send> {
        let mut uri = format!(
            "{}/v2/fake/outer/string",
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

        let body = param_body.map(|ref body| {
            serde_json::to_string(body).expect("impossible to fail to serialize")
        });
        if let Some(body) = body {
                *request.body_mut() = Body::from(body);
        }

        let header = &mimetypes::requests::FAKE_OUTER_STRING_SERIALIZE;
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
                                serde_json::from_str::<String>(body)
                                .map_err(|e| e.into())
                            )
                        )
                        .map(move |body| {
                            FakeOuterStringSerializeResponse::OutputString
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

    fn test_body_with_query_params(&self, param_query: String, param_body: models::User, context: &C) -> Box<dyn Future<Item=TestBodyWithQueryParamsResponse, Error=ApiError> + Send> {
        let mut uri = format!(
            "{}/v2/fake/body-with-query-params",
            self.base_path
        );

        // Query parameters
        let mut query_string = url::form_urlencoded::Serializer::new("".to_owned());
            query_string.append_pair("query", &param_query.to_string());
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

        let body = serde_json::to_string(&param_body).expect("impossible to fail to serialize");
                *request.body_mut() = Body::from(body);

        let header = &mimetypes::requests::TEST_BODY_WITH_QUERY_PARAMS;
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
                            TestBodyWithQueryParamsResponse::Success
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

    fn test_client_model(&self, param_body: models::Client, context: &C) -> Box<dyn Future<Item=TestClientModelResponse, Error=ApiError> + Send> {
        let mut uri = format!(
            "{}/v2/fake",
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
            .method("PATCH")
            .uri(uri)
            .body(Body::empty()) {
                Ok(req) => req,
                Err(e) => return Box::new(future::err(ApiError(format!("Unable to create request: {}", e))))
        };

        let body = serde_json::to_string(&param_body).expect("impossible to fail to serialize");
                *request.body_mut() = Body::from(body);

        let header = &mimetypes::requests::TEST_CLIENT_MODEL;
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
                                serde_json::from_str::<models::Client>(body)
                                .map_err(|e| e.into())
                            )
                        )
                        .map(move |body| {
                            TestClientModelResponse::SuccessfulOperation
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

    fn test_endpoint_parameters(&self, param_number: f64, param_double: f64, param_pattern_without_delimiter: String, param_byte: swagger::ByteArray, param_integer: Option<i32>, param_int32: Option<i32>, param_int64: Option<i64>, param_float: Option<f32>, param_string: Option<String>, param_binary: Option<swagger::ByteArray>, param_date: Option<chrono::DateTime<chrono::Utc>>, param_date_time: Option<chrono::DateTime<chrono::Utc>>, param_password: Option<String>, param_callback: Option<String>, context: &C) -> Box<dyn Future<Item=TestEndpointParametersResponse, Error=ApiError> + Send> {
        let mut uri = format!(
            "{}/v2/fake",
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

        let params = &[
            ("integer", param_integer.map(|param| format!("{:?}", param))),
            ("int32", param_int32.map(|param| format!("{:?}", param))),
            ("int64", param_int64.map(|param| format!("{:?}", param))),
            ("number", Some(format!("{:?}", param_number))),
            ("float", param_float.map(|param| format!("{:?}", param))),
            ("double", Some(format!("{:?}", param_double))),
            ("string", param_string),
            ("pattern_without_delimiter", Some(param_pattern_without_delimiter)),
            ("byte", Some(format!("{:?}", param_byte))),
            ("binary", param_binary.map(|param| format!("{:?}", param))),
            ("date", param_date.map(|param| format!("{:?}", param))),
            ("dateTime", param_date_time.map(|param| format!("{:?}", param))),
            ("password", param_password),
            ("callback", param_callback),
        ];
        let body = serde_urlencoded::to_string(params).expect("impossible to fail to serialize");

        let header = &mimetypes::requests::TEST_ENDPOINT_PARAMETERS;
        request.headers_mut().insert(CONTENT_TYPE, match HeaderValue::from_str(header) {
            Ok(h) => h,
            Err(e) => return Box::new(future::err(ApiError(format!("Unable to create header: {} - {}", header, e))))
        });
        *request.body_mut() = Body::from(body.into_bytes());

        let header = HeaderValue::from_str((context as &dyn Has<XSpanIdString>).get().0.clone().to_string().as_str());
        request.headers_mut().insert(HeaderName::from_static("x-span-id"), match header {
            Ok(h) => h,
            Err(e) => return Box::new(future::err(ApiError(format!("Unable to create X-Span ID header value: {}", e))))
        });

        if let Some(auth_data) = (context as &dyn Has<Option<AuthData>>).get().as_ref() {
            // Currently only authentication with Basic, API Key, and Bearer are supported
            match auth_data {
                &AuthData::Basic(ref basic_header) => {
                    let auth = swagger::auth::Header(basic_header.clone());
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
                400 => {
                    let body = response.into_body();
                    Box::new(
                        future::ok(
                            TestEndpointParametersResponse::InvalidUsernameSupplied
                        )
                    ) as Box<dyn Future<Item=_, Error=_> + Send>
                },
                404 => {
                    let body = response.into_body();
                    Box::new(
                        future::ok(
                            TestEndpointParametersResponse::UserNotFound
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

    fn test_enum_parameters(&self, param_enum_header_string_array: Option<&Vec<String>>, param_enum_header_string: Option<String>, param_enum_query_string_array: Option<&Vec<String>>, param_enum_query_string: Option<String>, param_enum_query_integer: Option<i32>, param_enum_query_double: Option<f64>, param_enum_form_string: Option<String>, context: &C) -> Box<dyn Future<Item=TestEnumParametersResponse, Error=ApiError> + Send> {
        let mut uri = format!(
            "{}/v2/fake",
            self.base_path
        );

        // Query parameters
        let mut query_string = url::form_urlencoded::Serializer::new("".to_owned());
        if let Some(param_enum_query_string_array) = param_enum_query_string_array {
            query_string.append_pair("enum_query_string_array", &param_enum_query_string_array.join(","));
        }
        if let Some(param_enum_query_string) = param_enum_query_string {
            query_string.append_pair("enum_query_string", &param_enum_query_string.to_string());
        }
        if let Some(param_enum_query_integer) = param_enum_query_integer {
            query_string.append_pair("enum_query_integer", &param_enum_query_integer.to_string());
        }
        if let Some(param_enum_query_double) = param_enum_query_double {
            query_string.append_pair("enum_query_double", &param_enum_query_double.to_string());
        }
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

        let params = &[
            ("enum_form_string", param_enum_form_string),
        ];
        let body = serde_urlencoded::to_string(params).expect("impossible to fail to serialize");

        let header = &mimetypes::requests::TEST_ENUM_PARAMETERS;
        request.headers_mut().insert(CONTENT_TYPE, match HeaderValue::from_str(header) {
            Ok(h) => h,
            Err(e) => return Box::new(future::err(ApiError(format!("Unable to create header: {} - {}", header, e))))
        });
        *request.body_mut() = Body::from(body.into_bytes());

        let header = HeaderValue::from_str((context as &dyn Has<XSpanIdString>).get().0.clone().to_string().as_str());
        request.headers_mut().insert(HeaderName::from_static("x-span-id"), match header {
            Ok(h) => h,
            Err(e) => return Box::new(future::err(ApiError(format!("Unable to create X-Span ID header value: {}", e))))
        });


        // Header parameters
        param_enum_header_string_array.map(|value| request.headers_mut().append(
            HeaderName::from_static("enum_header_string_array"),
            swagger::IntoHeaderValue(value.clone()).into()));
        param_enum_header_string.map(|value| request.headers_mut().append(
            HeaderName::from_static("enum_header_string"),
            swagger::IntoHeaderValue(value.clone()).into()));

        Box::new(self.client_service.request(request)
                             .map_err(|e| ApiError(format!("No response received: {}", e)))
                             .and_then(|mut response| {
            match response.status().as_u16() {
                400 => {
                    let body = response.into_body();
                    Box::new(
                        future::ok(
                            TestEnumParametersResponse::InvalidRequest
                        )
                    ) as Box<dyn Future<Item=_, Error=_> + Send>
                },
                404 => {
                    let body = response.into_body();
                    Box::new(
                        future::ok(
                            TestEnumParametersResponse::NotFound
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

    fn test_inline_additional_properties(&self, param_param: HashMap<String, String>, context: &C) -> Box<dyn Future<Item=TestInlineAdditionalPropertiesResponse, Error=ApiError> + Send> {
        let mut uri = format!(
            "{}/v2/fake/inline-additionalProperties",
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

        let body = serde_json::to_string(&param_param).expect("impossible to fail to serialize");
                *request.body_mut() = Body::from(body);

        let header = &mimetypes::requests::TEST_INLINE_ADDITIONAL_PROPERTIES;
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
                            TestInlineAdditionalPropertiesResponse::SuccessfulOperation
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

    fn test_json_form_data(&self, param_param: String, param_param2: String, context: &C) -> Box<dyn Future<Item=TestJsonFormDataResponse, Error=ApiError> + Send> {
        let mut uri = format!(
            "{}/v2/fake/jsonFormData",
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

        let params = &[
            ("param", Some(param_param)),
            ("param2", Some(param_param2)),
        ];
        let body = serde_urlencoded::to_string(params).expect("impossible to fail to serialize");

        let header = &mimetypes::requests::TEST_JSON_FORM_DATA;
        request.headers_mut().insert(CONTENT_TYPE, match HeaderValue::from_str(header) {
            Ok(h) => h,
            Err(e) => return Box::new(future::err(ApiError(format!("Unable to create header: {} - {}", header, e))))
        });
        *request.body_mut() = Body::from(body.into_bytes());

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
                            TestJsonFormDataResponse::SuccessfulOperation
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

    fn test_classname(&self, param_body: models::Client, context: &C) -> Box<dyn Future<Item=TestClassnameResponse, Error=ApiError> + Send> {
        let mut uri = format!(
            "{}/v2/fake_classname_test",
            self.base_path
        );

        // Query parameters
        let mut query_string = url::form_urlencoded::Serializer::new("".to_owned());
        if let Some(auth_data) = (context as &dyn Has<Option<AuthData>>).get().as_ref() {
            if let AuthData::ApiKey(ref api_key) = *auth_data {
                query_string.append_pair("api_key_query", api_key);
            }
        }
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
            .method("PATCH")
            .uri(uri)
            .body(Body::empty()) {
                Ok(req) => req,
                Err(e) => return Box::new(future::err(ApiError(format!("Unable to create request: {}", e))))
        };

        // Body parameter
        let body = serde_json::to_string(&param_body).expect("impossible to fail to serialize");
                *request.body_mut() = Body::from(body);

        let header = &mimetypes::requests::TEST_CLASSNAME;
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
                                serde_json::from_str::<models::Client>(body)
                                .map_err(|e| e.into())
                            )
                        )
                        .map(move |body| {
                            TestClassnameResponse::SuccessfulOperation
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

    fn add_pet(&self, param_body: models::Pet, context: &C) -> Box<dyn Future<Item=AddPetResponse, Error=ApiError> + Send> {
        let mut uri = format!(
            "{}/v2/pet",
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

        // Body parameter
        let body = param_body.to_xml();
                *request.body_mut() = Body::from(body);

        let header = &mimetypes::requests::ADD_PET;
        request.headers_mut().insert(CONTENT_TYPE, match HeaderValue::from_str(header) {
            Ok(h) => h,
            Err(e) => return Box::new(future::err(ApiError(format!("Unable to create header: {} - {}", header, e))))
        });

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
                405 => {
                    let body = response.into_body();
                    Box::new(
                        future::ok(
                            AddPetResponse::InvalidInput
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

    fn delete_pet(&self, param_pet_id: i64, param_api_key: Option<String>, context: &C) -> Box<dyn Future<Item=DeletePetResponse, Error=ApiError> + Send> {
        let mut uri = format!(
            "{}/v2/pet/{petId}",
            self.base_path, petId=utf8_percent_encode(&param_pet_id.to_string(), ID_ENCODE_SET)
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
            .method("DELETE")
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

        // Header parameters
        param_api_key.map(|value| request.headers_mut().append(
            HeaderName::from_static("api_key"),
            swagger::IntoHeaderValue(value.clone()).into()));

        Box::new(self.client_service.request(request)
                             .map_err(|e| ApiError(format!("No response received: {}", e)))
                             .and_then(|mut response| {
            match response.status().as_u16() {
                400 => {
                    let body = response.into_body();
                    Box::new(
                        future::ok(
                            DeletePetResponse::InvalidPetValue
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

    fn find_pets_by_status(&self, param_status: &Vec<String>, context: &C) -> Box<dyn Future<Item=FindPetsByStatusResponse, Error=ApiError> + Send> {
        let mut uri = format!(
            "{}/v2/pet/findByStatus",
            self.base_path
        );

        // Query parameters
        let mut query_string = url::form_urlencoded::Serializer::new("".to_owned());
            query_string.append_pair("status", &param_status.join(","));
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
                        body
                        .concat2()
                        .map_err(|e| ApiError(format!("Failed to read response: {}", e)))
                        .and_then(|body|
                            str::from_utf8(&body)
                            .map_err(|e| ApiError(format!("Response was not valid UTF8: {}", e)))
                            .and_then(|body|
                                // ToDo: this will move to swagger-rs and become a standard From conversion trait
                                // once https://github.com/RReverser/serde-xml-rs/pull/45 is accepted upstream
                                serde_xml_rs::from_str::<Vec<models::Pet>>(body)
                                .map_err(|e| ApiError(format!("Response body did not match the schema: {}", e)))
                            )
                        )
                        .map(move |body| {
                            FindPetsByStatusResponse::SuccessfulOperation
                            (body)
                        })
                    ) as Box<dyn Future<Item=_, Error=_> + Send>
                },
                400 => {
                    let body = response.into_body();
                    Box::new(
                        future::ok(
                            FindPetsByStatusResponse::InvalidStatusValue
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

    fn find_pets_by_tags(&self, param_tags: &Vec<String>, context: &C) -> Box<dyn Future<Item=FindPetsByTagsResponse, Error=ApiError> + Send> {
        let mut uri = format!(
            "{}/v2/pet/findByTags",
            self.base_path
        );

        // Query parameters
        let mut query_string = url::form_urlencoded::Serializer::new("".to_owned());
            query_string.append_pair("tags", &param_tags.join(","));
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
                        body
                        .concat2()
                        .map_err(|e| ApiError(format!("Failed to read response: {}", e)))
                        .and_then(|body|
                            str::from_utf8(&body)
                            .map_err(|e| ApiError(format!("Response was not valid UTF8: {}", e)))
                            .and_then(|body|
                                // ToDo: this will move to swagger-rs and become a standard From conversion trait
                                // once https://github.com/RReverser/serde-xml-rs/pull/45 is accepted upstream
                                serde_xml_rs::from_str::<Vec<models::Pet>>(body)
                                .map_err(|e| ApiError(format!("Response body did not match the schema: {}", e)))
                            )
                        )
                        .map(move |body| {
                            FindPetsByTagsResponse::SuccessfulOperation
                            (body)
                        })
                    ) as Box<dyn Future<Item=_, Error=_> + Send>
                },
                400 => {
                    let body = response.into_body();
                    Box::new(
                        future::ok(
                            FindPetsByTagsResponse::InvalidTagValue
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

    fn get_pet_by_id(&self, param_pet_id: i64, context: &C) -> Box<dyn Future<Item=GetPetByIdResponse, Error=ApiError> + Send> {
        let mut uri = format!(
            "{}/v2/pet/{petId}",
            self.base_path, petId=utf8_percent_encode(&param_pet_id.to_string(), ID_ENCODE_SET)
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
                &AuthData::ApiKey(ref api_key) => {
                    let header = match HeaderValue::from_str(&format!("{}", api_key)) {
                        Ok(h) => h,
                        Err(e) => return Box::new(future::err(ApiError(format!("Unable to create ApiKey header: {}", e))))
                    };
                    request.headers_mut().insert(
                        "ApiKey",
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
                        body
                        .concat2()
                        .map_err(|e| ApiError(format!("Failed to read response: {}", e)))
                        .and_then(|body|
                            str::from_utf8(&body)
                            .map_err(|e| ApiError(format!("Response was not valid UTF8: {}", e)))
                            .and_then(|body|
                                // ToDo: this will move to swagger-rs and become a standard From conversion trait
                                // once https://github.com/RReverser/serde-xml-rs/pull/45 is accepted upstream
                                serde_xml_rs::from_str::<models::Pet>(body)
                                .map_err(|e| ApiError(format!("Response body did not match the schema: {}", e)))
                            )
                        )
                        .map(move |body| {
                            GetPetByIdResponse::SuccessfulOperation
                            (body)
                        })
                    ) as Box<dyn Future<Item=_, Error=_> + Send>
                },
                400 => {
                    let body = response.into_body();
                    Box::new(
                        future::ok(
                            GetPetByIdResponse::InvalidIDSupplied
                        )
                    ) as Box<dyn Future<Item=_, Error=_> + Send>
                },
                404 => {
                    let body = response.into_body();
                    Box::new(
                        future::ok(
                            GetPetByIdResponse::PetNotFound
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

    fn update_pet(&self, param_body: models::Pet, context: &C) -> Box<dyn Future<Item=UpdatePetResponse, Error=ApiError> + Send> {
        let mut uri = format!(
            "{}/v2/pet",
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

        let body = param_body.to_xml();
                *request.body_mut() = Body::from(body);

        let header = &mimetypes::requests::UPDATE_PET;
        request.headers_mut().insert(CONTENT_TYPE, match HeaderValue::from_str(header) {
            Ok(h) => h,
            Err(e) => return Box::new(future::err(ApiError(format!("Unable to create header: {} - {}", header, e))))
        });

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
                400 => {
                    let body = response.into_body();
                    Box::new(
                        future::ok(
                            UpdatePetResponse::InvalidIDSupplied
                        )
                    ) as Box<dyn Future<Item=_, Error=_> + Send>
                },
                404 => {
                    let body = response.into_body();
                    Box::new(
                        future::ok(
                            UpdatePetResponse::PetNotFound
                        )
                    ) as Box<dyn Future<Item=_, Error=_> + Send>
                },
                405 => {
                    let body = response.into_body();
                    Box::new(
                        future::ok(
                            UpdatePetResponse::ValidationException
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

    fn update_pet_with_form(&self, param_pet_id: i64, param_name: Option<String>, param_status: Option<String>, context: &C) -> Box<dyn Future<Item=UpdatePetWithFormResponse, Error=ApiError> + Send> {
        let mut uri = format!(
            "{}/v2/pet/{petId}",
            self.base_path, petId=utf8_percent_encode(&param_pet_id.to_string(), ID_ENCODE_SET)
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

        let params = &[
            ("name", param_name),
            ("status", param_status),
        ];
        let body = serde_urlencoded::to_string(params).expect("impossible to fail to serialize");

        let header = &mimetypes::requests::UPDATE_PET_WITH_FORM;
        request.headers_mut().insert(CONTENT_TYPE, match HeaderValue::from_str(header) {
            Ok(h) => h,
            Err(e) => return Box::new(future::err(ApiError(format!("Unable to create header: {} - {}", header, e))))
        });
        *request.body_mut() = Body::from(body.into_bytes());

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
                405 => {
                    let body = response.into_body();
                    Box::new(
                        future::ok(
                            UpdatePetWithFormResponse::InvalidInput
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

    fn upload_file(&self, param_pet_id: i64, param_additional_metadata: Option<String>, param_file: Option<swagger::ByteArray>, context: &C) -> Box<dyn Future<Item=UploadFileResponse, Error=ApiError> + Send> {
        let mut uri = format!(
            "{}/v2/pet/{petId}/uploadImage",
            self.base_path, petId=utf8_percent_encode(&param_pet_id.to_string(), ID_ENCODE_SET)
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

        let additional_metadata_str = match serde_json::to_string(&param_additional_metadata) {
            Ok(str) => str,
            Err(e) => return Box::new(future::err(ApiError(format!("Unable to parse additional_metadata to string: {}", e)))),
        };

        let additional_metadata_vec = additional_metadata_str.as_bytes().to_vec();

        let additional_metadata_mime = mime_0_2::Mime::from_str("application/json").expect("impossible to fail to parse");

        let additional_metadata_cursor = Cursor::new(additional_metadata_vec);

        multipart.add_stream("additional_metadata",  additional_metadata_cursor,  None as Option<&str>, Some(additional_metadata_mime));  

        let file_str = match serde_json::to_string(&param_file) {
            Ok(str) => str,
            Err(e) => return Box::new(future::err(ApiError(format!("Unable to parse file to string: {}", e)))),
        };

        let file_vec = file_str.as_bytes().to_vec();

        let file_mime = mime_0_2::Mime::from_str("application/json").expect("impossible to fail to parse");

        let file_cursor = Cursor::new(file_vec);

        multipart.add_stream("file",  file_cursor,  None as Option<&str>, Some(file_mime));  

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
                        body
                        .concat2()
                        .map_err(|e| ApiError(format!("Failed to read response: {}", e)))
                        .and_then(|body|
                            str::from_utf8(&body)
                            .map_err(|e| ApiError(format!("Response was not valid UTF8: {}", e)))
                            .and_then(|body|
                                serde_json::from_str::<models::ApiResponse>(body)
                                .map_err(|e| e.into())
                            )
                        )
                        .map(move |body| {
                            UploadFileResponse::SuccessfulOperation
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

    fn delete_order(&self, param_order_id: String, context: &C) -> Box<dyn Future<Item=DeleteOrderResponse, Error=ApiError> + Send> {
        let mut uri = format!(
            "{}/v2/store/order/{order_id}",
            self.base_path, order_id=utf8_percent_encode(&param_order_id.to_string(), ID_ENCODE_SET)
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
            .method("DELETE")
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
                400 => {
                    let body = response.into_body();
                    Box::new(
                        future::ok(
                            DeleteOrderResponse::InvalidIDSupplied
                        )
                    ) as Box<dyn Future<Item=_, Error=_> + Send>
                },
                404 => {
                    let body = response.into_body();
                    Box::new(
                        future::ok(
                            DeleteOrderResponse::OrderNotFound
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

    fn get_inventory(&self, context: &C) -> Box<dyn Future<Item=GetInventoryResponse, Error=ApiError> + Send> {
        let mut uri = format!(
            "{}/v2/store/inventory",
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
                &AuthData::ApiKey(ref api_key) => {
                    let header = match HeaderValue::from_str(&format!("{}", api_key)) {
                        Ok(h) => h,
                        Err(e) => return Box::new(future::err(ApiError(format!("Unable to create ApiKey header: {}", e))))
                    };
                    request.headers_mut().insert(
                        "ApiKey",
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
                        body
                        .concat2()
                        .map_err(|e| ApiError(format!("Failed to read response: {}", e)))
                        .and_then(|body|
                            str::from_utf8(&body)
                            .map_err(|e| ApiError(format!("Response was not valid UTF8: {}", e)))
                            .and_then(|body|
                                serde_json::from_str::<HashMap<String, i32>>(body)
                                .map_err(|e| e.into())
                            )
                        )
                        .map(move |body| {
                            GetInventoryResponse::SuccessfulOperation
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

    fn get_order_by_id(&self, param_order_id: i64, context: &C) -> Box<dyn Future<Item=GetOrderByIdResponse, Error=ApiError> + Send> {
        let mut uri = format!(
            "{}/v2/store/order/{order_id}",
            self.base_path, order_id=utf8_percent_encode(&param_order_id.to_string(), ID_ENCODE_SET)
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
                                // ToDo: this will move to swagger-rs and become a standard From conversion trait
                                // once https://github.com/RReverser/serde-xml-rs/pull/45 is accepted upstream
                                serde_xml_rs::from_str::<models::Order>(body)
                                .map_err(|e| ApiError(format!("Response body did not match the schema: {}", e)))
                            )
                        )
                        .map(move |body| {
                            GetOrderByIdResponse::SuccessfulOperation
                            (body)
                        })
                    ) as Box<dyn Future<Item=_, Error=_> + Send>
                },
                400 => {
                    let body = response.into_body();
                    Box::new(
                        future::ok(
                            GetOrderByIdResponse::InvalidIDSupplied
                        )
                    ) as Box<dyn Future<Item=_, Error=_> + Send>
                },
                404 => {
                    let body = response.into_body();
                    Box::new(
                        future::ok(
                            GetOrderByIdResponse::OrderNotFound
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

    fn place_order(&self, param_body: models::Order, context: &C) -> Box<dyn Future<Item=PlaceOrderResponse, Error=ApiError> + Send> {
        let mut uri = format!(
            "{}/v2/store/order",
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

        let body = serde_json::to_string(&param_body).expect("impossible to fail to serialize");
                *request.body_mut() = Body::from(body);

        let header = &mimetypes::requests::PLACE_ORDER;
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
                                // ToDo: this will move to swagger-rs and become a standard From conversion trait
                                // once https://github.com/RReverser/serde-xml-rs/pull/45 is accepted upstream
                                serde_xml_rs::from_str::<models::Order>(body)
                                .map_err(|e| ApiError(format!("Response body did not match the schema: {}", e)))
                            )
                        )
                        .map(move |body| {
                            PlaceOrderResponse::SuccessfulOperation
                            (body)
                        })
                    ) as Box<dyn Future<Item=_, Error=_> + Send>
                },
                400 => {
                    let body = response.into_body();
                    Box::new(
                        future::ok(
                            PlaceOrderResponse::InvalidOrder
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

    fn create_user(&self, param_body: models::User, context: &C) -> Box<dyn Future<Item=CreateUserResponse, Error=ApiError> + Send> {
        let mut uri = format!(
            "{}/v2/user",
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

        // Body parameter
        let body = serde_json::to_string(&param_body).expect("impossible to fail to serialize");
                *request.body_mut() = Body::from(body);

        let header = &mimetypes::requests::CREATE_USER;
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
                0 => {
                    let body = response.into_body();
                    Box::new(
                        future::ok(
                            CreateUserResponse::SuccessfulOperation
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

    fn create_users_with_array_input(&self, param_body: &Vec<models::User>, context: &C) -> Box<dyn Future<Item=CreateUsersWithArrayInputResponse, Error=ApiError> + Send> {
        let mut uri = format!(
            "{}/v2/user/createWithArray",
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

        let body = serde_json::to_string(&param_body).expect("impossible to fail to serialize");
                *request.body_mut() = Body::from(body);

        let header = &mimetypes::requests::CREATE_USERS_WITH_ARRAY_INPUT;
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
                0 => {
                    let body = response.into_body();
                    Box::new(
                        future::ok(
                            CreateUsersWithArrayInputResponse::SuccessfulOperation
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

    fn create_users_with_list_input(&self, param_body: &Vec<models::User>, context: &C) -> Box<dyn Future<Item=CreateUsersWithListInputResponse, Error=ApiError> + Send> {
        let mut uri = format!(
            "{}/v2/user/createWithList",
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

        let body = serde_json::to_string(&param_body).expect("impossible to fail to serialize");
                *request.body_mut() = Body::from(body);

        let header = &mimetypes::requests::CREATE_USERS_WITH_LIST_INPUT;
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
                0 => {
                    let body = response.into_body();
                    Box::new(
                        future::ok(
                            CreateUsersWithListInputResponse::SuccessfulOperation
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

    fn delete_user(&self, param_username: String, context: &C) -> Box<dyn Future<Item=DeleteUserResponse, Error=ApiError> + Send> {
        let mut uri = format!(
            "{}/v2/user/{username}",
            self.base_path, username=utf8_percent_encode(&param_username.to_string(), ID_ENCODE_SET)
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
            .method("DELETE")
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
                400 => {
                    let body = response.into_body();
                    Box::new(
                        future::ok(
                            DeleteUserResponse::InvalidUsernameSupplied
                        )
                    ) as Box<dyn Future<Item=_, Error=_> + Send>
                },
                404 => {
                    let body = response.into_body();
                    Box::new(
                        future::ok(
                            DeleteUserResponse::UserNotFound
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

    fn get_user_by_name(&self, param_username: String, context: &C) -> Box<dyn Future<Item=GetUserByNameResponse, Error=ApiError> + Send> {
        let mut uri = format!(
            "{}/v2/user/{username}",
            self.base_path, username=utf8_percent_encode(&param_username.to_string(), ID_ENCODE_SET)
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
                                // ToDo: this will move to swagger-rs and become a standard From conversion trait
                                // once https://github.com/RReverser/serde-xml-rs/pull/45 is accepted upstream
                                serde_xml_rs::from_str::<models::User>(body)
                                .map_err(|e| ApiError(format!("Response body did not match the schema: {}", e)))
                            )
                        )
                        .map(move |body| {
                            GetUserByNameResponse::SuccessfulOperation
                            (body)
                        })
                    ) as Box<dyn Future<Item=_, Error=_> + Send>
                },
                400 => {
                    let body = response.into_body();
                    Box::new(
                        future::ok(
                            GetUserByNameResponse::InvalidUsernameSupplied
                        )
                    ) as Box<dyn Future<Item=_, Error=_> + Send>
                },
                404 => {
                    let body = response.into_body();
                    Box::new(
                        future::ok(
                            GetUserByNameResponse::UserNotFound
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

    fn login_user(&self, param_username: String, param_password: String, context: &C) -> Box<dyn Future<Item=LoginUserResponse, Error=ApiError> + Send> {
        let mut uri = format!(
            "{}/v2/user/login",
            self.base_path
        );

        // Query parameters
        let mut query_string = url::form_urlencoded::Serializer::new("".to_owned());
            query_string.append_pair("username", &param_username.to_string());
            query_string.append_pair("password", &param_password.to_string());
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
                    let response_x_rate_limit = match response.headers().get(HeaderName::from_static("x-rate-limit")) {
                        Some(response_x_rate_limit) => response_x_rate_limit.clone(),
                        None => return Box::new(future::err(ApiError(String::from("Required response header X-Rate-Limit for response 200 was not found.")))) as Box<dyn Future<Item=_, Error=_> + Send>,
                    };
                    let response_x_expires_after = match response.headers().get(HeaderName::from_static("x-expires-after")) {
                        Some(response_x_expires_after) => response_x_expires_after.clone(),
                        None => return Box::new(future::err(ApiError(String::from("Required response header X-Expires-After for response 200 was not found.")))) as Box<dyn Future<Item=_, Error=_> + Send>,
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
                                // ToDo: this will move to swagger-rs and become a standard From conversion trait
                                // once https://github.com/RReverser/serde-xml-rs/pull/45 is accepted upstream
                                serde_xml_rs::from_str::<String>(body)
                                .map_err(|e| ApiError(format!("Response body did not match the schema: {}", e)))
                            )
                        )
                        .map(move |body| {
                            LoginUserResponse::SuccessfulOperation
                            {
                               body: body,
                               x_rate_limit: (*Into::<swagger::IntoHeaderValue<i32>>::into(response_x_rate_limit)).clone(),
                               x_expires_after: (*Into::<swagger::IntoHeaderValue<chrono::DateTime<chrono::Utc>>>::into(response_x_expires_after)).clone(),
                            }
                        })
                    ) as Box<dyn Future<Item=_, Error=_> + Send>
                },
                400 => {
                    let body = response.into_body();
                    Box::new(
                        future::ok(
                            LoginUserResponse::InvalidUsername
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

    fn logout_user(&self, context: &C) -> Box<dyn Future<Item=LogoutUserResponse, Error=ApiError> + Send> {
        let mut uri = format!(
            "{}/v2/user/logout",
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
                0 => {
                    let body = response.into_body();
                    Box::new(
                        future::ok(
                            LogoutUserResponse::SuccessfulOperation
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

    fn update_user(&self, param_username: String, param_body: models::User, context: &C) -> Box<dyn Future<Item=UpdateUserResponse, Error=ApiError> + Send> {
        let mut uri = format!(
            "{}/v2/user/{username}",
            self.base_path, username=utf8_percent_encode(&param_username.to_string(), ID_ENCODE_SET)
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

        let body = serde_json::to_string(&param_body).expect("impossible to fail to serialize");
                *request.body_mut() = Body::from(body);

        let header = &mimetypes::requests::UPDATE_USER;
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
                400 => {
                    let body = response.into_body();
                    Box::new(
                        future::ok(
                            UpdateUserResponse::InvalidUserSupplied
                        )
                    ) as Box<dyn Future<Item=_, Error=_> + Send>
                },
                404 => {
                    let body = response.into_body();
                    Box::new(
                        future::ok(
                            UpdateUserResponse::UserNotFound
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
