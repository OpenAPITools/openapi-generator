#![allow(unused_extern_crates)]
extern crate tokio_core;
extern crate native_tls;
extern crate hyper_tls;
extern crate openssl;
extern crate mime;
extern crate chrono;
extern crate url;
extern crate multipart;
extern crate serde_urlencoded;


use self::multipart::client::lazy::Multipart;
use hyper;
use hyper::header::{Headers, ContentType};
use hyper::Uri;
use self::url::percent_encoding::{utf8_percent_encode, PATH_SEGMENT_ENCODE_SET, QUERY_ENCODE_SET};
use futures;
use futures::{Future, Stream};
use futures::{future, stream};
use self::tokio_core::reactor::Handle;
use std::borrow::Cow;
use std::io::{Read, Error, ErrorKind};
use std::error;
use std::fmt;
use std::path::Path;
use std::sync::Arc;
use std::str;
use std::str::FromStr;

use mimetypes;

use serde_json;
use serde_xml_rs;

#[allow(unused_imports)]
use std::collections::{HashMap, BTreeMap};
#[allow(unused_imports)]
use swagger;

use swagger::{Context, ApiError, XSpanId};

use {Api,
     TestSpecialTagsResponse,
     TestBodyWithQueryParamsResponse,
     FakeOuterBooleanSerializeResponse,
     FakeOuterCompositeSerializeResponse,
     FakeOuterNumberSerializeResponse,
     FakeOuterStringSerializeResponse,
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
use models;

/// Convert input into a base path, e.g. "http://example:123". Also checks the scheme as it goes.
fn into_base_path(input: &str, correct_scheme: Option<&'static str>) -> Result<String, ClientInitError> {
    // First convert to Uri, since a base path is a subset of Uri.
    let uri = Uri::from_str(input)?;

    let scheme = uri.scheme().ok_or(ClientInitError::InvalidScheme)?;

    // Check the scheme if necessary
    if let Some(correct_scheme) = correct_scheme {
        if scheme != correct_scheme {
            return Err(ClientInitError::InvalidScheme);
        }
    }

    let host = uri.host().ok_or_else(|| ClientInitError::MissingHost)?;
    let port = uri.port().map(|x| format!(":{}", x)).unwrap_or_default();
    Ok(format!("{}://{}{}", scheme, host, port))
}

/// A client that implements the API by making HTTP calls out to a server.
#[derive(Clone)]
pub struct Client {
    hyper_client: Arc<Fn(&Handle) -> Box<hyper::client::Service<Request=hyper::Request<hyper::Body>, Response=hyper::Response, Error=hyper::Error, Future=hyper::client::FutureResponse>> + Sync + Send>,
    handle: Arc<Handle>,
    base_path: String,
}

impl fmt::Debug for Client {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Client {{ base_path: {} }}", self.base_path)
    }
}

impl Client {

    /// Create an HTTP client.
    ///
    /// # Arguments
    /// * `handle` - tokio reactor handle to use for execution
    /// * `base_path` - base path of the client API, i.e. "www.my-api-implementation.com"
    pub fn try_new_http(handle: Handle, base_path: &str) -> Result<Client, ClientInitError> {
        let http_connector = swagger::http_connector();
        Self::try_new_with_connector::<hyper::client::HttpConnector>(
            handle,
            base_path,
            Some("http"),
            http_connector,
        )
    }

    /// Create a client with a TLS connection to the server.
    ///
    /// # Arguments
    /// * `handle` - tokio reactor handle to use for execution
    /// * `base_path` - base path of the client API, i.e. "www.my-api-implementation.com"
    /// * `ca_certificate` - Path to CA certificate used to authenticate the server
    pub fn try_new_https<CA>(
        handle: Handle,
        base_path: &str,
        ca_certificate: CA,
    ) -> Result<Client, ClientInitError>
    where
        CA: AsRef<Path>,
    {
        let https_connector = swagger::https_connector(ca_certificate);
        Self::try_new_with_connector::<hyper_tls::HttpsConnector<hyper::client::HttpConnector>>(
            handle,
            base_path,
            Some("https"),
            https_connector,
        )
    }

    /// Create a client with a mutually authenticated TLS connection to the server.
    ///
    /// # Arguments
    /// * `handle` - tokio reactor handle to use for execution
    /// * `base_path` - base path of the client API, i.e. "www.my-api-implementation.com"
    /// * `ca_certificate` - Path to CA certificate used to authenticate the server
    /// * `client_key` - Path to the client private key
    /// * `client_certificate` - Path to the client's public certificate associated with the private key
    pub fn try_new_https_mutual<CA, K, C, T>(
        handle: Handle,
        base_path: &str,
        ca_certificate: CA,
        client_key: K,
        client_certificate: C,
    ) -> Result<Client, ClientInitError>
    where
        CA: AsRef<Path>,
        K: AsRef<Path>,
        C: AsRef<Path>,
    {
        let https_connector =
            swagger::https_mutual_connector(ca_certificate, client_key, client_certificate);
        Self::try_new_with_connector::<hyper_tls::HttpsConnector<hyper::client::HttpConnector>>(
            handle,
            base_path,
            Some("https"),
            https_connector,
        )
    }

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
    /// * `handle` - tokio reactor handle to use for execution
    /// * `base_path` - base path of the client API, i.e. "www.my-api-implementation.com"
    /// * `protocol` - Which protocol to use when constructing the request url, e.g. `Some("http")`
    /// * `connector_fn` - Function which returns an implementation of `hyper::client::Connect`
    pub fn try_new_with_connector<C>(
        handle: Handle,
        base_path: &str,
        protocol: Option<&'static str>,
        connector_fn: Box<Fn(&Handle) -> C + Send + Sync>,
    ) -> Result<Client, ClientInitError>
    where
        C: hyper::client::Connect + hyper::client::Service,
    {
        let hyper_client = {
            move |handle: &Handle| -> Box<
                hyper::client::Service<
                    Request = hyper::Request<hyper::Body>,
                    Response = hyper::Response,
                    Error = hyper::Error,
                    Future = hyper::client::FutureResponse,
                >,
            > {
                let connector = connector_fn(handle);
                Box::new(hyper::Client::configure().connector(connector).build(
                    handle,
                ))
            }
        };

        Ok(Client {
            hyper_client: Arc::new(hyper_client),
            handle: Arc::new(handle),
            base_path: into_base_path(base_path, protocol)?,
        })
    }

    /// Constructor for creating a `Client` by passing in a pre-made `hyper` client.
    ///
    /// One should avoid relying on this function if possible, since it adds a dependency on the underlying transport
    /// implementation, which it would be better to abstract away. Therefore, using this function may lead to a loss of
    /// code generality, which may make it harder to move the application to a serverless environment, for example.
    ///
    /// The reason for this function's existence is to support legacy test code, which did mocking at the hyper layer.
    /// This is not a recommended way to write new tests. If other reasons are found for using this function, they
    /// should be mentioned here.
    pub fn try_new_with_hyper_client(hyper_client: Arc<Fn(&Handle) -> Box<hyper::client::Service<Request=hyper::Request<hyper::Body>, Response=hyper::Response, Error=hyper::Error, Future=hyper::client::FutureResponse>> + Sync + Send>,
                                     handle: Handle,
                                     base_path: &str)
                                    -> Result<Client, ClientInitError>
    {
        Ok(Client {
            hyper_client: hyper_client,
            handle: Arc::new(handle),
            base_path: into_base_path(base_path, None)?,
        })
    }
}

impl Api for Client {

    fn test_special_tags(&self, param_body: models::Client, context: &Context) -> Box<Future<Item=TestSpecialTagsResponse, Error=ApiError>> {


        let uri = format!(
            "{}/v2/another-fake/dummy",
            self.base_path
        );

        let uri = match Uri::from_str(&uri) {
            Ok(uri) => uri,
            Err(err) => return Box::new(futures::done(Err(ApiError(format!("Unable to build URI: {}", err))))),
        };

        let mut request = hyper::Request::new(hyper::Method::Patch, uri);


        let body = serde_json::to_string(&param_body).expect("impossible to fail to serialize");


        request.set_body(body.into_bytes());


        request.headers_mut().set(ContentType(mimetypes::requests::TEST_SPECIAL_TAGS.clone()));
        context.x_span_id.as_ref().map(|header| request.headers_mut().set(XSpanId(header.clone())));




        let hyper_client = (self.hyper_client)(&*self.handle);
        Box::new(hyper_client.call(request)
                             .map_err(|e| ApiError(format!("No response received: {}", e)))
                             .and_then(|mut response| {
            match response.status().as_u16() {
                200 => {
                    let body = response.body();
                    Box::new(

                        body
                        .concat2()
                        .map_err(|e| ApiError(format!("Failed to read response: {}", e)))
                        .and_then(|body| str::from_utf8(&body)
                                             .map_err(|e| ApiError(format!("Response was not valid UTF8: {}", e)))
                                             .and_then(|body|

                                                 serde_json::from_str::<models::Client>(body)
                                                     .map_err(|e| e.into())

                                             ))
                        .map(move |body|
                            TestSpecialTagsResponse::SuccessfulOperation(body)
                        )
                    ) as Box<Future<Item=_, Error=_>>
                },
                code => {
                    let headers = response.headers().clone();
                    Box::new(response.body()
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
                    ) as Box<Future<Item=_, Error=_>>
                }
            }
        }))

    }

    fn test_body_with_query_params(&self, param_body: models::User, param_query: String, context: &Context) -> Box<Future<Item=TestBodyWithQueryParamsResponse, Error=ApiError>> {

        // Query parameters
        let query_query = format!("query={query}&", query=param_query.to_string());


        let uri = format!(
            "{}/v2/fake/body-with-query-params?{query}",
            self.base_path,
            query=utf8_percent_encode(&query_query, QUERY_ENCODE_SET)
        );

        let uri = match Uri::from_str(&uri) {
            Ok(uri) => uri,
            Err(err) => return Box::new(futures::done(Err(ApiError(format!("Unable to build URI: {}", err))))),
        };

        let mut request = hyper::Request::new(hyper::Method::Put, uri);


        let body = serde_json::to_string(&param_body).expect("impossible to fail to serialize");


        request.set_body(body.into_bytes());


        request.headers_mut().set(ContentType(mimetypes::requests::TEST_BODY_WITH_QUERY_PARAMS.clone()));
        context.x_span_id.as_ref().map(|header| request.headers_mut().set(XSpanId(header.clone())));




        let hyper_client = (self.hyper_client)(&*self.handle);
        Box::new(hyper_client.call(request)
                             .map_err(|e| ApiError(format!("No response received: {}", e)))
                             .and_then(|mut response| {
            match response.status().as_u16() {
                200 => {
                    let body = response.body();
                    Box::new(

                        future::ok(
                            TestBodyWithQueryParamsResponse::Success
                        )
                    ) as Box<Future<Item=_, Error=_>>
                },
                code => {
                    let headers = response.headers().clone();
                    Box::new(response.body()
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
                    ) as Box<Future<Item=_, Error=_>>
                }
            }
        }))

    }

    fn fake_outer_boolean_serialize(&self, param_body: Option<models::OuterBoolean>, context: &Context) -> Box<Future<Item=FakeOuterBooleanSerializeResponse, Error=ApiError>> {


        let uri = format!(
            "{}/v2/fake/outer/boolean",
            self.base_path
        );

        let uri = match Uri::from_str(&uri) {
            Ok(uri) => uri,
            Err(err) => return Box::new(futures::done(Err(ApiError(format!("Unable to build URI: {}", err))))),
        };

        let mut request = hyper::Request::new(hyper::Method::Post, uri);

        let body = param_body.map(|ref body| {

            serde_json::to_string(body).expect("impossible to fail to serialize")
        });

if let Some(body) = body {
            request.set_body(body.into_bytes());
        }

        request.headers_mut().set(ContentType(mimetypes::requests::FAKE_OUTER_BOOLEAN_SERIALIZE.clone()));
        context.x_span_id.as_ref().map(|header| request.headers_mut().set(XSpanId(header.clone())));




        let hyper_client = (self.hyper_client)(&*self.handle);
        Box::new(hyper_client.call(request)
                             .map_err(|e| ApiError(format!("No response received: {}", e)))
                             .and_then(|mut response| {
            match response.status().as_u16() {
                200 => {
                    let body = response.body();
                    Box::new(

                        body
                        .concat2()
                        .map_err(|e| ApiError(format!("Failed to read response: {}", e)))
                        .and_then(|body| str::from_utf8(&body)
                                             .map_err(|e| ApiError(format!("Response was not valid UTF8: {}", e)))
                                             .and_then(|body|

                                                 serde_json::from_str::<models::OuterBoolean>(body)
                                                     .map_err(|e| e.into())

                                             ))
                        .map(move |body|
                            FakeOuterBooleanSerializeResponse::OutputBoolean(body)
                        )
                    ) as Box<Future<Item=_, Error=_>>
                },
                code => {
                    let headers = response.headers().clone();
                    Box::new(response.body()
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
                    ) as Box<Future<Item=_, Error=_>>
                }
            }
        }))

    }

    fn fake_outer_composite_serialize(&self, param_body: Option<models::OuterComposite>, context: &Context) -> Box<Future<Item=FakeOuterCompositeSerializeResponse, Error=ApiError>> {


        let uri = format!(
            "{}/v2/fake/outer/composite",
            self.base_path
        );

        let uri = match Uri::from_str(&uri) {
            Ok(uri) => uri,
            Err(err) => return Box::new(futures::done(Err(ApiError(format!("Unable to build URI: {}", err))))),
        };

        let mut request = hyper::Request::new(hyper::Method::Post, uri);

        let body = param_body.map(|ref body| {

            serde_json::to_string(body).expect("impossible to fail to serialize")
        });

if let Some(body) = body {
            request.set_body(body.into_bytes());
        }

        request.headers_mut().set(ContentType(mimetypes::requests::FAKE_OUTER_COMPOSITE_SERIALIZE.clone()));
        context.x_span_id.as_ref().map(|header| request.headers_mut().set(XSpanId(header.clone())));




        let hyper_client = (self.hyper_client)(&*self.handle);
        Box::new(hyper_client.call(request)
                             .map_err(|e| ApiError(format!("No response received: {}", e)))
                             .and_then(|mut response| {
            match response.status().as_u16() {
                200 => {
                    let body = response.body();
                    Box::new(

                        body
                        .concat2()
                        .map_err(|e| ApiError(format!("Failed to read response: {}", e)))
                        .and_then(|body| str::from_utf8(&body)
                                             .map_err(|e| ApiError(format!("Response was not valid UTF8: {}", e)))
                                             .and_then(|body|

                                                 serde_json::from_str::<models::OuterComposite>(body)
                                                     .map_err(|e| e.into())

                                             ))
                        .map(move |body|
                            FakeOuterCompositeSerializeResponse::OutputComposite(body)
                        )
                    ) as Box<Future<Item=_, Error=_>>
                },
                code => {
                    let headers = response.headers().clone();
                    Box::new(response.body()
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
                    ) as Box<Future<Item=_, Error=_>>
                }
            }
        }))

    }

    fn fake_outer_number_serialize(&self, param_body: Option<models::OuterNumber>, context: &Context) -> Box<Future<Item=FakeOuterNumberSerializeResponse, Error=ApiError>> {


        let uri = format!(
            "{}/v2/fake/outer/number",
            self.base_path
        );

        let uri = match Uri::from_str(&uri) {
            Ok(uri) => uri,
            Err(err) => return Box::new(futures::done(Err(ApiError(format!("Unable to build URI: {}", err))))),
        };

        let mut request = hyper::Request::new(hyper::Method::Post, uri);

        let body = param_body.map(|ref body| {

            serde_json::to_string(body).expect("impossible to fail to serialize")
        });

if let Some(body) = body {
            request.set_body(body.into_bytes());
        }

        request.headers_mut().set(ContentType(mimetypes::requests::FAKE_OUTER_NUMBER_SERIALIZE.clone()));
        context.x_span_id.as_ref().map(|header| request.headers_mut().set(XSpanId(header.clone())));




        let hyper_client = (self.hyper_client)(&*self.handle);
        Box::new(hyper_client.call(request)
                             .map_err(|e| ApiError(format!("No response received: {}", e)))
                             .and_then(|mut response| {
            match response.status().as_u16() {
                200 => {
                    let body = response.body();
                    Box::new(

                        body
                        .concat2()
                        .map_err(|e| ApiError(format!("Failed to read response: {}", e)))
                        .and_then(|body| str::from_utf8(&body)
                                             .map_err(|e| ApiError(format!("Response was not valid UTF8: {}", e)))
                                             .and_then(|body|

                                                 serde_json::from_str::<models::OuterNumber>(body)
                                                     .map_err(|e| e.into())

                                             ))
                        .map(move |body|
                            FakeOuterNumberSerializeResponse::OutputNumber(body)
                        )
                    ) as Box<Future<Item=_, Error=_>>
                },
                code => {
                    let headers = response.headers().clone();
                    Box::new(response.body()
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
                    ) as Box<Future<Item=_, Error=_>>
                }
            }
        }))

    }

    fn fake_outer_string_serialize(&self, param_body: Option<models::OuterString>, context: &Context) -> Box<Future<Item=FakeOuterStringSerializeResponse, Error=ApiError>> {


        let uri = format!(
            "{}/v2/fake/outer/string",
            self.base_path
        );

        let uri = match Uri::from_str(&uri) {
            Ok(uri) => uri,
            Err(err) => return Box::new(futures::done(Err(ApiError(format!("Unable to build URI: {}", err))))),
        };

        let mut request = hyper::Request::new(hyper::Method::Post, uri);

        let body = param_body.map(|ref body| {

            serde_json::to_string(body).expect("impossible to fail to serialize")
        });

if let Some(body) = body {
            request.set_body(body.into_bytes());
        }

        request.headers_mut().set(ContentType(mimetypes::requests::FAKE_OUTER_STRING_SERIALIZE.clone()));
        context.x_span_id.as_ref().map(|header| request.headers_mut().set(XSpanId(header.clone())));




        let hyper_client = (self.hyper_client)(&*self.handle);
        Box::new(hyper_client.call(request)
                             .map_err(|e| ApiError(format!("No response received: {}", e)))
                             .and_then(|mut response| {
            match response.status().as_u16() {
                200 => {
                    let body = response.body();
                    Box::new(

                        body
                        .concat2()
                        .map_err(|e| ApiError(format!("Failed to read response: {}", e)))
                        .and_then(|body| str::from_utf8(&body)
                                             .map_err(|e| ApiError(format!("Response was not valid UTF8: {}", e)))
                                             .and_then(|body|

                                                 serde_json::from_str::<models::OuterString>(body)
                                                     .map_err(|e| e.into())

                                             ))
                        .map(move |body|
                            FakeOuterStringSerializeResponse::OutputString(body)
                        )
                    ) as Box<Future<Item=_, Error=_>>
                },
                code => {
                    let headers = response.headers().clone();
                    Box::new(response.body()
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
                    ) as Box<Future<Item=_, Error=_>>
                }
            }
        }))

    }

    fn test_client_model(&self, param_body: models::Client, context: &Context) -> Box<Future<Item=TestClientModelResponse, Error=ApiError>> {


        let uri = format!(
            "{}/v2/fake",
            self.base_path
        );

        let uri = match Uri::from_str(&uri) {
            Ok(uri) => uri,
            Err(err) => return Box::new(futures::done(Err(ApiError(format!("Unable to build URI: {}", err))))),
        };

        let mut request = hyper::Request::new(hyper::Method::Patch, uri);


        let body = serde_json::to_string(&param_body).expect("impossible to fail to serialize");


        request.set_body(body.into_bytes());


        request.headers_mut().set(ContentType(mimetypes::requests::TEST_CLIENT_MODEL.clone()));
        context.x_span_id.as_ref().map(|header| request.headers_mut().set(XSpanId(header.clone())));




        let hyper_client = (self.hyper_client)(&*self.handle);
        Box::new(hyper_client.call(request)
                             .map_err(|e| ApiError(format!("No response received: {}", e)))
                             .and_then(|mut response| {
            match response.status().as_u16() {
                200 => {
                    let body = response.body();
                    Box::new(

                        body
                        .concat2()
                        .map_err(|e| ApiError(format!("Failed to read response: {}", e)))
                        .and_then(|body| str::from_utf8(&body)
                                             .map_err(|e| ApiError(format!("Response was not valid UTF8: {}", e)))
                                             .and_then(|body|

                                                 serde_json::from_str::<models::Client>(body)
                                                     .map_err(|e| e.into())

                                             ))
                        .map(move |body|
                            TestClientModelResponse::SuccessfulOperation(body)
                        )
                    ) as Box<Future<Item=_, Error=_>>
                },
                code => {
                    let headers = response.headers().clone();
                    Box::new(response.body()
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
                    ) as Box<Future<Item=_, Error=_>>
                }
            }
        }))

    }

    fn test_endpoint_parameters(&self, param_number: f64, param_double: f64, param_pattern_without_delimiter: String, param_byte: swagger::ByteArray, param_integer: Option<i32>, param_int32: Option<i32>, param_int64: Option<i64>, param_float: Option<f32>, param_string: Option<String>, param_binary: Option<swagger::ByteArray>, param_date: Option<chrono::DateTime<chrono::Utc>>, param_date_time: Option<chrono::DateTime<chrono::Utc>>, param_password: Option<String>, param_callback: Option<String>, context: &Context) -> Box<Future<Item=TestEndpointParametersResponse, Error=ApiError>> {


        let uri = format!(
            "{}/v2/fake",
            self.base_path
        );

        let uri = match Uri::from_str(&uri) {
            Ok(uri) => uri,
            Err(err) => return Box::new(futures::done(Err(ApiError(format!("Unable to build URI: {}", err))))),
        };

        let mut request = hyper::Request::new(hyper::Method::Post, uri);

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

        request.headers_mut().set(ContentType(mimetypes::requests::TEST_ENDPOINT_PARAMETERS.clone()));
        request.set_body(body.into_bytes());

        context.x_span_id.as_ref().map(|header| request.headers_mut().set(XSpanId(header.clone())));
        context.auth_data.as_ref().map(|auth_data| {
            if let &swagger::AuthData::Basic(ref basic_header) = auth_data {
                request.headers_mut().set(hyper::header::Authorization(
                    basic_header.clone(),
                ))
            }
        });



        let hyper_client = (self.hyper_client)(&*self.handle);
        Box::new(hyper_client.call(request)
                             .map_err(|e| ApiError(format!("No response received: {}", e)))
                             .and_then(|mut response| {
            match response.status().as_u16() {
                400 => {
                    let body = response.body();
                    Box::new(

                        future::ok(
                            TestEndpointParametersResponse::InvalidUsernameSupplied
                        )
                    ) as Box<Future<Item=_, Error=_>>
                },
                404 => {
                    let body = response.body();
                    Box::new(

                        future::ok(
                            TestEndpointParametersResponse::UserNotFound
                        )
                    ) as Box<Future<Item=_, Error=_>>
                },
                code => {
                    let headers = response.headers().clone();
                    Box::new(response.body()
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
                    ) as Box<Future<Item=_, Error=_>>
                }
            }
        }))

    }

    fn test_enum_parameters(&self, param_enum_form_string_array: Option<&Vec<String>>, param_enum_form_string: Option<String>, param_enum_header_string_array: Option<&Vec<String>>, param_enum_header_string: Option<String>, param_enum_query_string_array: Option<&Vec<String>>, param_enum_query_string: Option<String>, param_enum_query_integer: Option<i32>, param_enum_query_double: Option<f64>, context: &Context) -> Box<Future<Item=TestEnumParametersResponse, Error=ApiError>> {

        // Query parameters
        let query_enum_query_string_array = param_enum_query_string_array.map_or_else(String::new, |query| format!("enum_query_string_array={enum_query_string_array}&", enum_query_string_array=query.join(",")));
        let query_enum_query_string = param_enum_query_string.map_or_else(String::new, |query| format!("enum_query_string={enum_query_string}&", enum_query_string=query.to_string()));
        let query_enum_query_integer = param_enum_query_integer.map_or_else(String::new, |query| format!("enum_query_integer={enum_query_integer}&", enum_query_integer=query.to_string()));


        let uri = format!(
            "{}/v2/fake?{enum_query_string_array}{enum_query_string}{enum_query_integer}",
            self.base_path,
            enum_query_string_array=utf8_percent_encode(&query_enum_query_string_array, QUERY_ENCODE_SET),
            enum_query_string=utf8_percent_encode(&query_enum_query_string, QUERY_ENCODE_SET),
            enum_query_integer=utf8_percent_encode(&query_enum_query_integer, QUERY_ENCODE_SET)
        );

        let uri = match Uri::from_str(&uri) {
            Ok(uri) => uri,
            Err(err) => return Box::new(futures::done(Err(ApiError(format!("Unable to build URI: {}", err))))),
        };

        let mut request = hyper::Request::new(hyper::Method::Get, uri);

        let params = &[
            ("enum_form_string_array", param_enum_form_string_array.map(|param| format!("{:?}", param))),
            ("enum_form_string", param_enum_form_string),
            ("enum_query_double", param_enum_query_double.map(|param| format!("{:?}", param))),
        ];
        let body = serde_urlencoded::to_string(params).expect("impossible to fail to serialize");

        request.headers_mut().set(ContentType(mimetypes::requests::TEST_ENUM_PARAMETERS.clone()));
        request.set_body(body.into_bytes());

        context.x_span_id.as_ref().map(|header| request.headers_mut().set(XSpanId(header.clone())));

        // Header parameters
        header! { (RequestEnumHeaderStringArray, "enum_header_string_array") => (String)* }
        param_enum_header_string_array.map(|header| request.headers_mut().set(RequestEnumHeaderStringArray(header.clone())));
        header! { (RequestEnumHeaderString, "enum_header_string") => [String] }
        param_enum_header_string.map(|header| request.headers_mut().set(RequestEnumHeaderString(header)));




        let hyper_client = (self.hyper_client)(&*self.handle);
        Box::new(hyper_client.call(request)
                             .map_err(|e| ApiError(format!("No response received: {}", e)))
                             .and_then(|mut response| {
            match response.status().as_u16() {
                400 => {
                    let body = response.body();
                    Box::new(

                        future::ok(
                            TestEnumParametersResponse::InvalidRequest
                        )
                    ) as Box<Future<Item=_, Error=_>>
                },
                404 => {
                    let body = response.body();
                    Box::new(

                        future::ok(
                            TestEnumParametersResponse::NotFound
                        )
                    ) as Box<Future<Item=_, Error=_>>
                },
                code => {
                    let headers = response.headers().clone();
                    Box::new(response.body()
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
                    ) as Box<Future<Item=_, Error=_>>
                }
            }
        }))

    }

    fn test_inline_additional_properties(&self, param_param: object, context: &Context) -> Box<Future<Item=TestInlineAdditionalPropertiesResponse, Error=ApiError>> {


        let uri = format!(
            "{}/v2/fake/inline-additionalProperties",
            self.base_path
        );

        let uri = match Uri::from_str(&uri) {
            Ok(uri) => uri,
            Err(err) => return Box::new(futures::done(Err(ApiError(format!("Unable to build URI: {}", err))))),
        };

        let mut request = hyper::Request::new(hyper::Method::Post, uri);


        let body = serde_json::to_string(&param_param).expect("impossible to fail to serialize");


        request.set_body(body.into_bytes());


        request.headers_mut().set(ContentType(mimetypes::requests::TEST_INLINE_ADDITIONAL_PROPERTIES.clone()));
        context.x_span_id.as_ref().map(|header| request.headers_mut().set(XSpanId(header.clone())));




        let hyper_client = (self.hyper_client)(&*self.handle);
        Box::new(hyper_client.call(request)
                             .map_err(|e| ApiError(format!("No response received: {}", e)))
                             .and_then(|mut response| {
            match response.status().as_u16() {
                200 => {
                    let body = response.body();
                    Box::new(

                        future::ok(
                            TestInlineAdditionalPropertiesResponse::SuccessfulOperation
                        )
                    ) as Box<Future<Item=_, Error=_>>
                },
                code => {
                    let headers = response.headers().clone();
                    Box::new(response.body()
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
                    ) as Box<Future<Item=_, Error=_>>
                }
            }
        }))

    }

    fn test_json_form_data(&self, param_param: String, param_param2: String, context: &Context) -> Box<Future<Item=TestJsonFormDataResponse, Error=ApiError>> {


        let uri = format!(
            "{}/v2/fake/jsonFormData",
            self.base_path
        );

        let uri = match Uri::from_str(&uri) {
            Ok(uri) => uri,
            Err(err) => return Box::new(futures::done(Err(ApiError(format!("Unable to build URI: {}", err))))),
        };

        let mut request = hyper::Request::new(hyper::Method::Get, uri);

        let params = &[
            ("param", Some(param_param)),
            ("param2", Some(param_param2)),
        ];
        let body = serde_urlencoded::to_string(params).expect("impossible to fail to serialize");

        request.headers_mut().set(ContentType(mimetypes::requests::TEST_JSON_FORM_DATA.clone()));
        request.set_body(body.into_bytes());

        context.x_span_id.as_ref().map(|header| request.headers_mut().set(XSpanId(header.clone())));




        let hyper_client = (self.hyper_client)(&*self.handle);
        Box::new(hyper_client.call(request)
                             .map_err(|e| ApiError(format!("No response received: {}", e)))
                             .and_then(|mut response| {
            match response.status().as_u16() {
                200 => {
                    let body = response.body();
                    Box::new(

                        future::ok(
                            TestJsonFormDataResponse::SuccessfulOperation
                        )
                    ) as Box<Future<Item=_, Error=_>>
                },
                code => {
                    let headers = response.headers().clone();
                    Box::new(response.body()
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
                    ) as Box<Future<Item=_, Error=_>>
                }
            }
        }))

    }

    fn test_classname(&self, param_body: models::Client, context: &Context) -> Box<Future<Item=TestClassnameResponse, Error=ApiError>> {


        let uri = format!(
            "{}/v2/fake_classname_test",
            self.base_path
        );

        let uri = match Uri::from_str(&uri) {
            Ok(uri) => uri,
            Err(err) => return Box::new(futures::done(Err(ApiError(format!("Unable to build URI: {}", err))))),
        };

        let mut request = hyper::Request::new(hyper::Method::Patch, uri);


        let body = serde_json::to_string(&param_body).expect("impossible to fail to serialize");


        request.set_body(body.into_bytes());


        request.headers_mut().set(ContentType(mimetypes::requests::TEST_CLASSNAME.clone()));
        context.x_span_id.as_ref().map(|header| request.headers_mut().set(XSpanId(header.clone())));




        let hyper_client = (self.hyper_client)(&*self.handle);
        Box::new(hyper_client.call(request)
                             .map_err(|e| ApiError(format!("No response received: {}", e)))
                             .and_then(|mut response| {
            match response.status().as_u16() {
                200 => {
                    let body = response.body();
                    Box::new(

                        body
                        .concat2()
                        .map_err(|e| ApiError(format!("Failed to read response: {}", e)))
                        .and_then(|body| str::from_utf8(&body)
                                             .map_err(|e| ApiError(format!("Response was not valid UTF8: {}", e)))
                                             .and_then(|body|

                                                 serde_json::from_str::<models::Client>(body)
                                                     .map_err(|e| e.into())

                                             ))
                        .map(move |body|
                            TestClassnameResponse::SuccessfulOperation(body)
                        )
                    ) as Box<Future<Item=_, Error=_>>
                },
                code => {
                    let headers = response.headers().clone();
                    Box::new(response.body()
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
                    ) as Box<Future<Item=_, Error=_>>
                }
            }
        }))

    }

    fn add_pet(&self, param_body: models::Pet, context: &Context) -> Box<Future<Item=AddPetResponse, Error=ApiError>> {


        let uri = format!(
            "{}/v2/pet",
            self.base_path
        );

        let uri = match Uri::from_str(&uri) {
            Ok(uri) => uri,
            Err(err) => return Box::new(futures::done(Err(ApiError(format!("Unable to build URI: {}", err))))),
        };

        let mut request = hyper::Request::new(hyper::Method::Post, uri);


        let body = serde_xml_rs::to_string(&param_body).expect("impossible to fail to serialize");


        request.set_body(body.into_bytes());


        request.headers_mut().set(ContentType(mimetypes::requests::ADD_PET.clone()));
        context.x_span_id.as_ref().map(|header| request.headers_mut().set(XSpanId(header.clone())));




        let hyper_client = (self.hyper_client)(&*self.handle);
        Box::new(hyper_client.call(request)
                             .map_err(|e| ApiError(format!("No response received: {}", e)))
                             .and_then(|mut response| {
            match response.status().as_u16() {
                405 => {
                    let body = response.body();
                    Box::new(

                        future::ok(
                            AddPetResponse::InvalidInput
                        )
                    ) as Box<Future<Item=_, Error=_>>
                },
                code => {
                    let headers = response.headers().clone();
                    Box::new(response.body()
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
                    ) as Box<Future<Item=_, Error=_>>
                }
            }
        }))

    }

    fn delete_pet(&self, param_pet_id: i64, param_api_key: Option<String>, context: &Context) -> Box<Future<Item=DeletePetResponse, Error=ApiError>> {


        let uri = format!(
            "{}/v2/pet/{petId}",
            self.base_path, petId=utf8_percent_encode(&param_pet_id.to_string(), PATH_SEGMENT_ENCODE_SET)
        );

        let uri = match Uri::from_str(&uri) {
            Ok(uri) => uri,
            Err(err) => return Box::new(futures::done(Err(ApiError(format!("Unable to build URI: {}", err))))),
        };

        let mut request = hyper::Request::new(hyper::Method::Delete, uri);



        context.x_span_id.as_ref().map(|header| request.headers_mut().set(XSpanId(header.clone())));

        // Header parameters
        header! { (RequestApiKey, "api_key") => [String] }
        param_api_key.map(|header| request.headers_mut().set(RequestApiKey(header)));




        let hyper_client = (self.hyper_client)(&*self.handle);
        Box::new(hyper_client.call(request)
                             .map_err(|e| ApiError(format!("No response received: {}", e)))
                             .and_then(|mut response| {
            match response.status().as_u16() {
                400 => {
                    let body = response.body();
                    Box::new(

                        future::ok(
                            DeletePetResponse::InvalidPetValue
                        )
                    ) as Box<Future<Item=_, Error=_>>
                },
                code => {
                    let headers = response.headers().clone();
                    Box::new(response.body()
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
                    ) as Box<Future<Item=_, Error=_>>
                }
            }
        }))

    }

    fn find_pets_by_status(&self, param_status: &Vec<String>, context: &Context) -> Box<Future<Item=FindPetsByStatusResponse, Error=ApiError>> {

        // Query parameters
        let query_status = format!("status={status}&", status=param_status.join(","));


        let uri = format!(
            "{}/v2/pet/findByStatus?{status}",
            self.base_path,
            status=utf8_percent_encode(&query_status, QUERY_ENCODE_SET)
        );

        let uri = match Uri::from_str(&uri) {
            Ok(uri) => uri,
            Err(err) => return Box::new(futures::done(Err(ApiError(format!("Unable to build URI: {}", err))))),
        };

        let mut request = hyper::Request::new(hyper::Method::Get, uri);



        context.x_span_id.as_ref().map(|header| request.headers_mut().set(XSpanId(header.clone())));




        let hyper_client = (self.hyper_client)(&*self.handle);
        Box::new(hyper_client.call(request)
                             .map_err(|e| ApiError(format!("No response received: {}", e)))
                             .and_then(|mut response| {
            match response.status().as_u16() {
                200 => {
                    let body = response.body();
                    Box::new(

                        body
                        .concat2()
                        .map_err(|e| ApiError(format!("Failed to read response: {}", e)))
                        .and_then(|body| str::from_utf8(&body)
                                             .map_err(|e| ApiError(format!("Response was not valid UTF8: {}", e)))
                                             .and_then(|body|

                                                 // ToDo: this will move to swagger-rs and become a standard From conversion trait
                                                 // once https://github.com/RReverser/serde-xml-rs/pull/45 is accepted upstream
                                                 serde_xml_rs::from_str::<Vec<models::Pet>>(body)
                                                     .map_err(|e| ApiError(format!("Response body did not match the schema: {}", e)))

                                             ))
                        .map(move |body|
                            FindPetsByStatusResponse::SuccessfulOperation(body)
                        )
                    ) as Box<Future<Item=_, Error=_>>
                },
                400 => {
                    let body = response.body();
                    Box::new(

                        future::ok(
                            FindPetsByStatusResponse::InvalidStatusValue
                        )
                    ) as Box<Future<Item=_, Error=_>>
                },
                code => {
                    let headers = response.headers().clone();
                    Box::new(response.body()
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
                    ) as Box<Future<Item=_, Error=_>>
                }
            }
        }))

    }

    fn find_pets_by_tags(&self, param_tags: &Vec<String>, context: &Context) -> Box<Future<Item=FindPetsByTagsResponse, Error=ApiError>> {

        // Query parameters
        let query_tags = format!("tags={tags}&", tags=param_tags.join(","));


        let uri = format!(
            "{}/v2/pet/findByTags?{tags}",
            self.base_path,
            tags=utf8_percent_encode(&query_tags, QUERY_ENCODE_SET)
        );

        let uri = match Uri::from_str(&uri) {
            Ok(uri) => uri,
            Err(err) => return Box::new(futures::done(Err(ApiError(format!("Unable to build URI: {}", err))))),
        };

        let mut request = hyper::Request::new(hyper::Method::Get, uri);



        context.x_span_id.as_ref().map(|header| request.headers_mut().set(XSpanId(header.clone())));




        let hyper_client = (self.hyper_client)(&*self.handle);
        Box::new(hyper_client.call(request)
                             .map_err(|e| ApiError(format!("No response received: {}", e)))
                             .and_then(|mut response| {
            match response.status().as_u16() {
                200 => {
                    let body = response.body();
                    Box::new(

                        body
                        .concat2()
                        .map_err(|e| ApiError(format!("Failed to read response: {}", e)))
                        .and_then(|body| str::from_utf8(&body)
                                             .map_err(|e| ApiError(format!("Response was not valid UTF8: {}", e)))
                                             .and_then(|body|

                                                 // ToDo: this will move to swagger-rs and become a standard From conversion trait
                                                 // once https://github.com/RReverser/serde-xml-rs/pull/45 is accepted upstream
                                                 serde_xml_rs::from_str::<Vec<models::Pet>>(body)
                                                     .map_err(|e| ApiError(format!("Response body did not match the schema: {}", e)))

                                             ))
                        .map(move |body|
                            FindPetsByTagsResponse::SuccessfulOperation(body)
                        )
                    ) as Box<Future<Item=_, Error=_>>
                },
                400 => {
                    let body = response.body();
                    Box::new(

                        future::ok(
                            FindPetsByTagsResponse::InvalidTagValue
                        )
                    ) as Box<Future<Item=_, Error=_>>
                },
                code => {
                    let headers = response.headers().clone();
                    Box::new(response.body()
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
                    ) as Box<Future<Item=_, Error=_>>
                }
            }
        }))

    }

    fn get_pet_by_id(&self, param_pet_id: i64, context: &Context) -> Box<Future<Item=GetPetByIdResponse, Error=ApiError>> {


        let uri = format!(
            "{}/v2/pet/{petId}",
            self.base_path, petId=utf8_percent_encode(&param_pet_id.to_string(), PATH_SEGMENT_ENCODE_SET)
        );

        let uri = match Uri::from_str(&uri) {
            Ok(uri) => uri,
            Err(err) => return Box::new(futures::done(Err(ApiError(format!("Unable to build URI: {}", err))))),
        };

        let mut request = hyper::Request::new(hyper::Method::Get, uri);



        context.x_span_id.as_ref().map(|header| request.headers_mut().set(XSpanId(header.clone())));




        let hyper_client = (self.hyper_client)(&*self.handle);
        Box::new(hyper_client.call(request)
                             .map_err(|e| ApiError(format!("No response received: {}", e)))
                             .and_then(|mut response| {
            match response.status().as_u16() {
                200 => {
                    let body = response.body();
                    Box::new(

                        body
                        .concat2()
                        .map_err(|e| ApiError(format!("Failed to read response: {}", e)))
                        .and_then(|body| str::from_utf8(&body)
                                             .map_err(|e| ApiError(format!("Response was not valid UTF8: {}", e)))
                                             .and_then(|body|

                                                 // ToDo: this will move to swagger-rs and become a standard From conversion trait
                                                 // once https://github.com/RReverser/serde-xml-rs/pull/45 is accepted upstream
                                                 serde_xml_rs::from_str::<models::Pet>(body)
                                                     .map_err(|e| ApiError(format!("Response body did not match the schema: {}", e)))

                                             ))
                        .map(move |body|
                            GetPetByIdResponse::SuccessfulOperation(body)
                        )
                    ) as Box<Future<Item=_, Error=_>>
                },
                400 => {
                    let body = response.body();
                    Box::new(

                        future::ok(
                            GetPetByIdResponse::InvalidIDSupplied
                        )
                    ) as Box<Future<Item=_, Error=_>>
                },
                404 => {
                    let body = response.body();
                    Box::new(

                        future::ok(
                            GetPetByIdResponse::PetNotFound
                        )
                    ) as Box<Future<Item=_, Error=_>>
                },
                code => {
                    let headers = response.headers().clone();
                    Box::new(response.body()
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
                    ) as Box<Future<Item=_, Error=_>>
                }
            }
        }))

    }

    fn update_pet(&self, param_body: models::Pet, context: &Context) -> Box<Future<Item=UpdatePetResponse, Error=ApiError>> {


        let uri = format!(
            "{}/v2/pet",
            self.base_path
        );

        let uri = match Uri::from_str(&uri) {
            Ok(uri) => uri,
            Err(err) => return Box::new(futures::done(Err(ApiError(format!("Unable to build URI: {}", err))))),
        };

        let mut request = hyper::Request::new(hyper::Method::Put, uri);


        let body = serde_xml_rs::to_string(&param_body).expect("impossible to fail to serialize");


        request.set_body(body.into_bytes());


        request.headers_mut().set(ContentType(mimetypes::requests::UPDATE_PET.clone()));
        context.x_span_id.as_ref().map(|header| request.headers_mut().set(XSpanId(header.clone())));




        let hyper_client = (self.hyper_client)(&*self.handle);
        Box::new(hyper_client.call(request)
                             .map_err(|e| ApiError(format!("No response received: {}", e)))
                             .and_then(|mut response| {
            match response.status().as_u16() {
                400 => {
                    let body = response.body();
                    Box::new(

                        future::ok(
                            UpdatePetResponse::InvalidIDSupplied
                        )
                    ) as Box<Future<Item=_, Error=_>>
                },
                404 => {
                    let body = response.body();
                    Box::new(

                        future::ok(
                            UpdatePetResponse::PetNotFound
                        )
                    ) as Box<Future<Item=_, Error=_>>
                },
                405 => {
                    let body = response.body();
                    Box::new(

                        future::ok(
                            UpdatePetResponse::ValidationException
                        )
                    ) as Box<Future<Item=_, Error=_>>
                },
                code => {
                    let headers = response.headers().clone();
                    Box::new(response.body()
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
                    ) as Box<Future<Item=_, Error=_>>
                }
            }
        }))

    }

    fn update_pet_with_form(&self, param_pet_id: i64, param_name: Option<String>, param_status: Option<String>, context: &Context) -> Box<Future<Item=UpdatePetWithFormResponse, Error=ApiError>> {


        let uri = format!(
            "{}/v2/pet/{petId}",
            self.base_path, petId=utf8_percent_encode(&param_pet_id.to_string(), PATH_SEGMENT_ENCODE_SET)
        );

        let uri = match Uri::from_str(&uri) {
            Ok(uri) => uri,
            Err(err) => return Box::new(futures::done(Err(ApiError(format!("Unable to build URI: {}", err))))),
        };

        let mut request = hyper::Request::new(hyper::Method::Post, uri);

        let params = &[
            ("name", param_name),
            ("status", param_status),
        ];
        let body = serde_urlencoded::to_string(params).expect("impossible to fail to serialize");

        request.headers_mut().set(ContentType(mimetypes::requests::UPDATE_PET_WITH_FORM.clone()));
        request.set_body(body.into_bytes());

        context.x_span_id.as_ref().map(|header| request.headers_mut().set(XSpanId(header.clone())));




        let hyper_client = (self.hyper_client)(&*self.handle);
        Box::new(hyper_client.call(request)
                             .map_err(|e| ApiError(format!("No response received: {}", e)))
                             .and_then(|mut response| {
            match response.status().as_u16() {
                405 => {
                    let body = response.body();
                    Box::new(

                        future::ok(
                            UpdatePetWithFormResponse::InvalidInput
                        )
                    ) as Box<Future<Item=_, Error=_>>
                },
                code => {
                    let headers = response.headers().clone();
                    Box::new(response.body()
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
                    ) as Box<Future<Item=_, Error=_>>
                }
            }
        }))

    }

    fn upload_file(&self, param_pet_id: i64, param_additional_metadata: Option<String>, param_file: Box<Future<Item=Option<Box<Stream<Item=Vec<u8>, Error=Error> + Send>>, Error=Error> + Send>, context: &Context) -> Box<Future<Item=UploadFileResponse, Error=ApiError>> {


        let uri = format!(
            "{}/v2/pet/{petId}/uploadImage",
            self.base_path, petId=utf8_percent_encode(&param_pet_id.to_string(), PATH_SEGMENT_ENCODE_SET)
        );

        let uri = match Uri::from_str(&uri) {
            Ok(uri) => uri,
            Err(err) => return Box::new(futures::done(Err(ApiError(format!("Unable to build URI: {}", err))))),
        };

        let mut request = hyper::Request::new(hyper::Method::Post, uri);

        // Form data body
        let mut multipart = Multipart::new();

        // Helper function to convert a Stream into a String. The String can then be used to build the HTTP body.
        fn convert_stream_to_string(stream: Box<Stream<Item=Vec<u8>, Error=Error> + Send>) -> Result<String, ApiError> {

            stream.concat2()
              .wait()
              .map_err(|e| ApiError(format!("Unable to collect stream: {}", e)))
              .and_then(|body| String::from_utf8(body)
                .map_err(|e| ApiError(format!("Failed to convert utf8 stream to String: {}", e))))
        }

        if let Ok(Some(param_file)) = param_file.wait() { 
            match convert_stream_to_string(param_file) {
                Ok(param_file) => {
                    // Add file to multipart form.
                    multipart.add_text("file", param_file);
                },
                Err(err) => return Box::new(futures::done(Err(err))),
            }
        }

        let mut fields = match multipart.prepare() {
            Ok(fields) => fields,
            Err(err) => return Box::new(futures::done(Err(ApiError(format!("Unable to build request: {}", err))))),
        };

        let mut body_string = String::new();
        let body = fields.to_body().read_to_string(&mut body_string);
        let boundary = fields.boundary();
        let multipart_header = match mime::Mime::from_str(&format!("multipart/form-data;boundary={}", boundary)) {
            Ok(multipart_header) => multipart_header,
            Err(err) => return Box::new(futures::done(Err(ApiError(format!("Unable to build multipart header: {:?}", err))))),
        };

        context.x_span_id.as_ref().map(|header| request.headers_mut().set(XSpanId(header.clone())));



        request.headers_mut().set(ContentType(multipart_header));
        request.set_body(body_string.into_bytes());


        let hyper_client = (self.hyper_client)(&*self.handle);
        Box::new(hyper_client.call(request)
                             .map_err(|e| ApiError(format!("No response received: {}", e)))
                             .and_then(|mut response| {
            match response.status().as_u16() {
                200 => {
                    let body = response.body();
                    Box::new(

                        body
                        .concat2()
                        .map_err(|e| ApiError(format!("Failed to read response: {}", e)))
                        .and_then(|body| str::from_utf8(&body)
                                             .map_err(|e| ApiError(format!("Response was not valid UTF8: {}", e)))
                                             .and_then(|body|

                                                 serde_json::from_str::<models::ApiResponse>(body)
                                                     .map_err(|e| e.into())

                                             ))
                        .map(move |body|
                            UploadFileResponse::SuccessfulOperation(body)
                        )
                    ) as Box<Future<Item=_, Error=_>>
                },
                code => {
                    let headers = response.headers().clone();
                    Box::new(response.body()
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
                    ) as Box<Future<Item=_, Error=_>>
                }
            }
        }))

    }

    fn delete_order(&self, param_order_id: String, context: &Context) -> Box<Future<Item=DeleteOrderResponse, Error=ApiError>> {


        let uri = format!(
            "{}/v2/store/order/{order_id}",
            self.base_path, order_id=utf8_percent_encode(&param_order_id.to_string(), PATH_SEGMENT_ENCODE_SET)
        );

        let uri = match Uri::from_str(&uri) {
            Ok(uri) => uri,
            Err(err) => return Box::new(futures::done(Err(ApiError(format!("Unable to build URI: {}", err))))),
        };

        let mut request = hyper::Request::new(hyper::Method::Delete, uri);



        context.x_span_id.as_ref().map(|header| request.headers_mut().set(XSpanId(header.clone())));




        let hyper_client = (self.hyper_client)(&*self.handle);
        Box::new(hyper_client.call(request)
                             .map_err(|e| ApiError(format!("No response received: {}", e)))
                             .and_then(|mut response| {
            match response.status().as_u16() {
                400 => {
                    let body = response.body();
                    Box::new(

                        future::ok(
                            DeleteOrderResponse::InvalidIDSupplied
                        )
                    ) as Box<Future<Item=_, Error=_>>
                },
                404 => {
                    let body = response.body();
                    Box::new(

                        future::ok(
                            DeleteOrderResponse::OrderNotFound
                        )
                    ) as Box<Future<Item=_, Error=_>>
                },
                code => {
                    let headers = response.headers().clone();
                    Box::new(response.body()
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
                    ) as Box<Future<Item=_, Error=_>>
                }
            }
        }))

    }

    fn get_inventory(&self, context: &Context) -> Box<Future<Item=GetInventoryResponse, Error=ApiError>> {


        let uri = format!(
            "{}/v2/store/inventory",
            self.base_path
        );

        let uri = match Uri::from_str(&uri) {
            Ok(uri) => uri,
            Err(err) => return Box::new(futures::done(Err(ApiError(format!("Unable to build URI: {}", err))))),
        };

        let mut request = hyper::Request::new(hyper::Method::Get, uri);



        context.x_span_id.as_ref().map(|header| request.headers_mut().set(XSpanId(header.clone())));




        let hyper_client = (self.hyper_client)(&*self.handle);
        Box::new(hyper_client.call(request)
                             .map_err(|e| ApiError(format!("No response received: {}", e)))
                             .and_then(|mut response| {
            match response.status().as_u16() {
                200 => {
                    let body = response.body();
                    Box::new(

                        body
                        .concat2()
                        .map_err(|e| ApiError(format!("Failed to read response: {}", e)))
                        .and_then(|body| str::from_utf8(&body)
                                             .map_err(|e| ApiError(format!("Response was not valid UTF8: {}", e)))
                                             .and_then(|body|

                                                 serde_json::from_str::<HashMap<String, i32>>(body)
                                                     .map_err(|e| e.into())

                                             ))
                        .map(move |body|
                            GetInventoryResponse::SuccessfulOperation(body)
                        )
                    ) as Box<Future<Item=_, Error=_>>
                },
                code => {
                    let headers = response.headers().clone();
                    Box::new(response.body()
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
                    ) as Box<Future<Item=_, Error=_>>
                }
            }
        }))

    }

    fn get_order_by_id(&self, param_order_id: i64, context: &Context) -> Box<Future<Item=GetOrderByIdResponse, Error=ApiError>> {


        let uri = format!(
            "{}/v2/store/order/{order_id}",
            self.base_path, order_id=utf8_percent_encode(&param_order_id.to_string(), PATH_SEGMENT_ENCODE_SET)
        );

        let uri = match Uri::from_str(&uri) {
            Ok(uri) => uri,
            Err(err) => return Box::new(futures::done(Err(ApiError(format!("Unable to build URI: {}", err))))),
        };

        let mut request = hyper::Request::new(hyper::Method::Get, uri);



        context.x_span_id.as_ref().map(|header| request.headers_mut().set(XSpanId(header.clone())));




        let hyper_client = (self.hyper_client)(&*self.handle);
        Box::new(hyper_client.call(request)
                             .map_err(|e| ApiError(format!("No response received: {}", e)))
                             .and_then(|mut response| {
            match response.status().as_u16() {
                200 => {
                    let body = response.body();
                    Box::new(

                        body
                        .concat2()
                        .map_err(|e| ApiError(format!("Failed to read response: {}", e)))
                        .and_then(|body| str::from_utf8(&body)
                                             .map_err(|e| ApiError(format!("Response was not valid UTF8: {}", e)))
                                             .and_then(|body|

                                                 // ToDo: this will move to swagger-rs and become a standard From conversion trait
                                                 // once https://github.com/RReverser/serde-xml-rs/pull/45 is accepted upstream
                                                 serde_xml_rs::from_str::<models::Order>(body)
                                                     .map_err(|e| ApiError(format!("Response body did not match the schema: {}", e)))

                                             ))
                        .map(move |body|
                            GetOrderByIdResponse::SuccessfulOperation(body)
                        )
                    ) as Box<Future<Item=_, Error=_>>
                },
                400 => {
                    let body = response.body();
                    Box::new(

                        future::ok(
                            GetOrderByIdResponse::InvalidIDSupplied
                        )
                    ) as Box<Future<Item=_, Error=_>>
                },
                404 => {
                    let body = response.body();
                    Box::new(

                        future::ok(
                            GetOrderByIdResponse::OrderNotFound
                        )
                    ) as Box<Future<Item=_, Error=_>>
                },
                code => {
                    let headers = response.headers().clone();
                    Box::new(response.body()
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
                    ) as Box<Future<Item=_, Error=_>>
                }
            }
        }))

    }

    fn place_order(&self, param_body: models::Order, context: &Context) -> Box<Future<Item=PlaceOrderResponse, Error=ApiError>> {


        let uri = format!(
            "{}/v2/store/order",
            self.base_path
        );

        let uri = match Uri::from_str(&uri) {
            Ok(uri) => uri,
            Err(err) => return Box::new(futures::done(Err(ApiError(format!("Unable to build URI: {}", err))))),
        };

        let mut request = hyper::Request::new(hyper::Method::Post, uri);


        let body = serde_json::to_string(&param_body).expect("impossible to fail to serialize");


        request.set_body(body.into_bytes());


        request.headers_mut().set(ContentType(mimetypes::requests::PLACE_ORDER.clone()));
        context.x_span_id.as_ref().map(|header| request.headers_mut().set(XSpanId(header.clone())));




        let hyper_client = (self.hyper_client)(&*self.handle);
        Box::new(hyper_client.call(request)
                             .map_err(|e| ApiError(format!("No response received: {}", e)))
                             .and_then(|mut response| {
            match response.status().as_u16() {
                200 => {
                    let body = response.body();
                    Box::new(

                        body
                        .concat2()
                        .map_err(|e| ApiError(format!("Failed to read response: {}", e)))
                        .and_then(|body| str::from_utf8(&body)
                                             .map_err(|e| ApiError(format!("Response was not valid UTF8: {}", e)))
                                             .and_then(|body|

                                                 // ToDo: this will move to swagger-rs and become a standard From conversion trait
                                                 // once https://github.com/RReverser/serde-xml-rs/pull/45 is accepted upstream
                                                 serde_xml_rs::from_str::<models::Order>(body)
                                                     .map_err(|e| ApiError(format!("Response body did not match the schema: {}", e)))

                                             ))
                        .map(move |body|
                            PlaceOrderResponse::SuccessfulOperation(body)
                        )
                    ) as Box<Future<Item=_, Error=_>>
                },
                400 => {
                    let body = response.body();
                    Box::new(

                        future::ok(
                            PlaceOrderResponse::InvalidOrder
                        )
                    ) as Box<Future<Item=_, Error=_>>
                },
                code => {
                    let headers = response.headers().clone();
                    Box::new(response.body()
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
                    ) as Box<Future<Item=_, Error=_>>
                }
            }
        }))

    }

    fn create_user(&self, param_body: models::User, context: &Context) -> Box<Future<Item=CreateUserResponse, Error=ApiError>> {


        let uri = format!(
            "{}/v2/user",
            self.base_path
        );

        let uri = match Uri::from_str(&uri) {
            Ok(uri) => uri,
            Err(err) => return Box::new(futures::done(Err(ApiError(format!("Unable to build URI: {}", err))))),
        };

        let mut request = hyper::Request::new(hyper::Method::Post, uri);


        let body = serde_json::to_string(&param_body).expect("impossible to fail to serialize");


        request.set_body(body.into_bytes());


        request.headers_mut().set(ContentType(mimetypes::requests::CREATE_USER.clone()));
        context.x_span_id.as_ref().map(|header| request.headers_mut().set(XSpanId(header.clone())));




        let hyper_client = (self.hyper_client)(&*self.handle);
        Box::new(hyper_client.call(request)
                             .map_err(|e| ApiError(format!("No response received: {}", e)))
                             .and_then(|mut response| {
            match response.status().as_u16() {
                0 => {
                    let body = response.body();
                    Box::new(

                        future::ok(
                            CreateUserResponse::SuccessfulOperation
                        )
                    ) as Box<Future<Item=_, Error=_>>
                },
                code => {
                    let headers = response.headers().clone();
                    Box::new(response.body()
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
                    ) as Box<Future<Item=_, Error=_>>
                }
            }
        }))

    }

    fn create_users_with_array_input(&self, param_body: &Vec<models::User>, context: &Context) -> Box<Future<Item=CreateUsersWithArrayInputResponse, Error=ApiError>> {


        let uri = format!(
            "{}/v2/user/createWithArray",
            self.base_path
        );

        let uri = match Uri::from_str(&uri) {
            Ok(uri) => uri,
            Err(err) => return Box::new(futures::done(Err(ApiError(format!("Unable to build URI: {}", err))))),
        };

        let mut request = hyper::Request::new(hyper::Method::Post, uri);


        let body = serde_json::to_string(&param_body).expect("impossible to fail to serialize");


        request.set_body(body.into_bytes());


        request.headers_mut().set(ContentType(mimetypes::requests::CREATE_USERS_WITH_ARRAY_INPUT.clone()));
        context.x_span_id.as_ref().map(|header| request.headers_mut().set(XSpanId(header.clone())));




        let hyper_client = (self.hyper_client)(&*self.handle);
        Box::new(hyper_client.call(request)
                             .map_err(|e| ApiError(format!("No response received: {}", e)))
                             .and_then(|mut response| {
            match response.status().as_u16() {
                0 => {
                    let body = response.body();
                    Box::new(

                        future::ok(
                            CreateUsersWithArrayInputResponse::SuccessfulOperation
                        )
                    ) as Box<Future<Item=_, Error=_>>
                },
                code => {
                    let headers = response.headers().clone();
                    Box::new(response.body()
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
                    ) as Box<Future<Item=_, Error=_>>
                }
            }
        }))

    }

    fn create_users_with_list_input(&self, param_body: &Vec<models::User>, context: &Context) -> Box<Future<Item=CreateUsersWithListInputResponse, Error=ApiError>> {


        let uri = format!(
            "{}/v2/user/createWithList",
            self.base_path
        );

        let uri = match Uri::from_str(&uri) {
            Ok(uri) => uri,
            Err(err) => return Box::new(futures::done(Err(ApiError(format!("Unable to build URI: {}", err))))),
        };

        let mut request = hyper::Request::new(hyper::Method::Post, uri);


        let body = serde_json::to_string(&param_body).expect("impossible to fail to serialize");


        request.set_body(body.into_bytes());


        request.headers_mut().set(ContentType(mimetypes::requests::CREATE_USERS_WITH_LIST_INPUT.clone()));
        context.x_span_id.as_ref().map(|header| request.headers_mut().set(XSpanId(header.clone())));




        let hyper_client = (self.hyper_client)(&*self.handle);
        Box::new(hyper_client.call(request)
                             .map_err(|e| ApiError(format!("No response received: {}", e)))
                             .and_then(|mut response| {
            match response.status().as_u16() {
                0 => {
                    let body = response.body();
                    Box::new(

                        future::ok(
                            CreateUsersWithListInputResponse::SuccessfulOperation
                        )
                    ) as Box<Future<Item=_, Error=_>>
                },
                code => {
                    let headers = response.headers().clone();
                    Box::new(response.body()
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
                    ) as Box<Future<Item=_, Error=_>>
                }
            }
        }))

    }

    fn delete_user(&self, param_username: String, context: &Context) -> Box<Future<Item=DeleteUserResponse, Error=ApiError>> {


        let uri = format!(
            "{}/v2/user/{username}",
            self.base_path, username=utf8_percent_encode(&param_username.to_string(), PATH_SEGMENT_ENCODE_SET)
        );

        let uri = match Uri::from_str(&uri) {
            Ok(uri) => uri,
            Err(err) => return Box::new(futures::done(Err(ApiError(format!("Unable to build URI: {}", err))))),
        };

        let mut request = hyper::Request::new(hyper::Method::Delete, uri);



        context.x_span_id.as_ref().map(|header| request.headers_mut().set(XSpanId(header.clone())));




        let hyper_client = (self.hyper_client)(&*self.handle);
        Box::new(hyper_client.call(request)
                             .map_err(|e| ApiError(format!("No response received: {}", e)))
                             .and_then(|mut response| {
            match response.status().as_u16() {
                400 => {
                    let body = response.body();
                    Box::new(

                        future::ok(
                            DeleteUserResponse::InvalidUsernameSupplied
                        )
                    ) as Box<Future<Item=_, Error=_>>
                },
                404 => {
                    let body = response.body();
                    Box::new(

                        future::ok(
                            DeleteUserResponse::UserNotFound
                        )
                    ) as Box<Future<Item=_, Error=_>>
                },
                code => {
                    let headers = response.headers().clone();
                    Box::new(response.body()
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
                    ) as Box<Future<Item=_, Error=_>>
                }
            }
        }))

    }

    fn get_user_by_name(&self, param_username: String, context: &Context) -> Box<Future<Item=GetUserByNameResponse, Error=ApiError>> {


        let uri = format!(
            "{}/v2/user/{username}",
            self.base_path, username=utf8_percent_encode(&param_username.to_string(), PATH_SEGMENT_ENCODE_SET)
        );

        let uri = match Uri::from_str(&uri) {
            Ok(uri) => uri,
            Err(err) => return Box::new(futures::done(Err(ApiError(format!("Unable to build URI: {}", err))))),
        };

        let mut request = hyper::Request::new(hyper::Method::Get, uri);



        context.x_span_id.as_ref().map(|header| request.headers_mut().set(XSpanId(header.clone())));




        let hyper_client = (self.hyper_client)(&*self.handle);
        Box::new(hyper_client.call(request)
                             .map_err(|e| ApiError(format!("No response received: {}", e)))
                             .and_then(|mut response| {
            match response.status().as_u16() {
                200 => {
                    let body = response.body();
                    Box::new(

                        body
                        .concat2()
                        .map_err(|e| ApiError(format!("Failed to read response: {}", e)))
                        .and_then(|body| str::from_utf8(&body)
                                             .map_err(|e| ApiError(format!("Response was not valid UTF8: {}", e)))
                                             .and_then(|body|

                                                 // ToDo: this will move to swagger-rs and become a standard From conversion trait
                                                 // once https://github.com/RReverser/serde-xml-rs/pull/45 is accepted upstream
                                                 serde_xml_rs::from_str::<models::User>(body)
                                                     .map_err(|e| ApiError(format!("Response body did not match the schema: {}", e)))

                                             ))
                        .map(move |body|
                            GetUserByNameResponse::SuccessfulOperation(body)
                        )
                    ) as Box<Future<Item=_, Error=_>>
                },
                400 => {
                    let body = response.body();
                    Box::new(

                        future::ok(
                            GetUserByNameResponse::InvalidUsernameSupplied
                        )
                    ) as Box<Future<Item=_, Error=_>>
                },
                404 => {
                    let body = response.body();
                    Box::new(

                        future::ok(
                            GetUserByNameResponse::UserNotFound
                        )
                    ) as Box<Future<Item=_, Error=_>>
                },
                code => {
                    let headers = response.headers().clone();
                    Box::new(response.body()
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
                    ) as Box<Future<Item=_, Error=_>>
                }
            }
        }))

    }

    fn login_user(&self, param_username: String, param_password: String, context: &Context) -> Box<Future<Item=LoginUserResponse, Error=ApiError>> {

        // Query parameters
        let query_username = format!("username={username}&", username=param_username.to_string());
        let query_password = format!("password={password}&", password=param_password.to_string());


        let uri = format!(
            "{}/v2/user/login?{username}{password}",
            self.base_path,
            username=utf8_percent_encode(&query_username, QUERY_ENCODE_SET),
            password=utf8_percent_encode(&query_password, QUERY_ENCODE_SET)
        );

        let uri = match Uri::from_str(&uri) {
            Ok(uri) => uri,
            Err(err) => return Box::new(futures::done(Err(ApiError(format!("Unable to build URI: {}", err))))),
        };

        let mut request = hyper::Request::new(hyper::Method::Get, uri);



        context.x_span_id.as_ref().map(|header| request.headers_mut().set(XSpanId(header.clone())));




        let hyper_client = (self.hyper_client)(&*self.handle);
        Box::new(hyper_client.call(request)
                             .map_err(|e| ApiError(format!("No response received: {}", e)))
                             .and_then(|mut response| {
            match response.status().as_u16() {
                200 => {
                    header! { (ResponseXRateLimit, "X-Rate-Limit") => [i32] }
                    let response_x_rate_limit = match response.headers().get::<ResponseXRateLimit>() {
                        Some(response_x_rate_limit) => response_x_rate_limit.0.clone(),
                        None => return Box::new(future::err(ApiError(String::from("Required response header X-Rate-Limit for response 200 was not found.")))) as Box<Future<Item=_, Error=_>>,
                    };
                    header! { (ResponseXExpiresAfter, "X-Expires-After") => [chrono::DateTime<chrono::Utc>] }
                    let response_x_expires_after = match response.headers().get::<ResponseXExpiresAfter>() {
                        Some(response_x_expires_after) => response_x_expires_after.0.clone(),
                        None => return Box::new(future::err(ApiError(String::from("Required response header X-Expires-After for response 200 was not found.")))) as Box<Future<Item=_, Error=_>>,
                    };
                    let body = response.body();
                    Box::new(

                        body
                        .concat2()
                        .map_err(|e| ApiError(format!("Failed to read response: {}", e)))
                        .and_then(|body| str::from_utf8(&body)
                                             .map_err(|e| ApiError(format!("Response was not valid UTF8: {}", e)))
                                             .and_then(|body|

                                                 // ToDo: this will move to swagger-rs and become a standard From conversion trait
                                                 // once https://github.com/RReverser/serde-xml-rs/pull/45 is accepted upstream
                                                 serde_xml_rs::from_str::<String>(body)
                                                     .map_err(|e| ApiError(format!("Response body did not match the schema: {}", e)))

                                             ))
                        .map(move |body|
                            LoginUserResponse::SuccessfulOperation{ body: body, x_rate_limit: response_x_rate_limit, x_expires_after: response_x_expires_after }
                        )
                    ) as Box<Future<Item=_, Error=_>>
                },
                400 => {
                    let body = response.body();
                    Box::new(

                        future::ok(
                            LoginUserResponse::InvalidUsername
                        )
                    ) as Box<Future<Item=_, Error=_>>
                },
                code => {
                    let headers = response.headers().clone();
                    Box::new(response.body()
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
                    ) as Box<Future<Item=_, Error=_>>
                }
            }
        }))

    }

    fn logout_user(&self, context: &Context) -> Box<Future<Item=LogoutUserResponse, Error=ApiError>> {


        let uri = format!(
            "{}/v2/user/logout",
            self.base_path
        );

        let uri = match Uri::from_str(&uri) {
            Ok(uri) => uri,
            Err(err) => return Box::new(futures::done(Err(ApiError(format!("Unable to build URI: {}", err))))),
        };

        let mut request = hyper::Request::new(hyper::Method::Get, uri);



        context.x_span_id.as_ref().map(|header| request.headers_mut().set(XSpanId(header.clone())));




        let hyper_client = (self.hyper_client)(&*self.handle);
        Box::new(hyper_client.call(request)
                             .map_err(|e| ApiError(format!("No response received: {}", e)))
                             .and_then(|mut response| {
            match response.status().as_u16() {
                0 => {
                    let body = response.body();
                    Box::new(

                        future::ok(
                            LogoutUserResponse::SuccessfulOperation
                        )
                    ) as Box<Future<Item=_, Error=_>>
                },
                code => {
                    let headers = response.headers().clone();
                    Box::new(response.body()
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
                    ) as Box<Future<Item=_, Error=_>>
                }
            }
        }))

    }

    fn update_user(&self, param_username: String, param_body: models::User, context: &Context) -> Box<Future<Item=UpdateUserResponse, Error=ApiError>> {


        let uri = format!(
            "{}/v2/user/{username}",
            self.base_path, username=utf8_percent_encode(&param_username.to_string(), PATH_SEGMENT_ENCODE_SET)
        );

        let uri = match Uri::from_str(&uri) {
            Ok(uri) => uri,
            Err(err) => return Box::new(futures::done(Err(ApiError(format!("Unable to build URI: {}", err))))),
        };

        let mut request = hyper::Request::new(hyper::Method::Put, uri);


        let body = serde_json::to_string(&param_body).expect("impossible to fail to serialize");


        request.set_body(body.into_bytes());


        request.headers_mut().set(ContentType(mimetypes::requests::UPDATE_USER.clone()));
        context.x_span_id.as_ref().map(|header| request.headers_mut().set(XSpanId(header.clone())));




        let hyper_client = (self.hyper_client)(&*self.handle);
        Box::new(hyper_client.call(request)
                             .map_err(|e| ApiError(format!("No response received: {}", e)))
                             .and_then(|mut response| {
            match response.status().as_u16() {
                400 => {
                    let body = response.body();
                    Box::new(

                        future::ok(
                            UpdateUserResponse::InvalidUserSupplied
                        )
                    ) as Box<Future<Item=_, Error=_>>
                },
                404 => {
                    let body = response.body();
                    Box::new(

                        future::ok(
                            UpdateUserResponse::UserNotFound
                        )
                    ) as Box<Future<Item=_, Error=_>>
                },
                code => {
                    let headers = response.headers().clone();
                    Box::new(response.body()
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
                    ) as Box<Future<Item=_, Error=_>>
                }
            }
        }))

    }

}

#[derive(Debug)]
pub enum ClientInitError {
    InvalidScheme,
    InvalidUri(hyper::error::UriError),
    MissingHost,
    SslError(openssl::error::ErrorStack)
}

impl From<hyper::error::UriError> for ClientInitError {
    fn from(err: hyper::error::UriError) -> ClientInitError {
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
        (self as &fmt::Debug).fmt(f)
    }
}

impl error::Error for ClientInitError {
    fn description(&self) -> &str {
        "Failed to produce a hyper client."
    }
}
