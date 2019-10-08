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

use mime::Mime;
use std::io::Cursor; 
use multipart::client::lazy::Multipart;
use crate::mimetypes;
use serde_json;
use serde_xml_rs;

#[allow(unused_imports)]
use std::collections::{HashMap, BTreeMap};

use openapi_context::{ApiError, XSpanId, Has, AuthData};

use crate::{
    Api
    ,
    TestSpecialTagsResponse
    ,
    FakeOuterBooleanSerializeResponse,
    FakeOuterCompositeSerializeResponse,
    FakeOuterNumberSerializeResponse,
    FakeOuterStringSerializeResponse,
    HyphenParamResponse,
    TestBodyWithQueryParamsResponse,
    TestClientModelResponse,
    TestEndpointParametersResponse,
    TestEnumParametersResponse,
    TestInlineAdditionalPropertiesResponse,
    TestJsonFormDataResponse
    ,
    TestClassnameResponse
    ,
    AddPetResponse,
    DeletePetResponse,
    FindPetsByStatusResponse,
    FindPetsByTagsResponse,
    GetPetByIdResponse,
    UpdatePetResponse,
    UpdatePetWithFormResponse,
    UploadFileResponse
    ,
    DeleteOrderResponse,
    GetInventoryResponse,
    GetOrderByIdResponse,
    PlaceOrderResponse
    ,
    CreateUserResponse,
    CreateUsersWithArrayInputResponse,
    CreateUsersWithListInputResponse,
    DeleteUserResponse,
    GetUserByNameResponse,
    LoginUserResponse,
    LogoutUserResponse,
    UpdateUserResponse
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
    Ctx: Has<XSpanId> + Has<Option<AuthData>> + Send + Sync + 'static,
    Conn: hyper::client::connect::Connect + Clone + Send + Sync + 'static,
{

    async fn test_special_tags(&mut self, param_body: crate::models::Client, context: &Ctx) -> Result<TestSpecialTagsResponse, ApiError> {
        let mut uri = format!(
            "{}/v2/another-fake/dummy",
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
        request = request.method(hyper::Method::PATCH);
        request = request.uri(uri);



        let body = hyper::Body::empty();
        // Body parameter
        let body = serde_json::to_string(&param_body).expect("impossible to fail to serialize");
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
                let body = serde_json::from_str::<crate::models::Client>(body)
                    .map_err(ApiError::from)?;

                        Ok(TestSpecialTagsResponse::SuccessfulOperation(body))
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

    async fn fake_outer_boolean_serialize(&mut self, param_body: Option<crate::models::OuterBoolean>, context: &Ctx) -> Result<FakeOuterBooleanSerializeResponse, ApiError> {
        let mut uri = format!(
            "{}/v2/fake/outer/boolean",
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
        // Body parameter
        let body = param_body.map(|ref body| {
            serde_json::to_string(body).expect("impossible to fail to serialize")
        });

        request.header(ContentType::from(mimetypes::requests::FAKE_OUTER_BOOLEAN_SERIALIZE.clone()));
        let body = if let Some(body) = body {
        let body = hyper::Body::from(body);
        } else {
            hyper::Body::empty()
        };

        request.headers_mut().unwrap().typed_insert(XSpanId( (context as &dyn Has<XSpanId>).get().0.clone() ));
        let request = request.body(body).map_err(|e| ApiError(format!("Failed to build request: {}", e)))?;
        let mut response = self.client_service.call(request)
                             .map_err(|e| ApiError(format!("No response received: {}", e))).await?;
        match response.status().as_u16() {
            200 => {
                let body = hyper::body::aggregate(response.into_body()).await.map_err(|e| ApiError(format!("Error getting response: {}", e)))?;
                let body = str::from_utf8(body.bytes())
                    .map_err(|e| ApiError(format!("Response was not valid UTF8: {}", e)))?;
                let body = serde_json::from_str::<bool>(body)
                    .map_err(ApiError::from)?;

                        Ok(FakeOuterBooleanSerializeResponse::OutputBoolean(body))
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

    async fn fake_outer_composite_serialize(&mut self, param_body: Option<crate::models::OuterComposite>, context: &Ctx) -> Result<FakeOuterCompositeSerializeResponse, ApiError> {
        let mut uri = format!(
            "{}/v2/fake/outer/composite",
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
        let body = param_body.map(|ref body| {
            serde_json::to_string(body).expect("impossible to fail to serialize")
        });

        request.header(ContentType::from(mimetypes::requests::FAKE_OUTER_COMPOSITE_SERIALIZE.clone()));
        let body = if let Some(body) = body {
        let body = hyper::Body::from(body);
        } else {
            hyper::Body::empty()
        };

        request.headers_mut().unwrap().typed_insert(XSpanId( (context as &dyn Has<XSpanId>).get().0.clone() ));
        let request = request.body(body).map_err(|e| ApiError(format!("Failed to build request: {}", e)))?;
        let mut response = self.client_service.call(request)
                             .map_err(|e| ApiError(format!("No response received: {}", e))).await?;
        match response.status().as_u16() {
            200 => {
                let body = hyper::body::aggregate(response.into_body()).await.map_err(|e| ApiError(format!("Error getting response: {}", e)))?;
                let body = str::from_utf8(body.bytes())
                    .map_err(|e| ApiError(format!("Response was not valid UTF8: {}", e)))?;
                let body = serde_json::from_str::<crate::models::OuterComposite>(body)
                    .map_err(ApiError::from)?;

                        Ok(FakeOuterCompositeSerializeResponse::OutputComposite(body))
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

    async fn fake_outer_number_serialize(&mut self, param_body: Option<crate::models::OuterNumber>, context: &Ctx) -> Result<FakeOuterNumberSerializeResponse, ApiError> {
        let mut uri = format!(
            "{}/v2/fake/outer/number",
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
        let body = param_body.map(|ref body| {
            serde_json::to_string(body).expect("impossible to fail to serialize")
        });

        request.header(ContentType::from(mimetypes::requests::FAKE_OUTER_NUMBER_SERIALIZE.clone()));
        let body = if let Some(body) = body {
        let body = hyper::Body::from(body);
        } else {
            hyper::Body::empty()
        };

        request.headers_mut().unwrap().typed_insert(XSpanId( (context as &dyn Has<XSpanId>).get().0.clone() ));
        let request = request.body(body).map_err(|e| ApiError(format!("Failed to build request: {}", e)))?;
        let mut response = self.client_service.call(request)
                             .map_err(|e| ApiError(format!("No response received: {}", e))).await?;
        match response.status().as_u16() {
            200 => {
                let body = hyper::body::aggregate(response.into_body()).await.map_err(|e| ApiError(format!("Error getting response: {}", e)))?;
                let body = str::from_utf8(body.bytes())
                    .map_err(|e| ApiError(format!("Response was not valid UTF8: {}", e)))?;
                let body = serde_json::from_str::<f64>(body)
                    .map_err(ApiError::from)?;

                        Ok(FakeOuterNumberSerializeResponse::OutputNumber(body))
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

    async fn fake_outer_string_serialize(&mut self, param_body: Option<crate::models::OuterString>, context: &Ctx) -> Result<FakeOuterStringSerializeResponse, ApiError> {
        let mut uri = format!(
            "{}/v2/fake/outer/string",
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
        let body = param_body.map(|ref body| {
            serde_json::to_string(body).expect("impossible to fail to serialize")
        });

        request.header(ContentType::from(mimetypes::requests::FAKE_OUTER_STRING_SERIALIZE.clone()));
        let body = if let Some(body) = body {
        let body = hyper::Body::from(body);
        } else {
            hyper::Body::empty()
        };

        request.headers_mut().unwrap().typed_insert(XSpanId( (context as &dyn Has<XSpanId>).get().0.clone() ));
        let request = request.body(body).map_err(|e| ApiError(format!("Failed to build request: {}", e)))?;
        let mut response = self.client_service.call(request)
                             .map_err(|e| ApiError(format!("No response received: {}", e))).await?;
        match response.status().as_u16() {
            200 => {
                let body = hyper::body::aggregate(response.into_body()).await.map_err(|e| ApiError(format!("Error getting response: {}", e)))?;
                let body = str::from_utf8(body.bytes())
                    .map_err(|e| ApiError(format!("Response was not valid UTF8: {}", e)))?;
                let body = serde_json::from_str::<String>(body)
                    .map_err(ApiError::from)?;

                        Ok(FakeOuterStringSerializeResponse::OutputString(body))
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

    async fn hyphen_param(&mut self, param_hyphen_param: String, context: &Ctx) -> Result<HyphenParamResponse, ApiError> {
        let mut uri = format!(
            "{}/v2/fake/hyphenParam/{hyphen_param}",
            self.base_path, hyphen_param=utf8_percent_encode(&param_hyphen_param.to_string(), ID_ENCODE_SET)
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
                    HyphenParamResponse::Success
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

    async fn test_body_with_query_params(&mut self, param_query: String, param_body: crate::models::User, context: &Ctx) -> Result<TestBodyWithQueryParamsResponse, ApiError> {
        let mut uri = format!(
            "{}/v2/fake/body-with-query-params",
            self.base_path
        );

        {
            let mut query_string = url::form_urlencoded::Serializer::new("".to_owned());
        query_string.append_pair("query", &param_query.to_string());


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
        let body = serde_json::to_string(&param_body).expect("impossible to fail to serialize");
        let body = hyper::Body::from(body);

        request.headers_mut().unwrap().typed_insert(XSpanId( (context as &dyn Has<XSpanId>).get().0.clone() ));
        let request = request.body(body).map_err(|e| ApiError(format!("Failed to build request: {}", e)))?;
        let mut response = self.client_service.call(request)
                             .map_err(|e| ApiError(format!("No response received: {}", e))).await?;
        match response.status().as_u16() {
            200 => {
                let body = hyper::body::aggregate(response.into_body()).await.map_err(|e| ApiError(format!("Error getting response: {}", e)))?;

                Ok(
                    TestBodyWithQueryParamsResponse::Success
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

    async fn test_client_model(&mut self, param_body: crate::models::Client, context: &Ctx) -> Result<TestClientModelResponse, ApiError> {
        let mut uri = format!(
            "{}/v2/fake",
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
        request = request.method(hyper::Method::PATCH);
        request = request.uri(uri);



        let body = hyper::Body::empty();
        let body = serde_json::to_string(&param_body).expect("impossible to fail to serialize");
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
                let body = serde_json::from_str::<crate::models::Client>(body)
                    .map_err(ApiError::from)?;

                        Ok(TestClientModelResponse::SuccessfulOperation(body))
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

    async fn test_endpoint_parameters(&mut self, param_number: f64, param_double: f64, param_pattern_without_delimiter: String, param_byte: openapi_context::ByteArray, param_integer: Option<i32>, param_int32: Option<i32>, param_int64: Option<i64>, param_float: Option<f32>, param_string: Option<String>, param_binary: Option<openapi_context::ByteArray>, param_date: Option<chrono::DateTime<chrono::Utc>>, param_date_time: Option<chrono::DateTime<chrono::Utc>>, param_password: Option<String>, param_callback: Option<String>, context: &Ctx) -> Result<TestEndpointParametersResponse, ApiError> {
        let mut uri = format!(
            "{}/v2/fake",
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


        if let Some(auth_data) = (context as &dyn Has<Option<AuthData>>).get().as_ref() {
            // Currently only authentication with Basic, API Key, and Bearer are supported
            match auth_data {
                &AuthData::Basic(ref basic_header) => {
                    request.headers_mut().unwrap().typed_insert(headers::Authorization::basic(
                        basic_header.clone(),
                    ).map_err(|e| ApiError(format!("Failed to build request: {}", e)))?)
                },
                _ => {}
            }
        }


        let body = hyper::Body::empty();
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

        request.headers_mut().unwrap().typed_insert(headers::ContentType::from(mimetypes::requests::TEST_ENDPOINT_PARAMETERS.clone()));
        let body = hyper::Body::from(body);
        let body = hyper::Body::empty();

        request.headers_mut().unwrap().typed_insert(XSpanId( (context as &dyn Has<XSpanId>).get().0.clone() ));

        (context as &dyn Has<Option<AuthData>>).get().as_ref().map(|auth_data| {
            // Currently only authentication with Basic, API Key, and Bearer are supported
            match auth_data {
                &AuthData::Basic(ref basic_header) => {
                    request.headers_mut().unwrap().typed_insert(headers::Authorization(
                        basic_header.0.clone(),
                    ))
                },
                _ => {}
            }
        });
        let request = request.body(body).map_err(|e| ApiError(format!("Failed to build request: {}", e)))?;
        let mut response = self.client_service.call(request)
                             .map_err(|e| ApiError(format!("No response received: {}", e))).await?;
        match response.status().as_u16() {
            400 => {
                let body = hyper::body::aggregate(response.into_body()).await.map_err(|e| ApiError(format!("Error getting response: {}", e)))?;

                Ok(
                    TestEndpointParametersResponse::InvalidUsernameSupplied
                )
            },
            404 => {
                let body = hyper::body::aggregate(response.into_body()).await.map_err(|e| ApiError(format!("Error getting response: {}", e)))?;

                Ok(
                    TestEndpointParametersResponse::UserNotFound
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

    async fn test_enum_parameters(&mut self, param_enum_header_string_array: Option<&Vec<String>>, param_enum_header_string: Option<String>, param_enum_query_string_array: Option<&Vec<String>>, param_enum_query_string: Option<String>, param_enum_query_integer: Option<i32>, param_enum_query_double: Option<f64>, param_enum_form_string: Option<String>, context: &Ctx) -> Result<TestEnumParametersResponse, ApiError> {
        let mut uri = format!(
            "{}/v2/fake",
            self.base_path
        );

        {
            let mut query_string = url::form_urlencoded::Serializer::new("".to_owned());

           if let Some(enum_query_string_array) = param_enum_query_string_array {
                query_string.append_pair("enum_query_string_array", &enum_query_string_array.join(","));
            }
           if let Some(enum_query_string) = param_enum_query_string {
                query_string.append_pair("enum_query_string", &enum_query_string.to_string());
            }
           if let Some(enum_query_integer) = param_enum_query_integer {
                query_string.append_pair("enum_query_integer", &enum_query_integer.to_string());
            }
           if let Some(enum_query_double) = param_enum_query_double {
                query_string.append_pair("enum_query_double", &enum_query_double.to_string());
            }

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



            // Header parameters
        
                param_enum_header_string_array.as_ref().map(|header| request.headers_mut().unwrap().typed_insert(RequestEnumHeaderStringArray(header.clone())));
        
                param_enum_header_string.as_ref().map(|header| request.headers_mut().unwrap().typed_insert(RequestEnumHeaderString(header.clone())));
        
        let body = hyper::Body::empty();
        let params = &[
            ("enum_form_string", param_enum_form_string),
        ];
        let body = serde_urlencoded::to_string(params).expect("impossible to fail to serialize");

        request.headers_mut().unwrap().typed_insert(headers::ContentType::from(mimetypes::requests::TEST_ENUM_PARAMETERS.clone()));
        let body = hyper::Body::from(body);
        let body = hyper::Body::empty();

        request.headers_mut().unwrap().typed_insert(XSpanId( (context as &dyn Has<XSpanId>).get().0.clone() ));

        // Header parameters
        param_enum_header_string_array.as_ref().map(|header| request.headers_mut().unwrap().typed_insert(RequestEnumHeaderStringArray(header.clone())));
        param_enum_header_string.as_ref().map(|header| request.headers_mut().unwrap().typed_insert(RequestEnumHeaderString(header.clone())));
        let request = request.body(body).map_err(|e| ApiError(format!("Failed to build request: {}", e)))?;
        let mut response = self.client_service.call(request)
                             .map_err(|e| ApiError(format!("No response received: {}", e))).await?;
        match response.status().as_u16() {
            400 => {
                let body = hyper::body::aggregate(response.into_body()).await.map_err(|e| ApiError(format!("Error getting response: {}", e)))?;

                Ok(
                    TestEnumParametersResponse::InvalidRequest
                )
            },
            404 => {
                let body = hyper::body::aggregate(response.into_body()).await.map_err(|e| ApiError(format!("Error getting response: {}", e)))?;

                Ok(
                    TestEnumParametersResponse::NotFound
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

    async fn test_inline_additional_properties(&mut self, param_param: HashMap<String, String>, context: &Ctx) -> Result<TestInlineAdditionalPropertiesResponse, ApiError> {
        let mut uri = format!(
            "{}/v2/fake/inline-additionalProperties",
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
        let body = serde_json::to_string(&param_param).expect("impossible to fail to serialize");
        let body = hyper::Body::from(body);

        request.headers_mut().unwrap().typed_insert(XSpanId( (context as &dyn Has<XSpanId>).get().0.clone() ));
        let request = request.body(body).map_err(|e| ApiError(format!("Failed to build request: {}", e)))?;
        let mut response = self.client_service.call(request)
                             .map_err(|e| ApiError(format!("No response received: {}", e))).await?;
        match response.status().as_u16() {
            200 => {
                let body = hyper::body::aggregate(response.into_body()).await.map_err(|e| ApiError(format!("Error getting response: {}", e)))?;

                Ok(
                    TestInlineAdditionalPropertiesResponse::SuccessfulOperation
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

    async fn test_json_form_data(&mut self, param_param: String, param_param2: String, context: &Ctx) -> Result<TestJsonFormDataResponse, ApiError> {
        let mut uri = format!(
            "{}/v2/fake/jsonFormData",
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
        let params = &[
            ("param", Some(param_param)),
            ("param2", Some(param_param2)),
        ];
        let body = serde_urlencoded::to_string(params).expect("impossible to fail to serialize");

        request.headers_mut().unwrap().typed_insert(headers::ContentType::from(mimetypes::requests::TEST_JSON_FORM_DATA.clone()));
        let body = hyper::Body::from(body);
        let body = hyper::Body::empty();

        request.headers_mut().unwrap().typed_insert(XSpanId( (context as &dyn Has<XSpanId>).get().0.clone() ));
        let request = request.body(body).map_err(|e| ApiError(format!("Failed to build request: {}", e)))?;
        let mut response = self.client_service.call(request)
                             .map_err(|e| ApiError(format!("No response received: {}", e))).await?;
        match response.status().as_u16() {
            200 => {
                let body = hyper::body::aggregate(response.into_body()).await.map_err(|e| ApiError(format!("Error getting response: {}", e)))?;

                Ok(
                    TestJsonFormDataResponse::SuccessfulOperation
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

    async fn test_classname(&mut self, param_body: crate::models::Client, context: &Ctx) -> Result<TestClassnameResponse, ApiError> {
        let mut uri = format!(
            "{}/v2/fake_classname_test",
            self.base_path
        );

        {
            let mut query_string = url::form_urlencoded::Serializer::new("".to_owned());

        if let Some(auth_data) = (context as &dyn Has<Option<AuthData>>).get().as_ref() {
            if let AuthData::ApiKey(ref api_key) = *auth_data {
                query_string.append_pair("api_key_query", api_key);
            }
        }
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
        request = request.method(hyper::Method::PATCH);
        request = request.uri(uri);



        let body = hyper::Body::empty();
        // Body parameter
        let body = serde_json::to_string(&param_body).expect("impossible to fail to serialize");
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
                let body = serde_json::from_str::<crate::models::Client>(body)
                    .map_err(ApiError::from)?;

                        Ok(TestClassnameResponse::SuccessfulOperation(body))
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

    async fn add_pet(&mut self, param_body: crate::models::Pet, context: &Ctx) -> Result<AddPetResponse, ApiError> {
        let mut uri = format!(
            "{}/v2/pet",
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


        if let Some(auth_data) = (context as &dyn Has<Option<AuthData>>).get().as_ref() {
            // Currently only authentication with Basic, API Key, and Bearer are supported
            match auth_data {
                &AuthData::Bearer(ref bearer_header) => {
                    request.headers_mut().unwrap().typed_insert(headers::Authorization::bearer(
                        bearer_header.0.token(),
                    ).map_err(|e| ApiError(format!("Failed to build request: {}", e)))?)
                },
                _ => {}
            }
        }


        let body = hyper::Body::empty();
        // Body parameter
        let body = param_body.to_xml();
        let body = hyper::Body::from(body);

        request.headers_mut().unwrap().typed_insert(XSpanId( (context as &dyn Has<XSpanId>).get().0.clone() ));

        (context as &dyn Has<Option<AuthData>>).get().as_ref().map(|auth_data| {
            // Currently only authentication with Basic, API Key, and Bearer are supported
            match auth_data {
                &AuthData::Bearer(ref bearer_header) => {
                    request.headers_mut().unwrap().typed_insert(headers::Authorization(
                        bearer_header.0.clone(),
                    ))
                },
                _ => {}
            }
        });
        let request = request.body(body).map_err(|e| ApiError(format!("Failed to build request: {}", e)))?;
        let mut response = self.client_service.call(request)
                             .map_err(|e| ApiError(format!("No response received: {}", e))).await?;
        match response.status().as_u16() {
            405 => {
                let body = hyper::body::aggregate(response.into_body()).await.map_err(|e| ApiError(format!("Error getting response: {}", e)))?;

                Ok(
                    AddPetResponse::InvalidInput
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

    async fn delete_pet(&mut self, param_pet_id: i64, param_api_key: Option<String>, context: &Ctx) -> Result<DeletePetResponse, ApiError> {
        let mut uri = format!(
            "{}/v2/pet/{pet_id}",
            self.base_path, pet_id=utf8_percent_encode(&param_pet_id.to_string(), ID_ENCODE_SET)
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
        request = request.method(hyper::Method::DELETE);
        request = request.uri(uri);


        if let Some(auth_data) = (context as &dyn Has<Option<AuthData>>).get().as_ref() {
            // Currently only authentication with Basic, API Key, and Bearer are supported
            match auth_data {
                &AuthData::Bearer(ref bearer_header) => {
                    request.headers_mut().unwrap().typed_insert(headers::Authorization::bearer(
                        bearer_header.0.token(),
                    ).map_err(|e| ApiError(format!("Failed to build request: {}", e)))?)
                },
                _ => {}
            }
        }


            // Header parameters
        
                param_api_key.as_ref().map(|header| request.headers_mut().unwrap().typed_insert(RequestApiKey(header.clone())));
        
        let body = hyper::Body::empty();
        let body = hyper::Body::empty();

        request.headers_mut().unwrap().typed_insert(XSpanId( (context as &dyn Has<XSpanId>).get().0.clone() ));

        (context as &dyn Has<Option<AuthData>>).get().as_ref().map(|auth_data| {
            // Currently only authentication with Basic, API Key, and Bearer are supported
            match auth_data {
                &AuthData::Bearer(ref bearer_header) => {
                    request.headers_mut().unwrap().typed_insert(headers::Authorization(
                        bearer_header.0.clone(),
                    ))
                },
                _ => {}
            }
        });

        // Header parameters
        param_api_key.as_ref().map(|header| request.headers_mut().unwrap().typed_insert(RequestApiKey(header.clone())));
        let request = request.body(body).map_err(|e| ApiError(format!("Failed to build request: {}", e)))?;
        let mut response = self.client_service.call(request)
                             .map_err(|e| ApiError(format!("No response received: {}", e))).await?;
        match response.status().as_u16() {
            400 => {
                let body = hyper::body::aggregate(response.into_body()).await.map_err(|e| ApiError(format!("Error getting response: {}", e)))?;

                Ok(
                    DeletePetResponse::InvalidPetValue
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

    async fn find_pets_by_status(&mut self, param_status: &Vec<String>, context: &Ctx) -> Result<FindPetsByStatusResponse, ApiError> {
        let mut uri = format!(
            "{}/v2/pet/findByStatus",
            self.base_path
        );

        {
            let mut query_string = url::form_urlencoded::Serializer::new("".to_owned());
        query_string.append_pair("status", &param_status.join(","));


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


        if let Some(auth_data) = (context as &dyn Has<Option<AuthData>>).get().as_ref() {
            // Currently only authentication with Basic, API Key, and Bearer are supported
            match auth_data {
                &AuthData::Bearer(ref bearer_header) => {
                    request.headers_mut().unwrap().typed_insert(headers::Authorization::bearer(
                        bearer_header.0.token(),
                    ).map_err(|e| ApiError(format!("Failed to build request: {}", e)))?)
                },
                _ => {}
            }
        }


        let body = hyper::Body::empty();
        let body = hyper::Body::empty();

        request.headers_mut().unwrap().typed_insert(XSpanId( (context as &dyn Has<XSpanId>).get().0.clone() ));

        (context as &dyn Has<Option<AuthData>>).get().as_ref().map(|auth_data| {
            // Currently only authentication with Basic, API Key, and Bearer are supported
            match auth_data {
                &AuthData::Bearer(ref bearer_header) => {
                    request.headers_mut().unwrap().typed_insert(headers::Authorization(
                        bearer_header.0.clone(),
                    ))
                },
                _ => {}
            }
        });
        let request = request.body(body).map_err(|e| ApiError(format!("Failed to build request: {}", e)))?;
        let mut response = self.client_service.call(request)
                             .map_err(|e| ApiError(format!("No response received: {}", e))).await?;
        match response.status().as_u16() {
            200 => {
                let body = hyper::body::aggregate(response.into_body()).await.map_err(|e| ApiError(format!("Error getting response: {}", e)))?;
                let body = str::from_utf8(body.bytes())
                    .map_err(|e| ApiError(format!("Response was not valid UTF8: {}", e)))?;
                // ToDo: this will move to swagger-rs and become a standard From conversion trait
                // once https://github.com/RReverser/serde-xml-rs/pull/45 is accepted upstream
                let body = serde_xml_rs::from_str::<Vec<crate::models::Pet>>(body)
                    .map_err(|e| ApiError(format!("Response body did not match the schema: {}", e)))?;

                        Ok(FindPetsByStatusResponse::SuccessfulOperation(body))
            },
            400 => {
                let body = hyper::body::aggregate(response.into_body()).await.map_err(|e| ApiError(format!("Error getting response: {}", e)))?;

                Ok(
                    FindPetsByStatusResponse::InvalidStatusValue
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

    async fn find_pets_by_tags(&mut self, param_tags: &Vec<String>, context: &Ctx) -> Result<FindPetsByTagsResponse, ApiError> {
        let mut uri = format!(
            "{}/v2/pet/findByTags",
            self.base_path
        );

        {
            let mut query_string = url::form_urlencoded::Serializer::new("".to_owned());
        query_string.append_pair("tags", &param_tags.join(","));


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


        if let Some(auth_data) = (context as &dyn Has<Option<AuthData>>).get().as_ref() {
            // Currently only authentication with Basic, API Key, and Bearer are supported
            match auth_data {
                &AuthData::Bearer(ref bearer_header) => {
                    request.headers_mut().unwrap().typed_insert(headers::Authorization::bearer(
                        bearer_header.0.token(),
                    ).map_err(|e| ApiError(format!("Failed to build request: {}", e)))?)
                },
                _ => {}
            }
        }


        let body = hyper::Body::empty();
        let body = hyper::Body::empty();

        request.headers_mut().unwrap().typed_insert(XSpanId( (context as &dyn Has<XSpanId>).get().0.clone() ));

        (context as &dyn Has<Option<AuthData>>).get().as_ref().map(|auth_data| {
            // Currently only authentication with Basic, API Key, and Bearer are supported
            match auth_data {
                &AuthData::Bearer(ref bearer_header) => {
                    request.headers_mut().unwrap().typed_insert(headers::Authorization(
                        bearer_header.0.clone(),
                    ))
                },
                _ => {}
            }
        });
        let request = request.body(body).map_err(|e| ApiError(format!("Failed to build request: {}", e)))?;
        let mut response = self.client_service.call(request)
                             .map_err(|e| ApiError(format!("No response received: {}", e))).await?;
        match response.status().as_u16() {
            200 => {
                let body = hyper::body::aggregate(response.into_body()).await.map_err(|e| ApiError(format!("Error getting response: {}", e)))?;
                let body = str::from_utf8(body.bytes())
                    .map_err(|e| ApiError(format!("Response was not valid UTF8: {}", e)))?;
                // ToDo: this will move to swagger-rs and become a standard From conversion trait
                // once https://github.com/RReverser/serde-xml-rs/pull/45 is accepted upstream
                let body = serde_xml_rs::from_str::<Vec<crate::models::Pet>>(body)
                    .map_err(|e| ApiError(format!("Response body did not match the schema: {}", e)))?;

                        Ok(FindPetsByTagsResponse::SuccessfulOperation(body))
            },
            400 => {
                let body = hyper::body::aggregate(response.into_body()).await.map_err(|e| ApiError(format!("Error getting response: {}", e)))?;

                Ok(
                    FindPetsByTagsResponse::InvalidTagValue
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

    async fn get_pet_by_id(&mut self, param_pet_id: i64, context: &Ctx) -> Result<GetPetByIdResponse, ApiError> {
        let mut uri = format!(
            "{}/v2/pet/{pet_id}",
            self.base_path, pet_id=utf8_percent_encode(&param_pet_id.to_string(), ID_ENCODE_SET)
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


        if let Some(auth_data) = (context as &dyn Has<Option<AuthData>>).get().as_ref() {
            // Currently only authentication with Basic, API Key, and Bearer are supported
            match auth_data {
                &AuthData::ApiKey(ref api_key) => {
                    request.headers_mut().unwrap().typed_insert(
                        ApiKeyGetPetById1(api_key.to_string())
                    )
                },
                _ => {}
            }
        }


        let body = hyper::Body::empty();
        let body = hyper::Body::empty();

        request.headers_mut().unwrap().typed_insert(XSpanId( (context as &dyn Has<XSpanId>).get().0.clone() ));

        (context as &dyn Has<Option<AuthData>>).get().as_ref().map(|auth_data| {
            // Currently only authentication with Basic, API Key, and Bearer are supported
            match auth_data {
                &AuthData::ApiKey(ref api_key) => {
                    request.headers_mut().unwrap().typed_insert(
                        ApiKey1(api_key.to_string())
                    )
                },
                _ => {}
            }
        });
        let request = request.body(body).map_err(|e| ApiError(format!("Failed to build request: {}", e)))?;
        let mut response = self.client_service.call(request)
                             .map_err(|e| ApiError(format!("No response received: {}", e))).await?;
        match response.status().as_u16() {
            200 => {
                let body = hyper::body::aggregate(response.into_body()).await.map_err(|e| ApiError(format!("Error getting response: {}", e)))?;
                let body = str::from_utf8(body.bytes())
                    .map_err(|e| ApiError(format!("Response was not valid UTF8: {}", e)))?;
                // ToDo: this will move to swagger-rs and become a standard From conversion trait
                // once https://github.com/RReverser/serde-xml-rs/pull/45 is accepted upstream
                let body = serde_xml_rs::from_str::<crate::models::Pet>(body)
                    .map_err(|e| ApiError(format!("Response body did not match the schema: {}", e)))?;

                        Ok(GetPetByIdResponse::SuccessfulOperation(body))
            },
            400 => {
                let body = hyper::body::aggregate(response.into_body()).await.map_err(|e| ApiError(format!("Error getting response: {}", e)))?;

                Ok(
                    GetPetByIdResponse::InvalidIDSupplied
                )
            },
            404 => {
                let body = hyper::body::aggregate(response.into_body()).await.map_err(|e| ApiError(format!("Error getting response: {}", e)))?;

                Ok(
                    GetPetByIdResponse::PetNotFound
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

    async fn update_pet(&mut self, param_body: crate::models::Pet, context: &Ctx) -> Result<UpdatePetResponse, ApiError> {
        let mut uri = format!(
            "{}/v2/pet",
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


        if let Some(auth_data) = (context as &dyn Has<Option<AuthData>>).get().as_ref() {
            // Currently only authentication with Basic, API Key, and Bearer are supported
            match auth_data {
                &AuthData::Bearer(ref bearer_header) => {
                    request.headers_mut().unwrap().typed_insert(headers::Authorization::bearer(
                        bearer_header.0.token(),
                    ).map_err(|e| ApiError(format!("Failed to build request: {}", e)))?)
                },
                _ => {}
            }
        }


        let body = hyper::Body::empty();
        let body = param_body.to_xml();
        let body = hyper::Body::from(body);

        request.headers_mut().unwrap().typed_insert(XSpanId( (context as &dyn Has<XSpanId>).get().0.clone() ));

        (context as &dyn Has<Option<AuthData>>).get().as_ref().map(|auth_data| {
            // Currently only authentication with Basic, API Key, and Bearer are supported
            match auth_data {
                &AuthData::Bearer(ref bearer_header) => {
                    request.headers_mut().unwrap().typed_insert(headers::Authorization(
                        bearer_header.0.clone(),
                    ))
                },
                _ => {}
            }
        });
        let request = request.body(body).map_err(|e| ApiError(format!("Failed to build request: {}", e)))?;
        let mut response = self.client_service.call(request)
                             .map_err(|e| ApiError(format!("No response received: {}", e))).await?;
        match response.status().as_u16() {
            400 => {
                let body = hyper::body::aggregate(response.into_body()).await.map_err(|e| ApiError(format!("Error getting response: {}", e)))?;

                Ok(
                    UpdatePetResponse::InvalidIDSupplied
                )
            },
            404 => {
                let body = hyper::body::aggregate(response.into_body()).await.map_err(|e| ApiError(format!("Error getting response: {}", e)))?;

                Ok(
                    UpdatePetResponse::PetNotFound
                )
            },
            405 => {
                let body = hyper::body::aggregate(response.into_body()).await.map_err(|e| ApiError(format!("Error getting response: {}", e)))?;

                Ok(
                    UpdatePetResponse::ValidationException
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

    async fn update_pet_with_form(&mut self, param_pet_id: i64, param_name: Option<String>, param_status: Option<String>, context: &Ctx) -> Result<UpdatePetWithFormResponse, ApiError> {
        let mut uri = format!(
            "{}/v2/pet/{pet_id}",
            self.base_path, pet_id=utf8_percent_encode(&param_pet_id.to_string(), ID_ENCODE_SET)
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


        if let Some(auth_data) = (context as &dyn Has<Option<AuthData>>).get().as_ref() {
            // Currently only authentication with Basic, API Key, and Bearer are supported
            match auth_data {
                &AuthData::Bearer(ref bearer_header) => {
                    request.headers_mut().unwrap().typed_insert(headers::Authorization::bearer(
                        bearer_header.0.token(),
                    ).map_err(|e| ApiError(format!("Failed to build request: {}", e)))?)
                },
                _ => {}
            }
        }


        let body = hyper::Body::empty();
        let params = &[
            ("name", param_name),
            ("status", param_status),
        ];
        let body = serde_urlencoded::to_string(params).expect("impossible to fail to serialize");

        request.headers_mut().unwrap().typed_insert(headers::ContentType::from(mimetypes::requests::UPDATE_PET_WITH_FORM.clone()));
        let body = hyper::Body::from(body);
        let body = hyper::Body::empty();

        request.headers_mut().unwrap().typed_insert(XSpanId( (context as &dyn Has<XSpanId>).get().0.clone() ));

        (context as &dyn Has<Option<AuthData>>).get().as_ref().map(|auth_data| {
            // Currently only authentication with Basic, API Key, and Bearer are supported
            match auth_data {
                &AuthData::Bearer(ref bearer_header) => {
                    request.headers_mut().unwrap().typed_insert(headers::Authorization(
                        bearer_header.0.clone(),
                    ))
                },
                _ => {}
            }
        });
        let request = request.body(body).map_err(|e| ApiError(format!("Failed to build request: {}", e)))?;
        let mut response = self.client_service.call(request)
                             .map_err(|e| ApiError(format!("No response received: {}", e))).await?;
        match response.status().as_u16() {
            405 => {
                let body = hyper::body::aggregate(response.into_body()).await.map_err(|e| ApiError(format!("Error getting response: {}", e)))?;

                Ok(
                    UpdatePetWithFormResponse::InvalidInput
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

    async fn upload_file(&mut self, param_pet_id: i64, param_additional_metadata: Option<String>, param_file: Option<openapi_context::ByteArray>, context: &Ctx) -> Result<UploadFileResponse, ApiError> {
        let mut uri = format!(
            "{}/v2/pet/{pet_id}/uploadImage",
            self.base_path, pet_id=utf8_percent_encode(&param_pet_id.to_string(), ID_ENCODE_SET)
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


        if let Some(auth_data) = (context as &dyn Has<Option<AuthData>>).get().as_ref() {
            // Currently only authentication with Basic, API Key, and Bearer are supported
            match auth_data {
                &AuthData::Bearer(ref bearer_header) => {
                    request.headers_mut().unwrap().typed_insert(headers::Authorization::bearer(
                        bearer_header.0.token(),
                    ).map_err(|e| ApiError(format!("Failed to build request: {}", e)))?)
                },
                _ => {}
            }
        }


        let mut body_string = String::new();
        let multipart_header = {
            let mut multipart = Multipart::new();

        // For each parameter, encode as appropriate and add to the multipart body as a stream.

            let additional_metadata_str = match serde_json::to_string(&param_additional_metadata) {
                Ok(str) => str,
                Err(e) => return Err(ApiError(format!("Unable to parse additional_metadata to string: {}", e))),
            };

            let additional_metadata_vec = additional_metadata_str.as_bytes().to_vec();

            let additional_metadata_mime = mime_02::Mime::from_str("application/json").expect("impossible to fail to parse");

            let additional_metadata_cursor = Cursor::new(additional_metadata_vec);

            multipart.add_stream("additional_metadata",  additional_metadata_cursor,  None as Option<&str>, Some(additional_metadata_mime));


            let file_str = match serde_json::to_string(&param_file) {
                Ok(str) => str,
                Err(e) => return Err(ApiError(format!("Unable to parse file to string: {}", e))),
            };

            let file_vec = file_str.as_bytes().to_vec();

            let file_mime = mime_02::Mime::from_str("application/json").expect("impossible to fail to parse");

            let file_cursor = Cursor::new(file_vec);

            multipart.add_stream("file",  file_cursor,  None as Option<&str>, Some(file_mime));


            let mut fields = match multipart.prepare() {
                Ok(fields) => fields,
                Err(err) => return Err(ApiError(format!("Unable to build request: {}", err))),
            };

            fields.to_body().read_to_string(&mut body_string).unwrap();
            let boundary = fields.boundary();

            match Mime::from_str(&format!("multipart/form-data;boundary={}", boundary)) {
                Ok(multipart_header) => multipart_header,
                Err(err) => return Err(ApiError(format!("Unable to build multipart header: {:?}", err))),
            }
        };

        request.headers_mut().unwrap().typed_insert(headers::ContentType::from(multipart_header));
        let body = hyper::Body::from(body_string);


        request.headers_mut().unwrap().typed_insert(XSpanId( (context as &dyn Has<XSpanId>).get().0.clone() ));

        (context as &dyn Has<Option<AuthData>>).get().as_ref().map(|auth_data| {
            // Currently only authentication with Basic, API Key, and Bearer are supported
            match auth_data {
                &AuthData::Bearer(ref bearer_header) => {
                    request.headers_mut().unwrap().typed_insert(headers::Authorization(
                        bearer_header.0.clone(),
                    ))
                },
                _ => {}
            }
        });
        let request = request.body(body).map_err(|e| ApiError(format!("Failed to build request: {}", e)))?;
        let mut response = self.client_service.call(request)
                             .map_err(|e| ApiError(format!("No response received: {}", e))).await?;
        match response.status().as_u16() {
            200 => {
                let body = hyper::body::aggregate(response.into_body()).await.map_err(|e| ApiError(format!("Error getting response: {}", e)))?;
                let body = str::from_utf8(body.bytes())
                    .map_err(|e| ApiError(format!("Response was not valid UTF8: {}", e)))?;
                let body = serde_json::from_str::<crate::models::ApiResponse>(body)
                    .map_err(ApiError::from)?;

                        Ok(UploadFileResponse::SuccessfulOperation(body))
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

    async fn delete_order(&mut self, param_order_id: String, context: &Ctx) -> Result<DeleteOrderResponse, ApiError> {
        let mut uri = format!(
            "{}/v2/store/order/{order_id}",
            self.base_path, order_id=utf8_percent_encode(&param_order_id.to_string(), ID_ENCODE_SET)
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
        request = request.method(hyper::Method::DELETE);
        request = request.uri(uri);



        let body = hyper::Body::empty();
        let body = hyper::Body::empty();

        request.headers_mut().unwrap().typed_insert(XSpanId( (context as &dyn Has<XSpanId>).get().0.clone() ));
        let request = request.body(body).map_err(|e| ApiError(format!("Failed to build request: {}", e)))?;
        let mut response = self.client_service.call(request)
                             .map_err(|e| ApiError(format!("No response received: {}", e))).await?;
        match response.status().as_u16() {
            400 => {
                let body = hyper::body::aggregate(response.into_body()).await.map_err(|e| ApiError(format!("Error getting response: {}", e)))?;

                Ok(
                    DeleteOrderResponse::InvalidIDSupplied
                )
            },
            404 => {
                let body = hyper::body::aggregate(response.into_body()).await.map_err(|e| ApiError(format!("Error getting response: {}", e)))?;

                Ok(
                    DeleteOrderResponse::OrderNotFound
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

    async fn get_inventory(&mut self, context: &Ctx) -> Result<GetInventoryResponse, ApiError> {
        let mut uri = format!(
            "{}/v2/store/inventory",
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


        if let Some(auth_data) = (context as &dyn Has<Option<AuthData>>).get().as_ref() {
            // Currently only authentication with Basic, API Key, and Bearer are supported
            match auth_data {
                &AuthData::ApiKey(ref api_key) => {
                    request.headers_mut().unwrap().typed_insert(
                        ApiKeyGetInventory1(api_key.to_string())
                    )
                },
                _ => {}
            }
        }


        let body = hyper::Body::empty();
        let body = hyper::Body::empty();

        request.headers_mut().unwrap().typed_insert(XSpanId( (context as &dyn Has<XSpanId>).get().0.clone() ));

        (context as &dyn Has<Option<AuthData>>).get().as_ref().map(|auth_data| {
            // Currently only authentication with Basic, API Key, and Bearer are supported
            match auth_data {
                &AuthData::ApiKey(ref api_key) => {
                    request.headers_mut().unwrap().typed_insert(
                        ApiKey1(api_key.to_string())
                    )
                },
                _ => {}
            }
        });
        let request = request.body(body).map_err(|e| ApiError(format!("Failed to build request: {}", e)))?;
        let mut response = self.client_service.call(request)
                             .map_err(|e| ApiError(format!("No response received: {}", e))).await?;
        match response.status().as_u16() {
            200 => {
                let body = hyper::body::aggregate(response.into_body()).await.map_err(|e| ApiError(format!("Error getting response: {}", e)))?;
                let body = str::from_utf8(body.bytes())
                    .map_err(|e| ApiError(format!("Response was not valid UTF8: {}", e)))?;
                let body = serde_json::from_str::<HashMap<String, i32>>(body)
                    .map_err(ApiError::from)?;

                        Ok(GetInventoryResponse::SuccessfulOperation(body))
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

    async fn get_order_by_id(&mut self, param_order_id: i64, context: &Ctx) -> Result<GetOrderByIdResponse, ApiError> {
        let mut uri = format!(
            "{}/v2/store/order/{order_id}",
            self.base_path, order_id=utf8_percent_encode(&param_order_id.to_string(), ID_ENCODE_SET)
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
                // ToDo: this will move to swagger-rs and become a standard From conversion trait
                // once https://github.com/RReverser/serde-xml-rs/pull/45 is accepted upstream
                let body = serde_xml_rs::from_str::<crate::models::Order>(body)
                    .map_err(|e| ApiError(format!("Response body did not match the schema: {}", e)))?;

                        Ok(GetOrderByIdResponse::SuccessfulOperation(body))
            },
            400 => {
                let body = hyper::body::aggregate(response.into_body()).await.map_err(|e| ApiError(format!("Error getting response: {}", e)))?;

                Ok(
                    GetOrderByIdResponse::InvalidIDSupplied
                )
            },
            404 => {
                let body = hyper::body::aggregate(response.into_body()).await.map_err(|e| ApiError(format!("Error getting response: {}", e)))?;

                Ok(
                    GetOrderByIdResponse::OrderNotFound
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

    async fn place_order(&mut self, param_body: crate::models::Order, context: &Ctx) -> Result<PlaceOrderResponse, ApiError> {
        let mut uri = format!(
            "{}/v2/store/order",
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
        let body = serde_json::to_string(&param_body).expect("impossible to fail to serialize");
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
                // ToDo: this will move to swagger-rs and become a standard From conversion trait
                // once https://github.com/RReverser/serde-xml-rs/pull/45 is accepted upstream
                let body = serde_xml_rs::from_str::<crate::models::Order>(body)
                    .map_err(|e| ApiError(format!("Response body did not match the schema: {}", e)))?;

                        Ok(PlaceOrderResponse::SuccessfulOperation(body))
            },
            400 => {
                let body = hyper::body::aggregate(response.into_body()).await.map_err(|e| ApiError(format!("Error getting response: {}", e)))?;

                Ok(
                    PlaceOrderResponse::InvalidOrder
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

    async fn create_user(&mut self, param_body: crate::models::User, context: &Ctx) -> Result<CreateUserResponse, ApiError> {
        let mut uri = format!(
            "{}/v2/user",
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
        // Body parameter
        let body = serde_json::to_string(&param_body).expect("impossible to fail to serialize");
        let body = hyper::Body::from(body);

        request.headers_mut().unwrap().typed_insert(XSpanId( (context as &dyn Has<XSpanId>).get().0.clone() ));
        let request = request.body(body).map_err(|e| ApiError(format!("Failed to build request: {}", e)))?;
        let mut response = self.client_service.call(request)
                             .map_err(|e| ApiError(format!("No response received: {}", e))).await?;
        match response.status().as_u16() {
            0 => {
                let body = hyper::body::aggregate(response.into_body()).await.map_err(|e| ApiError(format!("Error getting response: {}", e)))?;

                Ok(
                    CreateUserResponse::SuccessfulOperation
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

    async fn create_users_with_array_input(&mut self, param_body: &Vec<crate::models::User>, context: &Ctx) -> Result<CreateUsersWithArrayInputResponse, ApiError> {
        let mut uri = format!(
            "{}/v2/user/createWithArray",
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
        let body = serde_json::to_string(&param_body).expect("impossible to fail to serialize");
        let body = hyper::Body::from(body);

        request.headers_mut().unwrap().typed_insert(XSpanId( (context as &dyn Has<XSpanId>).get().0.clone() ));
        let request = request.body(body).map_err(|e| ApiError(format!("Failed to build request: {}", e)))?;
        let mut response = self.client_service.call(request)
                             .map_err(|e| ApiError(format!("No response received: {}", e))).await?;
        match response.status().as_u16() {
            0 => {
                let body = hyper::body::aggregate(response.into_body()).await.map_err(|e| ApiError(format!("Error getting response: {}", e)))?;

                Ok(
                    CreateUsersWithArrayInputResponse::SuccessfulOperation
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

    async fn create_users_with_list_input(&mut self, param_body: &Vec<crate::models::User>, context: &Ctx) -> Result<CreateUsersWithListInputResponse, ApiError> {
        let mut uri = format!(
            "{}/v2/user/createWithList",
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
        let body = serde_json::to_string(&param_body).expect("impossible to fail to serialize");
        let body = hyper::Body::from(body);

        request.headers_mut().unwrap().typed_insert(XSpanId( (context as &dyn Has<XSpanId>).get().0.clone() ));
        let request = request.body(body).map_err(|e| ApiError(format!("Failed to build request: {}", e)))?;
        let mut response = self.client_service.call(request)
                             .map_err(|e| ApiError(format!("No response received: {}", e))).await?;
        match response.status().as_u16() {
            0 => {
                let body = hyper::body::aggregate(response.into_body()).await.map_err(|e| ApiError(format!("Error getting response: {}", e)))?;

                Ok(
                    CreateUsersWithListInputResponse::SuccessfulOperation
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

    async fn delete_user(&mut self, param_username: String, context: &Ctx) -> Result<DeleteUserResponse, ApiError> {
        let mut uri = format!(
            "{}/v2/user/{username}",
            self.base_path, username=utf8_percent_encode(&param_username.to_string(), ID_ENCODE_SET)
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
        request = request.method(hyper::Method::DELETE);
        request = request.uri(uri);



        let body = hyper::Body::empty();
        let body = hyper::Body::empty();

        request.headers_mut().unwrap().typed_insert(XSpanId( (context as &dyn Has<XSpanId>).get().0.clone() ));
        let request = request.body(body).map_err(|e| ApiError(format!("Failed to build request: {}", e)))?;
        let mut response = self.client_service.call(request)
                             .map_err(|e| ApiError(format!("No response received: {}", e))).await?;
        match response.status().as_u16() {
            400 => {
                let body = hyper::body::aggregate(response.into_body()).await.map_err(|e| ApiError(format!("Error getting response: {}", e)))?;

                Ok(
                    DeleteUserResponse::InvalidUsernameSupplied
                )
            },
            404 => {
                let body = hyper::body::aggregate(response.into_body()).await.map_err(|e| ApiError(format!("Error getting response: {}", e)))?;

                Ok(
                    DeleteUserResponse::UserNotFound
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

    async fn get_user_by_name(&mut self, param_username: String, context: &Ctx) -> Result<GetUserByNameResponse, ApiError> {
        let mut uri = format!(
            "{}/v2/user/{username}",
            self.base_path, username=utf8_percent_encode(&param_username.to_string(), ID_ENCODE_SET)
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
                // ToDo: this will move to swagger-rs and become a standard From conversion trait
                // once https://github.com/RReverser/serde-xml-rs/pull/45 is accepted upstream
                let body = serde_xml_rs::from_str::<crate::models::User>(body)
                    .map_err(|e| ApiError(format!("Response body did not match the schema: {}", e)))?;

                        Ok(GetUserByNameResponse::SuccessfulOperation(body))
            },
            400 => {
                let body = hyper::body::aggregate(response.into_body()).await.map_err(|e| ApiError(format!("Error getting response: {}", e)))?;

                Ok(
                    GetUserByNameResponse::InvalidUsernameSupplied
                )
            },
            404 => {
                let body = hyper::body::aggregate(response.into_body()).await.map_err(|e| ApiError(format!("Error getting response: {}", e)))?;

                Ok(
                    GetUserByNameResponse::UserNotFound
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

    async fn login_user(&mut self, param_username: String, param_password: String, context: &Ctx) -> Result<LoginUserResponse, ApiError> {
        let mut uri = format!(
            "{}/v2/user/login",
            self.base_path
        );

        {
            let mut query_string = url::form_urlencoded::Serializer::new("".to_owned());
        query_string.append_pair("username", &param_username.to_string());
        query_string.append_pair("password", &param_password.to_string());


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
                let response_x_rate_limit = match response.headers().typed_get::<ResponseXRateLimit>() {
                    Some(response_x_rate_limit) => response_x_rate_limit.0.clone(),
                    None => return Err(ApiError(String::from("Required response header X-Rate-Limit for response 200 was not found."))),
                };
                let response_x_expires_after = match response.headers().typed_get::<ResponseXExpiresAfter>() {
                    Some(response_x_expires_after) => response_x_expires_after.0.clone(),
                    None => return Err(ApiError(String::from("Required response header X-Expires-After for response 200 was not found."))),
                };
                let body = hyper::body::aggregate(response.into_body()).await.map_err(|e| ApiError(format!("Error getting response: {}", e)))?;
                let body = str::from_utf8(body.bytes())
                    .map_err(|e| ApiError(format!("Response was not valid UTF8: {}", e)))?;
                // ToDo: this will move to swagger-rs and become a standard From conversion trait
                // once https://github.com/RReverser/serde-xml-rs/pull/45 is accepted upstream
                let body = serde_xml_rs::from_str::<String>(body)
                    .map_err(|e| ApiError(format!("Response body did not match the schema: {}", e)))?;

                        Ok(LoginUserResponse::SuccessfulOperation{ body: body, x_rate_limit: response_x_rate_limit, x_expires_after: response_x_expires_after })
            },
            400 => {
                let body = hyper::body::aggregate(response.into_body()).await.map_err(|e| ApiError(format!("Error getting response: {}", e)))?;

                Ok(
                    LoginUserResponse::InvalidUsername
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

    async fn logout_user(&mut self, context: &Ctx) -> Result<LogoutUserResponse, ApiError> {
        let mut uri = format!(
            "{}/v2/user/logout",
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
            0 => {
                let body = hyper::body::aggregate(response.into_body()).await.map_err(|e| ApiError(format!("Error getting response: {}", e)))?;

                Ok(
                    LogoutUserResponse::SuccessfulOperation
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

    async fn update_user(&mut self, param_username: String, param_body: crate::models::User, context: &Ctx) -> Result<UpdateUserResponse, ApiError> {
        let mut uri = format!(
            "{}/v2/user/{username}",
            self.base_path, username=utf8_percent_encode(&param_username.to_string(), ID_ENCODE_SET)
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
        let body = serde_json::to_string(&param_body).expect("impossible to fail to serialize");
        let body = hyper::Body::from(body);

        request.headers_mut().unwrap().typed_insert(XSpanId( (context as &dyn Has<XSpanId>).get().0.clone() ));
        let request = request.body(body).map_err(|e| ApiError(format!("Failed to build request: {}", e)))?;
        let mut response = self.client_service.call(request)
                             .map_err(|e| ApiError(format!("No response received: {}", e))).await?;
        match response.status().as_u16() {
            400 => {
                let body = hyper::body::aggregate(response.into_body()).await.map_err(|e| ApiError(format!("Error getting response: {}", e)))?;

                Ok(
                    UpdateUserResponse::InvalidUserSupplied
                )
            },
            404 => {
                let body = hyper::body::aggregate(response.into_body()).await.map_err(|e| ApiError(format!("Error getting response: {}", e)))?;

                Ok(
                    UpdateUserResponse::UserNotFound
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
