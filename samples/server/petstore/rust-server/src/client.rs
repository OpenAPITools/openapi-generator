#![allow(unused_extern_crates)]
extern crate hyper_openssl;
extern crate chrono;
extern crate url;
extern crate multipart;

use multipart::client::lazy::Multipart;
use hyper;
use hyper::client::IntoUrl;
use hyper::mime;
use hyper::header::{Headers, ContentType};
use hyper::mime::{Mime, TopLevel, SubLevel, Attr, Value};
use hyper::Url;
use self::hyper_openssl::openssl;
use self::url::percent_encoding::{utf8_percent_encode, PATH_SEGMENT_ENCODE_SET, QUERY_ENCODE_SET};
use futures;
use futures::{Future, Stream};
use futures::{future, stream};
use std::borrow::Cow;
use std::io::{Read, Error};
use std::error;
use std::fmt;
use std::path::Path;
use std::sync::Arc;
use std::str;

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
fn into_base_path<T: IntoUrl>(input: T, correct_scheme: Option<&'static str>) -> Result<String, ClientInitError> {
    // First convert to Url, since a base path is a subset of Url.
    let url = input.into_url()?;

    let scheme = url.scheme();

    // Check the scheme if necessary
    if let Some(correct_scheme) = correct_scheme {
        if scheme != correct_scheme {
            return Err(ClientInitError::InvalidScheme);
        }
    }

    let host = url.host().ok_or_else(|| ClientInitError::MissingHost)?;
    let port = url.port().map(|x| format!(":{}", x)).unwrap_or_default();
    Ok(format!("{}://{}{}", scheme, host, port))
}

/// A client that implements the API by making HTTP calls out to a server.
#[derive(Clone)]
pub struct Client {
    base_path: String,
    hyper_client: Arc<Fn() -> hyper::client::Client + Sync + Send>,
}

impl fmt::Debug for Client {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Client {{ base_path: {} }}", self.base_path)
    }
}

impl Client {
    pub fn try_new_http<T>(base_path: T) -> Result<Client, ClientInitError>
        where T: IntoUrl
    {
        Ok(Client {
            base_path: into_base_path(base_path, Some("http"))?,
            hyper_client: Arc::new(hyper::client::Client::new),
        })
    }

    pub fn try_new_https<T, CA>(base_path: T,
                                ca_certificate: CA)
                            -> Result<Client, ClientInitError>
        where T: IntoUrl,
              CA: AsRef<Path>
    {
        let ca_certificate = ca_certificate.as_ref().to_owned();

        let https_hyper_client = move || {
            // SSL implementation
            let mut ssl = openssl::ssl::SslConnectorBuilder::new(openssl::ssl::SslMethod::tls()).unwrap();

            // Server authentication
            ssl.set_ca_file(ca_certificate.clone()).unwrap();

            let ssl = hyper_openssl::OpensslClient::from(ssl.build());
            let connector = hyper::net::HttpsConnector::new(ssl);
            hyper::client::Client::with_connector(connector)
        };

        Ok(Client {
                base_path: into_base_path(base_path, Some("https"))?,
                hyper_client: Arc::new(https_hyper_client),
            })
    }

    pub fn try_new_https_mutual<T, CA, K, C>(base_path: T,
                                             ca_certificate: CA,
                                             client_key: K,
                                             client_certificate: C)
                                             -> Result<Client, ClientInitError>
        where T: IntoUrl,
              CA: AsRef<Path>,
              K: AsRef<Path>,
              C: AsRef<Path>
    {
        let ca_certificate = ca_certificate.as_ref().to_owned();
        let client_key = client_key.as_ref().to_owned();
        let client_certificate = client_certificate.as_ref().to_owned();

        let https_mutual_hyper_client = move || {
            // SSL implementation
            let mut ssl = openssl::ssl::SslConnectorBuilder::new(openssl::ssl::SslMethod::tls()).unwrap();

            // Server authentication
            ssl.set_ca_file(ca_certificate.clone()).unwrap();

            // Client authentication
            ssl.set_private_key_file(client_key.clone(), openssl::x509::X509_FILETYPE_PEM).unwrap();
            ssl.set_certificate_chain_file(client_certificate.clone()).unwrap();
            ssl.check_private_key().unwrap();

            let ssl = hyper_openssl::OpensslClient::from(ssl.build());
            let connector = hyper::net::HttpsConnector::new(ssl);
            hyper::client::Client::with_connector(connector)
        };

        Ok(Client {
                base_path: into_base_path(base_path, Some("https"))?,
                hyper_client: Arc::new(https_mutual_hyper_client)
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
    pub fn try_new_with_hyper_client<T>(base_path: T,
                                    hyper_client: Arc<Fn() -> hyper::client::Client + Sync + Send>)
                                    -> Result<Client, ClientInitError>
        where T: IntoUrl
    {
        Ok(Client {
            base_path: into_base_path(base_path, None)?,
            hyper_client: hyper_client
        })
    }
}

impl Api for Client {

    fn test_special_tags(&self, param_body: models::Client, context: &Context) -> Box<Future<Item=TestSpecialTagsResponse, Error=ApiError> + Send> {


        let url = format!(
            "{}/v2/another-fake/dummy",
            self.base_path
        );


        let body = serde_json::to_string(&param_body).expect("impossible to fail to serialize");

        let hyper_client = (self.hyper_client)();
        let request = hyper_client.request(hyper::method::Method::Patch, &url);
        let mut custom_headers = hyper::header::Headers::new();

        let request = request.body(&body);

        custom_headers.set(ContentType(mimetypes::requests::TEST_SPECIAL_TAGS.clone()));
        context.x_span_id.as_ref().map(|header| custom_headers.set(XSpanId(header.clone())));


        let request = request.headers(custom_headers);

        // Helper function to provide a code block to use `?` in (to be replaced by the `catch` block when it exists).
        fn parse_response(mut response: hyper::client::response::Response) -> Result<TestSpecialTagsResponse, ApiError> {
            match response.status.to_u16() {
                200 => {
                    let mut buf = String::new();
                    response.read_to_string(&mut buf).map_err(|e| ApiError(format!("Response was not valid UTF8: {}", e)))?;
                    let body = serde_json::from_str::<models::Client>(&buf)?;

                    Ok(TestSpecialTagsResponse::SuccessfulOperation(body))
                },
                code => {
                    let mut buf = [0; 100];
                    let debug_body = match response.read(&mut buf) {
                        Ok(len) => match str::from_utf8(&buf[..len]) {
                            Ok(body) => Cow::from(body),
                            Err(_) => Cow::from(format!("<Body was not UTF8: {:?}>", &buf[..len].to_vec())),
                        },
                        Err(e) => Cow::from(format!("<Failed to read body: {}>", e)),
                    };
                    Err(ApiError(format!("Unexpected response code {}:\n{:?}\n\n{}",
                                         code,
                                         response.headers,
                                         debug_body)))
                }
            }
        }

        let result = request.send().map_err(|e| ApiError(format!("No response received: {}", e))).and_then(parse_response);
        Box::new(futures::done(result))
    }

    fn fake_outer_boolean_serialize(&self, param_body: Option<models::OuterBoolean>, context: &Context) -> Box<Future<Item=FakeOuterBooleanSerializeResponse, Error=ApiError> + Send> {


        let url = format!(
            "{}/v2/fake/outer/boolean",
            self.base_path
        );

        let body = param_body.map(|ref body| {

            serde_json::to_string(body).expect("impossible to fail to serialize")
        });
        let hyper_client = (self.hyper_client)();
        let request = hyper_client.request(hyper::method::Method::Post, &url);
        let mut custom_headers = hyper::header::Headers::new();

        let request = match body {
            Some(ref body) => request.body(body),
            None => request,
        };

        custom_headers.set(ContentType(mimetypes::requests::FAKE_OUTER_BOOLEAN_SERIALIZE.clone()));
        context.x_span_id.as_ref().map(|header| custom_headers.set(XSpanId(header.clone())));


        let request = request.headers(custom_headers);

        // Helper function to provide a code block to use `?` in (to be replaced by the `catch` block when it exists).
        fn parse_response(mut response: hyper::client::response::Response) -> Result<FakeOuterBooleanSerializeResponse, ApiError> {
            match response.status.to_u16() {
                200 => {
                    let mut buf = String::new();
                    response.read_to_string(&mut buf).map_err(|e| ApiError(format!("Response was not valid UTF8: {}", e)))?;
                    let body = serde_json::from_str::<models::OuterBoolean>(&buf)?;

                    Ok(FakeOuterBooleanSerializeResponse::OutputBoolean(body))
                },
                code => {
                    let mut buf = [0; 100];
                    let debug_body = match response.read(&mut buf) {
                        Ok(len) => match str::from_utf8(&buf[..len]) {
                            Ok(body) => Cow::from(body),
                            Err(_) => Cow::from(format!("<Body was not UTF8: {:?}>", &buf[..len].to_vec())),
                        },
                        Err(e) => Cow::from(format!("<Failed to read body: {}>", e)),
                    };
                    Err(ApiError(format!("Unexpected response code {}:\n{:?}\n\n{}",
                                         code,
                                         response.headers,
                                         debug_body)))
                }
            }
        }

        let result = request.send().map_err(|e| ApiError(format!("No response received: {}", e))).and_then(parse_response);
        Box::new(futures::done(result))
    }

    fn fake_outer_composite_serialize(&self, param_body: Option<models::OuterComposite>, context: &Context) -> Box<Future<Item=FakeOuterCompositeSerializeResponse, Error=ApiError> + Send> {


        let url = format!(
            "{}/v2/fake/outer/composite",
            self.base_path
        );

        let body = param_body.map(|ref body| {

            serde_json::to_string(body).expect("impossible to fail to serialize")
        });
        let hyper_client = (self.hyper_client)();
        let request = hyper_client.request(hyper::method::Method::Post, &url);
        let mut custom_headers = hyper::header::Headers::new();

        let request = match body {
            Some(ref body) => request.body(body),
            None => request,
        };

        custom_headers.set(ContentType(mimetypes::requests::FAKE_OUTER_COMPOSITE_SERIALIZE.clone()));
        context.x_span_id.as_ref().map(|header| custom_headers.set(XSpanId(header.clone())));


        let request = request.headers(custom_headers);

        // Helper function to provide a code block to use `?` in (to be replaced by the `catch` block when it exists).
        fn parse_response(mut response: hyper::client::response::Response) -> Result<FakeOuterCompositeSerializeResponse, ApiError> {
            match response.status.to_u16() {
                200 => {
                    let mut buf = String::new();
                    response.read_to_string(&mut buf).map_err(|e| ApiError(format!("Response was not valid UTF8: {}", e)))?;
                    let body = serde_json::from_str::<models::OuterComposite>(&buf)?;

                    Ok(FakeOuterCompositeSerializeResponse::OutputComposite(body))
                },
                code => {
                    let mut buf = [0; 100];
                    let debug_body = match response.read(&mut buf) {
                        Ok(len) => match str::from_utf8(&buf[..len]) {
                            Ok(body) => Cow::from(body),
                            Err(_) => Cow::from(format!("<Body was not UTF8: {:?}>", &buf[..len].to_vec())),
                        },
                        Err(e) => Cow::from(format!("<Failed to read body: {}>", e)),
                    };
                    Err(ApiError(format!("Unexpected response code {}:\n{:?}\n\n{}",
                                         code,
                                         response.headers,
                                         debug_body)))
                }
            }
        }

        let result = request.send().map_err(|e| ApiError(format!("No response received: {}", e))).and_then(parse_response);
        Box::new(futures::done(result))
    }

    fn fake_outer_number_serialize(&self, param_body: Option<models::OuterNumber>, context: &Context) -> Box<Future<Item=FakeOuterNumberSerializeResponse, Error=ApiError> + Send> {


        let url = format!(
            "{}/v2/fake/outer/number",
            self.base_path
        );

        let body = param_body.map(|ref body| {

            serde_json::to_string(body).expect("impossible to fail to serialize")
        });
        let hyper_client = (self.hyper_client)();
        let request = hyper_client.request(hyper::method::Method::Post, &url);
        let mut custom_headers = hyper::header::Headers::new();

        let request = match body {
            Some(ref body) => request.body(body),
            None => request,
        };

        custom_headers.set(ContentType(mimetypes::requests::FAKE_OUTER_NUMBER_SERIALIZE.clone()));
        context.x_span_id.as_ref().map(|header| custom_headers.set(XSpanId(header.clone())));


        let request = request.headers(custom_headers);

        // Helper function to provide a code block to use `?` in (to be replaced by the `catch` block when it exists).
        fn parse_response(mut response: hyper::client::response::Response) -> Result<FakeOuterNumberSerializeResponse, ApiError> {
            match response.status.to_u16() {
                200 => {
                    let mut buf = String::new();
                    response.read_to_string(&mut buf).map_err(|e| ApiError(format!("Response was not valid UTF8: {}", e)))?;
                    let body = serde_json::from_str::<models::OuterNumber>(&buf)?;

                    Ok(FakeOuterNumberSerializeResponse::OutputNumber(body))
                },
                code => {
                    let mut buf = [0; 100];
                    let debug_body = match response.read(&mut buf) {
                        Ok(len) => match str::from_utf8(&buf[..len]) {
                            Ok(body) => Cow::from(body),
                            Err(_) => Cow::from(format!("<Body was not UTF8: {:?}>", &buf[..len].to_vec())),
                        },
                        Err(e) => Cow::from(format!("<Failed to read body: {}>", e)),
                    };
                    Err(ApiError(format!("Unexpected response code {}:\n{:?}\n\n{}",
                                         code,
                                         response.headers,
                                         debug_body)))
                }
            }
        }

        let result = request.send().map_err(|e| ApiError(format!("No response received: {}", e))).and_then(parse_response);
        Box::new(futures::done(result))
    }

    fn fake_outer_string_serialize(&self, param_body: Option<models::OuterString>, context: &Context) -> Box<Future<Item=FakeOuterStringSerializeResponse, Error=ApiError> + Send> {


        let url = format!(
            "{}/v2/fake/outer/string",
            self.base_path
        );

        let body = param_body.map(|ref body| {

            serde_json::to_string(body).expect("impossible to fail to serialize")
        });
        let hyper_client = (self.hyper_client)();
        let request = hyper_client.request(hyper::method::Method::Post, &url);
        let mut custom_headers = hyper::header::Headers::new();

        let request = match body {
            Some(ref body) => request.body(body),
            None => request,
        };

        custom_headers.set(ContentType(mimetypes::requests::FAKE_OUTER_STRING_SERIALIZE.clone()));
        context.x_span_id.as_ref().map(|header| custom_headers.set(XSpanId(header.clone())));


        let request = request.headers(custom_headers);

        // Helper function to provide a code block to use `?` in (to be replaced by the `catch` block when it exists).
        fn parse_response(mut response: hyper::client::response::Response) -> Result<FakeOuterStringSerializeResponse, ApiError> {
            match response.status.to_u16() {
                200 => {
                    let mut buf = String::new();
                    response.read_to_string(&mut buf).map_err(|e| ApiError(format!("Response was not valid UTF8: {}", e)))?;
                    let body = serde_json::from_str::<models::OuterString>(&buf)?;

                    Ok(FakeOuterStringSerializeResponse::OutputString(body))
                },
                code => {
                    let mut buf = [0; 100];
                    let debug_body = match response.read(&mut buf) {
                        Ok(len) => match str::from_utf8(&buf[..len]) {
                            Ok(body) => Cow::from(body),
                            Err(_) => Cow::from(format!("<Body was not UTF8: {:?}>", &buf[..len].to_vec())),
                        },
                        Err(e) => Cow::from(format!("<Failed to read body: {}>", e)),
                    };
                    Err(ApiError(format!("Unexpected response code {}:\n{:?}\n\n{}",
                                         code,
                                         response.headers,
                                         debug_body)))
                }
            }
        }

        let result = request.send().map_err(|e| ApiError(format!("No response received: {}", e))).and_then(parse_response);
        Box::new(futures::done(result))
    }

    fn test_client_model(&self, param_body: models::Client, context: &Context) -> Box<Future<Item=TestClientModelResponse, Error=ApiError> + Send> {


        let url = format!(
            "{}/v2/fake",
            self.base_path
        );


        let body = serde_json::to_string(&param_body).expect("impossible to fail to serialize");

        let hyper_client = (self.hyper_client)();
        let request = hyper_client.request(hyper::method::Method::Patch, &url);
        let mut custom_headers = hyper::header::Headers::new();

        let request = request.body(&body);

        custom_headers.set(ContentType(mimetypes::requests::TEST_CLIENT_MODEL.clone()));
        context.x_span_id.as_ref().map(|header| custom_headers.set(XSpanId(header.clone())));


        let request = request.headers(custom_headers);

        // Helper function to provide a code block to use `?` in (to be replaced by the `catch` block when it exists).
        fn parse_response(mut response: hyper::client::response::Response) -> Result<TestClientModelResponse, ApiError> {
            match response.status.to_u16() {
                200 => {
                    let mut buf = String::new();
                    response.read_to_string(&mut buf).map_err(|e| ApiError(format!("Response was not valid UTF8: {}", e)))?;
                    let body = serde_json::from_str::<models::Client>(&buf)?;

                    Ok(TestClientModelResponse::SuccessfulOperation(body))
                },
                code => {
                    let mut buf = [0; 100];
                    let debug_body = match response.read(&mut buf) {
                        Ok(len) => match str::from_utf8(&buf[..len]) {
                            Ok(body) => Cow::from(body),
                            Err(_) => Cow::from(format!("<Body was not UTF8: {:?}>", &buf[..len].to_vec())),
                        },
                        Err(e) => Cow::from(format!("<Failed to read body: {}>", e)),
                    };
                    Err(ApiError(format!("Unexpected response code {}:\n{:?}\n\n{}",
                                         code,
                                         response.headers,
                                         debug_body)))
                }
            }
        }

        let result = request.send().map_err(|e| ApiError(format!("No response received: {}", e))).and_then(parse_response);
        Box::new(futures::done(result))
    }

    fn test_endpoint_parameters(&self, param_number: f64, param_double: f64, param_pattern_without_delimiter: String, param_byte: swagger::ByteArray, param_integer: Option<i32>, param_int32: Option<i32>, param_int64: Option<i64>, param_float: Option<f32>, param_string: Option<String>, param_binary: Option<swagger::ByteArray>, param_date: Option<chrono::DateTime<chrono::Utc>>, param_date_time: Option<chrono::DateTime<chrono::Utc>>, param_password: Option<String>, param_callback: Option<String>, context: &Context) -> Box<Future<Item=TestEndpointParametersResponse, Error=ApiError> + Send> {


        let url = format!(
            "{}/v2/fake",
            self.base_path
        );


        let hyper_client = (self.hyper_client)();
        let request = hyper_client.request(hyper::method::Method::Post, &url);
        let mut custom_headers = hyper::header::Headers::new();

        context.x_span_id.as_ref().map(|header| custom_headers.set(XSpanId(header.clone())));


        let request = request.headers(custom_headers);

        // Helper function to provide a code block to use `?` in (to be replaced by the `catch` block when it exists).
        fn parse_response(mut response: hyper::client::response::Response) -> Result<TestEndpointParametersResponse, ApiError> {
            match response.status.to_u16() {
                400 => {


                    Ok(TestEndpointParametersResponse::InvalidUsernameSupplied)
                },
                404 => {


                    Ok(TestEndpointParametersResponse::UserNotFound)
                },
                code => {
                    let mut buf = [0; 100];
                    let debug_body = match response.read(&mut buf) {
                        Ok(len) => match str::from_utf8(&buf[..len]) {
                            Ok(body) => Cow::from(body),
                            Err(_) => Cow::from(format!("<Body was not UTF8: {:?}>", &buf[..len].to_vec())),
                        },
                        Err(e) => Cow::from(format!("<Failed to read body: {}>", e)),
                    };
                    Err(ApiError(format!("Unexpected response code {}:\n{:?}\n\n{}",
                                         code,
                                         response.headers,
                                         debug_body)))
                }
            }
        }

        let result = request.send().map_err(|e| ApiError(format!("No response received: {}", e))).and_then(parse_response);
        Box::new(futures::done(result))
    }

    fn test_enum_parameters(&self, param_enum_form_string_array: Option<&Vec<String>>, param_enum_form_string: Option<String>, param_enum_header_string_array: Option<&Vec<String>>, param_enum_header_string: Option<String>, param_enum_query_string_array: Option<&Vec<String>>, param_enum_query_string: Option<String>, param_enum_query_integer: Option<i32>, param_enum_query_double: Option<f64>, context: &Context) -> Box<Future<Item=TestEnumParametersResponse, Error=ApiError> + Send> {

        // Query parameters
        let query_enum_query_string_array = param_enum_query_string_array.map_or_else(String::new, |query| format!("enum_query_string_array={enum_query_string_array}&", enum_query_string_array=query.join(",")));
        let query_enum_query_string = param_enum_query_string.map_or_else(String::new, |query| format!("enum_query_string={enum_query_string}&", enum_query_string=query.to_string()));
        let query_enum_query_integer = param_enum_query_integer.map_or_else(String::new, |query| format!("enum_query_integer={enum_query_integer}&", enum_query_integer=query.to_string()));


        let url = format!(
            "{}/v2/fake?{enum_query_string_array}{enum_query_string}{enum_query_integer}",
            self.base_path,
            enum_query_string_array=utf8_percent_encode(&query_enum_query_string_array, QUERY_ENCODE_SET),
            enum_query_string=utf8_percent_encode(&query_enum_query_string, QUERY_ENCODE_SET),
            enum_query_integer=utf8_percent_encode(&query_enum_query_integer, QUERY_ENCODE_SET)
        );


        let hyper_client = (self.hyper_client)();
        let request = hyper_client.request(hyper::method::Method::Get, &url);
        let mut custom_headers = hyper::header::Headers::new();

        context.x_span_id.as_ref().map(|header| custom_headers.set(XSpanId(header.clone())));

        // Header parameters
        header! { (RequestEnumHeaderStringArray, "enum_header_string_array") => (String)* }
        param_enum_header_string_array.map(|header| custom_headers.set(RequestEnumHeaderStringArray(header.clone())));
        header! { (RequestEnumHeaderString, "enum_header_string") => [String] }
        param_enum_header_string.map(|header| custom_headers.set(RequestEnumHeaderString(header)));


        let request = request.headers(custom_headers);

        // Helper function to provide a code block to use `?` in (to be replaced by the `catch` block when it exists).
        fn parse_response(mut response: hyper::client::response::Response) -> Result<TestEnumParametersResponse, ApiError> {
            match response.status.to_u16() {
                400 => {


                    Ok(TestEnumParametersResponse::InvalidRequest)
                },
                404 => {


                    Ok(TestEnumParametersResponse::NotFound)
                },
                code => {
                    let mut buf = [0; 100];
                    let debug_body = match response.read(&mut buf) {
                        Ok(len) => match str::from_utf8(&buf[..len]) {
                            Ok(body) => Cow::from(body),
                            Err(_) => Cow::from(format!("<Body was not UTF8: {:?}>", &buf[..len].to_vec())),
                        },
                        Err(e) => Cow::from(format!("<Failed to read body: {}>", e)),
                    };
                    Err(ApiError(format!("Unexpected response code {}:\n{:?}\n\n{}",
                                         code,
                                         response.headers,
                                         debug_body)))
                }
            }
        }

        let result = request.send().map_err(|e| ApiError(format!("No response received: {}", e))).and_then(parse_response);
        Box::new(futures::done(result))
    }

    fn test_inline_additional_properties(&self, param_param: object, context: &Context) -> Box<Future<Item=TestInlineAdditionalPropertiesResponse, Error=ApiError> + Send> {


        let url = format!(
            "{}/v2/fake/inline-additionalProperties",
            self.base_path
        );


        let body = serde_json::to_string(&param_param).expect("impossible to fail to serialize");

        let hyper_client = (self.hyper_client)();
        let request = hyper_client.request(hyper::method::Method::Post, &url);
        let mut custom_headers = hyper::header::Headers::new();

        let request = request.body(&body);

        custom_headers.set(ContentType(mimetypes::requests::TEST_INLINE_ADDITIONAL_PROPERTIES.clone()));
        context.x_span_id.as_ref().map(|header| custom_headers.set(XSpanId(header.clone())));


        let request = request.headers(custom_headers);

        // Helper function to provide a code block to use `?` in (to be replaced by the `catch` block when it exists).
        fn parse_response(mut response: hyper::client::response::Response) -> Result<TestInlineAdditionalPropertiesResponse, ApiError> {
            match response.status.to_u16() {
                200 => {


                    Ok(TestInlineAdditionalPropertiesResponse::SuccessfulOperation)
                },
                code => {
                    let mut buf = [0; 100];
                    let debug_body = match response.read(&mut buf) {
                        Ok(len) => match str::from_utf8(&buf[..len]) {
                            Ok(body) => Cow::from(body),
                            Err(_) => Cow::from(format!("<Body was not UTF8: {:?}>", &buf[..len].to_vec())),
                        },
                        Err(e) => Cow::from(format!("<Failed to read body: {}>", e)),
                    };
                    Err(ApiError(format!("Unexpected response code {}:\n{:?}\n\n{}",
                                         code,
                                         response.headers,
                                         debug_body)))
                }
            }
        }

        let result = request.send().map_err(|e| ApiError(format!("No response received: {}", e))).and_then(parse_response);
        Box::new(futures::done(result))
    }

    fn test_json_form_data(&self, param_param: String, param_param2: String, context: &Context) -> Box<Future<Item=TestJsonFormDataResponse, Error=ApiError> + Send> {


        let url = format!(
            "{}/v2/fake/jsonFormData",
            self.base_path
        );


        let hyper_client = (self.hyper_client)();
        let request = hyper_client.request(hyper::method::Method::Get, &url);
        let mut custom_headers = hyper::header::Headers::new();

        context.x_span_id.as_ref().map(|header| custom_headers.set(XSpanId(header.clone())));


        let request = request.headers(custom_headers);

        // Helper function to provide a code block to use `?` in (to be replaced by the `catch` block when it exists).
        fn parse_response(mut response: hyper::client::response::Response) -> Result<TestJsonFormDataResponse, ApiError> {
            match response.status.to_u16() {
                200 => {


                    Ok(TestJsonFormDataResponse::SuccessfulOperation)
                },
                code => {
                    let mut buf = [0; 100];
                    let debug_body = match response.read(&mut buf) {
                        Ok(len) => match str::from_utf8(&buf[..len]) {
                            Ok(body) => Cow::from(body),
                            Err(_) => Cow::from(format!("<Body was not UTF8: {:?}>", &buf[..len].to_vec())),
                        },
                        Err(e) => Cow::from(format!("<Failed to read body: {}>", e)),
                    };
                    Err(ApiError(format!("Unexpected response code {}:\n{:?}\n\n{}",
                                         code,
                                         response.headers,
                                         debug_body)))
                }
            }
        }

        let result = request.send().map_err(|e| ApiError(format!("No response received: {}", e))).and_then(parse_response);
        Box::new(futures::done(result))
    }

    fn test_classname(&self, param_body: models::Client, context: &Context) -> Box<Future<Item=TestClassnameResponse, Error=ApiError> + Send> {


        let url = format!(
            "{}/v2/fake_classname_test",
            self.base_path
        );


        let body = serde_json::to_string(&param_body).expect("impossible to fail to serialize");

        let hyper_client = (self.hyper_client)();
        let request = hyper_client.request(hyper::method::Method::Patch, &url);
        let mut custom_headers = hyper::header::Headers::new();

        let request = request.body(&body);

        custom_headers.set(ContentType(mimetypes::requests::TEST_CLASSNAME.clone()));
        context.x_span_id.as_ref().map(|header| custom_headers.set(XSpanId(header.clone())));


        let request = request.headers(custom_headers);

        // Helper function to provide a code block to use `?` in (to be replaced by the `catch` block when it exists).
        fn parse_response(mut response: hyper::client::response::Response) -> Result<TestClassnameResponse, ApiError> {
            match response.status.to_u16() {
                200 => {
                    let mut buf = String::new();
                    response.read_to_string(&mut buf).map_err(|e| ApiError(format!("Response was not valid UTF8: {}", e)))?;
                    let body = serde_json::from_str::<models::Client>(&buf)?;

                    Ok(TestClassnameResponse::SuccessfulOperation(body))
                },
                code => {
                    let mut buf = [0; 100];
                    let debug_body = match response.read(&mut buf) {
                        Ok(len) => match str::from_utf8(&buf[..len]) {
                            Ok(body) => Cow::from(body),
                            Err(_) => Cow::from(format!("<Body was not UTF8: {:?}>", &buf[..len].to_vec())),
                        },
                        Err(e) => Cow::from(format!("<Failed to read body: {}>", e)),
                    };
                    Err(ApiError(format!("Unexpected response code {}:\n{:?}\n\n{}",
                                         code,
                                         response.headers,
                                         debug_body)))
                }
            }
        }

        let result = request.send().map_err(|e| ApiError(format!("No response received: {}", e))).and_then(parse_response);
        Box::new(futures::done(result))
    }

    fn add_pet(&self, param_body: models::Pet, context: &Context) -> Box<Future<Item=AddPetResponse, Error=ApiError> + Send> {


        let url = format!(
            "{}/v2/pet",
            self.base_path
        );


        let body = serde_xml_rs::to_string(&param_body).expect("impossible to fail to serialize");

        let hyper_client = (self.hyper_client)();
        let request = hyper_client.request(hyper::method::Method::Post, &url);
        let mut custom_headers = hyper::header::Headers::new();

        let request = request.body(&body);

        custom_headers.set(ContentType(mimetypes::requests::ADD_PET.clone()));
        context.x_span_id.as_ref().map(|header| custom_headers.set(XSpanId(header.clone())));


        let request = request.headers(custom_headers);

        // Helper function to provide a code block to use `?` in (to be replaced by the `catch` block when it exists).
        fn parse_response(mut response: hyper::client::response::Response) -> Result<AddPetResponse, ApiError> {
            match response.status.to_u16() {
                405 => {


                    Ok(AddPetResponse::InvalidInput)
                },
                code => {
                    let mut buf = [0; 100];
                    let debug_body = match response.read(&mut buf) {
                        Ok(len) => match str::from_utf8(&buf[..len]) {
                            Ok(body) => Cow::from(body),
                            Err(_) => Cow::from(format!("<Body was not UTF8: {:?}>", &buf[..len].to_vec())),
                        },
                        Err(e) => Cow::from(format!("<Failed to read body: {}>", e)),
                    };
                    Err(ApiError(format!("Unexpected response code {}:\n{:?}\n\n{}",
                                         code,
                                         response.headers,
                                         debug_body)))
                }
            }
        }

        let result = request.send().map_err(|e| ApiError(format!("No response received: {}", e))).and_then(parse_response);
        Box::new(futures::done(result))
    }

    fn delete_pet(&self, param_pet_id: i64, param_api_key: Option<String>, context: &Context) -> Box<Future<Item=DeletePetResponse, Error=ApiError> + Send> {


        let url = format!(
            "{}/v2/pet/{petId}",
            self.base_path, petId=utf8_percent_encode(&param_pet_id.to_string(), PATH_SEGMENT_ENCODE_SET)
        );


        let hyper_client = (self.hyper_client)();
        let request = hyper_client.request(hyper::method::Method::Delete, &url);
        let mut custom_headers = hyper::header::Headers::new();

        context.x_span_id.as_ref().map(|header| custom_headers.set(XSpanId(header.clone())));

        // Header parameters
        header! { (RequestApiKey, "api_key") => [String] }
        param_api_key.map(|header| custom_headers.set(RequestApiKey(header)));


        let request = request.headers(custom_headers);

        // Helper function to provide a code block to use `?` in (to be replaced by the `catch` block when it exists).
        fn parse_response(mut response: hyper::client::response::Response) -> Result<DeletePetResponse, ApiError> {
            match response.status.to_u16() {
                400 => {


                    Ok(DeletePetResponse::InvalidPetValue)
                },
                code => {
                    let mut buf = [0; 100];
                    let debug_body = match response.read(&mut buf) {
                        Ok(len) => match str::from_utf8(&buf[..len]) {
                            Ok(body) => Cow::from(body),
                            Err(_) => Cow::from(format!("<Body was not UTF8: {:?}>", &buf[..len].to_vec())),
                        },
                        Err(e) => Cow::from(format!("<Failed to read body: {}>", e)),
                    };
                    Err(ApiError(format!("Unexpected response code {}:\n{:?}\n\n{}",
                                         code,
                                         response.headers,
                                         debug_body)))
                }
            }
        }

        let result = request.send().map_err(|e| ApiError(format!("No response received: {}", e))).and_then(parse_response);
        Box::new(futures::done(result))
    }

    fn find_pets_by_status(&self, param_status: &Vec<String>, context: &Context) -> Box<Future<Item=FindPetsByStatusResponse, Error=ApiError> + Send> {

        // Query parameters
        let query_status = format!("status={status}&", status=param_status.join(","));


        let url = format!(
            "{}/v2/pet/findByStatus?{status}",
            self.base_path,
            status=utf8_percent_encode(&query_status, QUERY_ENCODE_SET)
        );


        let hyper_client = (self.hyper_client)();
        let request = hyper_client.request(hyper::method::Method::Get, &url);
        let mut custom_headers = hyper::header::Headers::new();

        context.x_span_id.as_ref().map(|header| custom_headers.set(XSpanId(header.clone())));


        let request = request.headers(custom_headers);

        // Helper function to provide a code block to use `?` in (to be replaced by the `catch` block when it exists).
        fn parse_response(mut response: hyper::client::response::Response) -> Result<FindPetsByStatusResponse, ApiError> {
            match response.status.to_u16() {
                200 => {
                    let mut buf = String::new();
                    response.read_to_string(&mut buf).map_err(|e| ApiError(format!("Response was not valid UTF8: {}", e)))?;
                    // ToDo: this will move to swagger-rs and become a standard From conversion trait
                    // once https://github.com/RReverser/serde-xml-rs/pull/45 is accepted upstream
                    let body = serde_xml_rs::from_str::<Vec<models::Pet>>(&buf)
                        .map_err(|e| ApiError(format!("Response body did not match the schema: {}", e)))?;

                    Ok(FindPetsByStatusResponse::SuccessfulOperation(body))
                },
                400 => {


                    Ok(FindPetsByStatusResponse::InvalidStatusValue)
                },
                code => {
                    let mut buf = [0; 100];
                    let debug_body = match response.read(&mut buf) {
                        Ok(len) => match str::from_utf8(&buf[..len]) {
                            Ok(body) => Cow::from(body),
                            Err(_) => Cow::from(format!("<Body was not UTF8: {:?}>", &buf[..len].to_vec())),
                        },
                        Err(e) => Cow::from(format!("<Failed to read body: {}>", e)),
                    };
                    Err(ApiError(format!("Unexpected response code {}:\n{:?}\n\n{}",
                                         code,
                                         response.headers,
                                         debug_body)))
                }
            }
        }

        let result = request.send().map_err(|e| ApiError(format!("No response received: {}", e))).and_then(parse_response);
        Box::new(futures::done(result))
    }

    fn find_pets_by_tags(&self, param_tags: &Vec<String>, context: &Context) -> Box<Future<Item=FindPetsByTagsResponse, Error=ApiError> + Send> {

        // Query parameters
        let query_tags = format!("tags={tags}&", tags=param_tags.join(","));


        let url = format!(
            "{}/v2/pet/findByTags?{tags}",
            self.base_path,
            tags=utf8_percent_encode(&query_tags, QUERY_ENCODE_SET)
        );


        let hyper_client = (self.hyper_client)();
        let request = hyper_client.request(hyper::method::Method::Get, &url);
        let mut custom_headers = hyper::header::Headers::new();

        context.x_span_id.as_ref().map(|header| custom_headers.set(XSpanId(header.clone())));


        let request = request.headers(custom_headers);

        // Helper function to provide a code block to use `?` in (to be replaced by the `catch` block when it exists).
        fn parse_response(mut response: hyper::client::response::Response) -> Result<FindPetsByTagsResponse, ApiError> {
            match response.status.to_u16() {
                200 => {
                    let mut buf = String::new();
                    response.read_to_string(&mut buf).map_err(|e| ApiError(format!("Response was not valid UTF8: {}", e)))?;
                    // ToDo: this will move to swagger-rs and become a standard From conversion trait
                    // once https://github.com/RReverser/serde-xml-rs/pull/45 is accepted upstream
                    let body = serde_xml_rs::from_str::<Vec<models::Pet>>(&buf)
                        .map_err(|e| ApiError(format!("Response body did not match the schema: {}", e)))?;

                    Ok(FindPetsByTagsResponse::SuccessfulOperation(body))
                },
                400 => {


                    Ok(FindPetsByTagsResponse::InvalidTagValue)
                },
                code => {
                    let mut buf = [0; 100];
                    let debug_body = match response.read(&mut buf) {
                        Ok(len) => match str::from_utf8(&buf[..len]) {
                            Ok(body) => Cow::from(body),
                            Err(_) => Cow::from(format!("<Body was not UTF8: {:?}>", &buf[..len].to_vec())),
                        },
                        Err(e) => Cow::from(format!("<Failed to read body: {}>", e)),
                    };
                    Err(ApiError(format!("Unexpected response code {}:\n{:?}\n\n{}",
                                         code,
                                         response.headers,
                                         debug_body)))
                }
            }
        }

        let result = request.send().map_err(|e| ApiError(format!("No response received: {}", e))).and_then(parse_response);
        Box::new(futures::done(result))
    }

    fn get_pet_by_id(&self, param_pet_id: i64, context: &Context) -> Box<Future<Item=GetPetByIdResponse, Error=ApiError> + Send> {


        let url = format!(
            "{}/v2/pet/{petId}",
            self.base_path, petId=utf8_percent_encode(&param_pet_id.to_string(), PATH_SEGMENT_ENCODE_SET)
        );


        let hyper_client = (self.hyper_client)();
        let request = hyper_client.request(hyper::method::Method::Get, &url);
        let mut custom_headers = hyper::header::Headers::new();

        context.x_span_id.as_ref().map(|header| custom_headers.set(XSpanId(header.clone())));


        let request = request.headers(custom_headers);

        // Helper function to provide a code block to use `?` in (to be replaced by the `catch` block when it exists).
        fn parse_response(mut response: hyper::client::response::Response) -> Result<GetPetByIdResponse, ApiError> {
            match response.status.to_u16() {
                200 => {
                    let mut buf = String::new();
                    response.read_to_string(&mut buf).map_err(|e| ApiError(format!("Response was not valid UTF8: {}", e)))?;
                    // ToDo: this will move to swagger-rs and become a standard From conversion trait
                    // once https://github.com/RReverser/serde-xml-rs/pull/45 is accepted upstream
                    let body = serde_xml_rs::from_str::<models::Pet>(&buf)
                        .map_err(|e| ApiError(format!("Response body did not match the schema: {}", e)))?;

                    Ok(GetPetByIdResponse::SuccessfulOperation(body))
                },
                400 => {


                    Ok(GetPetByIdResponse::InvalidIDSupplied)
                },
                404 => {


                    Ok(GetPetByIdResponse::PetNotFound)
                },
                code => {
                    let mut buf = [0; 100];
                    let debug_body = match response.read(&mut buf) {
                        Ok(len) => match str::from_utf8(&buf[..len]) {
                            Ok(body) => Cow::from(body),
                            Err(_) => Cow::from(format!("<Body was not UTF8: {:?}>", &buf[..len].to_vec())),
                        },
                        Err(e) => Cow::from(format!("<Failed to read body: {}>", e)),
                    };
                    Err(ApiError(format!("Unexpected response code {}:\n{:?}\n\n{}",
                                         code,
                                         response.headers,
                                         debug_body)))
                }
            }
        }

        let result = request.send().map_err(|e| ApiError(format!("No response received: {}", e))).and_then(parse_response);
        Box::new(futures::done(result))
    }

    fn update_pet(&self, param_body: models::Pet, context: &Context) -> Box<Future<Item=UpdatePetResponse, Error=ApiError> + Send> {


        let url = format!(
            "{}/v2/pet",
            self.base_path
        );


        let body = serde_xml_rs::to_string(&param_body).expect("impossible to fail to serialize");

        let hyper_client = (self.hyper_client)();
        let request = hyper_client.request(hyper::method::Method::Put, &url);
        let mut custom_headers = hyper::header::Headers::new();

        let request = request.body(&body);

        custom_headers.set(ContentType(mimetypes::requests::UPDATE_PET.clone()));
        context.x_span_id.as_ref().map(|header| custom_headers.set(XSpanId(header.clone())));


        let request = request.headers(custom_headers);

        // Helper function to provide a code block to use `?` in (to be replaced by the `catch` block when it exists).
        fn parse_response(mut response: hyper::client::response::Response) -> Result<UpdatePetResponse, ApiError> {
            match response.status.to_u16() {
                400 => {


                    Ok(UpdatePetResponse::InvalidIDSupplied)
                },
                404 => {


                    Ok(UpdatePetResponse::PetNotFound)
                },
                405 => {


                    Ok(UpdatePetResponse::ValidationException)
                },
                code => {
                    let mut buf = [0; 100];
                    let debug_body = match response.read(&mut buf) {
                        Ok(len) => match str::from_utf8(&buf[..len]) {
                            Ok(body) => Cow::from(body),
                            Err(_) => Cow::from(format!("<Body was not UTF8: {:?}>", &buf[..len].to_vec())),
                        },
                        Err(e) => Cow::from(format!("<Failed to read body: {}>", e)),
                    };
                    Err(ApiError(format!("Unexpected response code {}:\n{:?}\n\n{}",
                                         code,
                                         response.headers,
                                         debug_body)))
                }
            }
        }

        let result = request.send().map_err(|e| ApiError(format!("No response received: {}", e))).and_then(parse_response);
        Box::new(futures::done(result))
    }

    fn update_pet_with_form(&self, param_pet_id: i64, param_name: Option<String>, param_status: Option<String>, context: &Context) -> Box<Future<Item=UpdatePetWithFormResponse, Error=ApiError> + Send> {


        let url = format!(
            "{}/v2/pet/{petId}",
            self.base_path, petId=utf8_percent_encode(&param_pet_id.to_string(), PATH_SEGMENT_ENCODE_SET)
        );


        let hyper_client = (self.hyper_client)();
        let request = hyper_client.request(hyper::method::Method::Post, &url);
        let mut custom_headers = hyper::header::Headers::new();

        context.x_span_id.as_ref().map(|header| custom_headers.set(XSpanId(header.clone())));


        let request = request.headers(custom_headers);

        // Helper function to provide a code block to use `?` in (to be replaced by the `catch` block when it exists).
        fn parse_response(mut response: hyper::client::response::Response) -> Result<UpdatePetWithFormResponse, ApiError> {
            match response.status.to_u16() {
                405 => {


                    Ok(UpdatePetWithFormResponse::InvalidInput)
                },
                code => {
                    let mut buf = [0; 100];
                    let debug_body = match response.read(&mut buf) {
                        Ok(len) => match str::from_utf8(&buf[..len]) {
                            Ok(body) => Cow::from(body),
                            Err(_) => Cow::from(format!("<Body was not UTF8: {:?}>", &buf[..len].to_vec())),
                        },
                        Err(e) => Cow::from(format!("<Failed to read body: {}>", e)),
                    };
                    Err(ApiError(format!("Unexpected response code {}:\n{:?}\n\n{}",
                                         code,
                                         response.headers,
                                         debug_body)))
                }
            }
        }

        let result = request.send().map_err(|e| ApiError(format!("No response received: {}", e))).and_then(parse_response);
        Box::new(futures::done(result))
    }

    fn upload_file(&self, param_pet_id: i64, param_additional_metadata: Option<String>, param_file: Box<Future<Item=Option<Box<Stream<Item=Vec<u8>, Error=Error> + Send>>, Error=Error> + Send>, context: &Context) -> Box<Future<Item=UploadFileResponse, Error=ApiError> + Send> {


        let url = format!(
            "{}/v2/pet/{petId}/uploadImage",
            self.base_path, petId=utf8_percent_encode(&param_pet_id.to_string(), PATH_SEGMENT_ENCODE_SET)
        );

        // Form data body
        let mut multipart = Multipart::new();

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
        let multipart_header = Mime(TopLevel::Multipart, SubLevel::FormData, vec![(Attr::Boundary, Value::Ext(boundary.to_string()))]);
        let hyper_client = (self.hyper_client)();
        let request = hyper_client.request(hyper::method::Method::Post, &url);
        let mut custom_headers = hyper::header::Headers::new();

        context.x_span_id.as_ref().map(|header| custom_headers.set(XSpanId(header.clone())));


        let request = request.headers(custom_headers);
        let request = request.header(ContentType(multipart_header))
                    .body(&body_string);


        // Helper function to provide a code block to use `?` in (to be replaced by the `catch` block when it exists).
        fn parse_response(mut response: hyper::client::response::Response) -> Result<UploadFileResponse, ApiError> {
            match response.status.to_u16() {
                200 => {
                    let mut buf = String::new();
                    response.read_to_string(&mut buf).map_err(|e| ApiError(format!("Response was not valid UTF8: {}", e)))?;
                    let body = serde_json::from_str::<models::ApiResponse>(&buf)?;

                    Ok(UploadFileResponse::SuccessfulOperation(body))
                },
                code => {
                    let mut buf = [0; 100];
                    let debug_body = match response.read(&mut buf) {
                        Ok(len) => match str::from_utf8(&buf[..len]) {
                            Ok(body) => Cow::from(body),
                            Err(_) => Cow::from(format!("<Body was not UTF8: {:?}>", &buf[..len].to_vec())),
                        },
                        Err(e) => Cow::from(format!("<Failed to read body: {}>", e)),
                    };
                    Err(ApiError(format!("Unexpected response code {}:\n{:?}\n\n{}",
                                         code,
                                         response.headers,
                                         debug_body)))
                }
            }
        }

        // Helper function to convert a Stream into a String. The String can then be used to build the HTTP body.
        fn convert_stream_to_string(stream: Box<Stream<Item=Vec<u8>, Error=Error> + Send>) -> Result<String, ApiError> {

            stream.fold(Vec::new(), |mut body, chunk| {
                body.extend(chunk.iter());
                future::ok::<Vec<u8>,Error>(body)
            }).wait()
              .map_err(|e| ApiError(format!("Unable to fold stream: {}", e)))
              .and_then(|body| String::from_utf8(body)
                .map_err(|e| ApiError(format!("Failed to convert utf8 stream to String: {}", e))))
        }

        let result = request.send().map_err(|e| ApiError(format!("No response received: {}", e))).and_then(parse_response);
        Box::new(futures::done(result))
    }

    fn delete_order(&self, param_order_id: String, context: &Context) -> Box<Future<Item=DeleteOrderResponse, Error=ApiError> + Send> {


        let url = format!(
            "{}/v2/store/order/{order_id}",
            self.base_path, order_id=utf8_percent_encode(&param_order_id.to_string(), PATH_SEGMENT_ENCODE_SET)
        );


        let hyper_client = (self.hyper_client)();
        let request = hyper_client.request(hyper::method::Method::Delete, &url);
        let mut custom_headers = hyper::header::Headers::new();

        context.x_span_id.as_ref().map(|header| custom_headers.set(XSpanId(header.clone())));


        let request = request.headers(custom_headers);

        // Helper function to provide a code block to use `?` in (to be replaced by the `catch` block when it exists).
        fn parse_response(mut response: hyper::client::response::Response) -> Result<DeleteOrderResponse, ApiError> {
            match response.status.to_u16() {
                400 => {


                    Ok(DeleteOrderResponse::InvalidIDSupplied)
                },
                404 => {


                    Ok(DeleteOrderResponse::OrderNotFound)
                },
                code => {
                    let mut buf = [0; 100];
                    let debug_body = match response.read(&mut buf) {
                        Ok(len) => match str::from_utf8(&buf[..len]) {
                            Ok(body) => Cow::from(body),
                            Err(_) => Cow::from(format!("<Body was not UTF8: {:?}>", &buf[..len].to_vec())),
                        },
                        Err(e) => Cow::from(format!("<Failed to read body: {}>", e)),
                    };
                    Err(ApiError(format!("Unexpected response code {}:\n{:?}\n\n{}",
                                         code,
                                         response.headers,
                                         debug_body)))
                }
            }
        }

        let result = request.send().map_err(|e| ApiError(format!("No response received: {}", e))).and_then(parse_response);
        Box::new(futures::done(result))
    }

    fn get_inventory(&self, context: &Context) -> Box<Future<Item=GetInventoryResponse, Error=ApiError> + Send> {


        let url = format!(
            "{}/v2/store/inventory",
            self.base_path
        );


        let hyper_client = (self.hyper_client)();
        let request = hyper_client.request(hyper::method::Method::Get, &url);
        let mut custom_headers = hyper::header::Headers::new();

        context.x_span_id.as_ref().map(|header| custom_headers.set(XSpanId(header.clone())));


        let request = request.headers(custom_headers);

        // Helper function to provide a code block to use `?` in (to be replaced by the `catch` block when it exists).
        fn parse_response(mut response: hyper::client::response::Response) -> Result<GetInventoryResponse, ApiError> {
            match response.status.to_u16() {
                200 => {
                    let mut buf = String::new();
                    response.read_to_string(&mut buf).map_err(|e| ApiError(format!("Response was not valid UTF8: {}", e)))?;
                    let body = serde_json::from_str::<HashMap<String, i32>>(&buf)?;

                    Ok(GetInventoryResponse::SuccessfulOperation(body))
                },
                code => {
                    let mut buf = [0; 100];
                    let debug_body = match response.read(&mut buf) {
                        Ok(len) => match str::from_utf8(&buf[..len]) {
                            Ok(body) => Cow::from(body),
                            Err(_) => Cow::from(format!("<Body was not UTF8: {:?}>", &buf[..len].to_vec())),
                        },
                        Err(e) => Cow::from(format!("<Failed to read body: {}>", e)),
                    };
                    Err(ApiError(format!("Unexpected response code {}:\n{:?}\n\n{}",
                                         code,
                                         response.headers,
                                         debug_body)))
                }
            }
        }

        let result = request.send().map_err(|e| ApiError(format!("No response received: {}", e))).and_then(parse_response);
        Box::new(futures::done(result))
    }

    fn get_order_by_id(&self, param_order_id: i64, context: &Context) -> Box<Future<Item=GetOrderByIdResponse, Error=ApiError> + Send> {


        let url = format!(
            "{}/v2/store/order/{order_id}",
            self.base_path, order_id=utf8_percent_encode(&param_order_id.to_string(), PATH_SEGMENT_ENCODE_SET)
        );


        let hyper_client = (self.hyper_client)();
        let request = hyper_client.request(hyper::method::Method::Get, &url);
        let mut custom_headers = hyper::header::Headers::new();

        context.x_span_id.as_ref().map(|header| custom_headers.set(XSpanId(header.clone())));


        let request = request.headers(custom_headers);

        // Helper function to provide a code block to use `?` in (to be replaced by the `catch` block when it exists).
        fn parse_response(mut response: hyper::client::response::Response) -> Result<GetOrderByIdResponse, ApiError> {
            match response.status.to_u16() {
                200 => {
                    let mut buf = String::new();
                    response.read_to_string(&mut buf).map_err(|e| ApiError(format!("Response was not valid UTF8: {}", e)))?;
                    // ToDo: this will move to swagger-rs and become a standard From conversion trait
                    // once https://github.com/RReverser/serde-xml-rs/pull/45 is accepted upstream
                    let body = serde_xml_rs::from_str::<models::Order>(&buf)
                        .map_err(|e| ApiError(format!("Response body did not match the schema: {}", e)))?;

                    Ok(GetOrderByIdResponse::SuccessfulOperation(body))
                },
                400 => {


                    Ok(GetOrderByIdResponse::InvalidIDSupplied)
                },
                404 => {


                    Ok(GetOrderByIdResponse::OrderNotFound)
                },
                code => {
                    let mut buf = [0; 100];
                    let debug_body = match response.read(&mut buf) {
                        Ok(len) => match str::from_utf8(&buf[..len]) {
                            Ok(body) => Cow::from(body),
                            Err(_) => Cow::from(format!("<Body was not UTF8: {:?}>", &buf[..len].to_vec())),
                        },
                        Err(e) => Cow::from(format!("<Failed to read body: {}>", e)),
                    };
                    Err(ApiError(format!("Unexpected response code {}:\n{:?}\n\n{}",
                                         code,
                                         response.headers,
                                         debug_body)))
                }
            }
        }

        let result = request.send().map_err(|e| ApiError(format!("No response received: {}", e))).and_then(parse_response);
        Box::new(futures::done(result))
    }

    fn place_order(&self, param_body: models::Order, context: &Context) -> Box<Future<Item=PlaceOrderResponse, Error=ApiError> + Send> {


        let url = format!(
            "{}/v2/store/order",
            self.base_path
        );


        let body = serde_json::to_string(&param_body).expect("impossible to fail to serialize");

        let hyper_client = (self.hyper_client)();
        let request = hyper_client.request(hyper::method::Method::Post, &url);
        let mut custom_headers = hyper::header::Headers::new();

        let request = request.body(&body);

        custom_headers.set(ContentType(mimetypes::requests::PLACE_ORDER.clone()));
        context.x_span_id.as_ref().map(|header| custom_headers.set(XSpanId(header.clone())));


        let request = request.headers(custom_headers);

        // Helper function to provide a code block to use `?` in (to be replaced by the `catch` block when it exists).
        fn parse_response(mut response: hyper::client::response::Response) -> Result<PlaceOrderResponse, ApiError> {
            match response.status.to_u16() {
                200 => {
                    let mut buf = String::new();
                    response.read_to_string(&mut buf).map_err(|e| ApiError(format!("Response was not valid UTF8: {}", e)))?;
                    // ToDo: this will move to swagger-rs and become a standard From conversion trait
                    // once https://github.com/RReverser/serde-xml-rs/pull/45 is accepted upstream
                    let body = serde_xml_rs::from_str::<models::Order>(&buf)
                        .map_err(|e| ApiError(format!("Response body did not match the schema: {}", e)))?;

                    Ok(PlaceOrderResponse::SuccessfulOperation(body))
                },
                400 => {


                    Ok(PlaceOrderResponse::InvalidOrder)
                },
                code => {
                    let mut buf = [0; 100];
                    let debug_body = match response.read(&mut buf) {
                        Ok(len) => match str::from_utf8(&buf[..len]) {
                            Ok(body) => Cow::from(body),
                            Err(_) => Cow::from(format!("<Body was not UTF8: {:?}>", &buf[..len].to_vec())),
                        },
                        Err(e) => Cow::from(format!("<Failed to read body: {}>", e)),
                    };
                    Err(ApiError(format!("Unexpected response code {}:\n{:?}\n\n{}",
                                         code,
                                         response.headers,
                                         debug_body)))
                }
            }
        }

        let result = request.send().map_err(|e| ApiError(format!("No response received: {}", e))).and_then(parse_response);
        Box::new(futures::done(result))
    }

    fn create_user(&self, param_body: models::User, context: &Context) -> Box<Future<Item=CreateUserResponse, Error=ApiError> + Send> {


        let url = format!(
            "{}/v2/user",
            self.base_path
        );


        let body = serde_json::to_string(&param_body).expect("impossible to fail to serialize");

        let hyper_client = (self.hyper_client)();
        let request = hyper_client.request(hyper::method::Method::Post, &url);
        let mut custom_headers = hyper::header::Headers::new();

        let request = request.body(&body);

        custom_headers.set(ContentType(mimetypes::requests::CREATE_USER.clone()));
        context.x_span_id.as_ref().map(|header| custom_headers.set(XSpanId(header.clone())));


        let request = request.headers(custom_headers);

        // Helper function to provide a code block to use `?` in (to be replaced by the `catch` block when it exists).
        fn parse_response(mut response: hyper::client::response::Response) -> Result<CreateUserResponse, ApiError> {
            match response.status.to_u16() {
                0 => {


                    Ok(CreateUserResponse::SuccessfulOperation)
                },
                code => {
                    let mut buf = [0; 100];
                    let debug_body = match response.read(&mut buf) {
                        Ok(len) => match str::from_utf8(&buf[..len]) {
                            Ok(body) => Cow::from(body),
                            Err(_) => Cow::from(format!("<Body was not UTF8: {:?}>", &buf[..len].to_vec())),
                        },
                        Err(e) => Cow::from(format!("<Failed to read body: {}>", e)),
                    };
                    Err(ApiError(format!("Unexpected response code {}:\n{:?}\n\n{}",
                                         code,
                                         response.headers,
                                         debug_body)))
                }
            }
        }

        let result = request.send().map_err(|e| ApiError(format!("No response received: {}", e))).and_then(parse_response);
        Box::new(futures::done(result))
    }

    fn create_users_with_array_input(&self, param_body: &Vec<models::User>, context: &Context) -> Box<Future<Item=CreateUsersWithArrayInputResponse, Error=ApiError> + Send> {


        let url = format!(
            "{}/v2/user/createWithArray",
            self.base_path
        );


        let body = serde_json::to_string(&param_body).expect("impossible to fail to serialize");

        let hyper_client = (self.hyper_client)();
        let request = hyper_client.request(hyper::method::Method::Post, &url);
        let mut custom_headers = hyper::header::Headers::new();

        let request = request.body(&body);

        custom_headers.set(ContentType(mimetypes::requests::CREATE_USERS_WITH_ARRAY_INPUT.clone()));
        context.x_span_id.as_ref().map(|header| custom_headers.set(XSpanId(header.clone())));


        let request = request.headers(custom_headers);

        // Helper function to provide a code block to use `?` in (to be replaced by the `catch` block when it exists).
        fn parse_response(mut response: hyper::client::response::Response) -> Result<CreateUsersWithArrayInputResponse, ApiError> {
            match response.status.to_u16() {
                0 => {


                    Ok(CreateUsersWithArrayInputResponse::SuccessfulOperation)
                },
                code => {
                    let mut buf = [0; 100];
                    let debug_body = match response.read(&mut buf) {
                        Ok(len) => match str::from_utf8(&buf[..len]) {
                            Ok(body) => Cow::from(body),
                            Err(_) => Cow::from(format!("<Body was not UTF8: {:?}>", &buf[..len].to_vec())),
                        },
                        Err(e) => Cow::from(format!("<Failed to read body: {}>", e)),
                    };
                    Err(ApiError(format!("Unexpected response code {}:\n{:?}\n\n{}",
                                         code,
                                         response.headers,
                                         debug_body)))
                }
            }
        }

        let result = request.send().map_err(|e| ApiError(format!("No response received: {}", e))).and_then(parse_response);
        Box::new(futures::done(result))
    }

    fn create_users_with_list_input(&self, param_body: &Vec<models::User>, context: &Context) -> Box<Future<Item=CreateUsersWithListInputResponse, Error=ApiError> + Send> {


        let url = format!(
            "{}/v2/user/createWithList",
            self.base_path
        );


        let body = serde_json::to_string(&param_body).expect("impossible to fail to serialize");

        let hyper_client = (self.hyper_client)();
        let request = hyper_client.request(hyper::method::Method::Post, &url);
        let mut custom_headers = hyper::header::Headers::new();

        let request = request.body(&body);

        custom_headers.set(ContentType(mimetypes::requests::CREATE_USERS_WITH_LIST_INPUT.clone()));
        context.x_span_id.as_ref().map(|header| custom_headers.set(XSpanId(header.clone())));


        let request = request.headers(custom_headers);

        // Helper function to provide a code block to use `?` in (to be replaced by the `catch` block when it exists).
        fn parse_response(mut response: hyper::client::response::Response) -> Result<CreateUsersWithListInputResponse, ApiError> {
            match response.status.to_u16() {
                0 => {


                    Ok(CreateUsersWithListInputResponse::SuccessfulOperation)
                },
                code => {
                    let mut buf = [0; 100];
                    let debug_body = match response.read(&mut buf) {
                        Ok(len) => match str::from_utf8(&buf[..len]) {
                            Ok(body) => Cow::from(body),
                            Err(_) => Cow::from(format!("<Body was not UTF8: {:?}>", &buf[..len].to_vec())),
                        },
                        Err(e) => Cow::from(format!("<Failed to read body: {}>", e)),
                    };
                    Err(ApiError(format!("Unexpected response code {}:\n{:?}\n\n{}",
                                         code,
                                         response.headers,
                                         debug_body)))
                }
            }
        }

        let result = request.send().map_err(|e| ApiError(format!("No response received: {}", e))).and_then(parse_response);
        Box::new(futures::done(result))
    }

    fn delete_user(&self, param_username: String, context: &Context) -> Box<Future<Item=DeleteUserResponse, Error=ApiError> + Send> {


        let url = format!(
            "{}/v2/user/{username}",
            self.base_path, username=utf8_percent_encode(&param_username.to_string(), PATH_SEGMENT_ENCODE_SET)
        );


        let hyper_client = (self.hyper_client)();
        let request = hyper_client.request(hyper::method::Method::Delete, &url);
        let mut custom_headers = hyper::header::Headers::new();

        context.x_span_id.as_ref().map(|header| custom_headers.set(XSpanId(header.clone())));


        let request = request.headers(custom_headers);

        // Helper function to provide a code block to use `?` in (to be replaced by the `catch` block when it exists).
        fn parse_response(mut response: hyper::client::response::Response) -> Result<DeleteUserResponse, ApiError> {
            match response.status.to_u16() {
                400 => {


                    Ok(DeleteUserResponse::InvalidUsernameSupplied)
                },
                404 => {


                    Ok(DeleteUserResponse::UserNotFound)
                },
                code => {
                    let mut buf = [0; 100];
                    let debug_body = match response.read(&mut buf) {
                        Ok(len) => match str::from_utf8(&buf[..len]) {
                            Ok(body) => Cow::from(body),
                            Err(_) => Cow::from(format!("<Body was not UTF8: {:?}>", &buf[..len].to_vec())),
                        },
                        Err(e) => Cow::from(format!("<Failed to read body: {}>", e)),
                    };
                    Err(ApiError(format!("Unexpected response code {}:\n{:?}\n\n{}",
                                         code,
                                         response.headers,
                                         debug_body)))
                }
            }
        }

        let result = request.send().map_err(|e| ApiError(format!("No response received: {}", e))).and_then(parse_response);
        Box::new(futures::done(result))
    }

    fn get_user_by_name(&self, param_username: String, context: &Context) -> Box<Future<Item=GetUserByNameResponse, Error=ApiError> + Send> {


        let url = format!(
            "{}/v2/user/{username}",
            self.base_path, username=utf8_percent_encode(&param_username.to_string(), PATH_SEGMENT_ENCODE_SET)
        );


        let hyper_client = (self.hyper_client)();
        let request = hyper_client.request(hyper::method::Method::Get, &url);
        let mut custom_headers = hyper::header::Headers::new();

        context.x_span_id.as_ref().map(|header| custom_headers.set(XSpanId(header.clone())));


        let request = request.headers(custom_headers);

        // Helper function to provide a code block to use `?` in (to be replaced by the `catch` block when it exists).
        fn parse_response(mut response: hyper::client::response::Response) -> Result<GetUserByNameResponse, ApiError> {
            match response.status.to_u16() {
                200 => {
                    let mut buf = String::new();
                    response.read_to_string(&mut buf).map_err(|e| ApiError(format!("Response was not valid UTF8: {}", e)))?;
                    // ToDo: this will move to swagger-rs and become a standard From conversion trait
                    // once https://github.com/RReverser/serde-xml-rs/pull/45 is accepted upstream
                    let body = serde_xml_rs::from_str::<models::User>(&buf)
                        .map_err(|e| ApiError(format!("Response body did not match the schema: {}", e)))?;

                    Ok(GetUserByNameResponse::SuccessfulOperation(body))
                },
                400 => {


                    Ok(GetUserByNameResponse::InvalidUsernameSupplied)
                },
                404 => {


                    Ok(GetUserByNameResponse::UserNotFound)
                },
                code => {
                    let mut buf = [0; 100];
                    let debug_body = match response.read(&mut buf) {
                        Ok(len) => match str::from_utf8(&buf[..len]) {
                            Ok(body) => Cow::from(body),
                            Err(_) => Cow::from(format!("<Body was not UTF8: {:?}>", &buf[..len].to_vec())),
                        },
                        Err(e) => Cow::from(format!("<Failed to read body: {}>", e)),
                    };
                    Err(ApiError(format!("Unexpected response code {}:\n{:?}\n\n{}",
                                         code,
                                         response.headers,
                                         debug_body)))
                }
            }
        }

        let result = request.send().map_err(|e| ApiError(format!("No response received: {}", e))).and_then(parse_response);
        Box::new(futures::done(result))
    }

    fn login_user(&self, param_username: String, param_password: String, context: &Context) -> Box<Future<Item=LoginUserResponse, Error=ApiError> + Send> {

        // Query parameters
        let query_username = format!("username={username}&", username=param_username.to_string());
        let query_password = format!("password={password}&", password=param_password.to_string());


        let url = format!(
            "{}/v2/user/login?{username}{password}",
            self.base_path,
            username=utf8_percent_encode(&query_username, QUERY_ENCODE_SET),
            password=utf8_percent_encode(&query_password, QUERY_ENCODE_SET)
        );


        let hyper_client = (self.hyper_client)();
        let request = hyper_client.request(hyper::method::Method::Get, &url);
        let mut custom_headers = hyper::header::Headers::new();

        context.x_span_id.as_ref().map(|header| custom_headers.set(XSpanId(header.clone())));


        let request = request.headers(custom_headers);

        // Helper function to provide a code block to use `?` in (to be replaced by the `catch` block when it exists).
        fn parse_response(mut response: hyper::client::response::Response) -> Result<LoginUserResponse, ApiError> {
            match response.status.to_u16() {
                200 => {
                    let mut buf = String::new();
                    response.read_to_string(&mut buf).map_err(|e| ApiError(format!("Response was not valid UTF8: {}", e)))?;
                    // ToDo: this will move to swagger-rs and become a standard From conversion trait
                    // once https://github.com/RReverser/serde-xml-rs/pull/45 is accepted upstream
                    let body = serde_xml_rs::from_str::<String>(&buf)
                        .map_err(|e| ApiError(format!("Response body did not match the schema: {}", e)))?;
                    header! { (ResponseXRateLimit, "X-Rate-Limit") => [i32] }
                    let response_x_rate_limit = response.headers.get::<ResponseXRateLimit>().ok_or_else(|| "Required response header X-Rate-Limit for response 200 was not found.")?;
                    header! { (ResponseXExpiresAfter, "X-Expires-After") => [chrono::DateTime<chrono::Utc>] }
                    let response_x_expires_after = response.headers.get::<ResponseXExpiresAfter>().ok_or_else(|| "Required response header X-Expires-After for response 200 was not found.")?;

                    Ok(LoginUserResponse::SuccessfulOperation{ body: body, x_rate_limit: response_x_rate_limit.0.clone(), x_expires_after: response_x_expires_after.0.clone() })
                },
                400 => {


                    Ok(LoginUserResponse::InvalidUsername)
                },
                code => {
                    let mut buf = [0; 100];
                    let debug_body = match response.read(&mut buf) {
                        Ok(len) => match str::from_utf8(&buf[..len]) {
                            Ok(body) => Cow::from(body),
                            Err(_) => Cow::from(format!("<Body was not UTF8: {:?}>", &buf[..len].to_vec())),
                        },
                        Err(e) => Cow::from(format!("<Failed to read body: {}>", e)),
                    };
                    Err(ApiError(format!("Unexpected response code {}:\n{:?}\n\n{}",
                                         code,
                                         response.headers,
                                         debug_body)))
                }
            }
        }

        let result = request.send().map_err(|e| ApiError(format!("No response received: {}", e))).and_then(parse_response);
        Box::new(futures::done(result))
    }

    fn logout_user(&self, context: &Context) -> Box<Future<Item=LogoutUserResponse, Error=ApiError> + Send> {


        let url = format!(
            "{}/v2/user/logout",
            self.base_path
        );


        let hyper_client = (self.hyper_client)();
        let request = hyper_client.request(hyper::method::Method::Get, &url);
        let mut custom_headers = hyper::header::Headers::new();

        context.x_span_id.as_ref().map(|header| custom_headers.set(XSpanId(header.clone())));


        let request = request.headers(custom_headers);

        // Helper function to provide a code block to use `?` in (to be replaced by the `catch` block when it exists).
        fn parse_response(mut response: hyper::client::response::Response) -> Result<LogoutUserResponse, ApiError> {
            match response.status.to_u16() {
                0 => {


                    Ok(LogoutUserResponse::SuccessfulOperation)
                },
                code => {
                    let mut buf = [0; 100];
                    let debug_body = match response.read(&mut buf) {
                        Ok(len) => match str::from_utf8(&buf[..len]) {
                            Ok(body) => Cow::from(body),
                            Err(_) => Cow::from(format!("<Body was not UTF8: {:?}>", &buf[..len].to_vec())),
                        },
                        Err(e) => Cow::from(format!("<Failed to read body: {}>", e)),
                    };
                    Err(ApiError(format!("Unexpected response code {}:\n{:?}\n\n{}",
                                         code,
                                         response.headers,
                                         debug_body)))
                }
            }
        }

        let result = request.send().map_err(|e| ApiError(format!("No response received: {}", e))).and_then(parse_response);
        Box::new(futures::done(result))
    }

    fn update_user(&self, param_username: String, param_body: models::User, context: &Context) -> Box<Future<Item=UpdateUserResponse, Error=ApiError> + Send> {


        let url = format!(
            "{}/v2/user/{username}",
            self.base_path, username=utf8_percent_encode(&param_username.to_string(), PATH_SEGMENT_ENCODE_SET)
        );


        let body = serde_json::to_string(&param_body).expect("impossible to fail to serialize");

        let hyper_client = (self.hyper_client)();
        let request = hyper_client.request(hyper::method::Method::Put, &url);
        let mut custom_headers = hyper::header::Headers::new();

        let request = request.body(&body);

        custom_headers.set(ContentType(mimetypes::requests::UPDATE_USER.clone()));
        context.x_span_id.as_ref().map(|header| custom_headers.set(XSpanId(header.clone())));


        let request = request.headers(custom_headers);

        // Helper function to provide a code block to use `?` in (to be replaced by the `catch` block when it exists).
        fn parse_response(mut response: hyper::client::response::Response) -> Result<UpdateUserResponse, ApiError> {
            match response.status.to_u16() {
                400 => {


                    Ok(UpdateUserResponse::InvalidUserSupplied)
                },
                404 => {


                    Ok(UpdateUserResponse::UserNotFound)
                },
                code => {
                    let mut buf = [0; 100];
                    let debug_body = match response.read(&mut buf) {
                        Ok(len) => match str::from_utf8(&buf[..len]) {
                            Ok(body) => Cow::from(body),
                            Err(_) => Cow::from(format!("<Body was not UTF8: {:?}>", &buf[..len].to_vec())),
                        },
                        Err(e) => Cow::from(format!("<Failed to read body: {}>", e)),
                    };
                    Err(ApiError(format!("Unexpected response code {}:\n{:?}\n\n{}",
                                         code,
                                         response.headers,
                                         debug_body)))
                }
            }
        }

        let result = request.send().map_err(|e| ApiError(format!("No response received: {}", e))).and_then(parse_response);
        Box::new(futures::done(result))
    }

}

#[derive(Debug)]
pub enum ClientInitError {
    InvalidScheme,
    InvalidUrl(hyper::error::ParseError),
    MissingHost,
    SslError(openssl::error::ErrorStack)
}

impl From<hyper::error::ParseError> for ClientInitError {
    fn from(err: hyper::error::ParseError) -> ClientInitError {
        ClientInitError::InvalidUrl(err)
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
