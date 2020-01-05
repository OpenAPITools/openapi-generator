#![allow(missing_docs, unused_variables, trivial_casts)]

extern crate openapi_v3;
extern crate futures;
#[macro_use]
extern crate swagger;
extern crate clap;
extern crate tokio;

use swagger::{ContextBuilder, EmptyContext, XSpanIdString, Has, Push, AuthData};

#[allow(unused_imports)]
use futures::{Future, future, Stream, stream};
#[allow(unused_imports)]
use openapi_v3::{Api, ApiNoContext, Client, ContextWrapperExt,
                      ApiError,
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
use clap::{App, Arg};

fn main() {
    let matches = App::new("client")
        .arg(Arg::with_name("operation")
            .help("Sets the operation to run")
            .possible_values(&[

                "MultigetGet",

                "MultipleAuthSchemeGet",

                "ReadonlyAuthSchemeGet",

                "RequiredOctetStreamPut",

                "ResponsesWithHeadersGet",

                "UuidGet",

                "XmlExtraPost",

                "XmlOtherPost",

                "XmlOtherPut",

                "XmlPost",

                "XmlPut",

            ])
            .required(true)
            .index(1))
        .arg(Arg::with_name("https")
            .long("https")
            .help("Whether to use HTTPS or not"))
        .arg(Arg::with_name("host")
            .long("host")
            .takes_value(true)
            .default_value("localhost")
            .help("Hostname to contact"))
        .arg(Arg::with_name("port")
            .long("port")
            .takes_value(true)
            .default_value("80")
            .help("Port to contact"))
        .get_matches();

    let is_https = matches.is_present("https");
    let base_url = format!("{}://{}:{}",
                           if is_https { "https" } else { "http" },
                           matches.value_of("host").unwrap(),
                           matches.value_of("port").unwrap());

    let client = if matches.is_present("https") {
        // Using Simple HTTPS
        Client::try_new_https(
            &base_url,
            "examples/ca.pem")
            .expect("Failed to create HTTPS client")
    } else {
        // Using HTTP
        Client::try_new_http(
            &base_url)
            .expect("Failed to create HTTP client")
    };

    let context: make_context_ty!(ContextBuilder, EmptyContext, Option<AuthData>, XSpanIdString) =
        make_context!(ContextBuilder, EmptyContext, None as Option<AuthData>, XSpanIdString::default());

    let client = client.with_context(context);

    match matches.value_of("operation") {

        Some("MultigetGet") => {
            let mut rt = tokio::runtime::Runtime::new().unwrap();
            let result = rt.block_on(client.multiget_get(
            ));
            println!("{:?} (X-Span-ID: {:?})", result, (client.context() as &dyn Has<XSpanIdString>).get().clone());
        },

        Some("MultipleAuthSchemeGet") => {
            let mut rt = tokio::runtime::Runtime::new().unwrap();
            let result = rt.block_on(client.multiple_auth_scheme_get(
            ));
            println!("{:?} (X-Span-ID: {:?})", result, (client.context() as &dyn Has<XSpanIdString>).get().clone());
        },

        Some("ReadonlyAuthSchemeGet") => {
            let mut rt = tokio::runtime::Runtime::new().unwrap();
            let result = rt.block_on(client.readonly_auth_scheme_get(
            ));
            println!("{:?} (X-Span-ID: {:?})", result, (client.context() as &dyn Has<XSpanIdString>).get().clone());
        },

        Some("RequiredOctetStreamPut") => {
            let mut rt = tokio::runtime::Runtime::new().unwrap();
            let result = rt.block_on(client.required_octet_stream_put(
                  swagger::ByteArray(Vec::from("BYTE_ARRAY_DATA_HERE"))
            ));
            println!("{:?} (X-Span-ID: {:?})", result, (client.context() as &dyn Has<XSpanIdString>).get().clone());
        },

        Some("ResponsesWithHeadersGet") => {
            let mut rt = tokio::runtime::Runtime::new().unwrap();
            let result = rt.block_on(client.responses_with_headers_get(
            ));
            println!("{:?} (X-Span-ID: {:?})", result, (client.context() as &dyn Has<XSpanIdString>).get().clone());
        },

        Some("UuidGet") => {
            let mut rt = tokio::runtime::Runtime::new().unwrap();
            let result = rt.block_on(client.uuid_get(
            ));
            println!("{:?} (X-Span-ID: {:?})", result, (client.context() as &dyn Has<XSpanIdString>).get().clone());
        },

        Some("XmlExtraPost") => {
            let mut rt = tokio::runtime::Runtime::new().unwrap();
            let result = rt.block_on(client.xml_extra_post(
                  None
            ));
            println!("{:?} (X-Span-ID: {:?})", result, (client.context() as &dyn Has<XSpanIdString>).get().clone());
        },

        Some("XmlOtherPost") => {
            let mut rt = tokio::runtime::Runtime::new().unwrap();
            let result = rt.block_on(client.xml_other_post(
                  None
            ));
            println!("{:?} (X-Span-ID: {:?})", result, (client.context() as &dyn Has<XSpanIdString>).get().clone());
        },

        Some("XmlOtherPut") => {
            let mut rt = tokio::runtime::Runtime::new().unwrap();
            let result = rt.block_on(client.xml_other_put(
                  None
            ));
            println!("{:?} (X-Span-ID: {:?})", result, (client.context() as &dyn Has<XSpanIdString>).get().clone());
        },

        Some("XmlPost") => {
            let mut rt = tokio::runtime::Runtime::new().unwrap();
            let result = rt.block_on(client.xml_post(
                  None
            ));
            println!("{:?} (X-Span-ID: {:?})", result, (client.context() as &dyn Has<XSpanIdString>).get().clone());
        },

        Some("XmlPut") => {
            let mut rt = tokio::runtime::Runtime::new().unwrap();
            let result = rt.block_on(client.xml_put(
                  None
            ));
            println!("{:?} (X-Span-ID: {:?})", result, (client.context() as &dyn Has<XSpanIdString>).get().clone());
        },

        _ => {
            panic!("Invalid operation provided")
        }
    }
}
