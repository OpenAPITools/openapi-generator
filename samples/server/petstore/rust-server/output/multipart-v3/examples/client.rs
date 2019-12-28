#![allow(missing_docs, unused_variables, trivial_casts)]

extern crate multipart_v3;
extern crate futures;
#[macro_use]
extern crate swagger;
extern crate clap;
extern crate tokio;

use swagger::{ContextBuilder, EmptyContext, XSpanIdString, Has, Push, AuthData};

#[allow(unused_imports)]
use futures::{Future, future, Stream, stream};
#[allow(unused_imports)]
use multipart_v3::{Api, ApiNoContext, Client, ContextWrapperExt,
                      ApiError,
                      MultipartRequestPostResponse
                     };
use clap::{App, Arg};

fn main() {
    let matches = App::new("client")
        .arg(Arg::with_name("operation")
            .help("Sets the operation to run")
            .possible_values(&[

                "MultipartRequestPost",

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

        Some("MultipartRequestPost") => {
            let mut rt = tokio::runtime::Runtime::new().unwrap();
            let result = rt.block_on(client.multipart_request_post(
                  "string_field_example".to_string(),
                  swagger::ByteArray(Vec::from("BYTE_ARRAY_DATA_HERE")),
                  Some("optional_string_field_example".to_string()),
                  None
            ));
            println!("{:?} (X-Span-ID: {:?})", result, (client.context() as &dyn Has<XSpanIdString>).get().clone());
        },

        _ => {
            panic!("Invalid operation provided")
        }
    }
}
