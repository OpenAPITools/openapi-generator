#![allow(missing_docs, unused_variables, trivial_casts)]

extern crate ops_v3;
extern crate futures;
#[macro_use]
extern crate swagger;
extern crate clap;
extern crate tokio;

use swagger::{ContextBuilder, EmptyContext, XSpanIdString, Has, Push, AuthData};

#[allow(unused_imports)]
use futures::{Future, future, Stream, stream};
#[allow(unused_imports)]
use ops_v3::{Api, ApiNoContext, Client, ContextWrapperExt,
                      ApiError,
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
use clap::{App, Arg};

fn main() {
    let matches = App::new("client")
        .arg(Arg::with_name("operation")
            .help("Sets the operation to run")
            .possible_values(&[

                "Op10Get",

                "Op11Get",

                "Op12Get",

                "Op13Get",

                "Op14Get",

                "Op15Get",

                "Op16Get",

                "Op17Get",

                "Op18Get",

                "Op19Get",

                "Op1Get",

                "Op20Get",

                "Op21Get",

                "Op22Get",

                "Op23Get",

                "Op24Get",

                "Op25Get",

                "Op26Get",

                "Op27Get",

                "Op28Get",

                "Op29Get",

                "Op2Get",

                "Op30Get",

                "Op31Get",

                "Op32Get",

                "Op33Get",

                "Op34Get",

                "Op35Get",

                "Op36Get",

                "Op37Get",

                "Op3Get",

                "Op4Get",

                "Op5Get",

                "Op6Get",

                "Op7Get",

                "Op8Get",

                "Op9Get",

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

        Some("Op10Get") => {
            let mut rt = tokio::runtime::Runtime::new().unwrap();
            let result = rt.block_on(client.op10_get(
            ));
            println!("{:?} (X-Span-ID: {:?})", result, (client.context() as &dyn Has<XSpanIdString>).get().clone());
        },

        Some("Op11Get") => {
            let mut rt = tokio::runtime::Runtime::new().unwrap();
            let result = rt.block_on(client.op11_get(
            ));
            println!("{:?} (X-Span-ID: {:?})", result, (client.context() as &dyn Has<XSpanIdString>).get().clone());
        },

        Some("Op12Get") => {
            let mut rt = tokio::runtime::Runtime::new().unwrap();
            let result = rt.block_on(client.op12_get(
            ));
            println!("{:?} (X-Span-ID: {:?})", result, (client.context() as &dyn Has<XSpanIdString>).get().clone());
        },

        Some("Op13Get") => {
            let mut rt = tokio::runtime::Runtime::new().unwrap();
            let result = rt.block_on(client.op13_get(
            ));
            println!("{:?} (X-Span-ID: {:?})", result, (client.context() as &dyn Has<XSpanIdString>).get().clone());
        },

        Some("Op14Get") => {
            let mut rt = tokio::runtime::Runtime::new().unwrap();
            let result = rt.block_on(client.op14_get(
            ));
            println!("{:?} (X-Span-ID: {:?})", result, (client.context() as &dyn Has<XSpanIdString>).get().clone());
        },

        Some("Op15Get") => {
            let mut rt = tokio::runtime::Runtime::new().unwrap();
            let result = rt.block_on(client.op15_get(
            ));
            println!("{:?} (X-Span-ID: {:?})", result, (client.context() as &dyn Has<XSpanIdString>).get().clone());
        },

        Some("Op16Get") => {
            let mut rt = tokio::runtime::Runtime::new().unwrap();
            let result = rt.block_on(client.op16_get(
            ));
            println!("{:?} (X-Span-ID: {:?})", result, (client.context() as &dyn Has<XSpanIdString>).get().clone());
        },

        Some("Op17Get") => {
            let mut rt = tokio::runtime::Runtime::new().unwrap();
            let result = rt.block_on(client.op17_get(
            ));
            println!("{:?} (X-Span-ID: {:?})", result, (client.context() as &dyn Has<XSpanIdString>).get().clone());
        },

        Some("Op18Get") => {
            let mut rt = tokio::runtime::Runtime::new().unwrap();
            let result = rt.block_on(client.op18_get(
            ));
            println!("{:?} (X-Span-ID: {:?})", result, (client.context() as &dyn Has<XSpanIdString>).get().clone());
        },

        Some("Op19Get") => {
            let mut rt = tokio::runtime::Runtime::new().unwrap();
            let result = rt.block_on(client.op19_get(
            ));
            println!("{:?} (X-Span-ID: {:?})", result, (client.context() as &dyn Has<XSpanIdString>).get().clone());
        },

        Some("Op1Get") => {
            let mut rt = tokio::runtime::Runtime::new().unwrap();
            let result = rt.block_on(client.op1_get(
            ));
            println!("{:?} (X-Span-ID: {:?})", result, (client.context() as &dyn Has<XSpanIdString>).get().clone());
        },

        Some("Op20Get") => {
            let mut rt = tokio::runtime::Runtime::new().unwrap();
            let result = rt.block_on(client.op20_get(
            ));
            println!("{:?} (X-Span-ID: {:?})", result, (client.context() as &dyn Has<XSpanIdString>).get().clone());
        },

        Some("Op21Get") => {
            let mut rt = tokio::runtime::Runtime::new().unwrap();
            let result = rt.block_on(client.op21_get(
            ));
            println!("{:?} (X-Span-ID: {:?})", result, (client.context() as &dyn Has<XSpanIdString>).get().clone());
        },

        Some("Op22Get") => {
            let mut rt = tokio::runtime::Runtime::new().unwrap();
            let result = rt.block_on(client.op22_get(
            ));
            println!("{:?} (X-Span-ID: {:?})", result, (client.context() as &dyn Has<XSpanIdString>).get().clone());
        },

        Some("Op23Get") => {
            let mut rt = tokio::runtime::Runtime::new().unwrap();
            let result = rt.block_on(client.op23_get(
            ));
            println!("{:?} (X-Span-ID: {:?})", result, (client.context() as &dyn Has<XSpanIdString>).get().clone());
        },

        Some("Op24Get") => {
            let mut rt = tokio::runtime::Runtime::new().unwrap();
            let result = rt.block_on(client.op24_get(
            ));
            println!("{:?} (X-Span-ID: {:?})", result, (client.context() as &dyn Has<XSpanIdString>).get().clone());
        },

        Some("Op25Get") => {
            let mut rt = tokio::runtime::Runtime::new().unwrap();
            let result = rt.block_on(client.op25_get(
            ));
            println!("{:?} (X-Span-ID: {:?})", result, (client.context() as &dyn Has<XSpanIdString>).get().clone());
        },

        Some("Op26Get") => {
            let mut rt = tokio::runtime::Runtime::new().unwrap();
            let result = rt.block_on(client.op26_get(
            ));
            println!("{:?} (X-Span-ID: {:?})", result, (client.context() as &dyn Has<XSpanIdString>).get().clone());
        },

        Some("Op27Get") => {
            let mut rt = tokio::runtime::Runtime::new().unwrap();
            let result = rt.block_on(client.op27_get(
            ));
            println!("{:?} (X-Span-ID: {:?})", result, (client.context() as &dyn Has<XSpanIdString>).get().clone());
        },

        Some("Op28Get") => {
            let mut rt = tokio::runtime::Runtime::new().unwrap();
            let result = rt.block_on(client.op28_get(
            ));
            println!("{:?} (X-Span-ID: {:?})", result, (client.context() as &dyn Has<XSpanIdString>).get().clone());
        },

        Some("Op29Get") => {
            let mut rt = tokio::runtime::Runtime::new().unwrap();
            let result = rt.block_on(client.op29_get(
            ));
            println!("{:?} (X-Span-ID: {:?})", result, (client.context() as &dyn Has<XSpanIdString>).get().clone());
        },

        Some("Op2Get") => {
            let mut rt = tokio::runtime::Runtime::new().unwrap();
            let result = rt.block_on(client.op2_get(
            ));
            println!("{:?} (X-Span-ID: {:?})", result, (client.context() as &dyn Has<XSpanIdString>).get().clone());
        },

        Some("Op30Get") => {
            let mut rt = tokio::runtime::Runtime::new().unwrap();
            let result = rt.block_on(client.op30_get(
            ));
            println!("{:?} (X-Span-ID: {:?})", result, (client.context() as &dyn Has<XSpanIdString>).get().clone());
        },

        Some("Op31Get") => {
            let mut rt = tokio::runtime::Runtime::new().unwrap();
            let result = rt.block_on(client.op31_get(
            ));
            println!("{:?} (X-Span-ID: {:?})", result, (client.context() as &dyn Has<XSpanIdString>).get().clone());
        },

        Some("Op32Get") => {
            let mut rt = tokio::runtime::Runtime::new().unwrap();
            let result = rt.block_on(client.op32_get(
            ));
            println!("{:?} (X-Span-ID: {:?})", result, (client.context() as &dyn Has<XSpanIdString>).get().clone());
        },

        Some("Op33Get") => {
            let mut rt = tokio::runtime::Runtime::new().unwrap();
            let result = rt.block_on(client.op33_get(
            ));
            println!("{:?} (X-Span-ID: {:?})", result, (client.context() as &dyn Has<XSpanIdString>).get().clone());
        },

        Some("Op34Get") => {
            let mut rt = tokio::runtime::Runtime::new().unwrap();
            let result = rt.block_on(client.op34_get(
            ));
            println!("{:?} (X-Span-ID: {:?})", result, (client.context() as &dyn Has<XSpanIdString>).get().clone());
        },

        Some("Op35Get") => {
            let mut rt = tokio::runtime::Runtime::new().unwrap();
            let result = rt.block_on(client.op35_get(
            ));
            println!("{:?} (X-Span-ID: {:?})", result, (client.context() as &dyn Has<XSpanIdString>).get().clone());
        },

        Some("Op36Get") => {
            let mut rt = tokio::runtime::Runtime::new().unwrap();
            let result = rt.block_on(client.op36_get(
            ));
            println!("{:?} (X-Span-ID: {:?})", result, (client.context() as &dyn Has<XSpanIdString>).get().clone());
        },

        Some("Op37Get") => {
            let mut rt = tokio::runtime::Runtime::new().unwrap();
            let result = rt.block_on(client.op37_get(
            ));
            println!("{:?} (X-Span-ID: {:?})", result, (client.context() as &dyn Has<XSpanIdString>).get().clone());
        },

        Some("Op3Get") => {
            let mut rt = tokio::runtime::Runtime::new().unwrap();
            let result = rt.block_on(client.op3_get(
            ));
            println!("{:?} (X-Span-ID: {:?})", result, (client.context() as &dyn Has<XSpanIdString>).get().clone());
        },

        Some("Op4Get") => {
            let mut rt = tokio::runtime::Runtime::new().unwrap();
            let result = rt.block_on(client.op4_get(
            ));
            println!("{:?} (X-Span-ID: {:?})", result, (client.context() as &dyn Has<XSpanIdString>).get().clone());
        },

        Some("Op5Get") => {
            let mut rt = tokio::runtime::Runtime::new().unwrap();
            let result = rt.block_on(client.op5_get(
            ));
            println!("{:?} (X-Span-ID: {:?})", result, (client.context() as &dyn Has<XSpanIdString>).get().clone());
        },

        Some("Op6Get") => {
            let mut rt = tokio::runtime::Runtime::new().unwrap();
            let result = rt.block_on(client.op6_get(
            ));
            println!("{:?} (X-Span-ID: {:?})", result, (client.context() as &dyn Has<XSpanIdString>).get().clone());
        },

        Some("Op7Get") => {
            let mut rt = tokio::runtime::Runtime::new().unwrap();
            let result = rt.block_on(client.op7_get(
            ));
            println!("{:?} (X-Span-ID: {:?})", result, (client.context() as &dyn Has<XSpanIdString>).get().clone());
        },

        Some("Op8Get") => {
            let mut rt = tokio::runtime::Runtime::new().unwrap();
            let result = rt.block_on(client.op8_get(
            ));
            println!("{:?} (X-Span-ID: {:?})", result, (client.context() as &dyn Has<XSpanIdString>).get().clone());
        },

        Some("Op9Get") => {
            let mut rt = tokio::runtime::Runtime::new().unwrap();
            let result = rt.block_on(client.op9_get(
            ));
            println!("{:?} (X-Span-ID: {:?})", result, (client.context() as &dyn Has<XSpanIdString>).get().clone());
        },

        _ => {
            panic!("Invalid operation provided")
        }
    }
}
