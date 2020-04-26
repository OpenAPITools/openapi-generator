#![allow(missing_docs, unused_variables, trivial_casts)]


#[allow(unused_imports)]
use futures::{Future, future, Stream, stream};
#[allow(unused_imports)]
use no_example_v3::{Api, ApiNoContext, Client, ContextWrapperExt, models,
                      ApiError,
                      OpGetResponse
                     };
use clap::{App, Arg};

#[allow(unused_imports)]
use log::info;

// swagger::Has may be unused if there are no examples
#[allow(unused_imports)]
use swagger::{ContextBuilder, EmptyContext, XSpanIdString, Has, Push, AuthData};

// rt may be unused if there are no examples
#[allow(unused_mut)]
fn main() {
    env_logger::init();

    let matches = App::new("client")
        .arg(Arg::with_name("operation")
            .help("Sets the operation to run")
            .possible_values(&[
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
            .default_value("8080")
            .help("Port to contact"))
        .get_matches();

    let is_https = matches.is_present("https");
    let base_url = format!("{}://{}:{}",
                           if is_https { "https" } else { "http" },
                           matches.value_of("host").unwrap(),
                           matches.value_of("port").unwrap());

    let client = if matches.is_present("https") {
        // Using Simple HTTPS
        Client::try_new_https(&base_url)
            .expect("Failed to create HTTPS client")
    } else {
        // Using HTTP
        Client::try_new_http(
            &base_url)
            .expect("Failed to create HTTP client")
    };

    let context: swagger::make_context_ty!(ContextBuilder, EmptyContext, Option<AuthData>, XSpanIdString) =
        swagger::make_context!(ContextBuilder, EmptyContext, None as Option<AuthData>, XSpanIdString::default());

    let client = client.with_context(context);

    let mut rt = tokio::runtime::Runtime::new().unwrap();

    match matches.value_of("operation") {
        /* Disabled because there's no example.
        Some("OpGet") => {
            let result = rt.block_on(client.op_get(
                  ???
            ));
            info!("{:?} (X-Span-ID: {:?})", result, (client.context() as &dyn Has<XSpanIdString>).get().clone());
        },
        */
        _ => {
            panic!("Invalid operation provided")
        }
    }
}
