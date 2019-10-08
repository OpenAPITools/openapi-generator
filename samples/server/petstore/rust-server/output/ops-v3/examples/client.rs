#![allow(missing_docs, unused_variables, trivial_casts)]
use openapi_context::{make_context_ty, make_context, ContextBuilder, EmptyContext, XSpanId, Has, Push, AuthData};

#[allow(unused_imports)]
use ops_v3::{ApiNoContext, ContextWrapperExt,
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
use clap::{App, Arg, ArgMatches};
use hyper_rustls::HttpsConnector;
use hyper::client::HttpConnector;
use ops_v3::Client;

async fn run_operation<'a, C>(matches: ArgMatches<'a>, client: Client<C>)
    where C: hyper::client::connect::Connect + Clone + Send + Sync + 'static
{
    let context: make_context_ty!(ContextBuilder, EmptyContext, Option<AuthData>, XSpanId) =
            make_context!(ContextBuilder, EmptyContext, None as Option<AuthData>, XSpanId(uuid::Uuid::new_v4().to_string()));
    let mut client = client.with_context(context);

    match matches.value_of("operation") {

        Some("Op10Get") => {
            let result = client.op10_get().await;
            println!("{:?} (X-Span-ID: {:?})", result, (client.context() as &dyn Has<XSpanId>).get().clone());
         },

        Some("Op11Get") => {
            let result = client.op11_get().await;
            println!("{:?} (X-Span-ID: {:?})", result, (client.context() as &dyn Has<XSpanId>).get().clone());
         },

        Some("Op12Get") => {
            let result = client.op12_get().await;
            println!("{:?} (X-Span-ID: {:?})", result, (client.context() as &dyn Has<XSpanId>).get().clone());
         },

        Some("Op13Get") => {
            let result = client.op13_get().await;
            println!("{:?} (X-Span-ID: {:?})", result, (client.context() as &dyn Has<XSpanId>).get().clone());
         },

        Some("Op14Get") => {
            let result = client.op14_get().await;
            println!("{:?} (X-Span-ID: {:?})", result, (client.context() as &dyn Has<XSpanId>).get().clone());
         },

        Some("Op15Get") => {
            let result = client.op15_get().await;
            println!("{:?} (X-Span-ID: {:?})", result, (client.context() as &dyn Has<XSpanId>).get().clone());
         },

        Some("Op16Get") => {
            let result = client.op16_get().await;
            println!("{:?} (X-Span-ID: {:?})", result, (client.context() as &dyn Has<XSpanId>).get().clone());
         },

        Some("Op17Get") => {
            let result = client.op17_get().await;
            println!("{:?} (X-Span-ID: {:?})", result, (client.context() as &dyn Has<XSpanId>).get().clone());
         },

        Some("Op18Get") => {
            let result = client.op18_get().await;
            println!("{:?} (X-Span-ID: {:?})", result, (client.context() as &dyn Has<XSpanId>).get().clone());
         },

        Some("Op19Get") => {
            let result = client.op19_get().await;
            println!("{:?} (X-Span-ID: {:?})", result, (client.context() as &dyn Has<XSpanId>).get().clone());
         },

        Some("Op1Get") => {
            let result = client.op1_get().await;
            println!("{:?} (X-Span-ID: {:?})", result, (client.context() as &dyn Has<XSpanId>).get().clone());
         },

        Some("Op20Get") => {
            let result = client.op20_get().await;
            println!("{:?} (X-Span-ID: {:?})", result, (client.context() as &dyn Has<XSpanId>).get().clone());
         },

        Some("Op21Get") => {
            let result = client.op21_get().await;
            println!("{:?} (X-Span-ID: {:?})", result, (client.context() as &dyn Has<XSpanId>).get().clone());
         },

        Some("Op22Get") => {
            let result = client.op22_get().await;
            println!("{:?} (X-Span-ID: {:?})", result, (client.context() as &dyn Has<XSpanId>).get().clone());
         },

        Some("Op23Get") => {
            let result = client.op23_get().await;
            println!("{:?} (X-Span-ID: {:?})", result, (client.context() as &dyn Has<XSpanId>).get().clone());
         },

        Some("Op24Get") => {
            let result = client.op24_get().await;
            println!("{:?} (X-Span-ID: {:?})", result, (client.context() as &dyn Has<XSpanId>).get().clone());
         },

        Some("Op25Get") => {
            let result = client.op25_get().await;
            println!("{:?} (X-Span-ID: {:?})", result, (client.context() as &dyn Has<XSpanId>).get().clone());
         },

        Some("Op26Get") => {
            let result = client.op26_get().await;
            println!("{:?} (X-Span-ID: {:?})", result, (client.context() as &dyn Has<XSpanId>).get().clone());
         },

        Some("Op27Get") => {
            let result = client.op27_get().await;
            println!("{:?} (X-Span-ID: {:?})", result, (client.context() as &dyn Has<XSpanId>).get().clone());
         },

        Some("Op28Get") => {
            let result = client.op28_get().await;
            println!("{:?} (X-Span-ID: {:?})", result, (client.context() as &dyn Has<XSpanId>).get().clone());
         },

        Some("Op29Get") => {
            let result = client.op29_get().await;
            println!("{:?} (X-Span-ID: {:?})", result, (client.context() as &dyn Has<XSpanId>).get().clone());
         },

        Some("Op2Get") => {
            let result = client.op2_get().await;
            println!("{:?} (X-Span-ID: {:?})", result, (client.context() as &dyn Has<XSpanId>).get().clone());
         },

        Some("Op30Get") => {
            let result = client.op30_get().await;
            println!("{:?} (X-Span-ID: {:?})", result, (client.context() as &dyn Has<XSpanId>).get().clone());
         },

        Some("Op31Get") => {
            let result = client.op31_get().await;
            println!("{:?} (X-Span-ID: {:?})", result, (client.context() as &dyn Has<XSpanId>).get().clone());
         },

        Some("Op32Get") => {
            let result = client.op32_get().await;
            println!("{:?} (X-Span-ID: {:?})", result, (client.context() as &dyn Has<XSpanId>).get().clone());
         },

        Some("Op33Get") => {
            let result = client.op33_get().await;
            println!("{:?} (X-Span-ID: {:?})", result, (client.context() as &dyn Has<XSpanId>).get().clone());
         },

        Some("Op34Get") => {
            let result = client.op34_get().await;
            println!("{:?} (X-Span-ID: {:?})", result, (client.context() as &dyn Has<XSpanId>).get().clone());
         },

        Some("Op35Get") => {
            let result = client.op35_get().await;
            println!("{:?} (X-Span-ID: {:?})", result, (client.context() as &dyn Has<XSpanId>).get().clone());
         },

        Some("Op36Get") => {
            let result = client.op36_get().await;
            println!("{:?} (X-Span-ID: {:?})", result, (client.context() as &dyn Has<XSpanId>).get().clone());
         },

        Some("Op37Get") => {
            let result = client.op37_get().await;
            println!("{:?} (X-Span-ID: {:?})", result, (client.context() as &dyn Has<XSpanId>).get().clone());
         },

        Some("Op3Get") => {
            let result = client.op3_get().await;
            println!("{:?} (X-Span-ID: {:?})", result, (client.context() as &dyn Has<XSpanId>).get().clone());
         },

        Some("Op4Get") => {
            let result = client.op4_get().await;
            println!("{:?} (X-Span-ID: {:?})", result, (client.context() as &dyn Has<XSpanId>).get().clone());
         },

        Some("Op5Get") => {
            let result = client.op5_get().await;
            println!("{:?} (X-Span-ID: {:?})", result, (client.context() as &dyn Has<XSpanId>).get().clone());
         },

        Some("Op6Get") => {
            let result = client.op6_get().await;
            println!("{:?} (X-Span-ID: {:?})", result, (client.context() as &dyn Has<XSpanId>).get().clone());
         },

        Some("Op7Get") => {
            let result = client.op7_get().await;
            println!("{:?} (X-Span-ID: {:?})", result, (client.context() as &dyn Has<XSpanId>).get().clone());
         },

        Some("Op8Get") => {
            let result = client.op8_get().await;
            println!("{:?} (X-Span-ID: {:?})", result, (client.context() as &dyn Has<XSpanId>).get().clone());
         },

        Some("Op9Get") => {
            let result = client.op9_get().await;
            println!("{:?} (X-Span-ID: {:?})", result, (client.context() as &dyn Has<XSpanId>).get().clone());
         },

        _ => {
            panic!("Invalid operation provided")
        }
    }
}

#[tokio::main]
async fn main() {
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
    if matches.is_present("https") {
        // Using Simple HTTPS
        let cli = Client::<HttpsConnector<HttpConnector>>::try_new_https(&base_url, "examples/ca.pem")
            .expect("Failed to create HTTPS client");
        run_operation(matches, cli).await
    } else {
        // Using HTTP
        let cli = Client::<HttpConnector>::try_new_http(&base_url)
            .expect("Failed to create HTTP client");
        run_operation(matches, cli).await
    }
}

