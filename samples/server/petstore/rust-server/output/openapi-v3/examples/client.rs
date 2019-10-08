#![allow(missing_docs, unused_variables, trivial_casts)]
use openapi_context::{make_context_ty, make_context, ContextBuilder, EmptyContext, XSpanId, Has, Push, AuthData};

#[allow(unused_imports)]
use openapi_v3::{ApiNoContext, ContextWrapperExt,
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
use clap::{App, Arg, ArgMatches};
use hyper_rustls::HttpsConnector;
use hyper::client::HttpConnector;
use openapi_v3::Client;

async fn run_operation<'a, C>(matches: ArgMatches<'a>, client: Client<C>)
    where C: hyper::client::connect::Connect + Clone + Send + Sync + 'static
{
    let context: make_context_ty!(ContextBuilder, EmptyContext, Option<AuthData>, XSpanId) =
            make_context!(ContextBuilder, EmptyContext, None as Option<AuthData>, XSpanId(uuid::Uuid::new_v4().to_string()));
    let mut client = client.with_context(context);

    match matches.value_of("operation") {

        Some("MultigetGet") => {
            let result = client.multiget_get().await;
            println!("{:?} (X-Span-ID: {:?})", result, (client.context() as &dyn Has<XSpanId>).get().clone());
         },

        Some("MultipleAuthSchemeGet") => {
            let result = client.multiple_auth_scheme_get().await;
            println!("{:?} (X-Span-ID: {:?})", result, (client.context() as &dyn Has<XSpanId>).get().clone());
         },

        Some("ReadonlyAuthSchemeGet") => {
            let result = client.readonly_auth_scheme_get().await;
            println!("{:?} (X-Span-ID: {:?})", result, (client.context() as &dyn Has<XSpanId>).get().clone());
         },

        Some("RequiredOctetStreamPut") => {
            let result = client.required_octet_stream_put(openapi_context::ByteArray(Vec::from("BYTE_ARRAY_DATA_HERE"))).await;
            println!("{:?} (X-Span-ID: {:?})", result, (client.context() as &dyn Has<XSpanId>).get().clone());
         },

        Some("ResponsesWithHeadersGet") => {
            let result = client.responses_with_headers_get().await;
            println!("{:?} (X-Span-ID: {:?})", result, (client.context() as &dyn Has<XSpanId>).get().clone());
         },

        Some("UuidGet") => {
            let result = client.uuid_get().await;
            println!("{:?} (X-Span-ID: {:?})", result, (client.context() as &dyn Has<XSpanId>).get().clone());
         },

        Some("XmlExtraPost") => {
            let result = client.xml_extra_post(None).await;
            println!("{:?} (X-Span-ID: {:?})", result, (client.context() as &dyn Has<XSpanId>).get().clone());
         },

        Some("XmlOtherPost") => {
            let result = client.xml_other_post(None).await;
            println!("{:?} (X-Span-ID: {:?})", result, (client.context() as &dyn Has<XSpanId>).get().clone());
         },

        Some("XmlOtherPut") => {
            let result = client.xml_other_put(None).await;
            println!("{:?} (X-Span-ID: {:?})", result, (client.context() as &dyn Has<XSpanId>).get().clone());
         },

        Some("XmlPost") => {
            let result = client.xml_post(None).await;
            println!("{:?} (X-Span-ID: {:?})", result, (client.context() as &dyn Has<XSpanId>).get().clone());
         },

        Some("XmlPut") => {
            let result = client.xml_put(None).await;
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

