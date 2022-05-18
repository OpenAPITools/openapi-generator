#![allow(missing_docs, unused_variables, trivial_casts)]

mod server;

#[allow(unused_imports)]
use futures::{future, Stream, stream};
#[allow(unused_imports)]
use openapi_v3::{Api, ApiNoContext, Client, ContextWrapperExt, models,
                      AnyOfGetResponse,
                      CallbackWithHeaderPostResponse,
                      ComplexQueryParamGetResponse,
                      EnumInPathPathParamGetResponse,
                      JsonComplexQueryParamGetResponse,
                      MandatoryRequestHeaderGetResponse,
                      MergePatchJsonGetResponse,
                      MultigetGetResponse,
                      MultipleAuthSchemeGetResponse,
                      OneOfGetResponse,
                      OverrideServerGetResponse,
                      ParamgetGetResponse,
                      ReadonlyAuthSchemeGetResponse,
                      RegisterCallbackPostResponse,
                      RequiredOctetStreamPutResponse,
                      ResponsesWithHeadersGetResponse,
                      Rfc7807GetResponse,
                      UntypedPropertyGetResponse,
                      UuidGetResponse,
                      XmlExtraPostResponse,
                      XmlOtherPostResponse,
                      XmlOtherPutResponse,
                      XmlPostResponse,
                      XmlPutResponse,
                      CreateRepoResponse,
                      GetRepoInfoResponse,
                     };
use clap::{App, Arg};

#[allow(unused_imports)]
use log::info;

// swagger::Has may be unused if there are no examples
#[allow(unused_imports)]
use swagger::{AuthData, ContextBuilder, EmptyContext, Has, Push, XSpanIdString};

type ClientContext = swagger::make_context_ty!(ContextBuilder, EmptyContext, Option<AuthData>, XSpanIdString);

// rt may be unused if there are no examples
#[allow(unused_mut)]
fn main() {
    env_logger::init();

    let matches = App::new("client")
        .arg(Arg::with_name("operation")
            .help("Sets the operation to run")
            .possible_values(&[
                "AnyOfGet",
                "CallbackWithHeaderPost",
                "ComplexQueryParamGet",
                "JsonComplexQueryParamGet",
                "MandatoryRequestHeaderGet",
                "MergePatchJsonGet",
                "MultigetGet",
                "MultipleAuthSchemeGet",
                "OneOfGet",
                "OverrideServerGet",
                "ParamgetGet",
                "ReadonlyAuthSchemeGet",
                "RegisterCallbackPost",
                "RequiredOctetStreamPut",
                "ResponsesWithHeadersGet",
                "Rfc7807Get",
                "UntypedPropertyGet",
                "UuidGet",
                "XmlExtraPost",
                "XmlOtherPost",
                "XmlOtherPut",
                "XmlPost",
                "XmlPut",
                "CreateRepo",
                "GetRepoInfo",
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

    let context: ClientContext =
        swagger::make_context!(ContextBuilder, EmptyContext, None as Option<AuthData>, XSpanIdString::default());

    let mut client : Box<dyn ApiNoContext<ClientContext>> = if matches.is_present("https") {
        // Using Simple HTTPS
        let client = Box::new(Client::try_new_https(&base_url)
            .expect("Failed to create HTTPS client"));
        Box::new(client.with_context(context))
    } else {
        // Using HTTP
        let client = Box::new(Client::try_new_http(
            &base_url)
            .expect("Failed to create HTTP client"));
        Box::new(client.with_context(context))
    };

    let mut rt = tokio::runtime::Runtime::new().unwrap();

    // We could do HTTPS here, but for simplicity we don't
    rt.spawn(server::create("127.0.0.1:8081", false));

    match matches.value_of("operation") {
        Some("AnyOfGet") => {
            let result = rt.block_on(client.any_of_get(
                  Some(&Vec::new())
            ));
            info!("{:?} (X-Span-ID: {:?})", result, (client.context() as &dyn Has<XSpanIdString>).get().clone());
        },
        Some("CallbackWithHeaderPost") => {
            let result = rt.block_on(client.callback_with_header_post(
                  "url_example".to_string()
            ));
            info!("{:?} (X-Span-ID: {:?})", result, (client.context() as &dyn Has<XSpanIdString>).get().clone());
        },
        Some("ComplexQueryParamGet") => {
            let result = rt.block_on(client.complex_query_param_get(
                  Some(&Vec::new())
            ));
            info!("{:?} (X-Span-ID: {:?})", result, (client.context() as &dyn Has<XSpanIdString>).get().clone());
        },
        /* Disabled because there's no example.
        Some("EnumInPathPathParamGet") => {
            let result = rt.block_on(client.enum_in_path_path_param_get(
                  ???
            ));
            info!("{:?} (X-Span-ID: {:?})", result, (client.context() as &dyn Has<XSpanIdString>).get().clone());
        },
        */
        Some("JsonComplexQueryParamGet") => {
            let result = rt.block_on(client.json_complex_query_param_get(
                  Some(&Vec::new())
            ));
            info!("{:?} (X-Span-ID: {:?})", result, (client.context() as &dyn Has<XSpanIdString>).get().clone());
        },
        Some("MandatoryRequestHeaderGet") => {
            let result = rt.block_on(client.mandatory_request_header_get(
                  "x_header_example".to_string()
            ));
            info!("{:?} (X-Span-ID: {:?})", result, (client.context() as &dyn Has<XSpanIdString>).get().clone());
        },
        Some("MergePatchJsonGet") => {
            let result = rt.block_on(client.merge_patch_json_get(
            ));
            info!("{:?} (X-Span-ID: {:?})", result, (client.context() as &dyn Has<XSpanIdString>).get().clone());
        },
        Some("MultigetGet") => {
            let result = rt.block_on(client.multiget_get(
            ));
            info!("{:?} (X-Span-ID: {:?})", result, (client.context() as &dyn Has<XSpanIdString>).get().clone());
        },
        Some("MultipleAuthSchemeGet") => {
            let result = rt.block_on(client.multiple_auth_scheme_get(
            ));
            info!("{:?} (X-Span-ID: {:?})", result, (client.context() as &dyn Has<XSpanIdString>).get().clone());
        },
        Some("OneOfGet") => {
            let result = rt.block_on(client.one_of_get(
            ));
            info!("{:?} (X-Span-ID: {:?})", result, (client.context() as &dyn Has<XSpanIdString>).get().clone());
        },
        Some("OverrideServerGet") => {
            let result = rt.block_on(client.override_server_get(
            ));
            info!("{:?} (X-Span-ID: {:?})", result, (client.context() as &dyn Has<XSpanIdString>).get().clone());
        },
        Some("ParamgetGet") => {
            let result = rt.block_on(client.paramget_get(
                  Some(serde_json::from_str::<uuid::Uuid>(r#"38400000-8cf0-11bd-b23e-10b96e4ef00d"#).expect("Failed to parse JSON example")),
                  None,
                  None
            ));
            info!("{:?} (X-Span-ID: {:?})", result, (client.context() as &dyn Has<XSpanIdString>).get().clone());
        },
        Some("ReadonlyAuthSchemeGet") => {
            let result = rt.block_on(client.readonly_auth_scheme_get(
            ));
            info!("{:?} (X-Span-ID: {:?})", result, (client.context() as &dyn Has<XSpanIdString>).get().clone());
        },
        Some("RegisterCallbackPost") => {
            let result = rt.block_on(client.register_callback_post(
                  "url_example".to_string()
            ));
            info!("{:?} (X-Span-ID: {:?})", result, (client.context() as &dyn Has<XSpanIdString>).get().clone());
        },
        Some("RequiredOctetStreamPut") => {
            let result = rt.block_on(client.required_octet_stream_put(
                  swagger::ByteArray(Vec::from("BYTE_ARRAY_DATA_HERE"))
            ));
            info!("{:?} (X-Span-ID: {:?})", result, (client.context() as &dyn Has<XSpanIdString>).get().clone());
        },
        Some("ResponsesWithHeadersGet") => {
            let result = rt.block_on(client.responses_with_headers_get(
            ));
            info!("{:?} (X-Span-ID: {:?})", result, (client.context() as &dyn Has<XSpanIdString>).get().clone());
        },
        Some("Rfc7807Get") => {
            let result = rt.block_on(client.rfc7807_get(
            ));
            info!("{:?} (X-Span-ID: {:?})", result, (client.context() as &dyn Has<XSpanIdString>).get().clone());
        },
        Some("UntypedPropertyGet") => {
            let result = rt.block_on(client.untyped_property_get(
                  None
            ));
            info!("{:?} (X-Span-ID: {:?})", result, (client.context() as &dyn Has<XSpanIdString>).get().clone());
        },
        Some("UuidGet") => {
            let result = rt.block_on(client.uuid_get(
            ));
            info!("{:?} (X-Span-ID: {:?})", result, (client.context() as &dyn Has<XSpanIdString>).get().clone());
        },
        Some("XmlExtraPost") => {
            let result = rt.block_on(client.xml_extra_post(
                  None
            ));
            info!("{:?} (X-Span-ID: {:?})", result, (client.context() as &dyn Has<XSpanIdString>).get().clone());
        },
        Some("XmlOtherPost") => {
            let result = rt.block_on(client.xml_other_post(
                  None
            ));
            info!("{:?} (X-Span-ID: {:?})", result, (client.context() as &dyn Has<XSpanIdString>).get().clone());
        },
        Some("XmlOtherPut") => {
            let result = rt.block_on(client.xml_other_put(
                  None
            ));
            info!("{:?} (X-Span-ID: {:?})", result, (client.context() as &dyn Has<XSpanIdString>).get().clone());
        },
        Some("XmlPost") => {
            let result = rt.block_on(client.xml_post(
                  None
            ));
            info!("{:?} (X-Span-ID: {:?})", result, (client.context() as &dyn Has<XSpanIdString>).get().clone());
        },
        Some("XmlPut") => {
            let result = rt.block_on(client.xml_put(
                  None
            ));
            info!("{:?} (X-Span-ID: {:?})", result, (client.context() as &dyn Has<XSpanIdString>).get().clone());
        },
        Some("CreateRepo") => {
            let result = rt.block_on(client.create_repo(
                  serde_json::from_str::<models::ObjectParam>(r#"{"requiredParam":true}"#).expect("Failed to parse JSON example")
            ));
            info!("{:?} (X-Span-ID: {:?})", result, (client.context() as &dyn Has<XSpanIdString>).get().clone());
        },
        Some("GetRepoInfo") => {
            let result = rt.block_on(client.get_repo_info(
                  "repo_id_example".to_string()
            ));
            info!("{:?} (X-Span-ID: {:?})", result, (client.context() as &dyn Has<XSpanIdString>).get().clone());
        },
        _ => {
            panic!("Invalid operation provided")
        }
    }
}
