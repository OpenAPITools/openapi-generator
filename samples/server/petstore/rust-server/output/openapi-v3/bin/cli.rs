//! CLI tool driving the API client
use anyhow::{anyhow, Context, Result};
use clap::Parser;
use log::{debug, info};
// models may be unused if all inputs are primitive types
#[allow(unused_imports)]
use openapi_v3::{
    models, ApiNoContext, Client, ContextWrapperExt,
    AnyOfGetResponse,
    CallbackWithHeaderPostResponse,
    ComplexQueryParamGetResponse,
    ExamplesTestResponse,
    FormTestResponse,
    GetWithBooleanParameterResponse,
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
    TwoFirstLetterHeadersResponse,
    UntypedPropertyGetResponse,
    UuidGetResponse,
    XmlExtraPostResponse,
    XmlOtherPostResponse,
    XmlOtherPutResponse,
    XmlPostResponse,
    XmlPutResponse,
    EnumInPathPathParamGetResponse,
    MultiplePathParamsWithVeryLongPathToTestFormattingPathParamAPathParamBGetResponse,
    CreateRepoResponse,
    GetRepoInfoResponse,
};
use simple_logger::SimpleLogger;
use swagger::{AuthData, ContextBuilder, EmptyContext, Push, XSpanIdString};

type ClientContext = swagger::make_context_ty!(
    ContextBuilder,
    EmptyContext,
    Option<AuthData>,
    XSpanIdString
);

#[derive(Parser, Debug)]
#[clap(
    name = "My title",
    version = "1.0.7",
    about = "CLI access to My title"
)]
struct Cli {
    #[clap(subcommand)]
    operation: Operation,

    /// Address or hostname of the server hosting this API, including optional port
    #[clap(short = 'a', long, default_value = "http://localhost")]
    server_address: String,

    /// Path to the client private key if using client-side TLS authentication
    #[cfg(not(any(target_os = "macos", target_os = "windows", target_os = "ios")))]
    #[clap(long, requires_all(&["client_certificate", "server_certificate"]))]
    client_key: Option<String>,

    /// Path to the client's public certificate associated with the private key
    #[cfg(not(any(target_os = "macos", target_os = "windows", target_os = "ios")))]
    #[clap(long, requires_all(&["client_key", "server_certificate"]))]
    client_certificate: Option<String>,

    /// Path to CA certificate used to authenticate the server
    #[cfg(not(any(target_os = "macos", target_os = "windows", target_os = "ios")))]
    #[clap(long)]
    server_certificate: Option<String>,

    /// If set, write output to file instead of stdout
    #[clap(short, long)]
    output_file: Option<String>,

    #[command(flatten)]
    verbosity: clap_verbosity_flag::Verbosity,

    /// Bearer token if used for authentication
    #[arg(env = "OPENAPI_V3_BEARER_TOKEN", hide_env = true)]
    bearer_token: Option<String>,
}

#[derive(Parser, Debug)]
enum Operation {
    AnyOfGet {
        /// list of any of objects
        #[clap(value_parser = parse_json::<Vec<models::AnyOfObject>>, long)]
        any_of: Option<Vec<models::AnyOfObject>>,
    },
    CallbackWithHeaderPost {
        url: String,
    },
    ComplexQueryParamGet {
        #[clap(value_parser = parse_json::<Vec<models::StringObject>>, long)]
        list_of_strings: Option<Vec<models::StringObject>>,
    },
    /// Test examples
    ExamplesTest {
        /// A list of IDs to get
        #[clap(value_parser = parse_json::<Vec<String>>, long)]
        ids: Option<Vec<String>>,
    },
    /// Test a Form Post
    FormTest {
        #[clap(value_parser = parse_json::<Vec<String>>, long)]
        required_array: Option<Vec<String>>,
    },
    GetWithBooleanParameter {
        /// Let's check apostrophes get encoded properly!
        #[clap(short, long)]
        iambool: bool,
    },
    JsonComplexQueryParamGet {
        #[clap(value_parser = parse_json::<Vec<models::StringObject>>, long)]
        list_of_strings: Option<Vec<models::StringObject>>,
    },
    MandatoryRequestHeaderGet {
        x_header: String,
    },
    MergePatchJsonGet {
    },
    /// Get some stuff.
    MultigetGet {
    },
    MultipleAuthSchemeGet {
    },
    OneOfGet {
    },
    OverrideServerGet {
    },
    /// Get some stuff with parameters.
    ParamgetGet {
        /// The stuff to get
        #[clap(value_parser = parse_json::<uuid::Uuid>)]
        uuid: Option<uuid::Uuid>,
        /// Some object to pass as query parameter
        #[clap(value_parser = parse_json::<models::ObjectParam>)]
        some_object: Option<models::ObjectParam>,
        /// Some list to pass as query parameter
        #[clap(value_parser = parse_json::<models::MyIdList>)]
        some_list: Option<models::MyIdList>,
    },
    ReadonlyAuthSchemeGet {
    },
    RegisterCallbackPost {
        url: String,
    },
    RequiredOctetStreamPut {
        #[clap(value_parser = parse_json::<swagger::ByteArray>)]
        body: swagger::ByteArray,
    },
    ResponsesWithHeadersGet {
    },
    Rfc7807Get {
    },
    TwoFirstLetterHeaders {
        #[clap(long)]
        x_header_one: Option<bool>,
        #[clap(long)]
        x_header_two: Option<bool>,
    },
    UntypedPropertyGet {
        #[clap(value_parser = parse_json::<models::ObjectUntypedProps>)]
        object_untyped_props: Option<models::ObjectUntypedProps>,
    },
    UuidGet {
    },
    XmlExtraPost {
        #[clap(value_parser = parse_json::<models::DuplicateXmlObject>)]
        duplicate_xml_object: Option<models::DuplicateXmlObject>,
    },
    XmlOtherPost {
        #[clap(value_parser = parse_json::<models::AnotherXmlObject>)]
        another_xml_object: Option<models::AnotherXmlObject>,
    },
    XmlOtherPut {
        #[clap(value_parser = parse_json::<models::AnotherXmlArray>)]
        another_xml_array: Option<models::AnotherXmlArray>,
    },
    /// Post an array.  It's important we test apostrophes, so include one here.
    XmlPost {
        #[clap(value_parser = parse_json::<models::XmlArray>)]
        xml_array: Option<models::XmlArray>,
    },
    XmlPut {
        #[clap(value_parser = parse_json::<models::XmlObject>)]
        xml_object: Option<models::XmlObject>,
    },
    EnumInPathPathParamGet {
        #[clap(value_parser = parse_json::<models::StringEnum>)]
        path_param: models::StringEnum,
    },
    MultiplePathParamsWithVeryLongPathToTestFormattingPathParamAPathParamBGet {
        path_param_a: String,
        path_param_b: String,
    },
    CreateRepo {
        #[clap(value_parser = parse_json::<models::ObjectParam>)]
        object_param: models::ObjectParam,
    },
    GetRepoInfo {
        repo_id: String,
    },
}

#[cfg(not(any(target_os = "macos", target_os = "windows", target_os = "ios")))]
fn create_client(args: &Cli, context: ClientContext) -> Result<Box<dyn ApiNoContext<ClientContext>>> {
    if args.client_certificate.is_some() {
        debug!("Using mutual TLS");
        let client = Client::try_new_https_mutual(
            &args.server_address,
            args.server_certificate.clone().unwrap(),
            args.client_key.clone().unwrap(),
            args.client_certificate.clone().unwrap(),
        )
        .context("Failed to create HTTPS client")?;
        Ok(Box::new(client.with_context(context)))
    } else if args.server_certificate.is_some() {
        debug!("Using TLS with pinned server certificate");
        let client =
            Client::try_new_https_pinned(&args.server_address, args.server_certificate.clone().unwrap())
                .context("Failed to create HTTPS client")?;
        Ok(Box::new(client.with_context(context)))
    } else {
        debug!("Using client without certificates");
        let client =
            Client::try_new(&args.server_address).context("Failed to create HTTP(S) client")?;
        Ok(Box::new(client.with_context(context)))
    }
}

#[cfg(any(target_os = "macos", target_os = "windows", target_os = "ios"))]
fn create_client(args: &Cli, context: ClientContext) -> Result<Box<dyn ApiNoContext<ClientContext>>> {
    let client =
        Client::try_new(&args.server_address).context("Failed to create HTTP(S) client")?;
    Ok(Box::new(client.with_context(context)))
}

#[tokio::main]
async fn main() -> Result<()> {
    let args = Cli::parse();
    if let Some(log_level) = args.verbosity.log_level() {
        SimpleLogger::new().with_level(log_level.to_level_filter()).init()?;
    }

    debug!("Arguments: {:?}", &args);

    let mut auth_data: Option<AuthData> = None;

    if let Some(ref bearer_token) = args.bearer_token {
        debug!("Using bearer token");
        auth_data = AuthData::bearer(bearer_token);
    }

    #[allow(trivial_casts)]
    let context = swagger::make_context!(
        ContextBuilder,
        EmptyContext,
        auth_data,
        XSpanIdString::default()
    );

    let client = create_client(&args, context)?;

    let result = match args.operation {
        Operation::AnyOfGet {
            any_of,
        } => {
            info!("Performing a AnyOfGet request");

            let result = client.any_of_get(
                any_of.as_ref(),
            ).await?;
            debug!("Result: {:?}", result);

            match result {
                AnyOfGetResponse::Success
                (body)
                => "Success\n".to_string()
                   +
                    &serde_json::to_string_pretty(&body)?,
                AnyOfGetResponse::AlternateSuccess
                (body)
                => "AlternateSuccess\n".to_string()
                   +
                    &serde_json::to_string_pretty(&body)?,
                AnyOfGetResponse::AnyOfSuccess
                (body)
                => "AnyOfSuccess\n".to_string()
                   +
                    &serde_json::to_string_pretty(&body)?,
            }
        }
        Operation::CallbackWithHeaderPost {
            url,
        } => {
            info!("Performing a CallbackWithHeaderPost request");

            let result = client.callback_with_header_post(
                url,
            ).await?;
            debug!("Result: {:?}", result);

            match result {
                CallbackWithHeaderPostResponse::OK
                => "OK\n".to_string()
                    ,
            }
        }
        Operation::ComplexQueryParamGet {
            list_of_strings,
        } => {
            info!("Performing a ComplexQueryParamGet request");

            let result = client.complex_query_param_get(
                list_of_strings.as_ref(),
            ).await?;
            debug!("Result: {:?}", result);

            match result {
                ComplexQueryParamGetResponse::Success
                => "Success\n".to_string()
                    ,
            }
        }
        Operation::ExamplesTest {
            ids,
        } => {
            info!("Performing a ExamplesTest request");

            let result = client.examples_test(
                ids.as_ref(),
            ).await?;
            debug!("Result: {:?}", result);

            match result {
                ExamplesTestResponse::OK
                (body)
                => "OK\n".to_string()
                   +
                    &serde_json::to_string_pretty(&body)?,
            }
        }
        Operation::FormTest {
            required_array,
        } => {
            info!("Performing a FormTest request");

            let result = client.form_test(
                required_array.as_ref(),
            ).await?;
            debug!("Result: {:?}", result);

            match result {
                FormTestResponse::OK
                => "OK\n".to_string()
                    ,
            }
        }
        Operation::GetWithBooleanParameter {
            iambool,
        } => {
            info!("Performing a GetWithBooleanParameter request");

            let result = client.get_with_boolean_parameter(
                iambool,
            ).await?;
            debug!("Result: {:?}", result);

            match result {
                GetWithBooleanParameterResponse::OK
                => "OK\n".to_string()
                    ,
            }
        }
        Operation::JsonComplexQueryParamGet {
            list_of_strings,
        } => {
            info!("Performing a JsonComplexQueryParamGet request");

            let result = client.json_complex_query_param_get(
                list_of_strings.as_ref(),
            ).await?;
            debug!("Result: {:?}", result);

            match result {
                JsonComplexQueryParamGetResponse::Success
                => "Success\n".to_string()
                    ,
            }
        }
        Operation::MandatoryRequestHeaderGet {
            x_header,
        } => {
            info!("Performing a MandatoryRequestHeaderGet request");

            let result = client.mandatory_request_header_get(
                x_header,
            ).await?;
            debug!("Result: {:?}", result);

            match result {
                MandatoryRequestHeaderGetResponse::Success
                => "Success\n".to_string()
                    ,
            }
        }
        Operation::MergePatchJsonGet {
        } => {
            info!("Performing a MergePatchJsonGet request");

            let result = client.merge_patch_json_get(
            ).await?;
            debug!("Result: {:?}", result);

            match result {
                MergePatchJsonGetResponse::Merge
                (body)
                => "Merge\n".to_string()
                   +
                    &serde_json::to_string_pretty(&body)?,
            }
        }
        Operation::MultigetGet {
        } => {
            info!("Performing a MultigetGet request");

            let result = client.multiget_get(
            ).await?;
            debug!("Result: {:?}", result);

            match result {
                MultigetGetResponse::JSONRsp
                (body)
                => "JSONRsp\n".to_string()
                   +
                    &serde_json::to_string_pretty(&body)?,
                MultigetGetResponse::XMLRsp
                (body)
                => "XMLRsp\n".to_string()
                   +
                    &serde_json::to_string_pretty(&body)?,
                MultigetGetResponse::OctetRsp
                (body)
                => "OctetRsp\n".to_string()
                   +
                    &serde_json::to_string_pretty(&body)?,
                MultigetGetResponse::StringRsp
                (body)
                => "StringRsp\n".to_string()
                   +
                    &serde_json::to_string_pretty(&body)?,
                MultigetGetResponse::DuplicateResponseLongText
                (body)
                => "DuplicateResponseLongText\n".to_string()
                   +
                    &serde_json::to_string_pretty(&body)?,
                MultigetGetResponse::DuplicateResponseLongText_2
                (body)
                => "DuplicateResponseLongText_2\n".to_string()
                   +
                    &serde_json::to_string_pretty(&body)?,
                MultigetGetResponse::DuplicateResponseLongText_3
                (body)
                => "DuplicateResponseLongText_3\n".to_string()
                   +
                    &serde_json::to_string_pretty(&body)?,
            }
        }
        Operation::MultipleAuthSchemeGet {
        } => {
            info!("Performing a MultipleAuthSchemeGet request");

            let result = client.multiple_auth_scheme_get(
            ).await?;
            debug!("Result: {:?}", result);

            match result {
                MultipleAuthSchemeGetResponse::CheckThatLimitingToMultipleRequiredAuthSchemesWorks
                => "CheckThatLimitingToMultipleRequiredAuthSchemesWorks\n".to_string()
                    ,
            }
        }
        Operation::OneOfGet {
        } => {
            info!("Performing a OneOfGet request");

            let result = client.one_of_get(
            ).await?;
            debug!("Result: {:?}", result);

            match result {
                OneOfGetResponse::Success
                (body)
                => "Success\n".to_string()
                   +
                    &serde_json::to_string_pretty(&body)?,
            }
        }
        Operation::OverrideServerGet {
        } => {
            info!("Performing a OverrideServerGet request");

            let result = client.override_server_get(
            ).await?;
            debug!("Result: {:?}", result);

            match result {
                OverrideServerGetResponse::Success
                => "Success\n".to_string()
                    ,
            }
        }
        Operation::ParamgetGet {
            uuid,
            some_object,
            some_list,
        } => {
            info!("Performing a ParamgetGet request");

            let result = client.paramget_get(
                uuid,
                some_object,
                some_list,
            ).await?;
            debug!("Result: {:?}", result);

            match result {
                ParamgetGetResponse::JSONRsp
                (body)
                => "JSONRsp\n".to_string()
                   +
                    &serde_json::to_string_pretty(&body)?,
            }
        }
        Operation::ReadonlyAuthSchemeGet {
        } => {
            info!("Performing a ReadonlyAuthSchemeGet request");

            let result = client.readonly_auth_scheme_get(
            ).await?;
            debug!("Result: {:?}", result);

            match result {
                ReadonlyAuthSchemeGetResponse::CheckThatLimitingToASingleRequiredAuthSchemeWorks
                => "CheckThatLimitingToASingleRequiredAuthSchemeWorks\n".to_string()
                    ,
            }
        }
        Operation::RegisterCallbackPost {
            url,
        } => {
            info!("Performing a RegisterCallbackPost request");

            let result = client.register_callback_post(
                url,
            ).await?;
            debug!("Result: {:?}", result);

            match result {
                RegisterCallbackPostResponse::OK
                => "OK\n".to_string()
                    ,
            }
        }
        Operation::RequiredOctetStreamPut {
            body,
        } => {
            info!("Performing a RequiredOctetStreamPut request");

            let result = client.required_octet_stream_put(
                body,
            ).await?;
            debug!("Result: {:?}", result);

            match result {
                RequiredOctetStreamPutResponse::OK
                => "OK\n".to_string()
                    ,
            }
        }
        Operation::ResponsesWithHeadersGet {
        } => {
            info!("Performing a ResponsesWithHeadersGet request");

            let result = client.responses_with_headers_get(
            ).await?;
            debug!("Result: {:?}", result);

            match result {
                ResponsesWithHeadersGetResponse::Success
                {
                    body,
                    success_info,
                    bool_header,
                    object_header,
                }
                => "Success\n".to_string()
                   +
                    &format!("body: {}\n", serde_json::to_string_pretty(&body)?) +
                    &format!(
                        "success_info: {}\n",
                        serde_json::to_string_pretty(&success_info)?
                    ) +
                    &format!(
                        "bool_header: {}\n",
                        serde_json::to_string_pretty(&bool_header)?
                    ) +
                    &format!(
                        "object_header: {}\n",
                        serde_json::to_string_pretty(&object_header)?
                    ),
                ResponsesWithHeadersGetResponse::PreconditionFailed
                {
                    further_info,
                    failure_info,
                }
                => "PreconditionFailed\n".to_string()
                    +
                    &format!(
                        "further_info: {}\n",
                        serde_json::to_string_pretty(&further_info)?
                    ) +
                    &format!(
                        "failure_info: {}\n",
                        serde_json::to_string_pretty(&failure_info)?
                    ),
            }
        }
        Operation::Rfc7807Get {
        } => {
            info!("Performing a Rfc7807Get request");

            let result = client.rfc7807_get(
            ).await?;
            debug!("Result: {:?}", result);

            match result {
                Rfc7807GetResponse::OK
                (body)
                => "OK\n".to_string()
                   +
                    &serde_json::to_string_pretty(&body)?,
                Rfc7807GetResponse::NotFound
                (body)
                => "NotFound\n".to_string()
                   +
                    &serde_json::to_string_pretty(&body)?,
                Rfc7807GetResponse::NotAcceptable
                (body)
                => "NotAcceptable\n".to_string()
                   +
                    &serde_json::to_string_pretty(&body)?,
            }
        }
        Operation::TwoFirstLetterHeaders {
            x_header_one,
            x_header_two,
        } => {
            info!("Performing a TwoFirstLetterHeaders request");

            let result = client.two_first_letter_headers(
                x_header_one,
                x_header_two,
            ).await?;
            debug!("Result: {:?}", result);

            match result {
                TwoFirstLetterHeadersResponse::OK
                => "OK\n".to_string()
                    ,
            }
        }
        Operation::UntypedPropertyGet {
            object_untyped_props,
        } => {
            info!("Performing a UntypedPropertyGet request");

            let result = client.untyped_property_get(
                object_untyped_props,
            ).await?;
            debug!("Result: {:?}", result);

            match result {
                UntypedPropertyGetResponse::CheckThatUntypedPropertiesWorks
                => "CheckThatUntypedPropertiesWorks\n".to_string()
                    ,
            }
        }
        Operation::UuidGet {
        } => {
            info!("Performing a UuidGet request");

            let result = client.uuid_get(
            ).await?;
            debug!("Result: {:?}", result);

            match result {
                UuidGetResponse::DuplicateResponseLongText
                (body)
                => "DuplicateResponseLongText\n".to_string()
                   +
                    &serde_json::to_string_pretty(&body)?,
            }
        }
        Operation::XmlExtraPost {
            duplicate_xml_object,
        } => {
            info!("Performing a XmlExtraPost request");

            let result = client.xml_extra_post(
                duplicate_xml_object,
            ).await?;
            debug!("Result: {:?}", result);

            match result {
                XmlExtraPostResponse::OK
                => "OK\n".to_string()
                    ,
                XmlExtraPostResponse::BadRequest
                => "BadRequest\n".to_string()
                    ,
            }
        }
        Operation::XmlOtherPost {
            another_xml_object,
        } => {
            info!("Performing a XmlOtherPost request");

            let result = client.xml_other_post(
                another_xml_object,
            ).await?;
            debug!("Result: {:?}", result);

            match result {
                XmlOtherPostResponse::OK
                (body)
                => "OK\n".to_string()
                   +
                    &serde_json::to_string_pretty(&body)?,
                XmlOtherPostResponse::BadRequest
                => "BadRequest\n".to_string()
                    ,
            }
        }
        Operation::XmlOtherPut {
            another_xml_array,
        } => {
            info!("Performing a XmlOtherPut request");

            let result = client.xml_other_put(
                another_xml_array,
            ).await?;
            debug!("Result: {:?}", result);

            match result {
                XmlOtherPutResponse::OK
                => "OK\n".to_string()
                    ,
                XmlOtherPutResponse::BadRequest
                => "BadRequest\n".to_string()
                    ,
            }
        }
        Operation::XmlPost {
            xml_array,
        } => {
            info!("Performing a XmlPost request");

            let result = client.xml_post(
                xml_array,
            ).await?;
            debug!("Result: {:?}", result);

            match result {
                XmlPostResponse::OK
                => "OK\n".to_string()
                    ,
                XmlPostResponse::BadRequest
                => "BadRequest\n".to_string()
                    ,
            }
        }
        Operation::XmlPut {
            xml_object,
        } => {
            info!("Performing a XmlPut request");

            let result = client.xml_put(
                xml_object,
            ).await?;
            debug!("Result: {:?}", result);

            match result {
                XmlPutResponse::OK
                => "OK\n".to_string()
                    ,
                XmlPutResponse::BadRequest
                => "BadRequest\n".to_string()
                    ,
            }
        }
        Operation::EnumInPathPathParamGet {
            path_param,
        } => {
            info!("Performing a EnumInPathPathParamGet request on {:?}", (
                &path_param
            ));

            let result = client.enum_in_path_path_param_get(
                path_param,
            ).await?;
            debug!("Result: {:?}", result);

            match result {
                EnumInPathPathParamGetResponse::Success
                => "Success\n".to_string()
                    ,
            }
        }
        Operation::MultiplePathParamsWithVeryLongPathToTestFormattingPathParamAPathParamBGet {
            path_param_a,
            path_param_b,
        } => {
            info!("Performing a MultiplePathParamsWithVeryLongPathToTestFormattingPathParamAPathParamBGet request on {:?}", (
                &path_param_a,
                &path_param_b
            ));

            let result = client.multiple_path_params_with_very_long_path_to_test_formatting_path_param_a_path_param_b_get(
                path_param_a,
                path_param_b,
            ).await?;
            debug!("Result: {:?}", result);

            match result {
                MultiplePathParamsWithVeryLongPathToTestFormattingPathParamAPathParamBGetResponse::Success
                => "Success\n".to_string()
                    ,
            }
        }
        Operation::CreateRepo {
            object_param,
        } => {
            info!("Performing a CreateRepo request");

            let result = client.create_repo(
                object_param,
            ).await?;
            debug!("Result: {:?}", result);

            match result {
                CreateRepoResponse::Success
                => "Success\n".to_string()
                    ,
            }
        }
        Operation::GetRepoInfo {
            repo_id,
        } => {
            info!("Performing a GetRepoInfo request on {:?}", (
                &repo_id
            ));

            let result = client.get_repo_info(
                repo_id,
            ).await?;
            debug!("Result: {:?}", result);

            match result {
                GetRepoInfoResponse::OK
                (body)
                => "OK\n".to_string()
                   +
                    &serde_json::to_string_pretty(&body)?,
            }
        }
    };

    if let Some(output_file) = args.output_file {
        std::fs::write(output_file, result)?
    } else {
        println!("{}", result);
    }
    Ok(())
}

// May be unused if all inputs are primitive types
#[allow(dead_code)]
fn parse_json<T: serde::de::DeserializeOwned>(json_string: &str) -> Result<T> {
    serde_json::from_str(json_string).map_err(|err| anyhow!("Error parsing input: {}", err))
}
