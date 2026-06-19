//! CLI tool driving the API client
use anyhow::{anyhow, Context, Result};
use log::{debug, info};
// models may be unused if all inputs are primitive types
#[allow(unused_imports)]
use multipart_v3::{
    models, ApiNoContext, Client, ContextWrapperExt,
    MultipartRelatedRequestPostResponse,
    MultipartRequestPostResponse,
    MultipleIdenticalMimeTypesPostResponse,
};
use simple_logger::SimpleLogger;
use structopt::StructOpt;
use swagger::{AuthData, ContextBuilder, EmptyContext, Push, XSpanIdString};

type ClientContext = swagger::make_context_ty!(
    ContextBuilder,
    EmptyContext,
    Option<AuthData>,
    XSpanIdString
);

#[derive(StructOpt, Debug)]
#[structopt(
    name = "Multipart OpenAPI V3 Rust Server Test",
    version = "1.0.7",
    about = "CLI access to Multipart OpenAPI V3 Rust Server Test"
)]
struct Cli {
    #[structopt(subcommand)]
    operation: Operation,

    /// Address or hostname of the server hosting this API, including optional port
    #[structopt(short = "a", long, default_value = "http://localhost")]
    server_address: String,

    /// Path to the client private key if using client-side TLS authentication
    #[cfg(not(any(target_os = "macos", target_os = "windows", target_os = "ios")))]
    #[structopt(long, requires_all(&["client-certificate", "server-certificate"]))]
    client_key: Option<String>,

    /// Path to the client's public certificate associated with the private key
    #[cfg(not(any(target_os = "macos", target_os = "windows", target_os = "ios")))]
    #[structopt(long, requires_all(&["client-key", "server-certificate"]))]
    client_certificate: Option<String>,

    /// Path to CA certificate used to authenticate the server
    #[cfg(not(any(target_os = "macos", target_os = "windows", target_os = "ios")))]
    #[structopt(long)]
    server_certificate: Option<String>,

    /// If set, write output to file instead of stdout
    #[structopt(short, long)]
    output_file: Option<String>,

    #[structopt(flatten)]
    verbosity: clap_verbosity_flag::Verbosity,
}

#[derive(StructOpt, Debug)]
enum Operation {
    MultipartRelatedRequestPost {
        #[structopt(parse(try_from_str = parse_json))]
        required_binary_field: swagger::ByteArray,
        #[structopt(parse(try_from_str = parse_json))]
        object_field: Option<models::MultipartRequestObjectField>,
        #[structopt(parse(try_from_str = parse_json))]
        optional_binary_field: Option<swagger::ByteArray>,
    },
    MultipartRequestPost {
        string_field: String,
        #[structopt(parse(try_from_str = parse_json))]
        binary_field: swagger::ByteArray,
        optional_string_field: Option<String>,
        #[structopt(parse(try_from_str = parse_json))]
        object_field: Option<models::MultipartRequestObjectField>,
    },
    MultipleIdenticalMimeTypesPost {
        #[structopt(parse(try_from_str = parse_json))]
        binary1: Option<swagger::ByteArray>,
        #[structopt(parse(try_from_str = parse_json))]
        binary2: Option<swagger::ByteArray>,
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
    let args = Cli::from_args();
    if let Some(log_level) = args.verbosity.log_level() {
        SimpleLogger::new().with_level(log_level.to_level_filter()).init()?;
    }

    debug!("Arguments: {:?}", &args);

    let auth_data: Option<AuthData> = None;

    #[allow(trivial_casts)]
    let context = swagger::make_context!(
        ContextBuilder,
        EmptyContext,
        auth_data,
        XSpanIdString::default()
    );

    let client = create_client(&args, context)?;

    let result = match args.operation {
        Operation::MultipartRelatedRequestPost {
            required_binary_field,
            object_field,
            optional_binary_field,
        } => {
            info!("Performing a MultipartRelatedRequestPost request");

            let result = client.multipart_related_request_post(
                required_binary_field,
                object_field,
                optional_binary_field,
            ).await?;
            debug!("Result: {:?}", result);

            match result {
                MultipartRelatedRequestPostResponse::OK
                => "OK\n".to_string()
                    ,
            }
        }
        Operation::MultipartRequestPost {
            string_field,
            binary_field,
            optional_string_field,
            object_field,
        } => {
            info!("Performing a MultipartRequestPost request");

            let result = client.multipart_request_post(
                string_field,
                binary_field,
                optional_string_field,
                object_field,
            ).await?;
            debug!("Result: {:?}", result);

            match result {
                MultipartRequestPostResponse::OK
                => "OK\n".to_string()
                    ,
            }
        }
        Operation::MultipleIdenticalMimeTypesPost {
            binary1,
            binary2,
        } => {
            info!("Performing a MultipleIdenticalMimeTypesPost request");

            let result = client.multiple_identical_mime_types_post(
                binary1,
                binary2,
            ).await?;
            debug!("Result: {:?}", result);

            match result {
                MultipleIdenticalMimeTypesPostResponse::OK
                => "OK\n".to_string()
                    ,
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
fn parse_json<'a, T: serde::de::Deserialize<'a>>(json_string: &'a str) -> Result<T> {
    serde_json::from_str(json_string).map_err(|err| anyhow!("Error parsing input: {}", err))
}
