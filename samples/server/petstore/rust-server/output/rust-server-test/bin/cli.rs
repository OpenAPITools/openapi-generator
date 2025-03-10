//! CLI tool driving the API client
use anyhow::{anyhow, Context, Result};
use log::{debug, info};
// models may be unused if all inputs are primitive types
#[allow(unused_imports)]
use rust_server_test::{
    models, ApiNoContext, Client, ContextWrapperExt,
    AllOfGetResponse,
    DummyGetResponse,
    DummyPutResponse,
    FileResponseGetResponse,
    GetStructuredYamlResponse,
    HtmlPostResponse,
    PostYamlResponse,
    RawJsonGetResponse,
    SoloObjectPostResponse,
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
    name = "rust-server-test",
    version = "2.3.4",
    about = "CLI access to rust-server-test"
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
    AllOfGet {
    },
    /// A dummy endpoint to make the spec valid.
    DummyGet {
    },
    DummyPut {
        #[structopt(parse(try_from_str = parse_json))]
        nested_response: models::DummyPutRequest,
    },
    /// Get a file
    FileResponseGet {
    },
    GetStructuredYaml {
    },
    /// Test HTML handling
    HtmlPost {
        body: String,
    },
    PostYaml {
        /// The YAML body to test
        value: String,
    },
    /// Get an arbitrary JSON blob.
    RawJsonGet {
    },
    /// Send an arbitrary JSON blob
    SoloObjectPost {
        value: serde_json::Value,
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
        Operation::AllOfGet {
        } => {
            info!("Performing a AllOfGet request");

            let result = client.all_of_get(
            ).await?;
            debug!("Result: {:?}", result);

            match result {
                AllOfGetResponse::OK
                (body)
                => "OK\n".to_string()
                   +
                    &serde_json::to_string_pretty(&body)?,
            }
        }
        Operation::DummyGet {
        } => {
            info!("Performing a DummyGet request");

            let result = client.dummy_get(
            ).await?;
            debug!("Result: {:?}", result);

            match result {
                DummyGetResponse::Success
                => "Success\n".to_string()
                    ,
            }
        }
        Operation::DummyPut {
            nested_response,
        } => {
            info!("Performing a DummyPut request");

            let result = client.dummy_put(
                nested_response,
            ).await?;
            debug!("Result: {:?}", result);

            match result {
                DummyPutResponse::Success
                => "Success\n".to_string()
                    ,
            }
        }
        Operation::FileResponseGet {
        } => {
            info!("Performing a FileResponseGet request");

            let result = client.file_response_get(
            ).await?;
            debug!("Result: {:?}", result);

            match result {
                FileResponseGetResponse::Success
                (body)
                => "Success\n".to_string()
                   +
                    &serde_json::to_string_pretty(&body)?,
            }
        }
        Operation::GetStructuredYaml {
        } => {
            info!("Performing a GetStructuredYaml request");

            let result = client.get_structured_yaml(
            ).await?;
            debug!("Result: {:?}", result);

            match result {
                GetStructuredYamlResponse::OK
                (body)
                => "OK\n".to_string()
                   +
                    &serde_json::to_string_pretty(&body)?,
            }
        }
        Operation::HtmlPost {
            body,
        } => {
            info!("Performing a HtmlPost request");

            let result = client.html_post(
                body,
            ).await?;
            debug!("Result: {:?}", result);

            match result {
                HtmlPostResponse::Success
                (body)
                => "Success\n".to_string()
                   +
                    &serde_json::to_string_pretty(&body)?,
            }
        }
        Operation::PostYaml {
            value,
        } => {
            info!("Performing a PostYaml request");

            let result = client.post_yaml(
                value,
            ).await?;
            debug!("Result: {:?}", result);

            match result {
                PostYamlResponse::OK
                => "OK\n".to_string()
                    ,
            }
        }
        Operation::RawJsonGet {
        } => {
            info!("Performing a RawJsonGet request");

            let result = client.raw_json_get(
            ).await?;
            debug!("Result: {:?}", result);

            match result {
                RawJsonGetResponse::Success
                (body)
                => "Success\n".to_string()
                   +
                    &serde_json::to_string_pretty(&body)?,
            }
        }
        Operation::SoloObjectPost {
            value,
        } => {
            info!("Performing a SoloObjectPost request");

            let result = client.solo_object_post(
                value,
            ).await?;
            debug!("Result: {:?}", result);

            match result {
                SoloObjectPostResponse::OK
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
