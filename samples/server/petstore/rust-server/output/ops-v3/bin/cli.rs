//! CLI tool driving the API client
use anyhow::{anyhow, Context, Result};
use clap::Parser;
use log::{debug, info};
// models may be unused if all inputs are primitive types
#[allow(unused_imports)]
use ops_v3::{
    models, ApiNoContext, Client, ContextWrapperExt,
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
    Op9GetResponse,
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
    name = "Regression test for large number of operations",
    version = "0.0.1",
    about = "CLI access to Regression test for large number of operations"
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
}

#[derive(Parser, Debug)]
enum Operation {
    Op10Get {
    },
    Op11Get {
    },
    Op12Get {
    },
    Op13Get {
    },
    Op14Get {
    },
    Op15Get {
    },
    Op16Get {
    },
    Op17Get {
    },
    Op18Get {
    },
    Op19Get {
    },
    Op1Get {
    },
    Op20Get {
    },
    Op21Get {
    },
    Op22Get {
    },
    Op23Get {
    },
    Op24Get {
    },
    Op25Get {
    },
    Op26Get {
    },
    Op27Get {
    },
    Op28Get {
    },
    Op29Get {
    },
    Op2Get {
    },
    Op30Get {
    },
    Op31Get {
    },
    Op32Get {
    },
    Op33Get {
    },
    Op34Get {
    },
    Op35Get {
    },
    Op36Get {
    },
    Op37Get {
    },
    Op3Get {
    },
    Op4Get {
    },
    Op5Get {
    },
    Op6Get {
    },
    Op7Get {
    },
    Op8Get {
    },
    Op9Get {
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
        Operation::Op10Get {
        } => {
            info!("Performing a Op10Get request");

            let result = client.op10_get(
            ).await?;
            debug!("Result: {:?}", result);

            match result {
                Op10GetResponse::OK
                => "OK\n".to_string()
                    ,
            }
        }
        Operation::Op11Get {
        } => {
            info!("Performing a Op11Get request");

            let result = client.op11_get(
            ).await?;
            debug!("Result: {:?}", result);

            match result {
                Op11GetResponse::OK
                => "OK\n".to_string()
                    ,
            }
        }
        Operation::Op12Get {
        } => {
            info!("Performing a Op12Get request");

            let result = client.op12_get(
            ).await?;
            debug!("Result: {:?}", result);

            match result {
                Op12GetResponse::OK
                => "OK\n".to_string()
                    ,
            }
        }
        Operation::Op13Get {
        } => {
            info!("Performing a Op13Get request");

            let result = client.op13_get(
            ).await?;
            debug!("Result: {:?}", result);

            match result {
                Op13GetResponse::OK
                => "OK\n".to_string()
                    ,
            }
        }
        Operation::Op14Get {
        } => {
            info!("Performing a Op14Get request");

            let result = client.op14_get(
            ).await?;
            debug!("Result: {:?}", result);

            match result {
                Op14GetResponse::OK
                => "OK\n".to_string()
                    ,
            }
        }
        Operation::Op15Get {
        } => {
            info!("Performing a Op15Get request");

            let result = client.op15_get(
            ).await?;
            debug!("Result: {:?}", result);

            match result {
                Op15GetResponse::OK
                => "OK\n".to_string()
                    ,
            }
        }
        Operation::Op16Get {
        } => {
            info!("Performing a Op16Get request");

            let result = client.op16_get(
            ).await?;
            debug!("Result: {:?}", result);

            match result {
                Op16GetResponse::OK
                => "OK\n".to_string()
                    ,
            }
        }
        Operation::Op17Get {
        } => {
            info!("Performing a Op17Get request");

            let result = client.op17_get(
            ).await?;
            debug!("Result: {:?}", result);

            match result {
                Op17GetResponse::OK
                => "OK\n".to_string()
                    ,
            }
        }
        Operation::Op18Get {
        } => {
            info!("Performing a Op18Get request");

            let result = client.op18_get(
            ).await?;
            debug!("Result: {:?}", result);

            match result {
                Op18GetResponse::OK
                => "OK\n".to_string()
                    ,
            }
        }
        Operation::Op19Get {
        } => {
            info!("Performing a Op19Get request");

            let result = client.op19_get(
            ).await?;
            debug!("Result: {:?}", result);

            match result {
                Op19GetResponse::OK
                => "OK\n".to_string()
                    ,
            }
        }
        Operation::Op1Get {
        } => {
            info!("Performing a Op1Get request");

            let result = client.op1_get(
            ).await?;
            debug!("Result: {:?}", result);

            match result {
                Op1GetResponse::OK
                => "OK\n".to_string()
                    ,
            }
        }
        Operation::Op20Get {
        } => {
            info!("Performing a Op20Get request");

            let result = client.op20_get(
            ).await?;
            debug!("Result: {:?}", result);

            match result {
                Op20GetResponse::OK
                => "OK\n".to_string()
                    ,
            }
        }
        Operation::Op21Get {
        } => {
            info!("Performing a Op21Get request");

            let result = client.op21_get(
            ).await?;
            debug!("Result: {:?}", result);

            match result {
                Op21GetResponse::OK
                => "OK\n".to_string()
                    ,
            }
        }
        Operation::Op22Get {
        } => {
            info!("Performing a Op22Get request");

            let result = client.op22_get(
            ).await?;
            debug!("Result: {:?}", result);

            match result {
                Op22GetResponse::OK
                => "OK\n".to_string()
                    ,
            }
        }
        Operation::Op23Get {
        } => {
            info!("Performing a Op23Get request");

            let result = client.op23_get(
            ).await?;
            debug!("Result: {:?}", result);

            match result {
                Op23GetResponse::OK
                => "OK\n".to_string()
                    ,
            }
        }
        Operation::Op24Get {
        } => {
            info!("Performing a Op24Get request");

            let result = client.op24_get(
            ).await?;
            debug!("Result: {:?}", result);

            match result {
                Op24GetResponse::OK
                => "OK\n".to_string()
                    ,
            }
        }
        Operation::Op25Get {
        } => {
            info!("Performing a Op25Get request");

            let result = client.op25_get(
            ).await?;
            debug!("Result: {:?}", result);

            match result {
                Op25GetResponse::OK
                => "OK\n".to_string()
                    ,
            }
        }
        Operation::Op26Get {
        } => {
            info!("Performing a Op26Get request");

            let result = client.op26_get(
            ).await?;
            debug!("Result: {:?}", result);

            match result {
                Op26GetResponse::OK
                => "OK\n".to_string()
                    ,
            }
        }
        Operation::Op27Get {
        } => {
            info!("Performing a Op27Get request");

            let result = client.op27_get(
            ).await?;
            debug!("Result: {:?}", result);

            match result {
                Op27GetResponse::OK
                => "OK\n".to_string()
                    ,
            }
        }
        Operation::Op28Get {
        } => {
            info!("Performing a Op28Get request");

            let result = client.op28_get(
            ).await?;
            debug!("Result: {:?}", result);

            match result {
                Op28GetResponse::OK
                => "OK\n".to_string()
                    ,
            }
        }
        Operation::Op29Get {
        } => {
            info!("Performing a Op29Get request");

            let result = client.op29_get(
            ).await?;
            debug!("Result: {:?}", result);

            match result {
                Op29GetResponse::OK
                => "OK\n".to_string()
                    ,
            }
        }
        Operation::Op2Get {
        } => {
            info!("Performing a Op2Get request");

            let result = client.op2_get(
            ).await?;
            debug!("Result: {:?}", result);

            match result {
                Op2GetResponse::OK
                => "OK\n".to_string()
                    ,
            }
        }
        Operation::Op30Get {
        } => {
            info!("Performing a Op30Get request");

            let result = client.op30_get(
            ).await?;
            debug!("Result: {:?}", result);

            match result {
                Op30GetResponse::OK
                => "OK\n".to_string()
                    ,
            }
        }
        Operation::Op31Get {
        } => {
            info!("Performing a Op31Get request");

            let result = client.op31_get(
            ).await?;
            debug!("Result: {:?}", result);

            match result {
                Op31GetResponse::OK
                => "OK\n".to_string()
                    ,
            }
        }
        Operation::Op32Get {
        } => {
            info!("Performing a Op32Get request");

            let result = client.op32_get(
            ).await?;
            debug!("Result: {:?}", result);

            match result {
                Op32GetResponse::OK
                => "OK\n".to_string()
                    ,
            }
        }
        Operation::Op33Get {
        } => {
            info!("Performing a Op33Get request");

            let result = client.op33_get(
            ).await?;
            debug!("Result: {:?}", result);

            match result {
                Op33GetResponse::OK
                => "OK\n".to_string()
                    ,
            }
        }
        Operation::Op34Get {
        } => {
            info!("Performing a Op34Get request");

            let result = client.op34_get(
            ).await?;
            debug!("Result: {:?}", result);

            match result {
                Op34GetResponse::OK
                => "OK\n".to_string()
                    ,
            }
        }
        Operation::Op35Get {
        } => {
            info!("Performing a Op35Get request");

            let result = client.op35_get(
            ).await?;
            debug!("Result: {:?}", result);

            match result {
                Op35GetResponse::OK
                => "OK\n".to_string()
                    ,
            }
        }
        Operation::Op36Get {
        } => {
            info!("Performing a Op36Get request");

            let result = client.op36_get(
            ).await?;
            debug!("Result: {:?}", result);

            match result {
                Op36GetResponse::OK
                => "OK\n".to_string()
                    ,
            }
        }
        Operation::Op37Get {
        } => {
            info!("Performing a Op37Get request");

            let result = client.op37_get(
            ).await?;
            debug!("Result: {:?}", result);

            match result {
                Op37GetResponse::OK
                => "OK\n".to_string()
                    ,
            }
        }
        Operation::Op3Get {
        } => {
            info!("Performing a Op3Get request");

            let result = client.op3_get(
            ).await?;
            debug!("Result: {:?}", result);

            match result {
                Op3GetResponse::OK
                => "OK\n".to_string()
                    ,
            }
        }
        Operation::Op4Get {
        } => {
            info!("Performing a Op4Get request");

            let result = client.op4_get(
            ).await?;
            debug!("Result: {:?}", result);

            match result {
                Op4GetResponse::OK
                => "OK\n".to_string()
                    ,
            }
        }
        Operation::Op5Get {
        } => {
            info!("Performing a Op5Get request");

            let result = client.op5_get(
            ).await?;
            debug!("Result: {:?}", result);

            match result {
                Op5GetResponse::OK
                => "OK\n".to_string()
                    ,
            }
        }
        Operation::Op6Get {
        } => {
            info!("Performing a Op6Get request");

            let result = client.op6_get(
            ).await?;
            debug!("Result: {:?}", result);

            match result {
                Op6GetResponse::OK
                => "OK\n".to_string()
                    ,
            }
        }
        Operation::Op7Get {
        } => {
            info!("Performing a Op7Get request");

            let result = client.op7_get(
            ).await?;
            debug!("Result: {:?}", result);

            match result {
                Op7GetResponse::OK
                => "OK\n".to_string()
                    ,
            }
        }
        Operation::Op8Get {
        } => {
            info!("Performing a Op8Get request");

            let result = client.op8_get(
            ).await?;
            debug!("Result: {:?}", result);

            match result {
                Op8GetResponse::OK
                => "OK\n".to_string()
                    ,
            }
        }
        Operation::Op9Get {
        } => {
            info!("Performing a Op9Get request");

            let result = client.op9_get(
            ).await?;
            debug!("Result: {:?}", result);

            match result {
                Op9GetResponse::OK
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
fn parse_json<T: serde::de::DeserializeOwned>(json_string: &str) -> Result<T> {
    serde_json::from_str(json_string).map_err(|err| anyhow!("Error parsing input: {}", err))
}
