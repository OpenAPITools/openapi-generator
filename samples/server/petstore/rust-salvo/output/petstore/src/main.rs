use petstore_salvo::create_service;
use salvo::prelude::*;
use tracing_subscriber;

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    // Initialize tracing
    tracing_subscriber::fmt::init();

    let service = create_service();
    let acceptor = TcpListener::new("0.0.0.0:7878").bind().await;

    tracing::info!("OpenAPI Petstore listening on http://0.0.0.0:7878");

    Server::new(acceptor).serve(service).await;

    Ok(())
}
