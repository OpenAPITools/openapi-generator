//! Main library entry point for petstore_api implementation.

mod server;

mod errors {
    error_chain!{}
}

pub use self::errors::*;

/// Instantiate a new server.
pub fn server() -> Result<server::Server> {
    Ok(server::Server {})
}
