//! Main library entry point for rust_server_test implementation.

pub mod server;

mod errors {
    use error_chain::error_chain;
    error_chain!{}
}

pub use self::errors::*;
pub use server::Server;
