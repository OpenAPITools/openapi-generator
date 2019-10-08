//! Main library entry point for petstore_with_fake_endpoints_models_for_testing implementation.

pub mod server;

mod errors {
    use error_chain::error_chain;
    error_chain!{}
}

pub use self::errors::*;
pub use server::Server;
