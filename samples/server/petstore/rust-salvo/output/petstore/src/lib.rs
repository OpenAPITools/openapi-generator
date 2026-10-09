#![allow(
    missing_docs,
    trivial_casts,
    unused_variables,
    unused_mut,
    unused_extern_crates,
    non_camel_case_types,
    unused_imports,
    unused_attributes
)]
#![allow(
    clippy::derive_partial_eq_without_eq,
    clippy::disallowed_names,
    clippy::too_many_arguments
)]

pub const BASE_PATH: &str = "/v2";
pub const API_VERSION: &str = "1.0.0";

pub mod handlers;
pub mod models;
pub mod routes;

pub use handlers::*;
pub use models::*;
pub use routes::*;

use salvo::prelude::*;


/// Create and configure the Salvo service.
///
/// Authentication middleware is attached per route (see `routes.rs`) so that
/// public operations are not blocked by a global auth hoop.
pub fn create_service() -> Service {
    let router = create_router();

    Service::new(router)
}

/// Create the main router with all API routes
pub fn create_router() -> Router {
    routes::create_router()
}
