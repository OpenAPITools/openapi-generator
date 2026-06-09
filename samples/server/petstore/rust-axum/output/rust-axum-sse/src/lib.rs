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

pub const BASE_PATH: &str = "";
pub const API_VERSION: &str = "1.0.0";

#[cfg(feature = "server")]
pub mod server;

pub mod apis;
pub mod models;
pub mod types;

#[cfg(feature = "server")]
pub(crate) mod header;
