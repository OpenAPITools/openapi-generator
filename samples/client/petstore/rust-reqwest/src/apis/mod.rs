use reqwest;
use serde_json;

#[derive(Debug)]
pub enum Error {
    Reqwest(reqwest::Error),
    Serde(serde_json::Error),
}

impl From<reqwest::Error> for Error {
    fn from(e: reqwest::Error) -> Self {
        return Error::Reqwest(e)
    }
}

impl From<serde_json::Error> for Error {
    fn from(e: serde_json::Error) -> Self {
        return Error::Serde(e)
    }
}

use super::models::*;

mod pet_api;
pub use self::pet_api::{ PetApi, PetApiClient };
mod store_api;
pub use self::store_api::{ StoreApi, StoreApiClient };
mod user_api;
pub use self::user_api::{ UserApi, UserApiClient };

pub mod configuration;
pub mod client;
