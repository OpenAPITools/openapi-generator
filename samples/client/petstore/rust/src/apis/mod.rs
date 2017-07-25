use hyper;
use serde_json;

#[derive(Debug)]
pub enum Error {
    Hyper(hyper::Error),
    Serde(serde_json::Error),
}

impl From<hyper::Error> for Error {
    fn from(e: hyper::Error) -> Self {
        return Error::Hyper(e)
    }
}

impl From<serde_json::Error> for Error {
    fn from(e: serde_json::Error) -> Self {
        return Error::Serde(e)
    }
}

use super::models;

mod get_pet_by_id_api;
pub use self::get_pet_by_id_api::get_pet_by_id;

mod update_pet_with_form_api;
pub use self::update_pet_with_form_api::update_pet_with_form;

mod add_pet_api;
pub use self::add_pet_api::add_pet;

pub mod configuration;
pub mod client;

mod pet_api;
pub use self::pet_api::PetAPI;
