use reqwest;
use serde_json;

#[derive(Debug)]
pub enum Error {
    Reqwest(reqwest::Error),
    Serde(serde_json::Error),
    Io(std::io::Error),
}

impl From<reqwest::Error> for Error {
    fn from(e: reqwest::Error) -> Self {
        Error::Reqwest(e)
    }
}

impl From<serde_json::Error> for Error {
    fn from(e: serde_json::Error) -> Self {
        Error::Serde(e)
    }
}

impl From<std::io::Error> for Error {
    fn from(e: std::io::Error) -> Self {
        Error::Io(e)
    }
}

pub fn urlencode<T: AsRef<str>>(s: T) -> String {
    ::url::form_urlencoded::byte_serialize(s.as_ref().as_bytes()).collect()
}

mod pet_api;
pub use self::pet_api::{ add_pet };
pub use self::pet_api::{ delete_pet };
pub use self::pet_api::{ find_pets_by_status };
pub use self::pet_api::{ find_pets_by_tags };
pub use self::pet_api::{ get_pet_by_id };
pub use self::pet_api::{ update_pet };
pub use self::pet_api::{ update_pet_with_form };
pub use self::pet_api::{ upload_file };
mod store_api;
pub use self::store_api::{ delete_order };
pub use self::store_api::{ get_inventory };
pub use self::store_api::{ get_order_by_id };
pub use self::store_api::{ place_order };
mod user_api;
pub use self::user_api::{ create_user };
pub use self::user_api::{ create_users_with_array_input };
pub use self::user_api::{ create_users_with_list_input };
pub use self::user_api::{ delete_user };
pub use self::user_api::{ get_user_by_name };
pub use self::user_api::{ login_user };
pub use self::user_api::{ logout_user };
pub use self::user_api::{ update_user };

pub mod configuration;
