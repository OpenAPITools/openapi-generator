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

//Add a new pet to the store
mod pet_api_func;
pub use self::pet_api::AddPet;
//Deletes a pet
mod pet_api_func;
pub use self::pet_api::DeletePet;
//Finds Pets by status
mod pet_api_func;
pub use self::pet_api::FindPetsByStatus;
//Finds Pets by tags
mod pet_api_func;
pub use self::pet_api::FindPetsByTags;
//Find pet by ID
mod pet_api_func;
pub use self::pet_api::GetPetById;
//Update an existing pet
mod pet_api_func;
pub use self::pet_api::UpdatePet;
//Updates a pet in the store with form data
mod pet_api_func;
pub use self::pet_api::UpdatePetWithForm;
//uploads an image
mod pet_api_func;
pub use self::pet_api::UploadFile;

mod pet_api_api;
pub use self::pet_api::PetApi;
//Delete purchase order by ID
mod store_api_func;
pub use self::store_api::DeleteOrder;
//Returns pet inventories by status
mod store_api_func;
pub use self::store_api::GetInventory;
//Find purchase order by ID
mod store_api_func;
pub use self::store_api::GetOrderById;
//Place an order for a pet
mod store_api_func;
pub use self::store_api::PlaceOrder;

mod store_api_api;
pub use self::store_api::StoreApi;
//Create user
mod user_api_func;
pub use self::user_api::CreateUser;
//Creates list of users with given input array
mod user_api_func;
pub use self::user_api::CreateUsersWithArrayInput;
//Creates list of users with given input array
mod user_api_func;
pub use self::user_api::CreateUsersWithListInput;
//Delete user
mod user_api_func;
pub use self::user_api::DeleteUser;
//Get user by user name
mod user_api_func;
pub use self::user_api::GetUserByName;
//Logs user into the system
mod user_api_func;
pub use self::user_api::LoginUser;
//Logs out current logged in user session
mod user_api_func;
pub use self::user_api::LogoutUser;
//Updated user
mod user_api_func;
pub use self::user_api::UpdateUser;

mod user_api_api;
pub use self::user_api::UserApi;

pub mod configuration;
pub mod client;
