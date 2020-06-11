use reqwest;
use serde_json;

#[derive(Debug, Clone)]
pub struct ResponseContent<T> {
    pub status: reqwest::StatusCode,
    pub content: String,
    pub entity: Option<T>,
}

#[derive(Debug)]
pub enum Error<T> {
    Reqwest(reqwest::Error),
    Serde(serde_json::Error),
    Io(std::io::Error),
    ResponseError(ResponseContent<T>),
}

impl <T> From<reqwest::Error> for Error<T> {
    fn from(e: reqwest::Error) -> Self {
        Error::Reqwest(e)
    }
}

impl <T> From<serde_json::Error> for Error<T> {
    fn from(e: serde_json::Error) -> Self {
        Error::Serde(e)
    }
}

impl <T> From<std::io::Error> for Error<T> {
    fn from(e: std::io::Error) -> Self {
        Error::Io(e)
    }
}

pub fn urlencode<T: AsRef<str>>(s: T) -> String {
    ::url::form_urlencoded::byte_serialize(s.as_ref().as_bytes()).collect()
}

<<<<<<< HEAD
mod pet_api;
pub use self::pet_api::{ add_pet };
pub use self::pet_api::AddPetParams as PetApiAddPetParams;
pub use self::pet_api::{ delete_pet };
pub use self::pet_api::DeletePetParams as PetApiDeletePetParams;
pub use self::pet_api::{ find_pets_by_status };
pub use self::pet_api::FindPetsByStatusParams as PetApiFindPetsByStatusParams;
pub use self::pet_api::{ find_pets_by_tags };
pub use self::pet_api::FindPetsByTagsParams as PetApiFindPetsByTagsParams;
pub use self::pet_api::{ get_pet_by_id };
pub use self::pet_api::GetPetByIdParams as PetApiGetPetByIdParams;
pub use self::pet_api::{ update_pet };
pub use self::pet_api::UpdatePetParams as PetApiUpdatePetParams;
pub use self::pet_api::{ update_pet_with_form };
pub use self::pet_api::UpdatePetWithFormParams as PetApiUpdatePetWithFormParams;
pub use self::pet_api::{ upload_file };
pub use self::pet_api::UploadFileParams as PetApiUploadFileParams;
mod store_api;
pub use self::store_api::{ delete_order };
pub use self::store_api::DeleteOrderParams as StoreApiDeleteOrderParams;
pub use self::store_api::{ get_inventory };
pub use self::store_api::{ get_order_by_id };
pub use self::store_api::GetOrderByIdParams as StoreApiGetOrderByIdParams;
pub use self::store_api::{ place_order };
pub use self::store_api::PlaceOrderParams as StoreApiPlaceOrderParams;
mod user_api;
pub use self::user_api::{ create_user };
pub use self::user_api::CreateUserParams as UserApiCreateUserParams;
pub use self::user_api::{ create_users_with_array_input };
pub use self::user_api::CreateUsersWithArrayInputParams as UserApiCreateUsersWithArrayInputParams;
pub use self::user_api::{ create_users_with_list_input };
pub use self::user_api::CreateUsersWithListInputParams as UserApiCreateUsersWithListInputParams;
pub use self::user_api::{ delete_user };
pub use self::user_api::DeleteUserParams as UserApiDeleteUserParams;
pub use self::user_api::{ get_user_by_name };
pub use self::user_api::GetUserByNameParams as UserApiGetUserByNameParams;
pub use self::user_api::{ login_user };
pub use self::user_api::LoginUserParams as UserApiLoginUserParams;
pub use self::user_api::{ logout_user };
pub use self::user_api::{ update_user };
pub use self::user_api::UpdateUserParams as UserApiUpdateUserParams;
=======
pub mod pet_api;
pub mod store_api;
pub mod user_api;
>>>>>>> 733fd9d601b19862de26b8424106f60cd9716e6c

pub mod configuration;
