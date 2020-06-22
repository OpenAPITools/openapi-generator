use std::rc::Rc;

use super::configuration::Configuration;

pub struct APIClient {
    pet_api: Box<dyn crate::apis::pet_api::PetApi>,
    store_api: Box<dyn crate::apis::store_api::StoreApi>,
    user_api: Box<dyn crate::apis::user_api::UserApi>,
}

impl APIClient {
    pub fn new(configuration: Configuration) -> APIClient {
        let rc = Rc::new(configuration);

        APIClient {
            pet_api: Box::new(crate::apis::pet_api::PetApiClient::new(rc.clone())),
            store_api: Box::new(crate::apis::store_api::StoreApiClient::new(rc.clone())),
            user_api: Box::new(crate::apis::user_api::UserApiClient::new(rc.clone())),
        }
    }

    pub fn pet_api(&self) -> &dyn crate::apis::pet_api::PetApi{
        self.pet_api.as_ref()
    }

    pub fn store_api(&self) -> &dyn crate::apis::store_api::StoreApi{
        self.store_api.as_ref()
    }

    pub fn user_api(&self) -> &dyn crate::apis::user_api::UserApi{
        self.user_api.as_ref()
    }

}
