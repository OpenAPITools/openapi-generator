use std::rc::Rc;

use hyper;
use super::configuration::Configuration;

pub struct APIClient {
    pet_api: Box<dyn crate::apis::PetApi>,
    store_api: Box<dyn crate::apis::StoreApi>,
    user_api: Box<dyn crate::apis::UserApi>,
}

impl APIClient {
    pub fn new<C: hyper::client::Connect>(configuration: Configuration<C>) -> APIClient {
        let rc = Rc::new(configuration);

        APIClient {
            pet_api: Box::new(crate::apis::PetApiClient::new(rc.clone())),
            store_api: Box::new(crate::apis::StoreApiClient::new(rc.clone())),
            user_api: Box::new(crate::apis::UserApiClient::new(rc.clone())),
        }
    }

    pub fn pet_api(&self) -> &dyn crate::apis::PetApi{
        self.pet_api.as_ref()
    }

    pub fn store_api(&self) -> &dyn crate::apis::StoreApi{
        self.store_api.as_ref()
    }

    pub fn user_api(&self) -> &dyn crate::apis::UserApi{
        self.user_api.as_ref()
    }

}
