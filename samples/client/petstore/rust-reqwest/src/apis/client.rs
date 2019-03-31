use std::rc::Rc;

use super::configuration::Configuration;

pub struct APIClient {
    configuration: Rc<Configuration>,
    pet_api: Box<::apis::PetApi>,
    store_api: Box<::apis::StoreApi>,
    user_api: Box<::apis::UserApi>,
}

impl APIClient {
    pub fn new(configuration: Configuration) -> APIClient {
        let rc = Rc::new(configuration);

        APIClient {
            configuration: rc.clone(),
            pet_api: Box::new(::apis::PetApiClient::new(rc.clone())),
            store_api: Box::new(::apis::StoreApiClient::new(rc.clone())),
            user_api: Box::new(::apis::UserApiClient::new(rc.clone())),
        }
    }

    pub fn pet_api(&self) -> &::apis::PetApi{
        self.pet_api.as_ref()
    }

    pub fn store_api(&self) -> &::apis::StoreApi{
        self.store_api.as_ref()
    }

    pub fn user_api(&self) -> &::apis::UserApi{
        self.user_api.as_ref()
    }

}
