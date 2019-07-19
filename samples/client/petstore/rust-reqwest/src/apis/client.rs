use std::rc::Rc;

use super::configuration::Configuration;

pub struct APIClient {
    configuration: Rc<Configuration>,
    pet_api: Box<crate::apis::PetApi>,
    store_api: Box<crate::apis::StoreApi>,
    user_api: Box<crate::apis::UserApi>,
}

impl APIClient {
    pub fn new(configuration: Configuration) -> APIClient {
        let rc = Rc::new(configuration);

        APIClient {
            configuration: rc.clone(),
            pet_api: Box::new(crate::apis::PetApiClient::new(rc.clone())),
            store_api: Box::new(crate::apis::StoreApiClient::new(rc.clone())),
            user_api: Box::new(crate::apis::UserApiClient::new(rc.clone())),
        }
    }

    pub fn pet_api(&self) -> &crate::apis::PetApi{
        self.pet_api.as_ref()
    }

    pub fn store_api(&self) -> &crate::apis::StoreApi{
        self.store_api.as_ref()
    }

    pub fn user_api(&self) -> &crate::apis::UserApi{
        self.user_api.as_ref()
    }

}
