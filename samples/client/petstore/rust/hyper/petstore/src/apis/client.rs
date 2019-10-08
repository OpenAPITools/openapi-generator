use std::sync::Arc;

use hyper;
use super::configuration::Configuration;

pub struct APIClient {
    pet_api: Box<dyn crate::apis::PetApi>,
    store_api: Box<dyn crate::apis::StoreApi>,
    user_api: Box<dyn crate::apis::UserApi>,
}

impl APIClient {
    pub fn new<C: hyper::client::connect::Connect + Clone + Send + Sync + 'static>(configuration: Configuration<C>) -> APIClient {
        let rc = Arc::new(configuration);

        APIClient {
            pet_api: Box::new(crate::apis::PetApiClient::new(Arc::clone(&rc))),
            store_api: Box::new(crate::apis::StoreApiClient::new(Arc::clone(&rc))),
            user_api: Box::new(crate::apis::UserApiClient::new(Arc::clone(&rc))),
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
