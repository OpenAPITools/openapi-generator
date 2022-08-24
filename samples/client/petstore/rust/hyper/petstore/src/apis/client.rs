use std::rc::Rc;

use hyper;
use super::configuration::Configuration;

pub struct APIClient {
    fake_api: Box<dyn crate::apis::FakeApi>,
    pet_api: Box<dyn crate::apis::PetApi>,
    store_api: Box<dyn crate::apis::StoreApi>,
    testing_api: Box<dyn crate::apis::TestingApi>,
    user_api: Box<dyn crate::apis::UserApi>,
}

impl APIClient {
    pub fn new<C: hyper::client::connect::Connect>(configuration: Configuration<C>) -> APIClient
        where C: Clone + std::marker::Send + Sync + 'static {
        let rc = Rc::new(configuration);

        APIClient {
            fake_api: Box::new(crate::apis::FakeApiClient::new(rc.clone())),
            pet_api: Box::new(crate::apis::PetApiClient::new(rc.clone())),
            store_api: Box::new(crate::apis::StoreApiClient::new(rc.clone())),
            testing_api: Box::new(crate::apis::TestingApiClient::new(rc.clone())),
            user_api: Box::new(crate::apis::UserApiClient::new(rc.clone())),
        }
    }

    pub fn fake_api(&self) -> &dyn crate::apis::FakeApi{
        self.fake_api.as_ref()
    }

    pub fn pet_api(&self) -> &dyn crate::apis::PetApi{
        self.pet_api.as_ref()
    }

    pub fn store_api(&self) -> &dyn crate::apis::StoreApi{
        self.store_api.as_ref()
    }

    pub fn testing_api(&self) -> &dyn crate::apis::TestingApi{
        self.testing_api.as_ref()
    }

    pub fn user_api(&self) -> &dyn crate::apis::UserApi{
        self.user_api.as_ref()
    }

}
