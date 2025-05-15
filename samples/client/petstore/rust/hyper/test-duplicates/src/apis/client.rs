use std::sync::Arc;

use hyper;
use hyper_util::client::legacy::connect::Connect;
use super::configuration::Configuration;

pub struct APIClient {
    testing_api: Box<dyn crate::apis::TestingApi>,
}

impl APIClient {
    pub fn new<C: Connect>(configuration: Configuration<C>) -> APIClient
        where C: Clone + std::marker::Send + Sync + 'static {
        let rc = Arc::new(configuration);

        APIClient {
            testing_api: Box::new(crate::apis::TestingApiClient::new(rc.clone())),
        }
    }

    pub fn testing_api(&self) -> &dyn crate::apis::TestingApi{
        self.testing_api.as_ref()
    }

}
