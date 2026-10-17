use std::sync::Arc;

use hyper;
use hyper_util::client::legacy::connect::Connect;
use super::configuration::Configuration;

pub struct APIClient {
    default_api: Box<dyn crate::apis::DefaultApi>,
}

impl APIClient {
    pub fn new<C: Connect>(configuration: Configuration<C>) -> APIClient
        where C: Clone + std::marker::Send + Sync + 'static {
        let rc = Arc::new(configuration);

        APIClient {
            default_api: Box::new(crate::apis::DefaultApiClient::new(rc.clone())),
        }
    }

    pub fn default_api(&self) -> &dyn crate::apis::DefaultApi{
        self.default_api.as_ref()
    }

}
