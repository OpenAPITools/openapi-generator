use std::sync::Arc;

use hyper;
use hyper_util::client::legacy::connect::Connect;
use super::configuration::Configuration;

pub struct APIClient {
    bar_api: Box<dyn crate::apis::BarApi>,
    foo_api: Box<dyn crate::apis::FooApi>,
}

impl APIClient {
    pub fn new<C: Connect>(configuration: Configuration<C>) -> APIClient
        where C: Clone + std::marker::Send + Sync + 'static {
        let rc = Arc::new(configuration);

        APIClient {
            bar_api: Box::new(crate::apis::BarApiClient::new(rc.clone())),
            foo_api: Box::new(crate::apis::FooApiClient::new(rc.clone())),
        }
    }

    pub fn bar_api(&self) -> &dyn crate::apis::BarApi{
        self.bar_api.as_ref()
    }

    pub fn foo_api(&self) -> &dyn crate::apis::FooApi{
        self.foo_api.as_ref()
    }

}
