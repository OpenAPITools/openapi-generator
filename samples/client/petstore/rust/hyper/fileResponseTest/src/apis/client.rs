use std::rc::Rc;

use hyper;
use super::configuration::Configuration;

pub struct APIClient {
    default_api: Box<dyn crate::apis::DefaultApi>,
}

impl APIClient {
    pub fn new<C: hyper::client::Connect>(configuration: Configuration<C>) -> APIClient {
        let rc = Rc::new(configuration);

        APIClient {
            default_api: Box::new(crate::apis::DefaultApiClient::new(rc.clone())),
        }
    }

    pub fn default_api(&self) -> &dyn crate::apis::DefaultApi{
        self.default_api.as_ref()
    }

}
