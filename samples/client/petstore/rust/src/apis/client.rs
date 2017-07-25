use std::rc::Rc;

use hyper;
use super::configuration::Configuration;
use super::pet_api;

pub struct APIClient<C: hyper::client::Connect> {
  configuration: Rc<Configuration<C>>,

  pet_api: Box<pet_api::PetAPI>,
}

impl<C: hyper::client::Connect> APIClient<C> {
  pub fn new(configuration: Configuration<C>) -> APIClient<C> {
    let rc = Rc::new(configuration);
    
    APIClient {
      configuration: rc.clone(),
      pet_api: Box::new(pet_api::PetAPIImpl::new(rc.clone())),
    }
  }

  pub fn pet_api(&self) -> &pet_api::PetAPI {
    self.pet_api.as_ref()
  }
}
