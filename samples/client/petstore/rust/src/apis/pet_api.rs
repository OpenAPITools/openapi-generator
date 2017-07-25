use std::rc::Rc;
use std::borrow::Borrow;

use hyper;
use serde_json;
use futures;
use futures::{Future, Stream};

use super::{Error, configuration, models};

pub trait PetAPI {
    fn add_pet(&self, pet: &models::Pet) -> Box<Future<Item = (), Error = Error>>;
}

pub struct PetAPIImpl<C: hyper::client::Connect> {
    configuration: Rc<configuration::Configuration<C>>,
}

impl<C: hyper::client::Connect> PetAPIImpl<C> {
    pub fn new(configuration: Rc<configuration::Configuration<C>>) -> PetAPIImpl<C> {
        PetAPIImpl {
            configuration: configuration,
        }
    }
}

impl<C: hyper::client::Connect>PetAPI for PetAPIImpl<C> {
    fn add_pet(&self, pet: &models::Pet) -> Box<Future<Item = (), Error = Error>> {
        let configuration: &configuration::Configuration<C> = self.configuration.borrow();
        let mut req = hyper::Request::new(
            hyper::Method::Post,
            format!("{}/pet", configuration.base_path).parse().unwrap());
        let serialized = serde_json::to_string(pet).unwrap();
        req.headers_mut().set(hyper::header::ContentType::json());
        req.headers_mut().set(hyper::header::ContentLength(serialized.len() as u64));
        req.set_body(serialized);

        Box::new(
            configuration.client.request(req).and_then(|res| { res.body().concat2() })
            .map_err(|e| Error::from(e))
            .and_then(|_| futures::future::ok(()))
        )
    }
}
