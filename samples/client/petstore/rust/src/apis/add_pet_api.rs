use hyper;
use serde_json;
use futures::{Future, Stream};
use futures;

use super::models;
use super::Error;

pub fn add_pet<C: hyper::client::Connect>(
    prefix: &str,
    cli: &hyper::client::Client<C>,
    pet: &models::Pet,
    ) -> Box<Future<Item = (), Error = Error>> {
    let mut req = hyper::Request::new(
        hyper::Method::Post,
        format!("{}/pet", prefix).parse().unwrap());
    let serialized = serde_json::to_string(pet).unwrap();
    req.headers_mut().set(hyper::header::ContentType::json());
    req.headers_mut().set(hyper::header::ContentLength(serialized.len() as u64));
    req.set_body(serialized);

    Box::new(
        cli.request(req).and_then(|res| { res.body().concat2() })
        .map_err(|e| Error::from(e))
        .and_then(|_| futures::future::ok(()))
    )
}
