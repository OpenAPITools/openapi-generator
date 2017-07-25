use hyper;
use serde_json;
use futures::{Future, Stream};

use super::models;
use super::Error;

pub fn get_pet_by_id<C: hyper::client::Connect>(
    prefix: &str,
    cli: &hyper::client::Client<C>,
    pet_id: i64,
    ) -> Box<Future<Item = models::Pet, Error = Error>> {
    Box::new(
        cli.get(format!("{}/pet/{}", prefix, pet_id).parse().unwrap())

        .and_then(|res| { res.body().concat2() }).map_err(|e| Error::from(e))
        
        .and_then(|body| {
            let parsed: Result<models::Pet, _> = serde_json::from_slice(&body);
            parsed.map_err(|e| Error::from(e))
        }).map_err(|e| Error::from(e))
    )
}
