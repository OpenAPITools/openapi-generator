use hyper;
use futures::{Future, Stream};
use url;
use futures;

use super::Error;

pub fn update_pet_with_form<C: hyper::client::Connect>(
    prefix: &str,
    cli: &hyper::client::Client<C>,
    pet_id: i64,
    name: &str,
    status: &str,
    ) -> Box<Future<Item = (), Error = Error>> {
    let mut req = hyper::Request::new(
        hyper::Method::Post,
        format!("{}/pet/{}", prefix, pet_id).parse().unwrap());
    let body = url::form_urlencoded::Serializer::new(String::new())
        .append_pair("name", name)
        .append_pair("status", status)
        .finish();
    req.headers_mut().set(hyper::header::ContentType::form_url_encoded());
    req.headers_mut().set(hyper::header::ContentLength(body.len() as u64));
    req.set_body(body);

    Box::new(
        cli.request(req).and_then(|res| { res.body().concat2() })
        .map_err(|e| Error::from(e))
        .and_then(|_| futures::future::ok(()))
    )
}
