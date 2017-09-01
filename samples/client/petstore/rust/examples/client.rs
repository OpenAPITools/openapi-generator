extern crate petstore_client;
extern crate hyper;
extern crate tokio_core;
extern crate futures;

use hyper::Client;
use hyper::client::HttpConnector;
use tokio_core::reactor::Core;
use futures::{Future};

fn main() {
    let mut core = Core::new().expect("failed to init core");
    let handle = core.handle();

    let new_pet = petstore_client::models::Pet::new("barker".to_owned(), vec![]).with_id(1337);

    let apicli = petstore_client::apis::client::APIClient::new(
        petstore_client::apis::configuration::Configuration::new(
            Client::configure().connector(HttpConnector::new(4, &handle)).build(&handle)));

    let work = apicli.pet_api().add_pet(new_pet)
    // petstore_client::apis::add_pet(api, &client, &new_pet)
    .and_then(|_| {
        apicli.pet_api().update_pet_with_form(1337, "barko", "escaped")
    })
    .and_then(|_| {
        apicli.pet_api().get_pet_by_id(1337)
    })
    .and_then(|pet| {
        println!("pet: {:?}", pet);
        futures::future::ok(())
    });

    core.run(work).expect("failed to run core");
}
