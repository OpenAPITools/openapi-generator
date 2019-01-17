extern crate futures;
extern crate hyper;
extern crate petstore_client;
extern crate tokio_core;

use futures::Future;
use hyper::client::HttpConnector;
use hyper::Client;
use tokio_core::reactor::Core;

fn main() {
    let mut core = Core::new().expect("failed to init core");
    let handle = core.handle();

    let apicli = petstore_client::apis::client::APIClient::new(
        petstore_client::apis::configuration::Configuration::new(
            Client::configure()
                .connector(HttpConnector::new(4, &handle))
                .build(&handle),
        ),
    );

    let new_pet = petstore_client::models::Pet::new("ferris".to_owned(), vec![]).with_id(128149);
    let work = apicli
        .pet_api()
        .add_pet(new_pet)
        .and_then(|_| {
            apicli
                .pet_api()
                .update_pet_with_form(128149, "ferris", "rusted")
        })
        .and_then(|_| apicli.pet_api().get_pet_by_id(128149))
        .and_then(|pet| {
            println!("pet: {:?}", pet);
            futures::future::ok(())
        });

    core.run(work).expect("failed to run core");
}
