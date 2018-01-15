extern crate futures;
extern crate hyper;
extern crate petstore_client;
extern crate tokio_core;

use hyper::Client;
use hyper::client::HttpConnector;
use tokio_core::reactor::Core;
use futures::Future;
use petstore_client::apis::client::APIClient;
use petstore_client::apis::Error;

fn main() {
    let mut core = Core::new().expect("failed to init core");
    let handle = core.handle();

    let mut configuration = petstore_client::apis::configuration::Configuration::new(
        Client::configure()
            .connector(HttpConnector::new(4, &handle))
            .build(&handle),
    );
    if let Ok(env_override) = std::env::var("PETSTORE_BASEPATH") {
        configuration.base_path = env_override;
    }

    let apicli = APIClient::new(configuration);
    let work = apicli
        .user_api()
        .delete_user("404")
        .then(|resp| match resp {
            Ok(resp) => {
                panic!(format!(
                    "update for nonexistent pet should fail, but got: {:?}",
                    resp
                ));
            }
            Err(Error::ApiError(s)) => {
                println!("got expected error: {:?}", s);
                futures::future::ok::<(), ()>(())
            }
            Err(Error::Hyper(e)) => {
                println!("network error: {}", e);
                futures::future::ok::<(), ()>(())
            }
            Err(e) => panic!("unexpected error: {:?}", e),
        });

    core.run(work).expect("failed to run core");
}
