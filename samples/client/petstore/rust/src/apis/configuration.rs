use hyper;

pub struct Configuration<C: hyper::client::Connect> {
  pub base_path: String,
  pub client: hyper::client::Client<C>,
}

impl<C: hyper::client::Connect> Configuration<C> {
  pub fn new(client: hyper::client::Client<C>) -> Configuration<C> {
    Configuration {
      base_path: "http://petstore.swagger.io:80/v2".to_owned(),
      client: client,
    }
  }
}
