use http;
use hyper;
use serde_json;

#[derive(Debug)]
pub enum Error {
    Api(ApiError),
    Header(hyper::http::header::InvalidHeaderValue),
    Http(http::Error),
    Hyper(hyper::Error),
    Serde(serde_json::Error),
    UriError(http::uri::InvalidUri),
}

#[derive(Debug)]
pub struct ApiError {
    pub code: hyper::StatusCode,
    pub body: hyper::body::Body,
}

impl From<(hyper::StatusCode, hyper::body::Body)> for Error {
    fn from(e: (hyper::StatusCode, hyper::body::Body)) -> Self {
        Error::Api(ApiError {
            code: e.0,
            body: e.1,
        })
    }
}

impl From<http::Error> for Error {
    fn from(e: http::Error) -> Self {
        return Error::Http(e)
    }
}

impl From<hyper::Error> for Error {
    fn from(e: hyper::Error) -> Self {
        return Error::Hyper(e)
    }
}

impl From<serde_json::Error> for Error {
    fn from(e: serde_json::Error) -> Self {
        return Error::Serde(e)
    }
}

mod request;

mod pet_api;
pub use self::pet_api::{ PetApi, PetApiClient };
mod store_api;
pub use self::store_api::{ StoreApi, StoreApiClient };
mod user_api;
pub use self::user_api::{ UserApi, UserApiClient };

pub mod configuration;
pub mod client;
