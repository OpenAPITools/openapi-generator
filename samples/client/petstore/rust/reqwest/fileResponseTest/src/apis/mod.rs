use reqwest;
use serde_json;

#[derive(Debug, Clone)]
pub struct ResponseErrorContent {
    pub status: reqwest::StatusCode,
    pub content: String,
    pub entity: Option<serde_json::Value>,
}

#[derive(Debug)]
pub enum Error {
    Reqwest(reqwest::Error),
    Serde(serde_json::Error),
    Io(std::io::Error),
    ResponseError(ResponseErrorContent),
}

impl From<reqwest::Error> for Error {
    fn from(e: reqwest::Error) -> Self {
        Error::Reqwest(e)
    }
}

impl From<serde_json::Error> for Error {
    fn from(e: serde_json::Error) -> Self {
        Error::Serde(e)
    }
}

impl From<std::io::Error> for Error {
    fn from(e: std::io::Error) -> Self {
        Error::Io(e)
    }
}

pub fn urlencode<T: AsRef<str>>(s: T) -> String {
    ::url::form_urlencoded::byte_serialize(s.as_ref().as_bytes()).collect()
}

mod default_api;
pub use self::default_api::{ DefaultApi, DefaultApiClient };

pub mod client;
pub mod configuration;
