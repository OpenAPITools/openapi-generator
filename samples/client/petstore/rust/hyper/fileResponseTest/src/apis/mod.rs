use hyper;
use serde;
use serde_json;

#[derive(Debug)]
pub enum Error<T> {
    Http(http::Error),
    Hyper(hyper::Error),
    Serde(serde_json::Error),
    ApiError(ApiError<T>),
}

impl<T: std::fmt::Debug> std::error::Error for Error<T> {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match self {
            Error::Http(inner) => Some(inner),
            Error::Hyper(inner) => Some(inner),
            Error::Serde(inner) => Some(inner),
            Error::ApiError(_) => None
        }
    }
}

impl<T: std::fmt::Debug> std::fmt::Display for Error<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Error::Http(inner) => write!(f, "query construction error: {}", inner),
            Error::Hyper(inner) => write!(f, "transport error: {}", inner),
            Error::Serde(inner) => write!(f, "serde error: {}", inner),
            Error::ApiError(inner) => write!(f, "api error: {:?}", inner), 
        }
    }
} 

#[derive(Debug)]
pub struct ApiError<T> {
    pub code: hyper::StatusCode,
    pub content: Option<T>,
}

impl<'de, T> From<(hyper::StatusCode, &'de [u8])> for Error<T> 
    where T: serde::Deserialize<'de> {
    fn from(e: (hyper::StatusCode, &'de [u8])) -> Self {
        if e.1.len() == 0 {
            return Error::ApiError(ApiError{
                code: e.0,
                content: None,
            });
        }
        match serde_json::from_slice::<T>(e.1) {
            Ok(t) => Error::ApiError(ApiError{
                code: e.0,
                content: Some(t),
            }),
            Err(e) => {
                Error::from(e)
            }
        }
    }
}

impl<T> From<hyper::Error> for Error<T> {
    fn from(e: hyper::Error) -> Self {
        Error::Hyper(e)
    }
}

impl<T> From<serde_json::Error> for Error<T> {
    fn from(e: serde_json::Error) -> Self {
        Error::Serde(e)
    }
}

impl<T> From<http::Error> for Error<T> {
    fn from(e: http::Error) -> Self {
        Error::Http(e)
    }
}

mod request;

mod default_api;
pub use self::default_api::{ DefaultApi, DefaultApiClient };

pub mod configuration;
pub mod client;
