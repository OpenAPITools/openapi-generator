use std::error;
use std::fmt;

#[derive(Debug, Clone)]
pub struct ResponseContent<T> {
    pub status: reqwest::StatusCode,
    pub content: String,
    pub entity: Option<T>,
}

#[derive(Debug)]
pub enum Error<T> {
    Reqwest(reqwest::Error),
    Serde(serde_json::Error),
    Io(std::io::Error),
    ResponseError(ResponseContent<T>),
}

impl <T> fmt::Display for Error<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let (module, e) = match self {
            Error::Reqwest(e) => ("reqwest", e.to_string()),
            Error::Serde(e) => ("serde", e.to_string()),
            Error::Io(e) => ("IO", e.to_string()),
            Error::ResponseError(e) => ("response", format!("status code {}", e.status)),
        };
        write!(f, "error in {}: {}", module, e)
    }
}

impl <T: fmt::Debug> error::Error for Error<T> {
    fn source(&self) -> Option<&(dyn error::Error + 'static)> {
        Some(match self {
            Error::Reqwest(e) => e,
            Error::Serde(e) => e,
            Error::Io(e) => e,
            Error::ResponseError(_) => return None,
        })
    }
}

impl <T> From<reqwest::Error> for Error<T> {
    fn from(e: reqwest::Error) -> Self {
        Error::Reqwest(e)
    }
}

impl <T> From<serde_json::Error> for Error<T> {
    fn from(e: serde_json::Error) -> Self {
        Error::Serde(e)
    }
}

impl <T> From<std::io::Error> for Error<T> {
    fn from(e: std::io::Error) -> Self {
        Error::Io(e)
    }
}

pub fn urlencode<T: AsRef<str>>(s: T) -> String {
    ::url::form_urlencoded::byte_serialize(s.as_ref().as_bytes()).collect()
}

pub fn parse_deep_object(prefix: &str, value: &serde_json::Value) -> Vec<(String, String)> {
    if let serde_json::Value::Object(object) = value {
        let mut params = vec![];

        for (key, value) in object {
            match value {
                serde_json::Value::Object(_) => params.append(&mut parse_deep_object(
                    &format!("{}[{}]", prefix, key),
                    value,
                )),
                serde_json::Value::Array(array) => {
                    for (i, value) in array.iter().enumerate() {
                        params.append(&mut parse_deep_object(
                            &format!("{}[{}][{}]", prefix, key, i),
                            value,
                        ));
                    }
                },
                serde_json::Value::String(s) => params.push((format!("{}[{}]", prefix, key), s.clone())),
                _ => params.push((format!("{}[{}]", prefix, key), value.to_string())),
            }
        }

        return params;
    }

    unimplemented!("Only objects are supported with style=deepObject")
}

/// Internal use only
/// A content type supported by this client.
#[allow(dead_code)]
enum ContentType {
    Json,
    Text,
    Unsupported(String)
}

impl From<&str> for ContentType {
    fn from(content_type: &str) -> Self {
        if content_type.starts_with("application") && content_type.contains("json") {
            return Self::Json;
        } else if content_type.starts_with("text/plain") {
            return Self::Text;
        } else {
            return Self::Unsupported(content_type.to_string());
        }
    }
}

pub mod fake_api;
pub mod pet_api;
pub mod store_api;
pub mod testing_api;
pub mod user_api;

pub mod configuration;

use std::sync::Arc;

pub trait Api {
    fn fake_api(&self) -> &dyn fake_api::FakeApi;
    fn pet_api(&self) -> &dyn pet_api::PetApi;
    fn store_api(&self) -> &dyn store_api::StoreApi;
    fn testing_api(&self) -> &dyn testing_api::TestingApi;
    fn user_api(&self) -> &dyn user_api::UserApi;
}

pub struct ApiClient {
    fake_api: Box<dyn fake_api::FakeApi>,
    pet_api: Box<dyn pet_api::PetApi>,
    store_api: Box<dyn store_api::StoreApi>,
    testing_api: Box<dyn testing_api::TestingApi>,
    user_api: Box<dyn user_api::UserApi>,
}

impl ApiClient {
    pub fn new(configuration: Arc<configuration::Configuration>) -> Self {
        Self {
            fake_api: Box::new(fake_api::FakeApiClient::new(configuration.clone())),
            pet_api: Box::new(pet_api::PetApiClient::new(configuration.clone())),
            store_api: Box::new(store_api::StoreApiClient::new(configuration.clone())),
            testing_api: Box::new(testing_api::TestingApiClient::new(configuration.clone())),
            user_api: Box::new(user_api::UserApiClient::new(configuration.clone())),
        }
    }
}

impl Api for ApiClient {
    fn fake_api(&self) -> &dyn fake_api::FakeApi {
        self.fake_api.as_ref()
    }
    fn pet_api(&self) -> &dyn pet_api::PetApi {
        self.pet_api.as_ref()
    }
    fn store_api(&self) -> &dyn store_api::StoreApi {
        self.store_api.as_ref()
    }
    fn testing_api(&self) -> &dyn testing_api::TestingApi {
        self.testing_api.as_ref()
    }
    fn user_api(&self) -> &dyn user_api::UserApi {
        self.user_api.as_ref()
    }
}

#[cfg(feature = "mockall")]
pub struct MockApiClient {
    pub fake_api_mock: fake_api::MockFakeApi,
    pub pet_api_mock: pet_api::MockPetApi,
    pub store_api_mock: store_api::MockStoreApi,
    pub testing_api_mock: testing_api::MockTestingApi,
    pub user_api_mock: user_api::MockUserApi,
}

#[cfg(feature = "mockall")]
impl MockApiClient {
    pub fn new() -> Self {
        Self {
            fake_api_mock: fake_api::MockFakeApi::new(),
            pet_api_mock: pet_api::MockPetApi::new(),
            store_api_mock: store_api::MockStoreApi::new(),
            testing_api_mock: testing_api::MockTestingApi::new(),
            user_api_mock: user_api::MockUserApi::new(),
        }
    }
}

#[cfg(feature = "mockall")]
impl Api for MockApiClient {
    fn fake_api(&self) -> &dyn fake_api::FakeApi {
        &self.fake_api_mock
    }
    fn pet_api(&self) -> &dyn pet_api::PetApi {
        &self.pet_api_mock
    }
    fn store_api(&self) -> &dyn store_api::StoreApi {
        &self.store_api_mock
    }
    fn testing_api(&self) -> &dyn testing_api::TestingApi {
        &self.testing_api_mock
    }
    fn user_api(&self) -> &dyn user_api::UserApi {
        &self.user_api_mock
    }
}

