use std::collections::HashMap;

use super::{configuration, Error};
use futures::StreamExt;
use hyper;
use serde;
use serde_json;
use std::sync::Arc;

pub(crate) struct ApiKey {
    pub in_header: bool,
    pub in_query: bool,
    pub param_name: String,
}

impl ApiKey {
    fn key(&self, prefix: &Option<String>, key: &str) -> String {
        match prefix {
            None => key.to_owned(),
            Some(ref prefix) => format!("{} {}", prefix, key),
        }
    }
}

#[allow(dead_code)]
pub(crate) enum Auth {
    None,
    ApiKey(ApiKey),
    Basic,
    Oauth,
}

pub(crate) struct Request {
    auth: Auth,
    method: hyper::Method,
    path: String,
    query_params: HashMap<String, String>,
    no_return_type: bool,
    path_params: HashMap<String, String>,
    form_params: HashMap<String, String>,
    header_params: HashMap<String, String>,
    // TODO: multiple body params are possible technically, but not supported here.
    serialized_body: Option<String>,
}

impl Request {
    pub fn new(method: hyper::Method, path: String) -> Self {
        Request {
            auth: Auth::None,
            method: method,
            path: path,
            query_params: HashMap::new(),
            path_params: HashMap::new(),
            form_params: HashMap::new(),
            header_params: HashMap::new(),
            serialized_body: None,
            no_return_type: false,
        }
    }

    pub fn with_body_param<T: serde::Serialize>(mut self, param: T) -> Self {
        self.serialized_body = Some(serde_json::to_string(&param).unwrap());
        self
    }

    pub fn with_header_param(mut self, basename: String, param: String) -> Self {
        self.header_params.insert(basename, param);
        self
    }

    pub fn with_query_param(mut self, basename: String, param: String) -> Self {
        self.query_params.insert(basename, param);
        self
    }

    pub fn with_path_param(mut self, basename: String, param: String) -> Self {
        self.path_params.insert(basename, param);
        self
    }

    pub fn with_form_param(mut self, basename: String, param: String) -> Self {
        self.form_params.insert(basename, param);
        self
    }

    pub fn returns_nothing(mut self) -> Self {
        self.no_return_type = true;
        self
    }

    pub fn with_auth(mut self, auth: Auth) -> Self {
        self.auth = auth;
        self
    }

    pub async fn execute<'a, C, U>(
        self,
        conf: Arc<configuration::Configuration<C>>,
    ) -> Result<U, Error<serde_json::Value>>
    where
        C: hyper::client::connect::Connect + Clone + Send + Sync + 'static,
        U: Sized + 'a,
        for<'de> U: serde::Deserialize<'de>,
    {
        //TODO: Safe replacement for this
        let mut query_string: Vec<String> = self.query_params.iter().map(|t| {
            format!("{}={}", t.0, t.1)
        }).collect();

        let mut path = self.path;
        for (k, v) in self.path_params {
            // replace {id} with the value of the id path param
            path = path.replace(&format!("{{{}}}", k), &v);
        }

        let mut req = hyper::Request::builder();

        for (k, v) in self.header_params {
            req = req.header(&k, v);
        }

        match self.auth {
            Auth::ApiKey(apikey) => {
                if let Some(ref key) = conf.api_key {
                    let val = apikey.key(&key.prefix, &key.key);
                    if apikey.in_query {
                        query_string.push(format!("{}={}", apikey.param_name, val));
                    }
                    if apikey.in_header {
                        req = req.header(&apikey.param_name, val);
                    }
                }
            }
            Auth::Basic => {
                if let Some(ref auth_conf) = conf.basic_auth {
                    let user_pass = if let Some(ref pass) = auth_conf.1 {
                        format!("{}:{}", auth_conf.0, pass)
                    } else {
                        auth_conf.0.clone()
                    };
                    let user_pass_b64 = base64::encode(&user_pass);
                    req = req.header(hyper::header::AUTHORIZATION.as_str(), format!("Basic {}", user_pass_b64));
                }
            }
            Auth::Oauth => {
                if let Some(ref token) = conf.oauth_access_token {
                    req = req.header(hyper::header::AUTHORIZATION.as_str(), format!("Bearer {}", token));
                }
            }
            Auth::None => {}
        }

        let mut uri_str = format!("{}{}", conf.base_path, path);

        let query_string = urlencoding::encode(&query_string.join("&"));
        if !query_string.is_empty() {
            uri_str += "?";
            uri_str += &query_string;
        }
        let uri: hyper::Uri = match uri_str.parse() {
            Err(e) => {
                return Err(Error::InvalidUriError(e));
            }
            Ok(u) => u,
        };

        req = req.method(self.method);
        req = req.uri(uri);
        if let Some(ref user_agent) = conf.user_agent {
            req = req.header(hyper::header::USER_AGENT.as_str(), user_agent.clone());
        }

        let body = if self.form_params.len() > 0 {
            req = req.header(hyper::header::CONTENT_TYPE.as_str(), "application/x-www-form-urlencoded");
            let mut enc = ::url::form_urlencoded::Serializer::new("".to_owned());
            for (k, v) in self.form_params {
                enc.append_pair(&k, &v);
            }
            hyper::Body::from(enc.finish())
        } else if let Some(body) = self.serialized_body {
            req = req.header(hyper::header::CONTENT_TYPE.as_str(), "application/json");
            req = req.header(hyper::header::CONTENT_LENGTH.as_str(), format!("{}", body.len() as u64));
            hyper::Body::from(body)
        } else {
            hyper::Body::empty()
        };

        let no_ret_type = self.no_return_type;
        let req = req.body(body).map_err(Error::from)?;
        let resp = conf.client
                .request(req)
                .await
                .map_err(|e| Error::from(e))?;
        let status = resp.status();
        let mut bytes = vec![];
        let mut body = resp.into_body();
        while let Some(c) = body.next().await {
            bytes.extend(c?.to_vec());
        }
        if !status.is_success() {
            return Err(Error::from((status, bytes.as_slice())));
        }
        let parsed: Result<U, _> = if no_ret_type {
            // This is a hack; if there's no_ret_type, U is (), but serde_json gives an
            // error when deserializing "" into (), so deserialize 'null' into it
            // instead.
            // An alternate option would be to require U: Default, and then return
            // U::default() here instead since () implements that, but then we'd
            // need to impl default for all models.
            serde_json::from_str("null")
        } else {
            serde_json::from_slice(bytes.as_slice())
        };
        parsed.map_err(|e| Error::from(e))
    }
}
