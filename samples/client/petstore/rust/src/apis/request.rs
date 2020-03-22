// use std::borrow::Cow;
use std::collections::HashMap;

use super::{configuration, Error};
use futures::{future::FutureExt, future::TryFutureExt, stream::TryStreamExt, Future};
use hyper::header::{HeaderValue, AUTHORIZATION, CONTENT_LENGTH, CONTENT_TYPE, USER_AGENT};

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
            method,
            path,
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

    pub fn execute<C, U>(
        self,
        conf: &configuration::Configuration<C>,
    ) -> impl Future<Output = Result<U, Error<serde_json::Value>>>
    where
        C: hyper::client::connect::Connect + Clone + Send + Sync + 'static,
        U: Sized,
        for<'de> U: serde::Deserialize<'de>,
    {
        let mut query_string = ::url::form_urlencoded::Serializer::new("".to_owned());
        // raw_headers is for headers we don't know the proper type of (e.g. custom api key
        // headers); headers is for ones we do know the type of.
        let mut raw_headers = HashMap::new();
        let mut headers = hyper::header::HeaderMap::new();

        let mut path = self.path;
        for (k, v) in self.path_params {
            // replace {id} with the value of the id path param
            path = path.replace(&format!("{{{}}}", k), &v);
        }

        for (k, v) in self.header_params {
            raw_headers.insert(k, v);
        }

        for (key, val) in self.query_params {
            query_string.append_pair(&key, &val);
        }

        match self.auth {
            Auth::ApiKey(apikey) => {
                if let Some(ref key) = conf.api_key {
                    let val = apikey.key(&key.prefix, &key.key);
                    if apikey.in_query {
                        query_string.append_pair(&apikey.param_name, &val);
                    }
                    if apikey.in_header {
                        raw_headers.insert(apikey.param_name, val);
                    }
                }
            }
            Auth::Basic => {
                if let Some(ref auth_conf) = conf.basic_auth {
                    let mut auth = auth_conf.0.clone();
                    if let Some(password) = &auth_conf.1 {
                        auth.push(' ');
                        auth.push_str(password);
                    }
                    let auth = format!("Basic {}", base64::encode(&auth));
                    headers.insert(AUTHORIZATION, HeaderValue::from_str(&auth).unwrap());
                }
            }
            Auth::Oauth => {
                if let Some(ref token) = conf.oauth_access_token {
                    let auth = format!("Bearer {}", token);
                    headers.insert(AUTHORIZATION, HeaderValue::from_str(&auth).unwrap());
                }
            }
            Auth::None => {}
        }

        let mut uri_str = format!("{}{}", conf.base_path, path);

        let query_string_str = query_string.finish();
        if query_string_str != "" {
            uri_str += "?";
            uri_str += &query_string_str;
        }
        let uri = match uri_str.parse::<hyper::Uri>() {
            Err(e) => {
                return futures::future::err(Error::Http(e.into())).left_future();
            }
            Ok(u) => u,
        };

        let mut builder = hyper::Request::builder();
        builder = builder.method(self.method).uri(uri);
        {
            if let Some(req_headers) = builder.headers_mut() {
                if let Some(ref user_agent) = conf.user_agent {
                    req_headers.insert(
                        USER_AGENT,
                        HeaderValue::from_str(&user_agent).expect("invalid user agent"),
                    );
                }

                req_headers.extend(headers.into_iter());

                for (key, val) in raw_headers {
                    req_headers.insert(
                        key.parse::<http::header::HeaderName>()
                            .expect("invalid custom header name"),
                        HeaderValue::from_str(&val).expect("invalid custom header value"),
                    );
                }
            }
        }

        let request_body = if self.form_params.len() > 0 {
            builder.headers_mut().map(|headers| {
                headers.insert(
                    CONTENT_TYPE,
                    HeaderValue::from_static("application/www-form-url-encoded"),
                );
            });
            let mut enc = ::url::form_urlencoded::Serializer::new("".to_owned());
            for (k, v) in self.form_params {
                enc.append_pair(&k, &v);
            }
            enc.finish()
        } else if let Some(body) = self.serialized_body {
            builder.headers_mut().map(|headers| {
                headers.insert(CONTENT_TYPE, HeaderValue::from_static("application/json"));
                headers.insert(
                    CONTENT_LENGTH,
                    HeaderValue::from_str(&format!("{}", body.len())).unwrap(),
                );
            });
            body
        } else {
            String::new()
        };
        let body = Result::<_, Box<dyn std::error::Error + Send + Sync>>::Ok(request_body);
        let body = futures::stream::once(futures::future::ready(body));
        let body = hyper::Body::wrap_stream(body);
        let request = builder.body(body);
        let request: hyper::Request<hyper::Body> = match request {
            Ok(req) => req,
            Err(err) => return futures::future::err(err.into()).left_future(),
        };

        let no_ret_type = self.no_return_type;
        let res = conf
            .client
            .request(request)
            .map_err(|e| Error::from(e))
            .map_ok(|mut resp| (resp.status(), std::mem::take(resp.body_mut())))
            .and_then(|(status, body): (_, hyper::Body)| {
                body.try_fold(Vec::new(), |mut cur: Vec<u8>, bytes: hyper::body::Bytes| {
                    cur.extend(bytes);
                    futures::future::ok(cur)
                })
                .map_ok(move |body: Vec<u8>| (status, body))
                .map_err(Into::into)
            })
            //.map(|debug: Result<(hyper::StatusCode, Vec<u8>), _>| debug)
            .and_then(|(status, body): (hyper::StatusCode, Vec<u8>)| {
                if status.is_success() {
                    futures::future::ok(body)
                } else {
                    futures::future::err(Error::from((status, &*body)))
                }
            });

        res.and_then(move |body: Vec<u8>| {
            let parsed: Result<U, _> = if no_ret_type {
                // This is a hack; if there's no_ret_type, U is (), but serde_json gives an
                // error when deserializing "" into (), so deserialize 'null' into it
                // instead.
                // An alternate option would be to require U: Default, and then return
                // U::default() here instead since () implements that, but then we'd
                // need to impl default for all models.
                serde_json::from_str("null")
            } else {
                serde_json::from_slice(&body)
            };
            match parsed {
                Ok(val) => futures::future::ok(val),
                Err(e) => futures::future::err(e.into()),
            }
        })
        .right_future()
    }
}
