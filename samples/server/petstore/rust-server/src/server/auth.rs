use std::io;
use hyper;
use hyper::{Request, Response, Error, StatusCode};
use server::url::form_urlencoded;
use swagger::auth::{Authorization, AuthData, Scopes};
use Api;

pub struct NewService<T> where T: hyper::server::NewService<Request=(Request,Option<AuthData>), Response=Response, Error=Error> {
    inner: T,
}

impl<T> NewService<T> where T: hyper::server::NewService<Request=(Request,Option<AuthData>), Response=Response, Error=Error> + 'static {
    pub fn new(inner: T) -> NewService<T> {
        NewService{inner}
    }
}

impl<T> hyper::server::NewService for NewService<T> where T: hyper::server::NewService<Request=(Request,Option<AuthData>), Response=Response, Error=Error> + 'static {
    type Request = Request;
    type Response = Response;
    type Error = Error;
    type Instance = Service<T::Instance>;

    fn new_service(&self) -> Result<Self::Instance, io::Error> {
        self.inner.new_service().map(|s| Service::new(s))
    }
}

/// Middleware to extract authentication data from request
pub struct Service<T> where T: hyper::server::Service<Request=(Request,Option<AuthData>), Response=Response, Error=Error> {
    inner: T,
}

impl<T> Service<T> where T: hyper::server::Service<Request=(Request,Option<AuthData>), Response=Response, Error=Error> {
    pub fn new(inner: T) -> Service<T> {
        Service{inner}
    }
}

impl<T> hyper::server::Service for Service<T> where T: hyper::server::Service<Request=(Request,Option<AuthData>), Response=Response, Error=Error> {
    type Request = Request;
    type Response = Response;
    type Error = Error;
    type Future = T::Future;

    fn call(&self, req: Self::Request) -> Self::Future {
        {
            header! { (ApiKey1, "api_key") => [String] }
            if let Some(header) = req.headers().get::<ApiKey1>().cloned() {
                let auth_data = AuthData::ApiKey(header.0);
                return self.inner.call((req, Some(auth_data)));
            }
        }
        {
            let key = form_urlencoded::parse(req.query().unwrap_or_default().as_bytes())
                .filter(|e| e.0 == "api_key_query")
                .map(|e| e.1.clone().into_owned())
                .nth(0);
            if let Some(key) = key {
                let auth_data = AuthData::ApiKey(key);
                return self.inner.call((req, Some(auth_data)));
            }
        }
        {
            use hyper::header::{Authorization, Basic, Bearer};
            use std::ops::Deref;
            if let Some(basic) = req.headers().get::<Authorization<Basic>>().cloned() {
                let auth_data = AuthData::Basic(basic.deref().clone());
                return self.inner.call((req, Some(auth_data)));
            }
        }
        {
            use hyper::header::{Authorization, Basic, Bearer};
            use std::ops::Deref;
            if let Some(bearer) = req.headers().get::<Authorization<Bearer>>().cloned() {
                let auth_data = AuthData::Bearer(bearer.deref().clone());
                return self.inner.call((req, Some(auth_data)));
            }
        }

        return self.inner.call((req, None));
    }
}
