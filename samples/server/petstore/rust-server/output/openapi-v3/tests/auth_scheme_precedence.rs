//! Runtime checks for auth-scheme precedence in the generated `AddContext` middleware.
//!
//! This file is generated. `swagger::auth::from_headers` returns an *untyped* `AuthData`,
//! matching an `Authorization` header that carries either `Basic` or `Bearer` credentials.
//! Every generated auth block returns early once it matches, so a block that does not check
//! which variant it received will claim credentials belonging to a different scheme and
//! prevent every later block - including API-key blocks - from ever running.
//!
//! The expectations below are derived from the security schemes this API declares, in the
//! order their blocks are generated.

#![cfg(feature = "server")]

use std::sync::{Arc, Mutex};

use hyper::service::Service;
use hyper::{Request, Response};
use swagger::auth::AuthData;
use swagger::{EmptyContext, Has};
use openapi_v3::context::AddContext;

/// Innermost service: records the `Option<AuthData>` that `AddContext` pushed onto the context.
#[derive(Clone, Default)]
struct CaptureAuthData(Arc<Mutex<Option<AuthData>>>);

impl<C, ReqBody> Service<(Request<ReqBody>, C)> for CaptureAuthData
where
    C: Has<Option<AuthData>>,
{
    type Response = Response<String>;
    type Error = std::convert::Infallible;
    type Future = std::future::Ready<Result<Self::Response, Self::Error>>;

    fn call(&self, (_request, context): (Request<ReqBody>, C)) -> Self::Future {
        let auth_data: &Option<AuthData> = context.get();
        *self.0.lock().expect("lock poisoned") = auth_data.clone();
        std::future::ready(Ok(Response::new(String::new())))
    }
}

/// Drives a request through `AddContext` and returns the `AuthData` it resolved.
fn resolve_auth_data(uri: &str, headers: &[(&str, &str)]) -> Option<AuthData> {
    let capture = CaptureAuthData::default();
    let service = AddContext::<_, EmptyContext>::new(capture.clone());

    let mut builder = Request::get(uri);
    for (name, value) in headers {
        builder = builder.header(*name, *value);
    }
    let request = builder.body(()).expect("request should build");

    futures::executor::block_on(service.call(request)).expect("service call should succeed");

    let resolved = capture.0.lock().expect("lock poisoned").clone();
    resolved
}

/// `dXNlcjpwYXNzd29yZA==` is `user:password`.
const BASIC_HEADER: &str = "Basic dXNlcjpwYXNzd29yZA==";
const BEARER_HEADER: &str = "Bearer some-token";

#[test]
fn no_credentials_resolve_to_no_auth_data() {
    assert_eq!(resolve_auth_data("/", &[]), None);
}

#[test]
fn basic_credentials_resolve_to_none_when_no_basic_scheme_is_declared() {
    assert_eq!(
        resolve_auth_data("/", &[("authorization", BASIC_HEADER)]),
        None,
    );
}

#[test]
fn bearer_credentials_resolve_to_the_declared_bearer_scheme() {
    assert_eq!(
        resolve_auth_data("/", &[("authorization", BEARER_HEADER)]),
        Some(AuthData::Bearer("some-token".to_owned())),
    );
}
