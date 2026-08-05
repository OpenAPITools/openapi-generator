//! Runtime regression tests for auth-scheme precedence in the generated `AddContext` middleware.
//!
//! `swagger::auth::from_headers` returns an *untyped* `AuthData`, matching an
//! `Authorization` header that carries either `Basic` or `Bearer` credentials. Every
//! generated auth block returns early once it matches, so a block that does not check
//! which variant it received will claim credentials belonging to a different scheme and
//! prevent every later block - including API-key blocks - from ever running.
//!
//! This spec generates the blocks in the following order, which is what makes the
//! behaviour observable from the outside:
//!
//! 1. `petstore_auth`   - OAuth2, reads `Authorization: Bearer`
//! 2. `api_key`         - API key, reads the `api_key` header
//! 3. `api_key_query`   - API key, reads the `api_key_query` query parameter
//! 4. `http_basic_test` - HTTP Basic, reads `Authorization: Basic`
//!
//! Presenting Basic credentials alongside an API key therefore proves whether block 1
//! stays in its lane: if it wrongly claims the Basic credentials it also swallows
//! blocks 2 and 3.

#![cfg(feature = "server")]

use std::sync::{Arc, Mutex};

use hyper::service::Service;
use hyper::{Request, Response};
use petstore_with_fake_endpoints_models_for_testing::context::AddContext;
use swagger::auth::AuthData;
use swagger::{EmptyContext, Has};

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

#[test]
fn bearer_block_does_not_claim_basic_credentials() {
    // The OAuth2 (Bearer) block is generated first. It must ignore Basic credentials and
    // let them fall through to the HTTP Basic block generated last.
    assert_eq!(
        resolve_auth_data("/", &[("authorization", BASIC_HEADER)]),
        Some(AuthData::Basic("user".to_owned(), "password".to_owned())),
    );
}

#[test]
fn basic_block_does_not_claim_bearer_credentials() {
    assert_eq!(
        resolve_auth_data("/", &[("authorization", "Bearer some-token")]),
        Some(AuthData::Bearer("some-token".to_owned())),
    );
}

#[test]
fn header_api_key_is_reachable_when_basic_credentials_are_also_present() {
    // Regression test: an unguarded Bearer block matches the Basic credentials, returns
    // early, and the `api_key` header block below it never runs.
    assert_eq!(
        resolve_auth_data(
            "/",
            &[("authorization", BASIC_HEADER), ("api_key", "header-key")],
        ),
        Some(AuthData::ApiKey("header-key".to_owned())),
    );
}

#[test]
fn query_api_key_is_reachable_when_basic_credentials_are_also_present() {
    // Same regression, for the query-parameter API-key block.
    assert_eq!(
        resolve_auth_data(
            "/?api_key_query=query-key",
            &[("authorization", BASIC_HEADER)],
        ),
        Some(AuthData::ApiKey("query-key".to_owned())),
    );
}

#[test]
fn no_credentials_resolve_to_no_auth_data() {
    assert_eq!(resolve_auth_data("/", &[]), None);
}
