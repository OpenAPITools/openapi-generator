//! Runtime regression tests for auth-scheme precedence in the generated `AddContext` middleware.
//!
//! Companion to the same test in the `petstore-with-fake-endpoints-models-for-testing`
//! sample. That spec generates the OAuth2 block first and the HTTP Basic block last, so it
//! can only prove that an `isOAuth` block leaves Basic credentials alone. This spec covers
//! the opposite direction - an `isBasicBasic` block ahead of an `isBasicBearer` block -
//! which is the `from_headers` pairing issue #24095 was reported against.
//!
//! This spec generates the blocks in the following order:
//!
//! 1. `basicAuth`  - HTTP Basic, reads `Authorization`
//! 2. `apiKeyAuth` - API key, reads the `x-api-key` header
//! 3. `bearerAuth` - HTTP Bearer, reads `Authorization`
//!
//! `swagger::auth::from_headers` returns an *untyped* `AuthData` and matches an
//! `Authorization` header carrying either HTTP scheme, and every generated block returns
//! early once it matches. So an unrestricted block 1 claims bearer credentials, and in
//! doing so also makes blocks 2 and 3 unreachable.
//!
//! The apiKey scheme sits deliberately *between* the two HTTP schemes: that is what makes
//! the bug observable from outside. Were blocks 1 and 3 adjacent, both the broken and the
//! fixed generator would resolve bearer credentials to `AuthData::Bearer` - via the wrong
//! block in the broken case - and no request-level assertion could distinguish them.

#![cfg(feature = "server")]

use std::sync::{Arc, Mutex};

use hyper::service::Service;
use hyper::{Request, Response};
use overlapping_auth_schemes::context::AddContext;
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
fn resolve_auth_data(headers: &[(&str, &str)]) -> Option<AuthData> {
    let capture = CaptureAuthData::default();
    let service = AddContext::<_, EmptyContext>::new(capture.clone());

    let mut builder = Request::get("/");
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
const BEARER_TOKEN: &str = "some-token";

#[test]
fn basic_block_does_not_swallow_bearer_credentials() {
    // The regression, in the only form that is observable at runtime: with an unrestricted
    // Basic block, block 1 claims the bearer credentials and returns, so the `x-api-key`
    // block below it never runs and this resolves to `AuthData::Bearer` instead.
    assert_eq!(
        resolve_auth_data(&[
            ("authorization", BEARER_HEADER),
            ("x-api-key", "header-key")
        ]),
        Some(AuthData::ApiKey("header-key".to_owned())),
    );
}

#[test]
fn bearer_block_is_still_reached_when_it_is_the_only_match() {
    // Restricting block 1 must not strand block 3: bearer credentials with no API key
    // still have to fall all the way through to the Bearer block.
    assert_eq!(
        resolve_auth_data(&[("authorization", BEARER_HEADER)]),
        Some(AuthData::Bearer(BEARER_TOKEN.to_owned())),
    );
}

#[test]
fn basic_credentials_are_claimed_by_the_basic_block() {
    // Block 1 legitimately matches here and must still take precedence over the API key.
    assert_eq!(
        resolve_auth_data(&[("authorization", BASIC_HEADER), ("x-api-key", "header-key")]),
        Some(AuthData::Basic("user".to_owned(), "password".to_owned())),
    );
}

#[test]
fn header_api_key_is_reachable_when_an_unhandled_authorization_scheme_is_present() {
    // Neither HTTP block handles `Digest`, so both must decline and leave the API key
    // block reachable.
    assert_eq!(
        resolve_auth_data(&[
            ("authorization", "Digest username=\"user\""),
            ("x-api-key", "header-key"),
        ]),
        Some(AuthData::ApiKey("header-key".to_owned())),
    );
}

#[test]
fn header_api_key_resolves_when_no_authorization_header_is_present() {
    assert_eq!(
        resolve_auth_data(&[("x-api-key", "header-key")]),
        Some(AuthData::ApiKey("header-key".to_owned())),
    );
}

#[test]
fn no_credentials_resolve_to_no_auth_data() {
    assert_eq!(resolve_auth_data(&[]), None);
}
