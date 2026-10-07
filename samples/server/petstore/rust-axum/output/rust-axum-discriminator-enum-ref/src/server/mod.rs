use std::collections::HashMap;

use axum::{body::Body, extract::*, response::Response, routing::*};
use axum_extra::{
    TypedHeader,
    extract::{CookieJar, Query as QueryExtra},
};
use bytes::Bytes;
use headers::Host;
use http::{HeaderMap, HeaderName, HeaderValue, Method, StatusCode, header::CONTENT_TYPE};
use tracing::error;
use validator::{Validate, ValidationErrors};

#[allow(unused_imports)]
use crate::{apis, models};
use crate::{header, types::*};
#[allow(unused_imports)]
use crate::{
    models::check_xss_map, models::check_xss_map_nested, models::check_xss_map_string,
    models::check_xss_string, models::check_xss_vec_string,
};

/// Setup API Server.
pub fn new<I, A, E>(api_impl: I) -> Router
where
    I: AsRef<A> + Clone + Send + Sync + 'static,
    A: apis::default::Default<E> + Send + Sync + 'static,
    E: std::fmt::Debug + Send + Sync + 'static,
{
    // build our application with a route
    Router::new()
        .route("/", post(foo::<I, A, E>))
        .with_state(api_impl)
}

#[derive(validator::Validate)]
#[allow(dead_code)]
struct FooBodyValidator<'a> {
    #[validate(nested)]
    body: &'a models::Pet,
}

#[tracing::instrument(skip_all)]
fn foo_validation(body: models::Pet) -> std::result::Result<(models::Pet,), ValidationErrors> {
    let b = FooBodyValidator { body: &body };
    b.validate()?;

    Ok((body,))
}
/// Foo - POST /
#[tracing::instrument(skip_all)]
async fn foo<I, A, E>(
    method: Method,
    TypedHeader(host): TypedHeader<Host>,
    cookies: CookieJar,
    State(api_impl): State<I>,
    Json(body): Json<models::Pet>,
) -> Result<Response, StatusCode>
where
    I: AsRef<A> + Send + Sync,
    A: apis::default::Default<E> + Send + Sync,
    E: std::fmt::Debug + Send + Sync + 'static,
{
    #[allow(clippy::redundant_closure)]
    let validation = tokio::task::spawn_blocking(move || foo_validation(body))
        .await
        .unwrap();

    let Ok((body,)) = validation else {
        return Response::builder()
            .status(StatusCode::BAD_REQUEST)
            .body(Body::from(validation.unwrap_err().to_string()))
            .map_err(|_| StatusCode::BAD_REQUEST);
    };

    let result = api_impl.as_ref().foo(&method, &host, &cookies, &body).await;

    let resp = match result {
        Ok(rsp) => match rsp {
            apis::default::FooResponse::Status200_Re(body) => {
                let mut response = Response::builder();
                let mut response = response.status(200);
                {
                    let mut response_headers = response.headers_mut().unwrap();
                    response_headers
                        .insert(CONTENT_TYPE, HeaderValue::from_static("application/json"));
                }

                let body_content = tokio::task::spawn_blocking(move || {
                    serde_json::to_vec(&body).map_err(|e| {
                        error!(error = ?e);
                        StatusCode::INTERNAL_SERVER_ERROR
                    })
                })
                .await
                .unwrap()?;
                response.body(Body::from(body_content))
            }
        },
        Err(why) => {
            // Application code returned an error. This should not happen, as the implementation should
            // return a valid response.
            return api_impl
                .as_ref()
                .handle_error(&method, &host, &cookies, why)
                .await;
        }
    };

    resp.map_err(|e| {
        error!(error = ?e);
        StatusCode::INTERNAL_SERVER_ERROR
    })
}

#[allow(dead_code)]
#[inline]
fn response_with_status_code_only(code: StatusCode) -> Result<Response, StatusCode> {
    Response::builder()
        .status(code)
        .body(Body::empty())
        .map_err(|_| code)
}
