use std::collections::HashMap;

use axum::{body::Body, extract::*, response::Response, routing::*};
use axum_extra::extract::{CookieJar, Multipart};
use bytes::Bytes;
use http::{header::CONTENT_TYPE, HeaderMap, HeaderName, HeaderValue, Method, StatusCode};
use tracing::error;
use validator::{Validate, ValidationErrors};

use crate::{header, types::*};

#[allow(unused_imports)]
use crate::models;

use crate::{
    Api, MultipartRelatedRequestPostResponse, MultipartRequestPostResponse,
    MultipleIdenticalMimeTypesPostResponse,
};

/// Setup API Server.
pub fn new<I, A>(api_impl: I) -> Router
where
    I: AsRef<A> + Clone + Send + Sync + 'static,
    A: Api + 'static,
{
    // build our application with a route
    Router::new()
        .route(
            "/multipart_related_request",
            post(multipart_related_request_post::<I, A>),
        )
        .route("/multipart_request", post(multipart_request_post::<I, A>))
        .route(
            "/multiple-identical-mime-types",
            post(multiple_identical_mime_types_post::<I, A>),
        )
        .with_state(api_impl)
}

#[tracing::instrument(skip_all)]
fn multipart_related_request_post_validation() -> std::result::Result<(), ValidationErrors> {
    Ok(())
}
/// MultipartRelatedRequestPost - POST /multipart_related_request
#[tracing::instrument(skip_all)]
async fn multipart_related_request_post<I, A>(
    method: Method,
    host: Host,
    cookies: CookieJar,
    State(api_impl): State<I>,
    body: axum::body::Body,
) -> Result<Response, StatusCode>
where
    I: AsRef<A> + Send + Sync,
    A: Api,
{
    #[allow(clippy::redundant_closure)]
    let validation =
        tokio::task::spawn_blocking(move || multipart_related_request_post_validation())
            .await
            .unwrap();

    let Ok(()) = validation else {
        return Response::builder()
            .status(StatusCode::BAD_REQUEST)
            .body(Body::from(validation.unwrap_err().to_string()))
            .map_err(|_| StatusCode::BAD_REQUEST);
    };

    let result = api_impl
        .as_ref()
        .multipart_related_request_post(method, host, cookies, body)
        .await;

    let mut response = Response::builder();

    let resp = match result {
        Ok(rsp) => match rsp {
            MultipartRelatedRequestPostResponse::Status201_OK => {
                let mut response = response.status(201);
                response.body(Body::empty())
            }
        },
        Err(_) => {
            // Application code returned an error. This should not happen, as the implementation should
            // return a valid response.
            response.status(500).body(Body::empty())
        }
    };

    resp.map_err(|e| {
        error!(error = ?e);
        StatusCode::INTERNAL_SERVER_ERROR
    })
}

#[tracing::instrument(skip_all)]
fn multipart_request_post_validation() -> std::result::Result<(), ValidationErrors> {
    Ok(())
}
/// MultipartRequestPost - POST /multipart_request
#[tracing::instrument(skip_all)]
async fn multipart_request_post<I, A>(
    method: Method,
    host: Host,
    cookies: CookieJar,
    State(api_impl): State<I>,
    body: Multipart,
) -> Result<Response, StatusCode>
where
    I: AsRef<A> + Send + Sync,
    A: Api,
{
    #[allow(clippy::redundant_closure)]
    let validation = tokio::task::spawn_blocking(move || multipart_request_post_validation())
        .await
        .unwrap();

    let Ok(()) = validation else {
        return Response::builder()
            .status(StatusCode::BAD_REQUEST)
            .body(Body::from(validation.unwrap_err().to_string()))
            .map_err(|_| StatusCode::BAD_REQUEST);
    };

    let result = api_impl
        .as_ref()
        .multipart_request_post(method, host, cookies, body)
        .await;

    let mut response = Response::builder();

    let resp = match result {
        Ok(rsp) => match rsp {
            MultipartRequestPostResponse::Status201_OK => {
                let mut response = response.status(201);
                response.body(Body::empty())
            }
        },
        Err(_) => {
            // Application code returned an error. This should not happen, as the implementation should
            // return a valid response.
            response.status(500).body(Body::empty())
        }
    };

    resp.map_err(|e| {
        error!(error = ?e);
        StatusCode::INTERNAL_SERVER_ERROR
    })
}

#[tracing::instrument(skip_all)]
fn multiple_identical_mime_types_post_validation() -> std::result::Result<(), ValidationErrors> {
    Ok(())
}
/// MultipleIdenticalMimeTypesPost - POST /multiple-identical-mime-types
#[tracing::instrument(skip_all)]
async fn multiple_identical_mime_types_post<I, A>(
    method: Method,
    host: Host,
    cookies: CookieJar,
    State(api_impl): State<I>,
    body: axum::body::Body,
) -> Result<Response, StatusCode>
where
    I: AsRef<A> + Send + Sync,
    A: Api,
{
    #[allow(clippy::redundant_closure)]
    let validation =
        tokio::task::spawn_blocking(move || multiple_identical_mime_types_post_validation())
            .await
            .unwrap();

    let Ok(()) = validation else {
        return Response::builder()
            .status(StatusCode::BAD_REQUEST)
            .body(Body::from(validation.unwrap_err().to_string()))
            .map_err(|_| StatusCode::BAD_REQUEST);
    };

    let result = api_impl
        .as_ref()
        .multiple_identical_mime_types_post(method, host, cookies, body)
        .await;

    let mut response = Response::builder();

    let resp = match result {
        Ok(rsp) => match rsp {
            MultipleIdenticalMimeTypesPostResponse::Status200_OK => {
                let mut response = response.status(200);
                response.body(Body::empty())
            }
        },
        Err(_) => {
            // Application code returned an error. This should not happen, as the implementation should
            // return a valid response.
            response.status(500).body(Body::empty())
        }
    };

    resp.map_err(|e| {
        error!(error = ?e);
        StatusCode::INTERNAL_SERVER_ERROR
    })
}
