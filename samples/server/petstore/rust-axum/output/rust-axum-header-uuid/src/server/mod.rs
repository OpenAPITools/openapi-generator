use std::collections::HashMap;

use axum::{body::Body, extract::*, response::Response, routing::*};
use axum_extra::extract::{CookieJar, Host, Query as QueryExtra};
use bytes::Bytes;
use http::{HeaderMap, HeaderName, HeaderValue, Method, StatusCode, header::CONTENT_TYPE};
use tracing::error;
use validator::{Validate, ValidationErrors};

use crate::{header, types::*};

#[allow(unused_imports)]
use crate::{apis, models};

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
        .route("/users", post(users_post::<I, A, E>))
        .with_state(api_impl)
}

#[tracing::instrument(skip_all)]
fn users_post_validation(
    header_params: models::UsersPostHeaderParams,
) -> std::result::Result<(models::UsersPostHeaderParams,), ValidationErrors> {
    header_params.validate()?;

    Ok((header_params,))
}
/// UsersPost - POST /users
#[tracing::instrument(skip_all)]
async fn users_post<I, A, E>(
    method: Method,
    host: Host,
    cookies: CookieJar,
    headers: HeaderMap,
    State(api_impl): State<I>,
) -> Result<Response, StatusCode>
where
    I: AsRef<A> + Send + Sync,
    A: apis::default::Default<E> + Send + Sync,
    E: std::fmt::Debug + Send + Sync + 'static,
{
    // Header parameters
    let header_params = {
        let header_some_uid = headers.get(HeaderName::from_static("some_uid"));

        let header_some_uid = match header_some_uid {
            Some(v) => match header::IntoHeaderValue::<uuid::Uuid>::try_from((*v).clone()) {
                Ok(result) => result.0,
                Err(err) => {
                    return Response::builder()
                        .status(StatusCode::BAD_REQUEST)
                        .body(Body::from(format!("Invalid header some_uid - {err}")))
                        .map_err(|e| {
                            error!(error = ?e);
                            StatusCode::INTERNAL_SERVER_ERROR
                        });
                }
            },
            None => {
                return Response::builder()
                    .status(StatusCode::BAD_REQUEST)
                    .body(Body::from("Missing required header some_uid"))
                    .map_err(|e| {
                        error!(error = ?e);
                        StatusCode::INTERNAL_SERVER_ERROR
                    });
            }
        };

        models::UsersPostHeaderParams {
            some_uid: header_some_uid,
        }
    };

    #[allow(clippy::redundant_closure)]
    let validation = tokio::task::spawn_blocking(move || users_post_validation(header_params))
        .await
        .unwrap();

    let Ok((header_params,)) = validation else {
        return Response::builder()
            .status(StatusCode::BAD_REQUEST)
            .body(Body::from(validation.unwrap_err().to_string()))
            .map_err(|_| StatusCode::BAD_REQUEST);
    };

    let result = api_impl
        .as_ref()
        .users_post(&method, &host, &cookies, &header_params)
        .await;

    let mut response = Response::builder();

    let resp = match result {
        Ok(rsp) => match rsp {
            apis::default::UsersPostResponse::Status201_AddedRowToTable(body) => {
                let mut response = response.status(201);
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
