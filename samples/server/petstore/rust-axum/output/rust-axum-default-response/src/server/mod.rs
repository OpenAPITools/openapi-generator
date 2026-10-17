use std::collections::HashMap;

use axum::{body::Body, extract::*, response::Response, routing::*};
use axum_extra::{
    extract::{CookieJar, Query as QueryExtra},
    TypedHeader,
};
use bytes::Bytes;
use headers::Host;
use http::{header::CONTENT_TYPE, HeaderMap, HeaderName, HeaderValue, Method, StatusCode};
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
        .route("/pets", get(list_pets::<I, A, E>))
        .route(
            "/pets/{pet_id}",
            delete(delete_pet::<I, A, E>).get(get_pet::<I, A, E>),
        )
        .with_state(api_impl)
}

#[tracing::instrument(skip_all)]
fn delete_pet_validation(
    path_params: models::DeletePetPathParams,
) -> std::result::Result<(models::DeletePetPathParams,), ValidationErrors> {
    path_params.validate()?;

    Ok((path_params,))
}
/// DeletePet - DELETE /pets/{petId}
#[tracing::instrument(skip_all)]
async fn delete_pet<I, A, E>(
    method: Method,
    TypedHeader(host): TypedHeader<Host>,
    cookies: CookieJar,
    Path(path_params): Path<models::DeletePetPathParams>,
    State(api_impl): State<I>,
) -> Result<Response, StatusCode>
where
    I: AsRef<A> + Send + Sync,
    A: apis::default::Default<E> + Send + Sync,
    E: std::fmt::Debug + Send + Sync + 'static,
{
    #[allow(clippy::redundant_closure)]
    let validation = tokio::task::spawn_blocking(move || delete_pet_validation(path_params))
        .await
        .unwrap();

    let Ok((path_params,)) = validation else {
        return Response::builder()
            .status(StatusCode::BAD_REQUEST)
            .body(Body::from(validation.unwrap_err().to_string()))
            .map_err(|_| StatusCode::BAD_REQUEST);
    };

    let result = api_impl
        .as_ref()
        .delete_pet(&method, &host, &cookies, &path_params)
        .await;

    let resp = match result {
        Ok(rsp) => match rsp {
            apis::default::DeletePetResponse::Status204_PetDeleted => {
                let mut response = Response::builder();
                let mut response = response.status(204);
                response.body(Body::empty())
            }
            apis::default::DeletePetResponse::Status0_UnexpectedError(status) => {
                let mut response = Response::builder();
                let status = StatusCode::from_u16(status).map_err(|e| {
                    error!(error = ?e);
                    StatusCode::INTERNAL_SERVER_ERROR
                })?;
                let mut response = response.status(status);
                response.body(Body::empty())
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

#[tracing::instrument(skip_all)]
fn get_pet_validation(
    path_params: models::GetPetPathParams,
) -> std::result::Result<(models::GetPetPathParams,), ValidationErrors> {
    path_params.validate()?;

    Ok((path_params,))
}
/// GetPet - GET /pets/{petId}
#[tracing::instrument(skip_all)]
async fn get_pet<I, A, E>(
    method: Method,
    TypedHeader(host): TypedHeader<Host>,
    cookies: CookieJar,
    Path(path_params): Path<models::GetPetPathParams>,
    State(api_impl): State<I>,
) -> Result<Response, StatusCode>
where
    I: AsRef<A> + Send + Sync,
    A: apis::default::Default<E> + Send + Sync,
    E: std::fmt::Debug + Send + Sync + 'static,
{
    #[allow(clippy::redundant_closure)]
    let validation = tokio::task::spawn_blocking(move || get_pet_validation(path_params))
        .await
        .unwrap();

    let Ok((path_params,)) = validation else {
        return Response::builder()
            .status(StatusCode::BAD_REQUEST)
            .body(Body::from(validation.unwrap_err().to_string()))
            .map_err(|_| StatusCode::BAD_REQUEST);
    };

    let result = api_impl
        .as_ref()
        .get_pet(&method, &host, &cookies, &path_params)
        .await;

    let resp = match result {
        Ok(rsp) => match rsp {
            apis::default::GetPetResponse::Status200_ThePet(body) => {
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
            apis::default::GetPetResponse::Status0_UnexpectedError(status, body) => {
                let mut response = Response::builder();
                let status = StatusCode::from_u16(status).map_err(|e| {
                    error!(error = ?e);
                    StatusCode::INTERNAL_SERVER_ERROR
                })?;
                let mut response = response.status(status);
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

#[tracing::instrument(skip_all)]
fn list_pets_validation() -> std::result::Result<(), ValidationErrors> {
    Ok(())
}
/// ListPets - GET /pets
#[tracing::instrument(skip_all)]
async fn list_pets<I, A, E>(
    method: Method,
    TypedHeader(host): TypedHeader<Host>,
    cookies: CookieJar,
    State(api_impl): State<I>,
) -> Result<Response, StatusCode>
where
    I: AsRef<A> + Send + Sync,
    A: apis::default::Default<E> + Send + Sync,
    E: std::fmt::Debug + Send + Sync + 'static,
{
    #[allow(clippy::redundant_closure)]
    let validation = tokio::task::spawn_blocking(move || list_pets_validation())
        .await
        .unwrap();

    let Ok(()) = validation else {
        return Response::builder()
            .status(StatusCode::BAD_REQUEST)
            .body(Body::from(validation.unwrap_err().to_string()))
            .map_err(|_| StatusCode::BAD_REQUEST);
    };

    let result = api_impl.as_ref().list_pets(&method, &host, &cookies).await;

    let resp = match result {
        Ok(rsp) => match rsp {
            apis::default::ListPetsResponse::Status200_AListOfPets { body, x_rate_limit } => {
                let mut response = Response::builder();
                if let Some(x_rate_limit) = x_rate_limit {
                    let x_rate_limit = match header::IntoHeaderValue(x_rate_limit).try_into() {
                        Ok(val) => val,
                        Err(e) => {
                            return Response::builder()
                                        .status(StatusCode::INTERNAL_SERVER_ERROR)
                                        .body(Body::from(format!("An internal server error occurred handling x_rate_limit header - {e}"))).map_err(|e| { error!(error = ?e); StatusCode::INTERNAL_SERVER_ERROR });
                        }
                    };

                    let mut response_headers = response.headers_mut().unwrap();
                    response_headers.insert(HeaderName::from_static("x-rate-limit"), x_rate_limit);
                }
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
            apis::default::ListPetsResponse::Status0_UnexpectedError {
                status,
                body,
                x_rate_limit,
            } => {
                let mut response = Response::builder();
                if let Some(x_rate_limit) = x_rate_limit {
                    let x_rate_limit = match header::IntoHeaderValue(x_rate_limit).try_into() {
                        Ok(val) => val,
                        Err(e) => {
                            return Response::builder()
                                        .status(StatusCode::INTERNAL_SERVER_ERROR)
                                        .body(Body::from(format!("An internal server error occurred handling x_rate_limit header - {e}"))).map_err(|e| { error!(error = ?e); StatusCode::INTERNAL_SERVER_ERROR });
                        }
                    };

                    let mut response_headers = response.headers_mut().unwrap();
                    response_headers.insert(HeaderName::from_static("x-rate-limit"), x_rate_limit);
                }
                let status = StatusCode::from_u16(status).map_err(|e| {
                    error!(error = ?e);
                    StatusCode::INTERNAL_SERVER_ERROR
                })?;
                let mut response = response.status(status);
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
