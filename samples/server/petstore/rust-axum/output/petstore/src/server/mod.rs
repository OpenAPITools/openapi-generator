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
pub fn new<I, A, E, C>(api_impl: I) -> Router
where
    I: AsRef<A> + Clone + Send + Sync + 'static,
    A: apis::pet::Pet<E, Claims = C>
        + apis::store::Store<E, Claims = C>
        + apis::user::User<E, Claims = C>
        + apis::ApiKeyAuthHeader<Claims = C>
        + Send
        + Sync
        + 'static,
    E: std::fmt::Debug + Send + Sync + 'static,
    C: Send + Sync + 'static,
{
    // build our application with a route
    Router::new()
        .route(
            "/v2/pet",
            post(add_pet::<I, A, E, C>).put(update_pet::<I, A, E, C>),
        )
        .route(
            "/v2/pet/findByStatus",
            get(find_pets_by_status::<I, A, E, C>),
        )
        .route("/v2/pet/findByTags", get(find_pets_by_tags::<I, A, E, C>))
        .route(
            "/v2/pet/{pet_id}",
            delete(delete_pet::<I, A, E, C>)
                .get(get_pet_by_id::<I, A, E, C>)
                .post(update_pet_with_form::<I, A, E, C>),
        )
        .route(
            "/v2/pet/{pet_id}/uploadImage",
            post(upload_file::<I, A, E, C>),
        )
        .route("/v2/store/inventory", get(get_inventory::<I, A, E, C>))
        .route("/v2/store/order", post(place_order::<I, A, E, C>))
        .route(
            "/v2/store/order/{order_id}",
            delete(delete_order::<I, A, E, C>).get(get_order_by_id::<I, A, E, C>),
        )
        .route("/v2/user", post(create_user::<I, A, E, C>))
        .route(
            "/v2/user/createWithArray",
            post(create_users_with_array_input::<I, A, E, C>),
        )
        .route(
            "/v2/user/createWithList",
            post(create_users_with_list_input::<I, A, E, C>),
        )
        .route("/v2/user/login", get(login_user::<I, A, E, C>))
        .route("/v2/user/logout", get(logout_user::<I, A, E, C>))
        .route(
            "/v2/user/{username}",
            delete(delete_user::<I, A, E, C>)
                .get(get_user_by_name::<I, A, E, C>)
                .put(update_user::<I, A, E, C>),
        )
        .with_state(api_impl)
}

#[derive(validator::Validate)]
#[allow(dead_code)]
struct AddPetBodyValidator<'a> {
    #[validate(nested)]
    body: &'a models::Pet,
}

#[tracing::instrument(skip_all)]
fn add_pet_validation(body: models::Pet) -> std::result::Result<(models::Pet,), ValidationErrors> {
    let b = AddPetBodyValidator { body: &body };
    b.validate()?;

    Ok((body,))
}
/// AddPet - POST /v2/pet
#[tracing::instrument(skip_all)]
async fn add_pet<I, A, E, C>(
    method: Method,
    host: Host,
    cookies: CookieJar,
    State(api_impl): State<I>,
    Json(body): Json<models::Pet>,
) -> Result<Response, StatusCode>
where
    I: AsRef<A> + Send + Sync,
    A: apis::pet::Pet<E, Claims = C> + Send + Sync,
    E: std::fmt::Debug + Send + Sync + 'static,
{
    #[allow(clippy::redundant_closure)]
    let validation = tokio::task::spawn_blocking(move || add_pet_validation(body))
        .await
        .unwrap();

    let Ok((body,)) = validation else {
        return Response::builder()
            .status(StatusCode::BAD_REQUEST)
            .body(Body::from(validation.unwrap_err().to_string()))
            .map_err(|_| StatusCode::BAD_REQUEST);
    };

    let result = api_impl
        .as_ref()
        .add_pet(&method, &host, &cookies, &body)
        .await;

    let mut response = Response::builder();

    let resp = match result {
        Ok(rsp) => match rsp {
            apis::pet::AddPetResponse::Status200_SuccessfulOperation(body) => {
                let mut response = response.status(200);
                {
                    let mut response_headers = response.headers_mut().unwrap();
                    response_headers.insert(CONTENT_TYPE, HeaderValue::from_static("text/plain"));
                }

                let body_content = body;
                response.body(Body::from(body_content))
            }
            apis::pet::AddPetResponse::Status405_InvalidInput => {
                let mut response = response.status(405);
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
fn delete_pet_validation(
    header_params: models::DeletePetHeaderParams,
    path_params: models::DeletePetPathParams,
) -> std::result::Result<
    (models::DeletePetHeaderParams, models::DeletePetPathParams),
    ValidationErrors,
> {
    header_params.validate()?;
    path_params.validate()?;

    Ok((header_params, path_params))
}
/// DeletePet - DELETE /v2/pet/{petId}
#[tracing::instrument(skip_all)]
async fn delete_pet<I, A, E, C>(
    method: Method,
    host: Host,
    cookies: CookieJar,
    headers: HeaderMap,
    Path(path_params): Path<models::DeletePetPathParams>,
    State(api_impl): State<I>,
) -> Result<Response, StatusCode>
where
    I: AsRef<A> + Send + Sync,
    A: apis::pet::Pet<E, Claims = C> + Send + Sync,
    E: std::fmt::Debug + Send + Sync + 'static,
{
    // Header parameters
    let header_params = {
        let header_api_key = headers.get(HeaderName::from_static("api_key"));

        let header_api_key = match header_api_key {
            Some(v) => match header::IntoHeaderValue::<String>::try_from((*v).clone()) {
                Ok(result) => Some(result.0),
                Err(err) => {
                    return Response::builder()
                        .status(StatusCode::BAD_REQUEST)
                        .body(Body::from(format!("Invalid header api_key - {err}")))
                        .map_err(|e| {
                            error!(error = ?e);
                            StatusCode::INTERNAL_SERVER_ERROR
                        });
                }
            },
            None => None,
        };

        models::DeletePetHeaderParams {
            api_key: header_api_key,
        }
    };

    #[allow(clippy::redundant_closure)]
    let validation =
        tokio::task::spawn_blocking(move || delete_pet_validation(header_params, path_params))
            .await
            .unwrap();

    let Ok((header_params, path_params)) = validation else {
        return Response::builder()
            .status(StatusCode::BAD_REQUEST)
            .body(Body::from(validation.unwrap_err().to_string()))
            .map_err(|_| StatusCode::BAD_REQUEST);
    };

    let result = api_impl
        .as_ref()
        .delete_pet(&method, &host, &cookies, &header_params, &path_params)
        .await;

    let mut response = Response::builder();

    let resp = match result {
        Ok(rsp) => match rsp {
            apis::pet::DeletePetResponse::Status400_InvalidPetValue => {
                let mut response = response.status(400);
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
fn find_pets_by_status_validation(
    query_params: models::FindPetsByStatusQueryParams,
) -> std::result::Result<(models::FindPetsByStatusQueryParams,), ValidationErrors> {
    query_params.validate()?;

    Ok((query_params,))
}
/// FindPetsByStatus - GET /v2/pet/findByStatus
#[tracing::instrument(skip_all)]
async fn find_pets_by_status<I, A, E, C>(
    method: Method,
    host: Host,
    cookies: CookieJar,
    QueryExtra(query_params): QueryExtra<models::FindPetsByStatusQueryParams>,
    State(api_impl): State<I>,
) -> Result<Response, StatusCode>
where
    I: AsRef<A> + Send + Sync,
    A: apis::pet::Pet<E, Claims = C> + Send + Sync,
    E: std::fmt::Debug + Send + Sync + 'static,
{
    #[allow(clippy::redundant_closure)]
    let validation =
        tokio::task::spawn_blocking(move || find_pets_by_status_validation(query_params))
            .await
            .unwrap();

    let Ok((query_params,)) = validation else {
        return Response::builder()
            .status(StatusCode::BAD_REQUEST)
            .body(Body::from(validation.unwrap_err().to_string()))
            .map_err(|_| StatusCode::BAD_REQUEST);
    };

    let result = api_impl
        .as_ref()
        .find_pets_by_status(&method, &host, &cookies, &query_params)
        .await;

    let mut response = Response::builder();

    let resp = match result {
        Ok(rsp) => match rsp {
            apis::pet::FindPetsByStatusResponse::Status200_SuccessfulOperation(body) => {
                let mut response = response.status(200);
                {
                    let mut response_headers = response.headers_mut().unwrap();
                    response_headers.insert(CONTENT_TYPE, HeaderValue::from_static("text/plain"));
                }

                let body_content = body;
                response.body(Body::from(body_content))
            }
            apis::pet::FindPetsByStatusResponse::Status400_InvalidStatusValue => {
                let mut response = response.status(400);
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
fn find_pets_by_tags_validation(
    query_params: models::FindPetsByTagsQueryParams,
) -> std::result::Result<(models::FindPetsByTagsQueryParams,), ValidationErrors> {
    query_params.validate()?;

    Ok((query_params,))
}
/// FindPetsByTags - GET /v2/pet/findByTags
#[tracing::instrument(skip_all)]
async fn find_pets_by_tags<I, A, E, C>(
    method: Method,
    host: Host,
    cookies: CookieJar,
    QueryExtra(query_params): QueryExtra<models::FindPetsByTagsQueryParams>,
    State(api_impl): State<I>,
) -> Result<Response, StatusCode>
where
    I: AsRef<A> + Send + Sync,
    A: apis::pet::Pet<E, Claims = C> + Send + Sync,
    E: std::fmt::Debug + Send + Sync + 'static,
{
    #[allow(clippy::redundant_closure)]
    let validation =
        tokio::task::spawn_blocking(move || find_pets_by_tags_validation(query_params))
            .await
            .unwrap();

    let Ok((query_params,)) = validation else {
        return Response::builder()
            .status(StatusCode::BAD_REQUEST)
            .body(Body::from(validation.unwrap_err().to_string()))
            .map_err(|_| StatusCode::BAD_REQUEST);
    };

    let result = api_impl
        .as_ref()
        .find_pets_by_tags(&method, &host, &cookies, &query_params)
        .await;

    let mut response = Response::builder();

    let resp = match result {
        Ok(rsp) => match rsp {
            apis::pet::FindPetsByTagsResponse::Status200_SuccessfulOperation(body) => {
                let mut response = response.status(200);
                {
                    let mut response_headers = response.headers_mut().unwrap();
                    response_headers.insert(CONTENT_TYPE, HeaderValue::from_static("text/plain"));
                }

                let body_content = body;
                response.body(Body::from(body_content))
            }
            apis::pet::FindPetsByTagsResponse::Status400_InvalidTagValue => {
                let mut response = response.status(400);
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
fn get_pet_by_id_validation(
    path_params: models::GetPetByIdPathParams,
) -> std::result::Result<(models::GetPetByIdPathParams,), ValidationErrors> {
    path_params.validate()?;

    Ok((path_params,))
}
/// GetPetById - GET /v2/pet/{petId}
#[tracing::instrument(skip_all)]
async fn get_pet_by_id<I, A, E, C>(
    method: Method,
    host: Host,
    cookies: CookieJar,
    headers: HeaderMap,
    Path(path_params): Path<models::GetPetByIdPathParams>,
    State(api_impl): State<I>,
) -> Result<Response, StatusCode>
where
    I: AsRef<A> + Send + Sync,
    A: apis::pet::Pet<E, Claims = C> + apis::ApiKeyAuthHeader<Claims = C> + Send + Sync,
    E: std::fmt::Debug + Send + Sync + 'static,
{
    // Authentication
    let claims_in_header = api_impl
        .as_ref()
        .extract_claims_from_header(&headers, "api_key")
        .await;
    let claims = None.or(claims_in_header);
    let Some(claims) = claims else {
        return response_with_status_code_only(StatusCode::UNAUTHORIZED);
    };

    #[allow(clippy::redundant_closure)]
    let validation = tokio::task::spawn_blocking(move || get_pet_by_id_validation(path_params))
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
        .get_pet_by_id(&method, &host, &cookies, &claims, &path_params)
        .await;

    let mut response = Response::builder();

    let resp = match result {
        Ok(rsp) => match rsp {
            apis::pet::GetPetByIdResponse::Status200_SuccessfulOperation(body) => {
                let mut response = response.status(200);
                {
                    let mut response_headers = response.headers_mut().unwrap();
                    response_headers.insert(CONTENT_TYPE, HeaderValue::from_static("text/plain"));
                }

                let body_content = body;
                response.body(Body::from(body_content))
            }
            apis::pet::GetPetByIdResponse::Status400_InvalidIDSupplied => {
                let mut response = response.status(400);
                response.body(Body::empty())
            }
            apis::pet::GetPetByIdResponse::Status404_PetNotFound => {
                let mut response = response.status(404);
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

#[derive(validator::Validate)]
#[allow(dead_code)]
struct UpdatePetBodyValidator<'a> {
    #[validate(nested)]
    body: &'a models::Pet,
}

#[tracing::instrument(skip_all)]
fn update_pet_validation(
    body: models::Pet,
) -> std::result::Result<(models::Pet,), ValidationErrors> {
    let b = UpdatePetBodyValidator { body: &body };
    b.validate()?;

    Ok((body,))
}
/// UpdatePet - PUT /v2/pet
#[tracing::instrument(skip_all)]
async fn update_pet<I, A, E, C>(
    method: Method,
    host: Host,
    cookies: CookieJar,
    State(api_impl): State<I>,
    Json(body): Json<models::Pet>,
) -> Result<Response, StatusCode>
where
    I: AsRef<A> + Send + Sync,
    A: apis::pet::Pet<E, Claims = C> + Send + Sync,
    E: std::fmt::Debug + Send + Sync + 'static,
{
    #[allow(clippy::redundant_closure)]
    let validation = tokio::task::spawn_blocking(move || update_pet_validation(body))
        .await
        .unwrap();

    let Ok((body,)) = validation else {
        return Response::builder()
            .status(StatusCode::BAD_REQUEST)
            .body(Body::from(validation.unwrap_err().to_string()))
            .map_err(|_| StatusCode::BAD_REQUEST);
    };

    let result = api_impl
        .as_ref()
        .update_pet(&method, &host, &cookies, &body)
        .await;

    let mut response = Response::builder();

    let resp = match result {
        Ok(rsp) => match rsp {
            apis::pet::UpdatePetResponse::Status200_SuccessfulOperation(body) => {
                let mut response = response.status(200);
                {
                    let mut response_headers = response.headers_mut().unwrap();
                    response_headers.insert(CONTENT_TYPE, HeaderValue::from_static("text/plain"));
                }

                let body_content = body;
                response.body(Body::from(body_content))
            }
            apis::pet::UpdatePetResponse::Status400_InvalidIDSupplied => {
                let mut response = response.status(400);
                response.body(Body::empty())
            }
            apis::pet::UpdatePetResponse::Status404_PetNotFound => {
                let mut response = response.status(404);
                response.body(Body::empty())
            }
            apis::pet::UpdatePetResponse::Status405_ValidationException => {
                let mut response = response.status(405);
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

#[derive(validator::Validate)]
#[allow(dead_code)]
struct UpdatePetWithFormBodyValidator<'a> {
    #[validate(nested)]
    body: &'a models::UpdatePetWithFormRequest,
}

#[tracing::instrument(skip_all)]
fn update_pet_with_form_validation(
    path_params: models::UpdatePetWithFormPathParams,
    body: Option<models::UpdatePetWithFormRequest>,
) -> std::result::Result<
    (
        models::UpdatePetWithFormPathParams,
        Option<models::UpdatePetWithFormRequest>,
    ),
    ValidationErrors,
> {
    path_params.validate()?;
    if let Some(body) = &body {
        let b = UpdatePetWithFormBodyValidator { body };
        b.validate()?;
    }

    Ok((path_params, body))
}
/// UpdatePetWithForm - POST /v2/pet/{petId}
#[tracing::instrument(skip_all)]
async fn update_pet_with_form<I, A, E, C>(
    method: Method,
    host: Host,
    cookies: CookieJar,
    Path(path_params): Path<models::UpdatePetWithFormPathParams>,
    State(api_impl): State<I>,
    Form(body): Form<Option<models::UpdatePetWithFormRequest>>,
) -> Result<Response, StatusCode>
where
    I: AsRef<A> + Send + Sync,
    A: apis::pet::Pet<E, Claims = C> + Send + Sync,
    E: std::fmt::Debug + Send + Sync + 'static,
{
    #[allow(clippy::redundant_closure)]
    let validation =
        tokio::task::spawn_blocking(move || update_pet_with_form_validation(path_params, body))
            .await
            .unwrap();

    let Ok((path_params, body)) = validation else {
        return Response::builder()
            .status(StatusCode::BAD_REQUEST)
            .body(Body::from(validation.unwrap_err().to_string()))
            .map_err(|_| StatusCode::BAD_REQUEST);
    };

    let result = api_impl
        .as_ref()
        .update_pet_with_form(&method, &host, &cookies, &path_params, &body)
        .await;

    let mut response = Response::builder();

    let resp = match result {
        Ok(rsp) => match rsp {
            apis::pet::UpdatePetWithFormResponse::Status405_InvalidInput => {
                let mut response = response.status(405);
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
fn upload_file_validation(
    path_params: models::UploadFilePathParams,
) -> std::result::Result<(models::UploadFilePathParams,), ValidationErrors> {
    path_params.validate()?;

    Ok((path_params,))
}
/// UploadFile - POST /v2/pet/{petId}/uploadImage
#[tracing::instrument(skip_all)]
async fn upload_file<I, A, E, C>(
    method: Method,
    host: Host,
    cookies: CookieJar,
    Path(path_params): Path<models::UploadFilePathParams>,
    State(api_impl): State<I>,
    body: Multipart,
) -> Result<Response, StatusCode>
where
    I: AsRef<A> + Send + Sync,
    A: apis::pet::Pet<E, Claims = C> + Send + Sync,
    E: std::fmt::Debug + Send + Sync + 'static,
{
    #[allow(clippy::redundant_closure)]
    let validation = tokio::task::spawn_blocking(move || upload_file_validation(path_params))
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
        .upload_file(&method, &host, &cookies, &path_params, body)
        .await;

    let mut response = Response::builder();

    let resp = match result {
        Ok(rsp) => match rsp {
            apis::pet::UploadFileResponse::Status200_SuccessfulOperation(body) => {
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

#[tracing::instrument(skip_all)]
fn delete_order_validation(
    path_params: models::DeleteOrderPathParams,
) -> std::result::Result<(models::DeleteOrderPathParams,), ValidationErrors> {
    path_params.validate()?;

    Ok((path_params,))
}
/// DeleteOrder - DELETE /v2/store/order/{orderId}
#[tracing::instrument(skip_all)]
async fn delete_order<I, A, E, C>(
    method: Method,
    host: Host,
    cookies: CookieJar,
    Path(path_params): Path<models::DeleteOrderPathParams>,
    State(api_impl): State<I>,
) -> Result<Response, StatusCode>
where
    I: AsRef<A> + Send + Sync,
    A: apis::store::Store<E, Claims = C> + Send + Sync,
    E: std::fmt::Debug + Send + Sync + 'static,
{
    #[allow(clippy::redundant_closure)]
    let validation = tokio::task::spawn_blocking(move || delete_order_validation(path_params))
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
        .delete_order(&method, &host, &cookies, &path_params)
        .await;

    let mut response = Response::builder();

    let resp = match result {
        Ok(rsp) => match rsp {
            apis::store::DeleteOrderResponse::Status400_InvalidIDSupplied => {
                let mut response = response.status(400);
                response.body(Body::empty())
            }
            apis::store::DeleteOrderResponse::Status404_OrderNotFound => {
                let mut response = response.status(404);
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
fn get_inventory_validation() -> std::result::Result<(), ValidationErrors> {
    Ok(())
}
/// GetInventory - GET /v2/store/inventory
#[tracing::instrument(skip_all)]
async fn get_inventory<I, A, E, C>(
    method: Method,
    host: Host,
    cookies: CookieJar,
    headers: HeaderMap,
    State(api_impl): State<I>,
) -> Result<Response, StatusCode>
where
    I: AsRef<A> + Send + Sync,
    A: apis::store::Store<E, Claims = C> + apis::ApiKeyAuthHeader<Claims = C> + Send + Sync,
    E: std::fmt::Debug + Send + Sync + 'static,
{
    // Authentication
    let claims_in_header = api_impl
        .as_ref()
        .extract_claims_from_header(&headers, "api_key")
        .await;
    let claims = None.or(claims_in_header);
    let Some(claims) = claims else {
        return response_with_status_code_only(StatusCode::UNAUTHORIZED);
    };

    #[allow(clippy::redundant_closure)]
    let validation = tokio::task::spawn_blocking(move || get_inventory_validation())
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
        .get_inventory(&method, &host, &cookies, &claims)
        .await;

    let mut response = Response::builder();

    let resp = match result {
        Ok(rsp) => match rsp {
            apis::store::GetInventoryResponse::Status200_SuccessfulOperation(body) => {
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

#[tracing::instrument(skip_all)]
fn get_order_by_id_validation(
    path_params: models::GetOrderByIdPathParams,
) -> std::result::Result<(models::GetOrderByIdPathParams,), ValidationErrors> {
    path_params.validate()?;

    Ok((path_params,))
}
/// GetOrderById - GET /v2/store/order/{orderId}
#[tracing::instrument(skip_all)]
async fn get_order_by_id<I, A, E, C>(
    method: Method,
    host: Host,
    cookies: CookieJar,
    Path(path_params): Path<models::GetOrderByIdPathParams>,
    State(api_impl): State<I>,
) -> Result<Response, StatusCode>
where
    I: AsRef<A> + Send + Sync,
    A: apis::store::Store<E, Claims = C> + Send + Sync,
    E: std::fmt::Debug + Send + Sync + 'static,
{
    #[allow(clippy::redundant_closure)]
    let validation = tokio::task::spawn_blocking(move || get_order_by_id_validation(path_params))
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
        .get_order_by_id(&method, &host, &cookies, &path_params)
        .await;

    let mut response = Response::builder();

    let resp = match result {
        Ok(rsp) => match rsp {
            apis::store::GetOrderByIdResponse::Status200_SuccessfulOperation(body) => {
                let mut response = response.status(200);
                {
                    let mut response_headers = response.headers_mut().unwrap();
                    response_headers.insert(CONTENT_TYPE, HeaderValue::from_static("text/plain"));
                }

                let body_content = body;
                response.body(Body::from(body_content))
            }
            apis::store::GetOrderByIdResponse::Status400_InvalidIDSupplied => {
                let mut response = response.status(400);
                response.body(Body::empty())
            }
            apis::store::GetOrderByIdResponse::Status404_OrderNotFound => {
                let mut response = response.status(404);
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

#[derive(validator::Validate)]
#[allow(dead_code)]
struct PlaceOrderBodyValidator<'a> {
    #[validate(nested)]
    body: &'a models::Order,
}

#[tracing::instrument(skip_all)]
fn place_order_validation(
    body: models::Order,
) -> std::result::Result<(models::Order,), ValidationErrors> {
    let b = PlaceOrderBodyValidator { body: &body };
    b.validate()?;

    Ok((body,))
}
/// PlaceOrder - POST /v2/store/order
#[tracing::instrument(skip_all)]
async fn place_order<I, A, E, C>(
    method: Method,
    host: Host,
    cookies: CookieJar,
    State(api_impl): State<I>,
    Json(body): Json<models::Order>,
) -> Result<Response, StatusCode>
where
    I: AsRef<A> + Send + Sync,
    A: apis::store::Store<E, Claims = C> + Send + Sync,
    E: std::fmt::Debug + Send + Sync + 'static,
{
    #[allow(clippy::redundant_closure)]
    let validation = tokio::task::spawn_blocking(move || place_order_validation(body))
        .await
        .unwrap();

    let Ok((body,)) = validation else {
        return Response::builder()
            .status(StatusCode::BAD_REQUEST)
            .body(Body::from(validation.unwrap_err().to_string()))
            .map_err(|_| StatusCode::BAD_REQUEST);
    };

    let result = api_impl
        .as_ref()
        .place_order(&method, &host, &cookies, &body)
        .await;

    let mut response = Response::builder();

    let resp = match result {
        Ok(rsp) => match rsp {
            apis::store::PlaceOrderResponse::Status200_SuccessfulOperation(body) => {
                let mut response = response.status(200);
                {
                    let mut response_headers = response.headers_mut().unwrap();
                    response_headers.insert(CONTENT_TYPE, HeaderValue::from_static("text/plain"));
                }

                let body_content = body;
                response.body(Body::from(body_content))
            }
            apis::store::PlaceOrderResponse::Status400_InvalidOrder => {
                let mut response = response.status(400);
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

#[derive(validator::Validate)]
#[allow(dead_code)]
struct CreateUserBodyValidator<'a> {
    #[validate(nested)]
    body: &'a models::User,
}

#[tracing::instrument(skip_all)]
fn create_user_validation(
    body: models::User,
) -> std::result::Result<(models::User,), ValidationErrors> {
    let b = CreateUserBodyValidator { body: &body };
    b.validate()?;

    Ok((body,))
}
/// CreateUser - POST /v2/user
#[tracing::instrument(skip_all)]
async fn create_user<I, A, E, C>(
    method: Method,
    host: Host,
    cookies: CookieJar,
    headers: HeaderMap,
    State(api_impl): State<I>,
    Json(body): Json<models::User>,
) -> Result<Response, StatusCode>
where
    I: AsRef<A> + Send + Sync,
    A: apis::user::User<E, Claims = C> + apis::ApiKeyAuthHeader<Claims = C> + Send + Sync,
    E: std::fmt::Debug + Send + Sync + 'static,
{
    // Authentication
    let claims_in_header = api_impl
        .as_ref()
        .extract_claims_from_header(&headers, "api_key")
        .await;
    let claims = None.or(claims_in_header);
    let Some(claims) = claims else {
        return response_with_status_code_only(StatusCode::UNAUTHORIZED);
    };

    #[allow(clippy::redundant_closure)]
    let validation = tokio::task::spawn_blocking(move || create_user_validation(body))
        .await
        .unwrap();

    let Ok((body,)) = validation else {
        return Response::builder()
            .status(StatusCode::BAD_REQUEST)
            .body(Body::from(validation.unwrap_err().to_string()))
            .map_err(|_| StatusCode::BAD_REQUEST);
    };

    let result = api_impl
        .as_ref()
        .create_user(&method, &host, &cookies, &claims, &body)
        .await;

    let mut response = Response::builder();

    let resp = match result {
        Ok(rsp) => match rsp {
            apis::user::CreateUserResponse::Status0_SuccessfulOperation => {
                let mut response = response.status(0);
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

#[derive(validator::Validate)]
#[allow(dead_code)]
struct CreateUsersWithArrayInputBodyValidator<'a> {
    #[validate(nested)]
    body: &'a Vec<models::User>,
}

#[tracing::instrument(skip_all)]
fn create_users_with_array_input_validation(
    body: Vec<models::User>,
) -> std::result::Result<(Vec<models::User>,), ValidationErrors> {
    let b = CreateUsersWithArrayInputBodyValidator { body: &body };
    b.validate()?;

    Ok((body,))
}
/// CreateUsersWithArrayInput - POST /v2/user/createWithArray
#[tracing::instrument(skip_all)]
async fn create_users_with_array_input<I, A, E, C>(
    method: Method,
    host: Host,
    cookies: CookieJar,
    headers: HeaderMap,
    State(api_impl): State<I>,
    Json(body): Json<Vec<models::User>>,
) -> Result<Response, StatusCode>
where
    I: AsRef<A> + Send + Sync,
    A: apis::user::User<E, Claims = C> + apis::ApiKeyAuthHeader<Claims = C> + Send + Sync,
    E: std::fmt::Debug + Send + Sync + 'static,
{
    // Authentication
    let claims_in_header = api_impl
        .as_ref()
        .extract_claims_from_header(&headers, "api_key")
        .await;
    let claims = None.or(claims_in_header);
    let Some(claims) = claims else {
        return response_with_status_code_only(StatusCode::UNAUTHORIZED);
    };

    #[allow(clippy::redundant_closure)]
    let validation =
        tokio::task::spawn_blocking(move || create_users_with_array_input_validation(body))
            .await
            .unwrap();

    let Ok((body,)) = validation else {
        return Response::builder()
            .status(StatusCode::BAD_REQUEST)
            .body(Body::from(validation.unwrap_err().to_string()))
            .map_err(|_| StatusCode::BAD_REQUEST);
    };

    let result = api_impl
        .as_ref()
        .create_users_with_array_input(&method, &host, &cookies, &claims, &body)
        .await;

    let mut response = Response::builder();

    let resp = match result {
        Ok(rsp) => match rsp {
            apis::user::CreateUsersWithArrayInputResponse::Status0_SuccessfulOperation => {
                let mut response = response.status(0);
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

#[derive(validator::Validate)]
#[allow(dead_code)]
struct CreateUsersWithListInputBodyValidator<'a> {
    #[validate(nested)]
    body: &'a Vec<models::User>,
}

#[tracing::instrument(skip_all)]
fn create_users_with_list_input_validation(
    body: Vec<models::User>,
) -> std::result::Result<(Vec<models::User>,), ValidationErrors> {
    let b = CreateUsersWithListInputBodyValidator { body: &body };
    b.validate()?;

    Ok((body,))
}
/// CreateUsersWithListInput - POST /v2/user/createWithList
#[tracing::instrument(skip_all)]
async fn create_users_with_list_input<I, A, E, C>(
    method: Method,
    host: Host,
    cookies: CookieJar,
    headers: HeaderMap,
    State(api_impl): State<I>,
    Json(body): Json<Vec<models::User>>,
) -> Result<Response, StatusCode>
where
    I: AsRef<A> + Send + Sync,
    A: apis::user::User<E, Claims = C> + apis::ApiKeyAuthHeader<Claims = C> + Send + Sync,
    E: std::fmt::Debug + Send + Sync + 'static,
{
    // Authentication
    let claims_in_header = api_impl
        .as_ref()
        .extract_claims_from_header(&headers, "api_key")
        .await;
    let claims = None.or(claims_in_header);
    let Some(claims) = claims else {
        return response_with_status_code_only(StatusCode::UNAUTHORIZED);
    };

    #[allow(clippy::redundant_closure)]
    let validation =
        tokio::task::spawn_blocking(move || create_users_with_list_input_validation(body))
            .await
            .unwrap();

    let Ok((body,)) = validation else {
        return Response::builder()
            .status(StatusCode::BAD_REQUEST)
            .body(Body::from(validation.unwrap_err().to_string()))
            .map_err(|_| StatusCode::BAD_REQUEST);
    };

    let result = api_impl
        .as_ref()
        .create_users_with_list_input(&method, &host, &cookies, &claims, &body)
        .await;

    let mut response = Response::builder();

    let resp = match result {
        Ok(rsp) => match rsp {
            apis::user::CreateUsersWithListInputResponse::Status0_SuccessfulOperation => {
                let mut response = response.status(0);
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
fn delete_user_validation(
    path_params: models::DeleteUserPathParams,
) -> std::result::Result<(models::DeleteUserPathParams,), ValidationErrors> {
    path_params.validate()?;

    Ok((path_params,))
}
/// DeleteUser - DELETE /v2/user/{username}
#[tracing::instrument(skip_all)]
async fn delete_user<I, A, E, C>(
    method: Method,
    host: Host,
    cookies: CookieJar,
    headers: HeaderMap,
    Path(path_params): Path<models::DeleteUserPathParams>,
    State(api_impl): State<I>,
) -> Result<Response, StatusCode>
where
    I: AsRef<A> + Send + Sync,
    A: apis::user::User<E, Claims = C> + apis::ApiKeyAuthHeader<Claims = C> + Send + Sync,
    E: std::fmt::Debug + Send + Sync + 'static,
{
    // Authentication
    let claims_in_header = api_impl
        .as_ref()
        .extract_claims_from_header(&headers, "api_key")
        .await;
    let claims = None.or(claims_in_header);
    let Some(claims) = claims else {
        return response_with_status_code_only(StatusCode::UNAUTHORIZED);
    };

    #[allow(clippy::redundant_closure)]
    let validation = tokio::task::spawn_blocking(move || delete_user_validation(path_params))
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
        .delete_user(&method, &host, &cookies, &claims, &path_params)
        .await;

    let mut response = Response::builder();

    let resp = match result {
        Ok(rsp) => match rsp {
            apis::user::DeleteUserResponse::Status400_InvalidUsernameSupplied => {
                let mut response = response.status(400);
                response.body(Body::empty())
            }
            apis::user::DeleteUserResponse::Status404_UserNotFound => {
                let mut response = response.status(404);
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
fn get_user_by_name_validation(
    path_params: models::GetUserByNamePathParams,
) -> std::result::Result<(models::GetUserByNamePathParams,), ValidationErrors> {
    path_params.validate()?;

    Ok((path_params,))
}
/// GetUserByName - GET /v2/user/{username}
#[tracing::instrument(skip_all)]
async fn get_user_by_name<I, A, E, C>(
    method: Method,
    host: Host,
    cookies: CookieJar,
    Path(path_params): Path<models::GetUserByNamePathParams>,
    State(api_impl): State<I>,
) -> Result<Response, StatusCode>
where
    I: AsRef<A> + Send + Sync,
    A: apis::user::User<E, Claims = C> + Send + Sync,
    E: std::fmt::Debug + Send + Sync + 'static,
{
    #[allow(clippy::redundant_closure)]
    let validation = tokio::task::spawn_blocking(move || get_user_by_name_validation(path_params))
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
        .get_user_by_name(&method, &host, &cookies, &path_params)
        .await;

    let mut response = Response::builder();

    let resp = match result {
        Ok(rsp) => match rsp {
            apis::user::GetUserByNameResponse::Status200_SuccessfulOperation(body) => {
                let mut response = response.status(200);
                {
                    let mut response_headers = response.headers_mut().unwrap();
                    response_headers.insert(CONTENT_TYPE, HeaderValue::from_static("text/plain"));
                }

                let body_content = body;
                response.body(Body::from(body_content))
            }
            apis::user::GetUserByNameResponse::Status400_InvalidUsernameSupplied => {
                let mut response = response.status(400);
                response.body(Body::empty())
            }
            apis::user::GetUserByNameResponse::Status404_UserNotFound => {
                let mut response = response.status(404);
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
fn login_user_validation(
    query_params: models::LoginUserQueryParams,
) -> std::result::Result<(models::LoginUserQueryParams,), ValidationErrors> {
    query_params.validate()?;

    Ok((query_params,))
}
/// LoginUser - GET /v2/user/login
#[tracing::instrument(skip_all)]
async fn login_user<I, A, E, C>(
    method: Method,
    host: Host,
    cookies: CookieJar,
    QueryExtra(query_params): QueryExtra<models::LoginUserQueryParams>,
    State(api_impl): State<I>,
) -> Result<Response, StatusCode>
where
    I: AsRef<A> + Send + Sync,
    A: apis::user::User<E, Claims = C> + Send + Sync,
    E: std::fmt::Debug + Send + Sync + 'static,
{
    #[allow(clippy::redundant_closure)]
    let validation = tokio::task::spawn_blocking(move || login_user_validation(query_params))
        .await
        .unwrap();

    let Ok((query_params,)) = validation else {
        return Response::builder()
            .status(StatusCode::BAD_REQUEST)
            .body(Body::from(validation.unwrap_err().to_string()))
            .map_err(|_| StatusCode::BAD_REQUEST);
    };

    let result = api_impl
        .as_ref()
        .login_user(&method, &host, &cookies, &query_params)
        .await;

    let mut response = Response::builder();

    let resp = match result {
        Ok(rsp) => match rsp {
            apis::user::LoginUserResponse::Status200_SuccessfulOperation {
                body,
                set_cookie,
                x_rate_limit,
                x_expires_after,
            } => {
                if let Some(set_cookie) = set_cookie {
                    let set_cookie = match header::IntoHeaderValue(set_cookie).try_into() {
                        Ok(val) => val,
                        Err(e) => {
                            return Response::builder()
                                                                    .status(StatusCode::INTERNAL_SERVER_ERROR)
                                                                    .body(Body::from(format!("An internal server error occurred handling set_cookie header - {e}"))).map_err(|e| { error!(error = ?e); StatusCode::INTERNAL_SERVER_ERROR });
                        }
                    };

                    {
                        let mut response_headers = response.headers_mut().unwrap();
                        response_headers.insert(HeaderName::from_static("set-cookie"), set_cookie);
                    }
                }
                if let Some(x_rate_limit) = x_rate_limit {
                    let x_rate_limit = match header::IntoHeaderValue(x_rate_limit).try_into() {
                        Ok(val) => val,
                        Err(e) => {
                            return Response::builder()
                                                                    .status(StatusCode::INTERNAL_SERVER_ERROR)
                                                                    .body(Body::from(format!("An internal server error occurred handling x_rate_limit header - {e}"))).map_err(|e| { error!(error = ?e); StatusCode::INTERNAL_SERVER_ERROR });
                        }
                    };

                    {
                        let mut response_headers = response.headers_mut().unwrap();
                        response_headers
                            .insert(HeaderName::from_static("x-rate-limit"), x_rate_limit);
                    }
                }
                if let Some(x_expires_after) = x_expires_after {
                    let x_expires_after = match header::IntoHeaderValue(x_expires_after).try_into()
                    {
                        Ok(val) => val,
                        Err(e) => {
                            return Response::builder()
                                                                    .status(StatusCode::INTERNAL_SERVER_ERROR)
                                                                    .body(Body::from(format!("An internal server error occurred handling x_expires_after header - {e}"))).map_err(|e| { error!(error = ?e); StatusCode::INTERNAL_SERVER_ERROR });
                        }
                    };

                    {
                        let mut response_headers = response.headers_mut().unwrap();
                        response_headers
                            .insert(HeaderName::from_static("x-expires-after"), x_expires_after);
                    }
                }
                let mut response = response.status(200);
                {
                    let mut response_headers = response.headers_mut().unwrap();
                    response_headers.insert(CONTENT_TYPE, HeaderValue::from_static("text/plain"));
                }

                let body_content = body;
                response.body(Body::from(body_content))
            }
            apis::user::LoginUserResponse::Status400_InvalidUsername => {
                let mut response = response.status(400);
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
fn logout_user_validation() -> std::result::Result<(), ValidationErrors> {
    Ok(())
}
/// LogoutUser - GET /v2/user/logout
#[tracing::instrument(skip_all)]
async fn logout_user<I, A, E, C>(
    method: Method,
    host: Host,
    cookies: CookieJar,
    headers: HeaderMap,
    State(api_impl): State<I>,
) -> Result<Response, StatusCode>
where
    I: AsRef<A> + Send + Sync,
    A: apis::user::User<E, Claims = C> + apis::ApiKeyAuthHeader<Claims = C> + Send + Sync,
    E: std::fmt::Debug + Send + Sync + 'static,
{
    // Authentication
    let claims_in_header = api_impl
        .as_ref()
        .extract_claims_from_header(&headers, "api_key")
        .await;
    let claims = None.or(claims_in_header);
    let Some(claims) = claims else {
        return response_with_status_code_only(StatusCode::UNAUTHORIZED);
    };

    #[allow(clippy::redundant_closure)]
    let validation = tokio::task::spawn_blocking(move || logout_user_validation())
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
        .logout_user(&method, &host, &cookies, &claims)
        .await;

    let mut response = Response::builder();

    let resp = match result {
        Ok(rsp) => match rsp {
            apis::user::LogoutUserResponse::Status0_SuccessfulOperation => {
                let mut response = response.status(0);
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

#[derive(validator::Validate)]
#[allow(dead_code)]
struct UpdateUserBodyValidator<'a> {
    #[validate(nested)]
    body: &'a models::User,
}

#[tracing::instrument(skip_all)]
fn update_user_validation(
    path_params: models::UpdateUserPathParams,
    body: models::User,
) -> std::result::Result<(models::UpdateUserPathParams, models::User), ValidationErrors> {
    path_params.validate()?;
    let b = UpdateUserBodyValidator { body: &body };
    b.validate()?;

    Ok((path_params, body))
}
/// UpdateUser - PUT /v2/user/{username}
#[tracing::instrument(skip_all)]
async fn update_user<I, A, E, C>(
    method: Method,
    host: Host,
    cookies: CookieJar,
    headers: HeaderMap,
    Path(path_params): Path<models::UpdateUserPathParams>,
    State(api_impl): State<I>,
    Json(body): Json<models::User>,
) -> Result<Response, StatusCode>
where
    I: AsRef<A> + Send + Sync,
    A: apis::user::User<E, Claims = C> + apis::ApiKeyAuthHeader<Claims = C> + Send + Sync,
    E: std::fmt::Debug + Send + Sync + 'static,
{
    // Authentication
    let claims_in_header = api_impl
        .as_ref()
        .extract_claims_from_header(&headers, "api_key")
        .await;
    let claims = None.or(claims_in_header);
    let Some(claims) = claims else {
        return response_with_status_code_only(StatusCode::UNAUTHORIZED);
    };

    #[allow(clippy::redundant_closure)]
    let validation = tokio::task::spawn_blocking(move || update_user_validation(path_params, body))
        .await
        .unwrap();

    let Ok((path_params, body)) = validation else {
        return Response::builder()
            .status(StatusCode::BAD_REQUEST)
            .body(Body::from(validation.unwrap_err().to_string()))
            .map_err(|_| StatusCode::BAD_REQUEST);
    };

    let result = api_impl
        .as_ref()
        .update_user(&method, &host, &cookies, &claims, &path_params, &body)
        .await;

    let mut response = Response::builder();

    let resp = match result {
        Ok(rsp) => match rsp {
            apis::user::UpdateUserResponse::Status400_InvalidUserSupplied => {
                let mut response = response.status(400);
                response.body(Body::empty())
            }
            apis::user::UpdateUserResponse::Status404_UserNotFound => {
                let mut response = response.status(404);
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

#[allow(dead_code)]
#[inline]
fn response_with_status_code_only(code: StatusCode) -> Result<Response, StatusCode> {
    Response::builder()
        .status(code)
        .body(Body::empty())
        .map_err(|_| code)
}
