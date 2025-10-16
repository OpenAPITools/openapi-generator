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
    A: apis::another_fake::AnotherFake<E>
        + apis::fake::Fake<E, Claims = C>
        + apis::fake_classname_tags123::FakeClassnameTags123<E>
        + apis::pet::Pet<E, Claims = C>
        + apis::store::Store<E, Claims = C>
        + apis::user::User<E>
        + apis::ApiKeyAuthHeader<Claims = C>
        + apis::ApiAuthBasic<Claims = C>
        + Send
        + Sync
        + 'static,
    E: std::fmt::Debug + Send + Sync + 'static,
    C: Send + Sync + 'static,
{
    // build our application with a route
    Router::new()
        .route(
            "/v2/another-fake/dummy",
            patch(test_special_tags::<I, A, E>),
        )
        .route(
            "/v2/fake",
            get(test_enum_parameters::<I, A, E, C>)
                .patch(test_client_model::<I, A, E, C>)
                .post(test_endpoint_parameters::<I, A, E, C>),
        )
        .route(
            "/v2/fake/body-with-query-params",
            put(test_body_with_query_params::<I, A, E, C>),
        )
        .route(
            "/v2/fake/hyphenParam/{hyphen_param}",
            get(hyphen_param::<I, A, E, C>),
        )
        .route(
            "/v2/fake/inline-additionalProperties",
            post(test_inline_additional_properties::<I, A, E, C>),
        )
        .route(
            "/v2/fake/jsonFormData",
            get(test_json_form_data::<I, A, E, C>),
        )
        .route(
            "/v2/fake/operation-with-numeric-id",
            get(call123example::<I, A, E, C>),
        )
        .route(
            "/v2/fake/outer/boolean",
            post(fake_outer_boolean_serialize::<I, A, E, C>),
        )
        .route(
            "/v2/fake/outer/composite",
            post(fake_outer_composite_serialize::<I, A, E, C>),
        )
        .route(
            "/v2/fake/outer/number",
            post(fake_outer_number_serialize::<I, A, E, C>),
        )
        .route(
            "/v2/fake/outer/string",
            post(fake_outer_string_serialize::<I, A, E, C>),
        )
        .route(
            "/v2/fake/response-with-numerical-description",
            get(fake_response_with_numerical_description::<I, A, E, C>),
        )
        .route("/v2/fake_classname_test", patch(test_classname::<I, A, E>))
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
        .route("/v2/user", post(create_user::<I, A, E>))
        .route(
            "/v2/user/createWithArray",
            post(create_users_with_array_input::<I, A, E>),
        )
        .route(
            "/v2/user/createWithList",
            post(create_users_with_list_input::<I, A, E>),
        )
        .route("/v2/user/login", get(login_user::<I, A, E>))
        .route("/v2/user/logout", get(logout_user::<I, A, E>))
        .route(
            "/v2/user/{username}",
            delete(delete_user::<I, A, E>)
                .get(get_user_by_name::<I, A, E>)
                .put(update_user::<I, A, E>),
        )
        .with_state(api_impl)
}

#[derive(validator::Validate)]
#[allow(dead_code)]
struct TestSpecialTagsBodyValidator<'a> {
    #[validate(nested)]
    body: &'a models::Client,
}

#[tracing::instrument(skip_all)]
fn test_special_tags_validation(
    body: models::Client,
) -> std::result::Result<(models::Client,), ValidationErrors> {
    let b = TestSpecialTagsBodyValidator { body: &body };
    b.validate()?;

    Ok((body,))
}
/// TestSpecialTags - PATCH /v2/another-fake/dummy
#[tracing::instrument(skip_all)]
async fn test_special_tags<I, A, E>(
    method: Method,
    host: Host,
    cookies: CookieJar,
    State(api_impl): State<I>,
    Json(body): Json<models::Client>,
) -> Result<Response, StatusCode>
where
    I: AsRef<A> + Send + Sync,
    A: apis::another_fake::AnotherFake<E> + Send + Sync,
    E: std::fmt::Debug + Send + Sync + 'static,
{
    #[allow(clippy::redundant_closure)]
    let validation = tokio::task::spawn_blocking(move || test_special_tags_validation(body))
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
        .test_special_tags(&method, &host, &cookies, &body)
        .await;

    let mut response = Response::builder();

    let resp = match result {
        Ok(rsp) => match rsp {
            apis::another_fake::TestSpecialTagsResponse::Status200_SuccessfulOperation(body) => {
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
fn call123example_validation() -> std::result::Result<(), ValidationErrors> {
    Ok(())
}
/// Call123example - GET /v2/fake/operation-with-numeric-id
#[tracing::instrument(skip_all)]
async fn call123example<I, A, E, C>(
    method: Method,
    host: Host,
    cookies: CookieJar,
    State(api_impl): State<I>,
) -> Result<Response, StatusCode>
where
    I: AsRef<A> + Send + Sync,
    A: apis::fake::Fake<E, Claims = C> + Send + Sync,
    E: std::fmt::Debug + Send + Sync + 'static,
{
    #[allow(clippy::redundant_closure)]
    let validation = tokio::task::spawn_blocking(move || call123example_validation())
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
        .call123example(&method, &host, &cookies)
        .await;

    let mut response = Response::builder();

    let resp = match result {
        Ok(rsp) => match rsp {
            apis::fake::Call123exampleResponse::Status200_Success => {
                let mut response = response.status(200);
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
struct FakeOuterBooleanSerializeBodyValidator<'a> {
    #[validate(nested)]
    body: &'a models::OuterBoolean,
}

#[tracing::instrument(skip_all)]
fn fake_outer_boolean_serialize_validation(
    body: Option<models::OuterBoolean>,
) -> std::result::Result<(Option<models::OuterBoolean>,), ValidationErrors> {
    if let Some(body) = &body {
        let b = FakeOuterBooleanSerializeBodyValidator { body };
        b.validate()?;
    }

    Ok((body,))
}
/// FakeOuterBooleanSerialize - POST /v2/fake/outer/boolean
#[tracing::instrument(skip_all)]
async fn fake_outer_boolean_serialize<I, A, E, C>(
    method: Method,
    host: Host,
    cookies: CookieJar,
    State(api_impl): State<I>,
    Json(body): Json<Option<models::OuterBoolean>>,
) -> Result<Response, StatusCode>
where
    I: AsRef<A> + Send + Sync,
    A: apis::fake::Fake<E, Claims = C> + Send + Sync,
    E: std::fmt::Debug + Send + Sync + 'static,
{
    #[allow(clippy::redundant_closure)]
    let validation =
        tokio::task::spawn_blocking(move || fake_outer_boolean_serialize_validation(body))
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
        .fake_outer_boolean_serialize(&method, &host, &cookies, &body)
        .await;

    let mut response = Response::builder();

    let resp = match result {
        Ok(rsp) => match rsp {
            apis::fake::FakeOuterBooleanSerializeResponse::Status200_OutputBoolean(body) => {
                let mut response = response.status(200);
                {
                    let mut response_headers = response.headers_mut().unwrap();
                    response_headers.insert(CONTENT_TYPE, HeaderValue::from_static("*/*"));
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

#[derive(validator::Validate)]
#[allow(dead_code)]
struct FakeOuterCompositeSerializeBodyValidator<'a> {
    #[validate(nested)]
    body: &'a models::OuterComposite,
}

#[tracing::instrument(skip_all)]
fn fake_outer_composite_serialize_validation(
    body: Option<models::OuterComposite>,
) -> std::result::Result<(Option<models::OuterComposite>,), ValidationErrors> {
    if let Some(body) = &body {
        let b = FakeOuterCompositeSerializeBodyValidator { body };
        b.validate()?;
    }

    Ok((body,))
}
/// FakeOuterCompositeSerialize - POST /v2/fake/outer/composite
#[tracing::instrument(skip_all)]
async fn fake_outer_composite_serialize<I, A, E, C>(
    method: Method,
    host: Host,
    cookies: CookieJar,
    State(api_impl): State<I>,
    Json(body): Json<Option<models::OuterComposite>>,
) -> Result<Response, StatusCode>
where
    I: AsRef<A> + Send + Sync,
    A: apis::fake::Fake<E, Claims = C> + Send + Sync,
    E: std::fmt::Debug + Send + Sync + 'static,
{
    #[allow(clippy::redundant_closure)]
    let validation =
        tokio::task::spawn_blocking(move || fake_outer_composite_serialize_validation(body))
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
        .fake_outer_composite_serialize(&method, &host, &cookies, &body)
        .await;

    let mut response = Response::builder();

    let resp = match result {
        Ok(rsp) => match rsp {
            apis::fake::FakeOuterCompositeSerializeResponse::Status200_OutputComposite(body) => {
                let mut response = response.status(200);
                {
                    let mut response_headers = response.headers_mut().unwrap();
                    response_headers.insert(CONTENT_TYPE, HeaderValue::from_static("*/*"));
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

#[derive(validator::Validate)]
#[allow(dead_code)]
struct FakeOuterNumberSerializeBodyValidator<'a> {
    #[validate(nested)]
    body: &'a models::OuterNumber,
}

#[tracing::instrument(skip_all)]
fn fake_outer_number_serialize_validation(
    body: Option<models::OuterNumber>,
) -> std::result::Result<(Option<models::OuterNumber>,), ValidationErrors> {
    if let Some(body) = &body {
        let b = FakeOuterNumberSerializeBodyValidator { body };
        b.validate()?;
    }

    Ok((body,))
}
/// FakeOuterNumberSerialize - POST /v2/fake/outer/number
#[tracing::instrument(skip_all)]
async fn fake_outer_number_serialize<I, A, E, C>(
    method: Method,
    host: Host,
    cookies: CookieJar,
    State(api_impl): State<I>,
    Json(body): Json<Option<models::OuterNumber>>,
) -> Result<Response, StatusCode>
where
    I: AsRef<A> + Send + Sync,
    A: apis::fake::Fake<E, Claims = C> + Send + Sync,
    E: std::fmt::Debug + Send + Sync + 'static,
{
    #[allow(clippy::redundant_closure)]
    let validation =
        tokio::task::spawn_blocking(move || fake_outer_number_serialize_validation(body))
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
        .fake_outer_number_serialize(&method, &host, &cookies, &body)
        .await;

    let mut response = Response::builder();

    let resp = match result {
        Ok(rsp) => match rsp {
            apis::fake::FakeOuterNumberSerializeResponse::Status200_OutputNumber(body) => {
                let mut response = response.status(200);
                {
                    let mut response_headers = response.headers_mut().unwrap();
                    response_headers.insert(CONTENT_TYPE, HeaderValue::from_static("*/*"));
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

#[derive(validator::Validate)]
#[allow(dead_code)]
struct FakeOuterStringSerializeBodyValidator<'a> {
    #[validate(nested)]
    body: &'a models::OuterString,
}

#[tracing::instrument(skip_all)]
fn fake_outer_string_serialize_validation(
    body: Option<models::OuterString>,
) -> std::result::Result<(Option<models::OuterString>,), ValidationErrors> {
    if let Some(body) = &body {
        let b = FakeOuterStringSerializeBodyValidator { body };
        b.validate()?;
    }

    Ok((body,))
}
/// FakeOuterStringSerialize - POST /v2/fake/outer/string
#[tracing::instrument(skip_all)]
async fn fake_outer_string_serialize<I, A, E, C>(
    method: Method,
    host: Host,
    cookies: CookieJar,
    State(api_impl): State<I>,
    Json(body): Json<Option<models::OuterString>>,
) -> Result<Response, StatusCode>
where
    I: AsRef<A> + Send + Sync,
    A: apis::fake::Fake<E, Claims = C> + Send + Sync,
    E: std::fmt::Debug + Send + Sync + 'static,
{
    #[allow(clippy::redundant_closure)]
    let validation =
        tokio::task::spawn_blocking(move || fake_outer_string_serialize_validation(body))
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
        .fake_outer_string_serialize(&method, &host, &cookies, &body)
        .await;

    let mut response = Response::builder();

    let resp = match result {
        Ok(rsp) => match rsp {
            apis::fake::FakeOuterStringSerializeResponse::Status200_OutputString(body) => {
                let mut response = response.status(200);
                {
                    let mut response_headers = response.headers_mut().unwrap();
                    response_headers.insert(CONTENT_TYPE, HeaderValue::from_static("*/*"));
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
fn fake_response_with_numerical_description_validation() -> std::result::Result<(), ValidationErrors>
{
    Ok(())
}
/// FakeResponseWithNumericalDescription - GET /v2/fake/response-with-numerical-description
#[tracing::instrument(skip_all)]
async fn fake_response_with_numerical_description<I, A, E, C>(
    method: Method,
    host: Host,
    cookies: CookieJar,
    State(api_impl): State<I>,
) -> Result<Response, StatusCode>
where
    I: AsRef<A> + Send + Sync,
    A: apis::fake::Fake<E, Claims = C> + Send + Sync,
    E: std::fmt::Debug + Send + Sync + 'static,
{
    #[allow(clippy::redundant_closure)]
    let validation =
        tokio::task::spawn_blocking(move || fake_response_with_numerical_description_validation())
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
        .fake_response_with_numerical_description(&method, &host, &cookies)
        .await;

    let mut response = Response::builder();

    let resp = match result {
        Ok(rsp) => match rsp {
            apis::fake::FakeResponseWithNumericalDescriptionResponse::Status200 => {
                let mut response = response.status(200);
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
fn hyphen_param_validation(
    path_params: models::HyphenParamPathParams,
) -> std::result::Result<(models::HyphenParamPathParams,), ValidationErrors> {
    path_params.validate()?;

    Ok((path_params,))
}
/// HyphenParam - GET /v2/fake/hyphenParam/{hyphen-param}
#[tracing::instrument(skip_all)]
async fn hyphen_param<I, A, E, C>(
    method: Method,
    host: Host,
    cookies: CookieJar,
    Path(path_params): Path<models::HyphenParamPathParams>,
    State(api_impl): State<I>,
) -> Result<Response, StatusCode>
where
    I: AsRef<A> + Send + Sync,
    A: apis::fake::Fake<E, Claims = C> + Send + Sync,
    E: std::fmt::Debug + Send + Sync + 'static,
{
    #[allow(clippy::redundant_closure)]
    let validation = tokio::task::spawn_blocking(move || hyphen_param_validation(path_params))
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
        .hyphen_param(&method, &host, &cookies, &path_params)
        .await;

    let mut response = Response::builder();

    let resp = match result {
        Ok(rsp) => match rsp {
            apis::fake::HyphenParamResponse::Status200_Success => {
                let mut response = response.status(200);
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
struct TestBodyWithQueryParamsBodyValidator<'a> {
    #[validate(nested)]
    body: &'a models::User,
}

#[tracing::instrument(skip_all)]
fn test_body_with_query_params_validation(
    query_params: models::TestBodyWithQueryParamsQueryParams,
    body: models::User,
) -> std::result::Result<(models::TestBodyWithQueryParamsQueryParams, models::User), ValidationErrors>
{
    query_params.validate()?;
    let b = TestBodyWithQueryParamsBodyValidator { body: &body };
    b.validate()?;

    Ok((query_params, body))
}
/// TestBodyWithQueryParams - PUT /v2/fake/body-with-query-params
#[tracing::instrument(skip_all)]
async fn test_body_with_query_params<I, A, E, C>(
    method: Method,
    host: Host,
    cookies: CookieJar,
    QueryExtra(query_params): QueryExtra<models::TestBodyWithQueryParamsQueryParams>,
    State(api_impl): State<I>,
    Json(body): Json<models::User>,
) -> Result<Response, StatusCode>
where
    I: AsRef<A> + Send + Sync,
    A: apis::fake::Fake<E, Claims = C> + Send + Sync,
    E: std::fmt::Debug + Send + Sync + 'static,
{
    #[allow(clippy::redundant_closure)]
    let validation = tokio::task::spawn_blocking(move || {
        test_body_with_query_params_validation(query_params, body)
    })
    .await
    .unwrap();

    let Ok((query_params, body)) = validation else {
        return Response::builder()
            .status(StatusCode::BAD_REQUEST)
            .body(Body::from(validation.unwrap_err().to_string()))
            .map_err(|_| StatusCode::BAD_REQUEST);
    };

    let result = api_impl
        .as_ref()
        .test_body_with_query_params(&method, &host, &cookies, &query_params, &body)
        .await;

    let mut response = Response::builder();

    let resp = match result {
        Ok(rsp) => match rsp {
            apis::fake::TestBodyWithQueryParamsResponse::Status200_Success => {
                let mut response = response.status(200);
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
struct TestClientModelBodyValidator<'a> {
    #[validate(nested)]
    body: &'a models::Client,
}

#[tracing::instrument(skip_all)]
fn test_client_model_validation(
    body: models::Client,
) -> std::result::Result<(models::Client,), ValidationErrors> {
    let b = TestClientModelBodyValidator { body: &body };
    b.validate()?;

    Ok((body,))
}
/// TestClientModel - PATCH /v2/fake
#[tracing::instrument(skip_all)]
async fn test_client_model<I, A, E, C>(
    method: Method,
    host: Host,
    cookies: CookieJar,
    State(api_impl): State<I>,
    Json(body): Json<models::Client>,
) -> Result<Response, StatusCode>
where
    I: AsRef<A> + Send + Sync,
    A: apis::fake::Fake<E, Claims = C> + Send + Sync,
    E: std::fmt::Debug + Send + Sync + 'static,
{
    #[allow(clippy::redundant_closure)]
    let validation = tokio::task::spawn_blocking(move || test_client_model_validation(body))
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
        .test_client_model(&method, &host, &cookies, &body)
        .await;

    let mut response = Response::builder();

    let resp = match result {
        Ok(rsp) => match rsp {
            apis::fake::TestClientModelResponse::Status200_SuccessfulOperation(body) => {
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

#[derive(validator::Validate)]
#[allow(dead_code)]
struct TestEndpointParametersBodyValidator<'a> {
    #[validate(nested)]
    body: &'a models::TestEndpointParametersRequest,
}

#[tracing::instrument(skip_all)]
fn test_endpoint_parameters_validation(
    body: models::TestEndpointParametersRequest,
) -> std::result::Result<(models::TestEndpointParametersRequest,), ValidationErrors> {
    let b = TestEndpointParametersBodyValidator { body: &body };
    b.validate()?;

    Ok((body,))
}
/// TestEndpointParameters - POST /v2/fake
#[tracing::instrument(skip_all)]
async fn test_endpoint_parameters<I, A, E, C>(
    method: Method,
    host: Host,
    cookies: CookieJar,
    headers: HeaderMap,
    State(api_impl): State<I>,
    Form(body): Form<models::TestEndpointParametersRequest>,
) -> Result<Response, StatusCode>
where
    I: AsRef<A> + Send + Sync,
    A: apis::fake::Fake<E, Claims = C> + apis::ApiAuthBasic<Claims = C> + Send + Sync,
    E: std::fmt::Debug + Send + Sync + 'static,
{
    // Authentication
    let claims_in_auth_header = api_impl
        .as_ref()
        .extract_claims_from_auth_header(apis::BasicAuthKind::Basic, &headers, "authorization")
        .await;
    let claims = None.or(claims_in_auth_header);
    let Some(claims) = claims else {
        return response_with_status_code_only(StatusCode::UNAUTHORIZED);
    };

    #[allow(clippy::redundant_closure)]
    let validation = tokio::task::spawn_blocking(move || test_endpoint_parameters_validation(body))
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
        .test_endpoint_parameters(&method, &host, &cookies, &claims, &body)
        .await;

    let mut response = Response::builder();

    let resp = match result {
        Ok(rsp) => match rsp {
            apis::fake::TestEndpointParametersResponse::Status400_InvalidUsernameSupplied => {
                let mut response = response.status(400);
                response.body(Body::empty())
            }
            apis::fake::TestEndpointParametersResponse::Status404_UserNotFound => {
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
struct TestEnumParametersBodyValidator<'a> {
    #[validate(nested)]
    body: &'a models::TestEnumParametersRequest,
}

#[tracing::instrument(skip_all)]
fn test_enum_parameters_validation(
    header_params: models::TestEnumParametersHeaderParams,
    query_params: models::TestEnumParametersQueryParams,
    body: Option<models::TestEnumParametersRequest>,
) -> std::result::Result<
    (
        models::TestEnumParametersHeaderParams,
        models::TestEnumParametersQueryParams,
        Option<models::TestEnumParametersRequest>,
    ),
    ValidationErrors,
> {
    header_params.validate()?;
    query_params.validate()?;
    if let Some(body) = &body {
        let b = TestEnumParametersBodyValidator { body };
        b.validate()?;
    }

    Ok((header_params, query_params, body))
}
/// TestEnumParameters - GET /v2/fake
#[tracing::instrument(skip_all)]
async fn test_enum_parameters<I, A, E, C>(
    method: Method,
    host: Host,
    cookies: CookieJar,
    headers: HeaderMap,
    QueryExtra(query_params): QueryExtra<models::TestEnumParametersQueryParams>,
    State(api_impl): State<I>,
    Form(body): Form<Option<models::TestEnumParametersRequest>>,
) -> Result<Response, StatusCode>
where
    I: AsRef<A> + Send + Sync,
    A: apis::fake::Fake<E, Claims = C> + Send + Sync,
    E: std::fmt::Debug + Send + Sync + 'static,
{
    // Header parameters
    let header_params = {
        let header_enum_header_string_array =
            headers.get(HeaderName::from_static("enum_header_string_array"));

        let header_enum_header_string_array = match header_enum_header_string_array {
            Some(v) => match header::IntoHeaderValue::<Vec<String>>::try_from((*v).clone()) {
                Ok(result) => Some(result.0),
                Err(err) => {
                    return Response::builder()
                        .status(StatusCode::BAD_REQUEST)
                        .body(Body::from(format!(
                            "Invalid header enum_header_string_array - {err}"
                        )))
                        .map_err(|e| {
                            error!(error = ?e);
                            StatusCode::INTERNAL_SERVER_ERROR
                        });
                }
            },
            None => None,
        };
        let header_enum_header_string = headers.get(HeaderName::from_static("enum_header_string"));

        let header_enum_header_string = match header_enum_header_string {
            Some(v) => match header::IntoHeaderValue::<String>::try_from((*v).clone()) {
                Ok(result) => Some(result.0),
                Err(err) => {
                    return Response::builder()
                        .status(StatusCode::BAD_REQUEST)
                        .body(Body::from(format!(
                            "Invalid header enum_header_string - {err}"
                        )))
                        .map_err(|e| {
                            error!(error = ?e);
                            StatusCode::INTERNAL_SERVER_ERROR
                        });
                }
            },
            None => None,
        };

        models::TestEnumParametersHeaderParams {
            enum_header_string_array: header_enum_header_string_array,
            enum_header_string: header_enum_header_string,
        }
    };

    #[allow(clippy::redundant_closure)]
    let validation = tokio::task::spawn_blocking(move || {
        test_enum_parameters_validation(header_params, query_params, body)
    })
    .await
    .unwrap();

    let Ok((header_params, query_params, body)) = validation else {
        return Response::builder()
            .status(StatusCode::BAD_REQUEST)
            .body(Body::from(validation.unwrap_err().to_string()))
            .map_err(|_| StatusCode::BAD_REQUEST);
    };

    let result = api_impl
        .as_ref()
        .test_enum_parameters(
            &method,
            &host,
            &cookies,
            &header_params,
            &query_params,
            &body,
        )
        .await;

    let mut response = Response::builder();

    let resp = match result {
        Ok(rsp) => match rsp {
            apis::fake::TestEnumParametersResponse::Status400_InvalidRequest => {
                let mut response = response.status(400);
                response.body(Body::empty())
            }
            apis::fake::TestEnumParametersResponse::Status404_NotFound => {
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
struct TestInlineAdditionalPropertiesBodyValidator<'a> {
    #[validate(custom(function = "check_xss_map_string"))]
    body: &'a std::collections::HashMap<String, String>,
}

#[tracing::instrument(skip_all)]
fn test_inline_additional_properties_validation(
    body: std::collections::HashMap<String, String>,
) -> std::result::Result<(std::collections::HashMap<String, String>,), ValidationErrors> {
    let b = TestInlineAdditionalPropertiesBodyValidator { body: &body };
    b.validate()?;

    Ok((body,))
}
/// TestInlineAdditionalProperties - POST /v2/fake/inline-additionalProperties
#[tracing::instrument(skip_all)]
async fn test_inline_additional_properties<I, A, E, C>(
    method: Method,
    host: Host,
    cookies: CookieJar,
    State(api_impl): State<I>,
    Json(body): Json<std::collections::HashMap<String, String>>,
) -> Result<Response, StatusCode>
where
    I: AsRef<A> + Send + Sync,
    A: apis::fake::Fake<E, Claims = C> + Send + Sync,
    E: std::fmt::Debug + Send + Sync + 'static,
{
    #[allow(clippy::redundant_closure)]
    let validation =
        tokio::task::spawn_blocking(move || test_inline_additional_properties_validation(body))
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
        .test_inline_additional_properties(&method, &host, &cookies, &body)
        .await;

    let mut response = Response::builder();

    let resp = match result {
        Ok(rsp) => match rsp {
            apis::fake::TestInlineAdditionalPropertiesResponse::Status200_SuccessfulOperation => {
                let mut response = response.status(200);
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
struct TestJsonFormDataBodyValidator<'a> {
    #[validate(nested)]
    body: &'a models::TestJsonFormDataRequest,
}

#[tracing::instrument(skip_all)]
fn test_json_form_data_validation(
    body: models::TestJsonFormDataRequest,
) -> std::result::Result<(models::TestJsonFormDataRequest,), ValidationErrors> {
    let b = TestJsonFormDataBodyValidator { body: &body };
    b.validate()?;

    Ok((body,))
}
/// TestJsonFormData - GET /v2/fake/jsonFormData
#[tracing::instrument(skip_all)]
async fn test_json_form_data<I, A, E, C>(
    method: Method,
    host: Host,
    cookies: CookieJar,
    State(api_impl): State<I>,
    Form(body): Form<models::TestJsonFormDataRequest>,
) -> Result<Response, StatusCode>
where
    I: AsRef<A> + Send + Sync,
    A: apis::fake::Fake<E, Claims = C> + Send + Sync,
    E: std::fmt::Debug + Send + Sync + 'static,
{
    #[allow(clippy::redundant_closure)]
    let validation = tokio::task::spawn_blocking(move || test_json_form_data_validation(body))
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
        .test_json_form_data(&method, &host, &cookies, &body)
        .await;

    let mut response = Response::builder();

    let resp = match result {
        Ok(rsp) => match rsp {
            apis::fake::TestJsonFormDataResponse::Status200_SuccessfulOperation => {
                let mut response = response.status(200);
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
struct TestClassnameBodyValidator<'a> {
    #[validate(nested)]
    body: &'a models::Client,
}

#[tracing::instrument(skip_all)]
fn test_classname_validation(
    body: models::Client,
) -> std::result::Result<(models::Client,), ValidationErrors> {
    let b = TestClassnameBodyValidator { body: &body };
    b.validate()?;

    Ok((body,))
}
/// TestClassname - PATCH /v2/fake_classname_test
#[tracing::instrument(skip_all)]
async fn test_classname<I, A, E>(
    method: Method,
    host: Host,
    cookies: CookieJar,
    State(api_impl): State<I>,
    Json(body): Json<models::Client>,
) -> Result<Response, StatusCode>
where
    I: AsRef<A> + Send + Sync,
    A: apis::fake_classname_tags123::FakeClassnameTags123<E> + Send + Sync,
    E: std::fmt::Debug + Send + Sync + 'static,
{
    #[allow(clippy::redundant_closure)]
    let validation = tokio::task::spawn_blocking(move || test_classname_validation(body))
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
        .test_classname(&method, &host, &cookies, &body)
        .await;

    let mut response = Response::builder();

    let resp = match result {
        Ok(rsp) => match rsp {
            apis::fake_classname_tags123::TestClassnameResponse::Status200_SuccessfulOperation(
                body,
            ) => {
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
/// DeleteOrder - DELETE /v2/store/order/{order_id}
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
/// GetOrderById - GET /v2/store/order/{order_id}
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
async fn create_user<I, A, E>(
    method: Method,
    host: Host,
    cookies: CookieJar,
    State(api_impl): State<I>,
    Json(body): Json<models::User>,
) -> Result<Response, StatusCode>
where
    I: AsRef<A> + Send + Sync,
    A: apis::user::User<E> + Send + Sync,
    E: std::fmt::Debug + Send + Sync + 'static,
{
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
        .create_user(&method, &host, &cookies, &body)
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
async fn create_users_with_array_input<I, A, E>(
    method: Method,
    host: Host,
    cookies: CookieJar,
    State(api_impl): State<I>,
    Json(body): Json<Vec<models::User>>,
) -> Result<Response, StatusCode>
where
    I: AsRef<A> + Send + Sync,
    A: apis::user::User<E> + Send + Sync,
    E: std::fmt::Debug + Send + Sync + 'static,
{
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
        .create_users_with_array_input(&method, &host, &cookies, &body)
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
async fn create_users_with_list_input<I, A, E>(
    method: Method,
    host: Host,
    cookies: CookieJar,
    State(api_impl): State<I>,
    Json(body): Json<Vec<models::User>>,
) -> Result<Response, StatusCode>
where
    I: AsRef<A> + Send + Sync,
    A: apis::user::User<E> + Send + Sync,
    E: std::fmt::Debug + Send + Sync + 'static,
{
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
        .create_users_with_list_input(&method, &host, &cookies, &body)
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
async fn delete_user<I, A, E>(
    method: Method,
    host: Host,
    cookies: CookieJar,
    Path(path_params): Path<models::DeleteUserPathParams>,
    State(api_impl): State<I>,
) -> Result<Response, StatusCode>
where
    I: AsRef<A> + Send + Sync,
    A: apis::user::User<E> + Send + Sync,
    E: std::fmt::Debug + Send + Sync + 'static,
{
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
        .delete_user(&method, &host, &cookies, &path_params)
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
async fn get_user_by_name<I, A, E>(
    method: Method,
    host: Host,
    cookies: CookieJar,
    Path(path_params): Path<models::GetUserByNamePathParams>,
    State(api_impl): State<I>,
) -> Result<Response, StatusCode>
where
    I: AsRef<A> + Send + Sync,
    A: apis::user::User<E> + Send + Sync,
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
async fn login_user<I, A, E>(
    method: Method,
    host: Host,
    cookies: CookieJar,
    QueryExtra(query_params): QueryExtra<models::LoginUserQueryParams>,
    State(api_impl): State<I>,
) -> Result<Response, StatusCode>
where
    I: AsRef<A> + Send + Sync,
    A: apis::user::User<E> + Send + Sync,
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
                x_rate_limit,
                x_expires_after,
            } => {
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
async fn logout_user<I, A, E>(
    method: Method,
    host: Host,
    cookies: CookieJar,
    State(api_impl): State<I>,
) -> Result<Response, StatusCode>
where
    I: AsRef<A> + Send + Sync,
    A: apis::user::User<E> + Send + Sync,
    E: std::fmt::Debug + Send + Sync + 'static,
{
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
        .logout_user(&method, &host, &cookies)
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
async fn update_user<I, A, E>(
    method: Method,
    host: Host,
    cookies: CookieJar,
    Path(path_params): Path<models::UpdateUserPathParams>,
    State(api_impl): State<I>,
    Json(body): Json<models::User>,
) -> Result<Response, StatusCode>
where
    I: AsRef<A> + Send + Sync,
    A: apis::user::User<E> + Send + Sync,
    E: std::fmt::Debug + Send + Sync + 'static,
{
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
        .update_user(&method, &host, &cookies, &path_params, &body)
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
