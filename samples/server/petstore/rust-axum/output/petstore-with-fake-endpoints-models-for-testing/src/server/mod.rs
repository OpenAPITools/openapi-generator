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
    AddPetResponse, Api, Call123exampleResponse, CreateUserResponse,
    CreateUsersWithArrayInputResponse, CreateUsersWithListInputResponse, DeleteOrderResponse,
    DeletePetResponse, DeleteUserResponse, FakeOuterBooleanSerializeResponse,
    FakeOuterCompositeSerializeResponse, FakeOuterNumberSerializeResponse,
    FakeOuterStringSerializeResponse, FakeResponseWithNumericalDescriptionResponse,
    FindPetsByStatusResponse, FindPetsByTagsResponse, GetInventoryResponse, GetOrderByIdResponse,
    GetPetByIdResponse, GetUserByNameResponse, HyphenParamResponse, LoginUserResponse,
    LogoutUserResponse, PlaceOrderResponse, TestBodyWithQueryParamsResponse, TestClassnameResponse,
    TestClientModelResponse, TestEndpointParametersResponse, TestEnumParametersResponse,
    TestInlineAdditionalPropertiesResponse, TestJsonFormDataResponse, TestSpecialTagsResponse,
    UpdatePetResponse, UpdatePetWithFormResponse, UpdateUserResponse, UploadFileResponse,
};

/// Setup API Server.
pub fn new<I, A>(api_impl: I) -> Router
where
    I: AsRef<A> + Clone + Send + Sync + 'static,
    A: Api + 'static,
{
    // build our application with a route
    Router::new()
        .route("/v2/another-fake/dummy", patch(test_special_tags::<I, A>))
        .route(
            "/v2/fake",
            get(test_enum_parameters::<I, A>)
                .patch(test_client_model::<I, A>)
                .post(test_endpoint_parameters::<I, A>),
        )
        .route(
            "/v2/fake/body-with-query-params",
            put(test_body_with_query_params::<I, A>),
        )
        .route(
            "/v2/fake/hyphenParam/:hyphen_param",
            get(hyphen_param::<I, A>),
        )
        .route(
            "/v2/fake/inline-additionalProperties",
            post(test_inline_additional_properties::<I, A>),
        )
        .route("/v2/fake/jsonFormData", get(test_json_form_data::<I, A>))
        .route(
            "/v2/fake/operation-with-numeric-id",
            get(call123example::<I, A>),
        )
        .route(
            "/v2/fake/outer/boolean",
            post(fake_outer_boolean_serialize::<I, A>),
        )
        .route(
            "/v2/fake/outer/composite",
            post(fake_outer_composite_serialize::<I, A>),
        )
        .route(
            "/v2/fake/outer/number",
            post(fake_outer_number_serialize::<I, A>),
        )
        .route(
            "/v2/fake/outer/string",
            post(fake_outer_string_serialize::<I, A>),
        )
        .route(
            "/v2/fake/response-with-numerical-description",
            get(fake_response_with_numerical_description::<I, A>),
        )
        .route("/v2/fake_classname_test", patch(test_classname::<I, A>))
        .route("/v2/pet", post(add_pet::<I, A>).put(update_pet::<I, A>))
        .route(
            "/v2/pet/:pet_id",
            delete(delete_pet::<I, A>)
                .get(get_pet_by_id::<I, A>)
                .post(update_pet_with_form::<I, A>),
        )
        .route("/v2/pet/:pet_id/uploadImage", post(upload_file::<I, A>))
        .route("/v2/pet/findByStatus", get(find_pets_by_status::<I, A>))
        .route("/v2/pet/findByTags", get(find_pets_by_tags::<I, A>))
        .route("/v2/store/inventory", get(get_inventory::<I, A>))
        .route("/v2/store/order", post(place_order::<I, A>))
        .route(
            "/v2/store/order/:order_id",
            delete(delete_order::<I, A>).get(get_order_by_id::<I, A>),
        )
        .route("/v2/user", post(create_user::<I, A>))
        .route(
            "/v2/user/:username",
            delete(delete_user::<I, A>)
                .get(get_user_by_name::<I, A>)
                .put(update_user::<I, A>),
        )
        .route(
            "/v2/user/createWithArray",
            post(create_users_with_array_input::<I, A>),
        )
        .route(
            "/v2/user/createWithList",
            post(create_users_with_list_input::<I, A>),
        )
        .route("/v2/user/login", get(login_user::<I, A>))
        .route("/v2/user/logout", get(logout_user::<I, A>))
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
async fn test_special_tags<I, A>(
    method: Method,
    host: Host,
    cookies: CookieJar,
    State(api_impl): State<I>,
    Json(body): Json<models::Client>,
) -> Result<Response, StatusCode>
where
    I: AsRef<A> + Send + Sync,
    A: Api,
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
        .test_special_tags(method, host, cookies, body)
        .await;

    let mut response = Response::builder();

    let resp = match result {
        Ok(rsp) => match rsp {
            TestSpecialTagsResponse::Status200_SuccessfulOperation(body) => {
                let mut response = response.status(200);
                {
                    let mut response_headers = response.headers_mut().unwrap();
                    response_headers.insert(
                        CONTENT_TYPE,
                        HeaderValue::from_str("application/json").map_err(|e| {
                            error!(error = ?e);
                            StatusCode::INTERNAL_SERVER_ERROR
                        })?,
                    );
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
fn call123example_validation() -> std::result::Result<(), ValidationErrors> {
    Ok(())
}
/// Call123example - GET /v2/fake/operation-with-numeric-id
#[tracing::instrument(skip_all)]
async fn call123example<I, A>(
    method: Method,
    host: Host,
    cookies: CookieJar,
    State(api_impl): State<I>,
) -> Result<Response, StatusCode>
where
    I: AsRef<A> + Send + Sync,
    A: Api,
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
        .call123example(method, host, cookies)
        .await;

    let mut response = Response::builder();

    let resp = match result {
        Ok(rsp) => match rsp {
            Call123exampleResponse::Status200_Success => {
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
async fn fake_outer_boolean_serialize<I, A>(
    method: Method,
    host: Host,
    cookies: CookieJar,
    State(api_impl): State<I>,
    Json(body): Json<Option<models::OuterBoolean>>,
) -> Result<Response, StatusCode>
where
    I: AsRef<A> + Send + Sync,
    A: Api,
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
        .fake_outer_boolean_serialize(method, host, cookies, body)
        .await;

    let mut response = Response::builder();

    let resp = match result {
        Ok(rsp) => match rsp {
            FakeOuterBooleanSerializeResponse::Status200_OutputBoolean(body) => {
                let mut response = response.status(200);
                {
                    let mut response_headers = response.headers_mut().unwrap();
                    response_headers.insert(
                        CONTENT_TYPE,
                        HeaderValue::from_str("*/*").map_err(|e| {
                            error!(error = ?e);
                            StatusCode::INTERNAL_SERVER_ERROR
                        })?,
                    );
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
async fn fake_outer_composite_serialize<I, A>(
    method: Method,
    host: Host,
    cookies: CookieJar,
    State(api_impl): State<I>,
    Json(body): Json<Option<models::OuterComposite>>,
) -> Result<Response, StatusCode>
where
    I: AsRef<A> + Send + Sync,
    A: Api,
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
        .fake_outer_composite_serialize(method, host, cookies, body)
        .await;

    let mut response = Response::builder();

    let resp = match result {
        Ok(rsp) => match rsp {
            FakeOuterCompositeSerializeResponse::Status200_OutputComposite(body) => {
                let mut response = response.status(200);
                {
                    let mut response_headers = response.headers_mut().unwrap();
                    response_headers.insert(
                        CONTENT_TYPE,
                        HeaderValue::from_str("*/*").map_err(|e| {
                            error!(error = ?e);
                            StatusCode::INTERNAL_SERVER_ERROR
                        })?,
                    );
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
async fn fake_outer_number_serialize<I, A>(
    method: Method,
    host: Host,
    cookies: CookieJar,
    State(api_impl): State<I>,
    Json(body): Json<Option<models::OuterNumber>>,
) -> Result<Response, StatusCode>
where
    I: AsRef<A> + Send + Sync,
    A: Api,
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
        .fake_outer_number_serialize(method, host, cookies, body)
        .await;

    let mut response = Response::builder();

    let resp = match result {
        Ok(rsp) => match rsp {
            FakeOuterNumberSerializeResponse::Status200_OutputNumber(body) => {
                let mut response = response.status(200);
                {
                    let mut response_headers = response.headers_mut().unwrap();
                    response_headers.insert(
                        CONTENT_TYPE,
                        HeaderValue::from_str("*/*").map_err(|e| {
                            error!(error = ?e);
                            StatusCode::INTERNAL_SERVER_ERROR
                        })?,
                    );
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
async fn fake_outer_string_serialize<I, A>(
    method: Method,
    host: Host,
    cookies: CookieJar,
    State(api_impl): State<I>,
    Json(body): Json<Option<models::OuterString>>,
) -> Result<Response, StatusCode>
where
    I: AsRef<A> + Send + Sync,
    A: Api,
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
        .fake_outer_string_serialize(method, host, cookies, body)
        .await;

    let mut response = Response::builder();

    let resp = match result {
        Ok(rsp) => match rsp {
            FakeOuterStringSerializeResponse::Status200_OutputString(body) => {
                let mut response = response.status(200);
                {
                    let mut response_headers = response.headers_mut().unwrap();
                    response_headers.insert(
                        CONTENT_TYPE,
                        HeaderValue::from_str("*/*").map_err(|e| {
                            error!(error = ?e);
                            StatusCode::INTERNAL_SERVER_ERROR
                        })?,
                    );
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
fn fake_response_with_numerical_description_validation() -> std::result::Result<(), ValidationErrors>
{
    Ok(())
}
/// FakeResponseWithNumericalDescription - GET /v2/fake/response-with-numerical-description
#[tracing::instrument(skip_all)]
async fn fake_response_with_numerical_description<I, A>(
    method: Method,
    host: Host,
    cookies: CookieJar,
    State(api_impl): State<I>,
) -> Result<Response, StatusCode>
where
    I: AsRef<A> + Send + Sync,
    A: Api,
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
        .fake_response_with_numerical_description(method, host, cookies)
        .await;

    let mut response = Response::builder();

    let resp = match result {
        Ok(rsp) => match rsp {
            FakeResponseWithNumericalDescriptionResponse::Status200 => {
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

#[tracing::instrument(skip_all)]
fn hyphen_param_validation(
    path_params: models::HyphenParamPathParams,
) -> std::result::Result<(models::HyphenParamPathParams,), ValidationErrors> {
    path_params.validate()?;

    Ok((path_params,))
}
/// HyphenParam - GET /v2/fake/hyphenParam/{hyphen-param}
#[tracing::instrument(skip_all)]
async fn hyphen_param<I, A>(
    method: Method,
    host: Host,
    cookies: CookieJar,
    Path(path_params): Path<models::HyphenParamPathParams>,
    State(api_impl): State<I>,
) -> Result<Response, StatusCode>
where
    I: AsRef<A> + Send + Sync,
    A: Api,
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
        .hyphen_param(method, host, cookies, path_params)
        .await;

    let mut response = Response::builder();

    let resp = match result {
        Ok(rsp) => match rsp {
            HyphenParamResponse::Status200_Success => {
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
async fn test_body_with_query_params<I, A>(
    method: Method,
    host: Host,
    cookies: CookieJar,
    Query(query_params): Query<models::TestBodyWithQueryParamsQueryParams>,
    State(api_impl): State<I>,
    Json(body): Json<models::User>,
) -> Result<Response, StatusCode>
where
    I: AsRef<A> + Send + Sync,
    A: Api,
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
        .test_body_with_query_params(method, host, cookies, query_params, body)
        .await;

    let mut response = Response::builder();

    let resp = match result {
        Ok(rsp) => match rsp {
            TestBodyWithQueryParamsResponse::Status200_Success => {
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
async fn test_client_model<I, A>(
    method: Method,
    host: Host,
    cookies: CookieJar,
    State(api_impl): State<I>,
    Json(body): Json<models::Client>,
) -> Result<Response, StatusCode>
where
    I: AsRef<A> + Send + Sync,
    A: Api,
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
        .test_client_model(method, host, cookies, body)
        .await;

    let mut response = Response::builder();

    let resp = match result {
        Ok(rsp) => match rsp {
            TestClientModelResponse::Status200_SuccessfulOperation(body) => {
                let mut response = response.status(200);
                {
                    let mut response_headers = response.headers_mut().unwrap();
                    response_headers.insert(
                        CONTENT_TYPE,
                        HeaderValue::from_str("application/json").map_err(|e| {
                            error!(error = ?e);
                            StatusCode::INTERNAL_SERVER_ERROR
                        })?,
                    );
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
fn test_endpoint_parameters_validation() -> std::result::Result<(), ValidationErrors> {
    Ok(())
}
/// TestEndpointParameters - POST /v2/fake
#[tracing::instrument(skip_all)]
async fn test_endpoint_parameters<I, A>(
    method: Method,
    host: Host,
    cookies: CookieJar,
    State(api_impl): State<I>,
) -> Result<Response, StatusCode>
where
    I: AsRef<A> + Send + Sync,
    A: Api,
{
    #[allow(clippy::redundant_closure)]
    let validation = tokio::task::spawn_blocking(move || test_endpoint_parameters_validation())
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
        .test_endpoint_parameters(method, host, cookies)
        .await;

    let mut response = Response::builder();

    let resp = match result {
        Ok(rsp) => match rsp {
            TestEndpointParametersResponse::Status400_InvalidUsernameSupplied => {
                let mut response = response.status(400);
                response.body(Body::empty())
            }
            TestEndpointParametersResponse::Status404_UserNotFound => {
                let mut response = response.status(404);
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
fn test_enum_parameters_validation(
    header_params: models::TestEnumParametersHeaderParams,
    query_params: models::TestEnumParametersQueryParams,
) -> std::result::Result<
    (
        models::TestEnumParametersHeaderParams,
        models::TestEnumParametersQueryParams,
    ),
    ValidationErrors,
> {
    header_params.validate()?;
    query_params.validate()?;

    Ok((header_params, query_params))
}
/// TestEnumParameters - GET /v2/fake
#[tracing::instrument(skip_all)]
async fn test_enum_parameters<I, A>(
    method: Method,
    host: Host,
    cookies: CookieJar,
    headers: HeaderMap,
    Query(query_params): Query<models::TestEnumParametersQueryParams>,
    State(api_impl): State<I>,
) -> Result<Response, StatusCode>
where
    I: AsRef<A> + Send + Sync,
    A: Api,
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
                            "Invalid header enum_header_string_array - {}",
                            err
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
                            "Invalid header enum_header_string - {}",
                            err
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
        test_enum_parameters_validation(header_params, query_params)
    })
    .await
    .unwrap();

    let Ok((header_params, query_params)) = validation else {
        return Response::builder()
            .status(StatusCode::BAD_REQUEST)
            .body(Body::from(validation.unwrap_err().to_string()))
            .map_err(|_| StatusCode::BAD_REQUEST);
    };

    let result = api_impl
        .as_ref()
        .test_enum_parameters(method, host, cookies, header_params, query_params)
        .await;

    let mut response = Response::builder();

    let resp = match result {
        Ok(rsp) => match rsp {
            TestEnumParametersResponse::Status400_InvalidRequest => {
                let mut response = response.status(400);
                response.body(Body::empty())
            }
            TestEnumParametersResponse::Status404_NotFound => {
                let mut response = response.status(404);
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

#[derive(validator::Validate)]
#[allow(dead_code)]
struct TestInlineAdditionalPropertiesBodyValidator<'a> {
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
async fn test_inline_additional_properties<I, A>(
    method: Method,
    host: Host,
    cookies: CookieJar,
    State(api_impl): State<I>,
    Json(body): Json<std::collections::HashMap<String, String>>,
) -> Result<Response, StatusCode>
where
    I: AsRef<A> + Send + Sync,
    A: Api,
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
        .test_inline_additional_properties(method, host, cookies, body)
        .await;

    let mut response = Response::builder();

    let resp = match result {
        Ok(rsp) => match rsp {
            TestInlineAdditionalPropertiesResponse::Status200_SuccessfulOperation => {
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

#[tracing::instrument(skip_all)]
fn test_json_form_data_validation() -> std::result::Result<(), ValidationErrors> {
    Ok(())
}
/// TestJsonFormData - GET /v2/fake/jsonFormData
#[tracing::instrument(skip_all)]
async fn test_json_form_data<I, A>(
    method: Method,
    host: Host,
    cookies: CookieJar,
    State(api_impl): State<I>,
) -> Result<Response, StatusCode>
where
    I: AsRef<A> + Send + Sync,
    A: Api,
{
    #[allow(clippy::redundant_closure)]
    let validation = tokio::task::spawn_blocking(move || test_json_form_data_validation())
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
        .test_json_form_data(method, host, cookies)
        .await;

    let mut response = Response::builder();

    let resp = match result {
        Ok(rsp) => match rsp {
            TestJsonFormDataResponse::Status200_SuccessfulOperation => {
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
async fn test_classname<I, A>(
    method: Method,
    host: Host,
    cookies: CookieJar,
    State(api_impl): State<I>,
    Json(body): Json<models::Client>,
) -> Result<Response, StatusCode>
where
    I: AsRef<A> + Send + Sync,
    A: Api,
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
        .test_classname(method, host, cookies, body)
        .await;

    let mut response = Response::builder();

    let resp = match result {
        Ok(rsp) => match rsp {
            TestClassnameResponse::Status200_SuccessfulOperation(body) => {
                let mut response = response.status(200);
                {
                    let mut response_headers = response.headers_mut().unwrap();
                    response_headers.insert(
                        CONTENT_TYPE,
                        HeaderValue::from_str("application/json").map_err(|e| {
                            error!(error = ?e);
                            StatusCode::INTERNAL_SERVER_ERROR
                        })?,
                    );
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
async fn add_pet<I, A>(
    method: Method,
    host: Host,
    cookies: CookieJar,
    State(api_impl): State<I>,
    Json(body): Json<models::Pet>,
) -> Result<Response, StatusCode>
where
    I: AsRef<A> + Send + Sync,
    A: Api,
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

    let result = api_impl.as_ref().add_pet(method, host, cookies, body).await;

    let mut response = Response::builder();

    let resp = match result {
        Ok(rsp) => match rsp {
            AddPetResponse::Status405_InvalidInput => {
                let mut response = response.status(405);
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
async fn delete_pet<I, A>(
    method: Method,
    host: Host,
    cookies: CookieJar,
    headers: HeaderMap,
    Path(path_params): Path<models::DeletePetPathParams>,
    State(api_impl): State<I>,
) -> Result<Response, StatusCode>
where
    I: AsRef<A> + Send + Sync,
    A: Api,
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
                        .body(Body::from(format!("Invalid header api_key - {}", err)))
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
        .delete_pet(method, host, cookies, header_params, path_params)
        .await;

    let mut response = Response::builder();

    let resp = match result {
        Ok(rsp) => match rsp {
            DeletePetResponse::Status400_InvalidPetValue => {
                let mut response = response.status(400);
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
fn find_pets_by_status_validation(
    query_params: models::FindPetsByStatusQueryParams,
) -> std::result::Result<(models::FindPetsByStatusQueryParams,), ValidationErrors> {
    query_params.validate()?;

    Ok((query_params,))
}
/// FindPetsByStatus - GET /v2/pet/findByStatus
#[tracing::instrument(skip_all)]
async fn find_pets_by_status<I, A>(
    method: Method,
    host: Host,
    cookies: CookieJar,
    Query(query_params): Query<models::FindPetsByStatusQueryParams>,
    State(api_impl): State<I>,
) -> Result<Response, StatusCode>
where
    I: AsRef<A> + Send + Sync,
    A: Api,
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
        .find_pets_by_status(method, host, cookies, query_params)
        .await;

    let mut response = Response::builder();

    let resp = match result {
        Ok(rsp) => match rsp {
            FindPetsByStatusResponse::Status200_SuccessfulOperation(body) => {
                let mut response = response.status(200);
                {
                    let mut response_headers = response.headers_mut().unwrap();
                    response_headers.insert(
                        CONTENT_TYPE,
                        HeaderValue::from_str("text/plain").map_err(|e| {
                            error!(error = ?e);
                            StatusCode::INTERNAL_SERVER_ERROR
                        })?,
                    );
                }

                let body_content = body;
                response.body(Body::from(body_content))
            }
            FindPetsByStatusResponse::Status400_InvalidStatusValue => {
                let mut response = response.status(400);
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
fn find_pets_by_tags_validation(
    query_params: models::FindPetsByTagsQueryParams,
) -> std::result::Result<(models::FindPetsByTagsQueryParams,), ValidationErrors> {
    query_params.validate()?;

    Ok((query_params,))
}
/// FindPetsByTags - GET /v2/pet/findByTags
#[tracing::instrument(skip_all)]
async fn find_pets_by_tags<I, A>(
    method: Method,
    host: Host,
    cookies: CookieJar,
    Query(query_params): Query<models::FindPetsByTagsQueryParams>,
    State(api_impl): State<I>,
) -> Result<Response, StatusCode>
where
    I: AsRef<A> + Send + Sync,
    A: Api,
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
        .find_pets_by_tags(method, host, cookies, query_params)
        .await;

    let mut response = Response::builder();

    let resp = match result {
        Ok(rsp) => match rsp {
            FindPetsByTagsResponse::Status200_SuccessfulOperation(body) => {
                let mut response = response.status(200);
                {
                    let mut response_headers = response.headers_mut().unwrap();
                    response_headers.insert(
                        CONTENT_TYPE,
                        HeaderValue::from_str("text/plain").map_err(|e| {
                            error!(error = ?e);
                            StatusCode::INTERNAL_SERVER_ERROR
                        })?,
                    );
                }

                let body_content = body;
                response.body(Body::from(body_content))
            }
            FindPetsByTagsResponse::Status400_InvalidTagValue => {
                let mut response = response.status(400);
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
fn get_pet_by_id_validation(
    path_params: models::GetPetByIdPathParams,
) -> std::result::Result<(models::GetPetByIdPathParams,), ValidationErrors> {
    path_params.validate()?;

    Ok((path_params,))
}
/// GetPetById - GET /v2/pet/{petId}
#[tracing::instrument(skip_all)]
async fn get_pet_by_id<I, A>(
    method: Method,
    host: Host,
    cookies: CookieJar,
    Path(path_params): Path<models::GetPetByIdPathParams>,
    State(api_impl): State<I>,
) -> Result<Response, StatusCode>
where
    I: AsRef<A> + Send + Sync,
    A: Api,
{
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
        .get_pet_by_id(method, host, cookies, path_params)
        .await;

    let mut response = Response::builder();

    let resp = match result {
        Ok(rsp) => match rsp {
            GetPetByIdResponse::Status200_SuccessfulOperation(body) => {
                let mut response = response.status(200);
                {
                    let mut response_headers = response.headers_mut().unwrap();
                    response_headers.insert(
                        CONTENT_TYPE,
                        HeaderValue::from_str("text/plain").map_err(|e| {
                            error!(error = ?e);
                            StatusCode::INTERNAL_SERVER_ERROR
                        })?,
                    );
                }

                let body_content = body;
                response.body(Body::from(body_content))
            }
            GetPetByIdResponse::Status400_InvalidIDSupplied => {
                let mut response = response.status(400);
                response.body(Body::empty())
            }
            GetPetByIdResponse::Status404_PetNotFound => {
                let mut response = response.status(404);
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
async fn update_pet<I, A>(
    method: Method,
    host: Host,
    cookies: CookieJar,
    State(api_impl): State<I>,
    Json(body): Json<models::Pet>,
) -> Result<Response, StatusCode>
where
    I: AsRef<A> + Send + Sync,
    A: Api,
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
        .update_pet(method, host, cookies, body)
        .await;

    let mut response = Response::builder();

    let resp = match result {
        Ok(rsp) => match rsp {
            UpdatePetResponse::Status400_InvalidIDSupplied => {
                let mut response = response.status(400);
                response.body(Body::empty())
            }
            UpdatePetResponse::Status404_PetNotFound => {
                let mut response = response.status(404);
                response.body(Body::empty())
            }
            UpdatePetResponse::Status405_ValidationException => {
                let mut response = response.status(405);
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
fn update_pet_with_form_validation(
    path_params: models::UpdatePetWithFormPathParams,
) -> std::result::Result<(models::UpdatePetWithFormPathParams,), ValidationErrors> {
    path_params.validate()?;

    Ok((path_params,))
}
/// UpdatePetWithForm - POST /v2/pet/{petId}
#[tracing::instrument(skip_all)]
async fn update_pet_with_form<I, A>(
    method: Method,
    host: Host,
    cookies: CookieJar,
    Path(path_params): Path<models::UpdatePetWithFormPathParams>,
    State(api_impl): State<I>,
) -> Result<Response, StatusCode>
where
    I: AsRef<A> + Send + Sync,
    A: Api,
{
    #[allow(clippy::redundant_closure)]
    let validation =
        tokio::task::spawn_blocking(move || update_pet_with_form_validation(path_params))
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
        .update_pet_with_form(method, host, cookies, path_params)
        .await;

    let mut response = Response::builder();

    let resp = match result {
        Ok(rsp) => match rsp {
            UpdatePetWithFormResponse::Status405_InvalidInput => {
                let mut response = response.status(405);
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
fn upload_file_validation(
    path_params: models::UploadFilePathParams,
) -> std::result::Result<(models::UploadFilePathParams,), ValidationErrors> {
    path_params.validate()?;

    Ok((path_params,))
}
/// UploadFile - POST /v2/pet/{petId}/uploadImage
#[tracing::instrument(skip_all)]
async fn upload_file<I, A>(
    method: Method,
    host: Host,
    cookies: CookieJar,
    Path(path_params): Path<models::UploadFilePathParams>,
    State(api_impl): State<I>,
    body: Multipart,
) -> Result<Response, StatusCode>
where
    I: AsRef<A> + Send + Sync,
    A: Api,
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
        .upload_file(method, host, cookies, path_params, body)
        .await;

    let mut response = Response::builder();

    let resp = match result {
        Ok(rsp) => match rsp {
            UploadFileResponse::Status200_SuccessfulOperation(body) => {
                let mut response = response.status(200);
                {
                    let mut response_headers = response.headers_mut().unwrap();
                    response_headers.insert(
                        CONTENT_TYPE,
                        HeaderValue::from_str("application/json").map_err(|e| {
                            error!(error = ?e);
                            StatusCode::INTERNAL_SERVER_ERROR
                        })?,
                    );
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
fn delete_order_validation(
    path_params: models::DeleteOrderPathParams,
) -> std::result::Result<(models::DeleteOrderPathParams,), ValidationErrors> {
    path_params.validate()?;

    Ok((path_params,))
}
/// DeleteOrder - DELETE /v2/store/order/{order_id}
#[tracing::instrument(skip_all)]
async fn delete_order<I, A>(
    method: Method,
    host: Host,
    cookies: CookieJar,
    Path(path_params): Path<models::DeleteOrderPathParams>,
    State(api_impl): State<I>,
) -> Result<Response, StatusCode>
where
    I: AsRef<A> + Send + Sync,
    A: Api,
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
        .delete_order(method, host, cookies, path_params)
        .await;

    let mut response = Response::builder();

    let resp = match result {
        Ok(rsp) => match rsp {
            DeleteOrderResponse::Status400_InvalidIDSupplied => {
                let mut response = response.status(400);
                response.body(Body::empty())
            }
            DeleteOrderResponse::Status404_OrderNotFound => {
                let mut response = response.status(404);
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
fn get_inventory_validation() -> std::result::Result<(), ValidationErrors> {
    Ok(())
}
/// GetInventory - GET /v2/store/inventory
#[tracing::instrument(skip_all)]
async fn get_inventory<I, A>(
    method: Method,
    host: Host,
    cookies: CookieJar,
    State(api_impl): State<I>,
) -> Result<Response, StatusCode>
where
    I: AsRef<A> + Send + Sync,
    A: Api,
{
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

    let result = api_impl.as_ref().get_inventory(method, host, cookies).await;

    let mut response = Response::builder();

    let resp = match result {
        Ok(rsp) => match rsp {
            GetInventoryResponse::Status200_SuccessfulOperation(body) => {
                let mut response = response.status(200);
                {
                    let mut response_headers = response.headers_mut().unwrap();
                    response_headers.insert(
                        CONTENT_TYPE,
                        HeaderValue::from_str("application/json").map_err(|e| {
                            error!(error = ?e);
                            StatusCode::INTERNAL_SERVER_ERROR
                        })?,
                    );
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
fn get_order_by_id_validation(
    path_params: models::GetOrderByIdPathParams,
) -> std::result::Result<(models::GetOrderByIdPathParams,), ValidationErrors> {
    path_params.validate()?;

    Ok((path_params,))
}
/// GetOrderById - GET /v2/store/order/{order_id}
#[tracing::instrument(skip_all)]
async fn get_order_by_id<I, A>(
    method: Method,
    host: Host,
    cookies: CookieJar,
    Path(path_params): Path<models::GetOrderByIdPathParams>,
    State(api_impl): State<I>,
) -> Result<Response, StatusCode>
where
    I: AsRef<A> + Send + Sync,
    A: Api,
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
        .get_order_by_id(method, host, cookies, path_params)
        .await;

    let mut response = Response::builder();

    let resp = match result {
        Ok(rsp) => match rsp {
            GetOrderByIdResponse::Status200_SuccessfulOperation(body) => {
                let mut response = response.status(200);
                {
                    let mut response_headers = response.headers_mut().unwrap();
                    response_headers.insert(
                        CONTENT_TYPE,
                        HeaderValue::from_str("text/plain").map_err(|e| {
                            error!(error = ?e);
                            StatusCode::INTERNAL_SERVER_ERROR
                        })?,
                    );
                }

                let body_content = body;
                response.body(Body::from(body_content))
            }
            GetOrderByIdResponse::Status400_InvalidIDSupplied => {
                let mut response = response.status(400);
                response.body(Body::empty())
            }
            GetOrderByIdResponse::Status404_OrderNotFound => {
                let mut response = response.status(404);
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
async fn place_order<I, A>(
    method: Method,
    host: Host,
    cookies: CookieJar,
    State(api_impl): State<I>,
    Json(body): Json<models::Order>,
) -> Result<Response, StatusCode>
where
    I: AsRef<A> + Send + Sync,
    A: Api,
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
        .place_order(method, host, cookies, body)
        .await;

    let mut response = Response::builder();

    let resp = match result {
        Ok(rsp) => match rsp {
            PlaceOrderResponse::Status200_SuccessfulOperation(body) => {
                let mut response = response.status(200);
                {
                    let mut response_headers = response.headers_mut().unwrap();
                    response_headers.insert(
                        CONTENT_TYPE,
                        HeaderValue::from_str("text/plain").map_err(|e| {
                            error!(error = ?e);
                            StatusCode::INTERNAL_SERVER_ERROR
                        })?,
                    );
                }

                let body_content = body;
                response.body(Body::from(body_content))
            }
            PlaceOrderResponse::Status400_InvalidOrder => {
                let mut response = response.status(400);
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
async fn create_user<I, A>(
    method: Method,
    host: Host,
    cookies: CookieJar,
    State(api_impl): State<I>,
    Json(body): Json<models::User>,
) -> Result<Response, StatusCode>
where
    I: AsRef<A> + Send + Sync,
    A: Api,
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
        .create_user(method, host, cookies, body)
        .await;

    let mut response = Response::builder();

    let resp = match result {
        Ok(rsp) => match rsp {
            CreateUserResponse::Status0_SuccessfulOperation => {
                let mut response = response.status(0);
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
async fn create_users_with_array_input<I, A>(
    method: Method,
    host: Host,
    cookies: CookieJar,
    State(api_impl): State<I>,
    Json(body): Json<Vec<models::User>>,
) -> Result<Response, StatusCode>
where
    I: AsRef<A> + Send + Sync,
    A: Api,
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
        .create_users_with_array_input(method, host, cookies, body)
        .await;

    let mut response = Response::builder();

    let resp = match result {
        Ok(rsp) => match rsp {
            CreateUsersWithArrayInputResponse::Status0_SuccessfulOperation => {
                let mut response = response.status(0);
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
async fn create_users_with_list_input<I, A>(
    method: Method,
    host: Host,
    cookies: CookieJar,
    State(api_impl): State<I>,
    Json(body): Json<Vec<models::User>>,
) -> Result<Response, StatusCode>
where
    I: AsRef<A> + Send + Sync,
    A: Api,
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
        .create_users_with_list_input(method, host, cookies, body)
        .await;

    let mut response = Response::builder();

    let resp = match result {
        Ok(rsp) => match rsp {
            CreateUsersWithListInputResponse::Status0_SuccessfulOperation => {
                let mut response = response.status(0);
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
fn delete_user_validation(
    path_params: models::DeleteUserPathParams,
) -> std::result::Result<(models::DeleteUserPathParams,), ValidationErrors> {
    path_params.validate()?;

    Ok((path_params,))
}
/// DeleteUser - DELETE /v2/user/{username}
#[tracing::instrument(skip_all)]
async fn delete_user<I, A>(
    method: Method,
    host: Host,
    cookies: CookieJar,
    Path(path_params): Path<models::DeleteUserPathParams>,
    State(api_impl): State<I>,
) -> Result<Response, StatusCode>
where
    I: AsRef<A> + Send + Sync,
    A: Api,
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
        .delete_user(method, host, cookies, path_params)
        .await;

    let mut response = Response::builder();

    let resp = match result {
        Ok(rsp) => match rsp {
            DeleteUserResponse::Status400_InvalidUsernameSupplied => {
                let mut response = response.status(400);
                response.body(Body::empty())
            }
            DeleteUserResponse::Status404_UserNotFound => {
                let mut response = response.status(404);
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
fn get_user_by_name_validation(
    path_params: models::GetUserByNamePathParams,
) -> std::result::Result<(models::GetUserByNamePathParams,), ValidationErrors> {
    path_params.validate()?;

    Ok((path_params,))
}
/// GetUserByName - GET /v2/user/{username}
#[tracing::instrument(skip_all)]
async fn get_user_by_name<I, A>(
    method: Method,
    host: Host,
    cookies: CookieJar,
    Path(path_params): Path<models::GetUserByNamePathParams>,
    State(api_impl): State<I>,
) -> Result<Response, StatusCode>
where
    I: AsRef<A> + Send + Sync,
    A: Api,
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
        .get_user_by_name(method, host, cookies, path_params)
        .await;

    let mut response = Response::builder();

    let resp = match result {
        Ok(rsp) => match rsp {
            GetUserByNameResponse::Status200_SuccessfulOperation(body) => {
                let mut response = response.status(200);
                {
                    let mut response_headers = response.headers_mut().unwrap();
                    response_headers.insert(
                        CONTENT_TYPE,
                        HeaderValue::from_str("text/plain").map_err(|e| {
                            error!(error = ?e);
                            StatusCode::INTERNAL_SERVER_ERROR
                        })?,
                    );
                }

                let body_content = body;
                response.body(Body::from(body_content))
            }
            GetUserByNameResponse::Status400_InvalidUsernameSupplied => {
                let mut response = response.status(400);
                response.body(Body::empty())
            }
            GetUserByNameResponse::Status404_UserNotFound => {
                let mut response = response.status(404);
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
fn login_user_validation(
    query_params: models::LoginUserQueryParams,
) -> std::result::Result<(models::LoginUserQueryParams,), ValidationErrors> {
    query_params.validate()?;

    Ok((query_params,))
}
/// LoginUser - GET /v2/user/login
#[tracing::instrument(skip_all)]
async fn login_user<I, A>(
    method: Method,
    host: Host,
    cookies: CookieJar,
    Query(query_params): Query<models::LoginUserQueryParams>,
    State(api_impl): State<I>,
) -> Result<Response, StatusCode>
where
    I: AsRef<A> + Send + Sync,
    A: Api,
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
        .login_user(method, host, cookies, query_params)
        .await;

    let mut response = Response::builder();

    let resp = match result {
        Ok(rsp) => match rsp {
            LoginUserResponse::Status200_SuccessfulOperation {
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
                                                                    .body(Body::from(format!("An internal server error occurred handling x_rate_limit header - {}", e))).map_err(|e| { error!(error = ?e); StatusCode::INTERNAL_SERVER_ERROR });
                        }
                    };

                    {
                        let mut response_headers = response.headers_mut().unwrap();
                        response_headers.insert(HeaderName::from_static(""), x_rate_limit);
                    }
                }
                if let Some(x_expires_after) = x_expires_after {
                    let x_expires_after = match header::IntoHeaderValue(x_expires_after).try_into()
                    {
                        Ok(val) => val,
                        Err(e) => {
                            return Response::builder()
                                                                    .status(StatusCode::INTERNAL_SERVER_ERROR)
                                                                    .body(Body::from(format!("An internal server error occurred handling x_expires_after header - {}", e))).map_err(|e| { error!(error = ?e); StatusCode::INTERNAL_SERVER_ERROR });
                        }
                    };

                    {
                        let mut response_headers = response.headers_mut().unwrap();
                        response_headers.insert(HeaderName::from_static(""), x_expires_after);
                    }
                }

                let mut response = response.status(200);
                {
                    let mut response_headers = response.headers_mut().unwrap();
                    response_headers.insert(
                        CONTENT_TYPE,
                        HeaderValue::from_str("text/plain").map_err(|e| {
                            error!(error = ?e);
                            StatusCode::INTERNAL_SERVER_ERROR
                        })?,
                    );
                }

                let body_content = body;
                response.body(Body::from(body_content))
            }
            LoginUserResponse::Status400_InvalidUsername => {
                let mut response = response.status(400);
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
fn logout_user_validation() -> std::result::Result<(), ValidationErrors> {
    Ok(())
}
/// LogoutUser - GET /v2/user/logout
#[tracing::instrument(skip_all)]
async fn logout_user<I, A>(
    method: Method,
    host: Host,
    cookies: CookieJar,
    State(api_impl): State<I>,
) -> Result<Response, StatusCode>
where
    I: AsRef<A> + Send + Sync,
    A: Api,
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

    let result = api_impl.as_ref().logout_user(method, host, cookies).await;

    let mut response = Response::builder();

    let resp = match result {
        Ok(rsp) => match rsp {
            LogoutUserResponse::Status0_SuccessfulOperation => {
                let mut response = response.status(0);
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
async fn update_user<I, A>(
    method: Method,
    host: Host,
    cookies: CookieJar,
    Path(path_params): Path<models::UpdateUserPathParams>,
    State(api_impl): State<I>,
    Json(body): Json<models::User>,
) -> Result<Response, StatusCode>
where
    I: AsRef<A> + Send + Sync,
    A: Api,
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
        .update_user(method, host, cookies, path_params, body)
        .await;

    let mut response = Response::builder();

    let resp = match result {
        Ok(rsp) => match rsp {
            UpdateUserResponse::Status400_InvalidUserSupplied => {
                let mut response = response.status(400);
                response.body(Body::empty())
            }
            UpdateUserResponse::Status404_UserNotFound => {
                let mut response = response.status(404);
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
