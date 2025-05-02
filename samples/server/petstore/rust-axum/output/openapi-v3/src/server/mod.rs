use std::collections::HashMap;

use axum::{body::Body, extract::*, response::Response, routing::*};
use axum_extra::extract::{CookieJar, Host, Query as QueryExtra};
use bytes::Bytes;
use http::{header::CONTENT_TYPE, HeaderMap, HeaderName, HeaderValue, Method, StatusCode};
use tracing::error;
use validator::{Validate, ValidationErrors};

use crate::{header, types::*};

#[allow(unused_imports)]
use crate::{apis, models};

/// Setup API Server.
pub fn new<I, A, E>(api_impl: I) -> Router
where
    I: AsRef<A> + Clone + Send + Sync + 'static,
    A: apis::default::Default<E>
        + apis::info_repo::InfoRepo<E>
        + apis::repo::Repo<E>
        + Send
        + Sync
        + 'static,
    E: std::fmt::Debug + Send + Sync + 'static,
{
    // build our application with a route
    Router::new()
        .route("/any-of",
            get(any_of_get::<I, A, E>)
        )
        .route("/callback-with-header",
            post(callback_with_header_post::<I, A, E>)
        )
        .route("/complex-query-param",
            get(complex_query_param_get::<I, A, E>)
        )
        .route("/enum_in_path/{path_param}",
            get(enum_in_path_path_param_get::<I, A, E>)
        )
        .route("/form-test",
            post(form_test::<I, A, E>)
        )
        .route("/get-with-bool",
            get(get_with_boolean_parameter::<I, A, E>)
        )
        .route("/json-complex-query-param",
            get(json_complex_query_param_get::<I, A, E>)
        )
        .route("/mandatory-request-header",
            get(mandatory_request_header_get::<I, A, E>)
        )
        .route("/merge-patch-json",
            get(merge_patch_json_get::<I, A, E>)
        )
        .route("/multiget",
            get(multiget_get::<I, A, E>)
        )
        .route("/multiple-path-params-with-very-long-path-to-test-formatting/{path_param_a}/{path_param_b}",
            get(multiple_path_params_with_very_long_path_to_test_formatting_path_param_a_path_param_b_get::<I, A, E>)
        )
        .route("/multiple_auth_scheme",
            get(multiple_auth_scheme_get::<I, A, E>)
        )
        .route("/one-of",
            get(one_of_get::<I, A, E>)
        )
        .route("/operation-two-first-letter-headers",
            post(two_first_letter_headers::<I, A, E>)
        )
        .route("/override-server",
            get(override_server_get::<I, A, E>)
        )
        .route("/paramget",
            get(paramget_get::<I, A, E>)
        )
        .route("/readonly_auth_scheme",
            get(readonly_auth_scheme_get::<I, A, E>)
        )
        .route("/register-callback",
            post(register_callback_post::<I, A, E>)
        )
        .route("/repos",
            post(create_repo::<I, A, E>)
        )
        .route("/repos/{repo_id}",
            get(get_repo_info::<I, A, E>).get(get_repo_info::<I, A, E>)
        )
        .route("/required_octet_stream",
            put(required_octet_stream_put::<I, A, E>)
        )
        .route("/responses_with_headers",
            get(responses_with_headers_get::<I, A, E>)
        )
        .route("/rfc7807",
            get(rfc7807_get::<I, A, E>)
        )
        .route("/untyped_property",
            get(untyped_property_get::<I, A, E>)
        )
        .route("/uuid",
            get(uuid_get::<I, A, E>)
        )
        .route("/xml",
            post(xml_post::<I, A, E>).put(xml_put::<I, A, E>)
        )
        .route("/xml_extra",
            post(xml_extra_post::<I, A, E>)
        )
        .route("/xml_other",
            post(xml_other_post::<I, A, E>).put(xml_other_put::<I, A, E>)
        )
        .with_state(api_impl)
}

#[tracing::instrument(skip_all)]
fn any_of_get_validation(
    query_params: models::AnyOfGetQueryParams,
) -> std::result::Result<(models::AnyOfGetQueryParams,), ValidationErrors> {
    query_params.validate()?;

    Ok((query_params,))
}
/// AnyOfGet - GET /any-of
#[tracing::instrument(skip_all)]
async fn any_of_get<I, A, E>(
    method: Method,
    host: Host,
    cookies: CookieJar,
    QueryExtra(query_params): QueryExtra<models::AnyOfGetQueryParams>,
    State(api_impl): State<I>,
) -> Result<Response, StatusCode>
where
    I: AsRef<A> + Send + Sync,
    A: apis::default::Default<E> + Send + Sync,
    E: std::fmt::Debug + Send + Sync + 'static,
{
    let validation = any_of_get_validation(query_params);

    let Ok((query_params,)) = validation else {
        return Response::builder()
            .status(StatusCode::BAD_REQUEST)
            .body(Body::from(validation.unwrap_err().to_string()))
            .map_err(|_| StatusCode::BAD_REQUEST);
    };

    let result = api_impl
        .as_ref()
        .any_of_get(&method, &host, &cookies, &query_params)
        .await;

    let mut response = Response::builder();

    let resp = match result {
        Ok(rsp) => match rsp {
            apis::default::AnyOfGetResponse::Status200_Success(body) => {
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
            apis::default::AnyOfGetResponse::Status201_AlternateSuccess(body) => {
                let mut response = response.status(201);
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
            apis::default::AnyOfGetResponse::Status202_AnyOfSuccess(body) => {
                let mut response = response.status(202);
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
fn callback_with_header_post_validation(
    query_params: models::CallbackWithHeaderPostQueryParams,
) -> std::result::Result<(models::CallbackWithHeaderPostQueryParams,), ValidationErrors> {
    query_params.validate()?;

    Ok((query_params,))
}
/// CallbackWithHeaderPost - POST /callback-with-header
#[tracing::instrument(skip_all)]
async fn callback_with_header_post<I, A, E>(
    method: Method,
    host: Host,
    cookies: CookieJar,
    QueryExtra(query_params): QueryExtra<models::CallbackWithHeaderPostQueryParams>,
    State(api_impl): State<I>,
) -> Result<Response, StatusCode>
where
    I: AsRef<A> + Send + Sync,
    A: apis::default::Default<E> + Send + Sync,
    E: std::fmt::Debug + Send + Sync + 'static,
{
    let validation = callback_with_header_post_validation(query_params);

    let Ok((query_params,)) = validation else {
        return Response::builder()
            .status(StatusCode::BAD_REQUEST)
            .body(Body::from(validation.unwrap_err().to_string()))
            .map_err(|_| StatusCode::BAD_REQUEST);
    };

    let result = api_impl
        .as_ref()
        .callback_with_header_post(&method, &host, &cookies, &query_params)
        .await;

    let mut response = Response::builder();

    let resp = match result {
        Ok(rsp) => match rsp {
            apis::default::CallbackWithHeaderPostResponse::Status204_OK => {
                let mut response = response.status(204);
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
fn complex_query_param_get_validation(
    query_params: models::ComplexQueryParamGetQueryParams,
) -> std::result::Result<(models::ComplexQueryParamGetQueryParams,), ValidationErrors> {
    query_params.validate()?;

    Ok((query_params,))
}
/// ComplexQueryParamGet - GET /complex-query-param
#[tracing::instrument(skip_all)]
async fn complex_query_param_get<I, A, E>(
    method: Method,
    host: Host,
    cookies: CookieJar,
    QueryExtra(query_params): QueryExtra<models::ComplexQueryParamGetQueryParams>,
    State(api_impl): State<I>,
) -> Result<Response, StatusCode>
where
    I: AsRef<A> + Send + Sync,
    A: apis::default::Default<E> + Send + Sync,
    E: std::fmt::Debug + Send + Sync + 'static,
{
    let validation = complex_query_param_get_validation(query_params);

    let Ok((query_params,)) = validation else {
        return Response::builder()
            .status(StatusCode::BAD_REQUEST)
            .body(Body::from(validation.unwrap_err().to_string()))
            .map_err(|_| StatusCode::BAD_REQUEST);
    };

    let result = api_impl
        .as_ref()
        .complex_query_param_get(&method, &host, &cookies, &query_params)
        .await;

    let mut response = Response::builder();

    let resp = match result {
        Ok(rsp) => match rsp {
            apis::default::ComplexQueryParamGetResponse::Status200_Success => {
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
fn enum_in_path_path_param_get_validation(
    path_params: models::EnumInPathPathParamGetPathParams,
) -> std::result::Result<(models::EnumInPathPathParamGetPathParams,), ValidationErrors> {
    path_params.validate()?;

    Ok((path_params,))
}
/// EnumInPathPathParamGet - GET /enum_in_path/{path_param}
#[tracing::instrument(skip_all)]
async fn enum_in_path_path_param_get<I, A, E>(
    method: Method,
    host: Host,
    cookies: CookieJar,
    Path(path_params): Path<models::EnumInPathPathParamGetPathParams>,
    State(api_impl): State<I>,
) -> Result<Response, StatusCode>
where
    I: AsRef<A> + Send + Sync,
    A: apis::default::Default<E> + Send + Sync,
    E: std::fmt::Debug + Send + Sync + 'static,
{
    let validation = enum_in_path_path_param_get_validation(path_params);

    let Ok((path_params,)) = validation else {
        return Response::builder()
            .status(StatusCode::BAD_REQUEST)
            .body(Body::from(validation.unwrap_err().to_string()))
            .map_err(|_| StatusCode::BAD_REQUEST);
    };

    let result = api_impl
        .as_ref()
        .enum_in_path_path_param_get(&method, &host, &cookies, &path_params)
        .await;

    let mut response = Response::builder();

    let resp = match result {
        Ok(rsp) => match rsp {
            apis::default::EnumInPathPathParamGetResponse::Status200_Success => {
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
struct FormTestBodyValidator<'a> {
    #[validate(nested)]
    body: &'a models::FormTestRequest,
}

#[tracing::instrument(skip_all)]
fn form_test_validation(
    body: models::FormTestRequest,
) -> std::result::Result<(models::FormTestRequest,), ValidationErrors> {
    let b = FormTestBodyValidator { body: &body };
    b.validate()?;

    Ok((body,))
}
/// FormTest - POST /form-test
#[tracing::instrument(skip_all)]
async fn form_test<I, A, E>(
    method: Method,
    host: Host,
    cookies: CookieJar,
    State(api_impl): State<I>,
    Form(body): Form<models::FormTestRequest>,
) -> Result<Response, StatusCode>
where
    I: AsRef<A> + Send + Sync,
    A: apis::default::Default<E> + Send + Sync,
    E: std::fmt::Debug + Send + Sync + 'static,
{
    let validation = form_test_validation(body);

    let Ok((body,)) = validation else {
        return Response::builder()
            .status(StatusCode::BAD_REQUEST)
            .body(Body::from(validation.unwrap_err().to_string()))
            .map_err(|_| StatusCode::BAD_REQUEST);
    };

    let result = api_impl
        .as_ref()
        .form_test(&method, &host, &cookies, &body)
        .await;

    let mut response = Response::builder();

    let resp = match result {
        Ok(rsp) => match rsp {
            apis::default::FormTestResponse::Status200_OK => {
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
fn get_with_boolean_parameter_validation(
    query_params: models::GetWithBooleanParameterQueryParams,
) -> std::result::Result<(models::GetWithBooleanParameterQueryParams,), ValidationErrors> {
    query_params.validate()?;

    Ok((query_params,))
}
/// GetWithBooleanParameter - GET /get-with-bool
#[tracing::instrument(skip_all)]
async fn get_with_boolean_parameter<I, A, E>(
    method: Method,
    host: Host,
    cookies: CookieJar,
    QueryExtra(query_params): QueryExtra<models::GetWithBooleanParameterQueryParams>,
    State(api_impl): State<I>,
) -> Result<Response, StatusCode>
where
    I: AsRef<A> + Send + Sync,
    A: apis::default::Default<E> + Send + Sync,
    E: std::fmt::Debug + Send + Sync + 'static,
{
    let validation = get_with_boolean_parameter_validation(query_params);

    let Ok((query_params,)) = validation else {
        return Response::builder()
            .status(StatusCode::BAD_REQUEST)
            .body(Body::from(validation.unwrap_err().to_string()))
            .map_err(|_| StatusCode::BAD_REQUEST);
    };

    let result = api_impl
        .as_ref()
        .get_with_boolean_parameter(&method, &host, &cookies, &query_params)
        .await;

    let mut response = Response::builder();

    let resp = match result {
        Ok(rsp) => match rsp {
            apis::default::GetWithBooleanParameterResponse::Status200_OK => {
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
fn json_complex_query_param_get_validation(
    query_params: models::JsonComplexQueryParamGetQueryParams,
) -> std::result::Result<(models::JsonComplexQueryParamGetQueryParams,), ValidationErrors> {
    query_params.validate()?;

    Ok((query_params,))
}
/// JsonComplexQueryParamGet - GET /json-complex-query-param
#[tracing::instrument(skip_all)]
async fn json_complex_query_param_get<I, A, E>(
    method: Method,
    host: Host,
    cookies: CookieJar,
    QueryExtra(query_params): QueryExtra<models::JsonComplexQueryParamGetQueryParams>,
    State(api_impl): State<I>,
) -> Result<Response, StatusCode>
where
    I: AsRef<A> + Send + Sync,
    A: apis::default::Default<E> + Send + Sync,
    E: std::fmt::Debug + Send + Sync + 'static,
{
    let validation = json_complex_query_param_get_validation(query_params);

    let Ok((query_params,)) = validation else {
        return Response::builder()
            .status(StatusCode::BAD_REQUEST)
            .body(Body::from(validation.unwrap_err().to_string()))
            .map_err(|_| StatusCode::BAD_REQUEST);
    };

    let result = api_impl
        .as_ref()
        .json_complex_query_param_get(&method, &host, &cookies, &query_params)
        .await;

    let mut response = Response::builder();

    let resp = match result {
        Ok(rsp) => match rsp {
            apis::default::JsonComplexQueryParamGetResponse::Status200_Success => {
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
fn mandatory_request_header_get_validation(
    header_params: models::MandatoryRequestHeaderGetHeaderParams,
) -> std::result::Result<(models::MandatoryRequestHeaderGetHeaderParams,), ValidationErrors> {
    header_params.validate()?;

    Ok((header_params,))
}
/// MandatoryRequestHeaderGet - GET /mandatory-request-header
#[tracing::instrument(skip_all)]
async fn mandatory_request_header_get<I, A, E>(
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
        let header_x_header = headers.get(HeaderName::from_static("x-header"));

        let header_x_header = match header_x_header {
            Some(v) => match header::IntoHeaderValue::<String>::try_from((*v).clone()) {
                Ok(result) => result.0,
                Err(err) => {
                    return Response::builder()
                        .status(StatusCode::BAD_REQUEST)
                        .body(Body::from(format!("Invalid header X-Header - {}", err)))
                        .map_err(|e| {
                            error!(error = ?e);
                            StatusCode::INTERNAL_SERVER_ERROR
                        });
                }
            },
            None => {
                return Response::builder()
                    .status(StatusCode::BAD_REQUEST)
                    .body(Body::from("Missing required header X-Header"))
                    .map_err(|e| {
                        error!(error = ?e);
                        StatusCode::INTERNAL_SERVER_ERROR
                    });
            }
        };

        models::MandatoryRequestHeaderGetHeaderParams {
            x_header: header_x_header,
        }
    };

    let validation = mandatory_request_header_get_validation(header_params);

    let Ok((header_params,)) = validation else {
        return Response::builder()
            .status(StatusCode::BAD_REQUEST)
            .body(Body::from(validation.unwrap_err().to_string()))
            .map_err(|_| StatusCode::BAD_REQUEST);
    };

    let result = api_impl
        .as_ref()
        .mandatory_request_header_get(&method, &host, &cookies, &header_params)
        .await;

    let mut response = Response::builder();

    let resp = match result {
        Ok(rsp) => match rsp {
            apis::default::MandatoryRequestHeaderGetResponse::Status200_Success => {
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
fn merge_patch_json_get_validation() -> std::result::Result<(), ValidationErrors> {
    Ok(())
}
/// MergePatchJsonGet - GET /merge-patch-json
#[tracing::instrument(skip_all)]
async fn merge_patch_json_get<I, A, E>(
    method: Method,
    host: Host,
    cookies: CookieJar,
    State(api_impl): State<I>,
) -> Result<Response, StatusCode>
where
    I: AsRef<A> + Send + Sync,
    A: apis::default::Default<E> + Send + Sync,
    E: std::fmt::Debug + Send + Sync + 'static,
{
    let validation = merge_patch_json_get_validation();

    let Ok(()) = validation else {
        return Response::builder()
            .status(StatusCode::BAD_REQUEST)
            .body(Body::from(validation.unwrap_err().to_string()))
            .map_err(|_| StatusCode::BAD_REQUEST);
    };

    let result = api_impl
        .as_ref()
        .merge_patch_json_get(&method, &host, &cookies)
        .await;

    let mut response = Response::builder();

    let resp = match result {
        Ok(rsp) => match rsp {
            apis::default::MergePatchJsonGetResponse::Status200_Merge(body) => {
                let mut response = response.status(200);
                {
                    let mut response_headers = response.headers_mut().unwrap();
                    response_headers.insert(
                        CONTENT_TYPE,
                        HeaderValue::from_str("application/merge-patch+json").map_err(|e| {
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
fn multiget_get_validation() -> std::result::Result<(), ValidationErrors> {
    Ok(())
}
/// MultigetGet - GET /multiget
#[tracing::instrument(skip_all)]
async fn multiget_get<I, A, E>(
    method: Method,
    host: Host,
    cookies: CookieJar,
    State(api_impl): State<I>,
) -> Result<Response, StatusCode>
where
    I: AsRef<A> + Send + Sync,
    A: apis::default::Default<E> + Send + Sync,
    E: std::fmt::Debug + Send + Sync + 'static,
{
    let validation = multiget_get_validation();

    let Ok(()) = validation else {
        return Response::builder()
            .status(StatusCode::BAD_REQUEST)
            .body(Body::from(validation.unwrap_err().to_string()))
            .map_err(|_| StatusCode::BAD_REQUEST);
    };

    let result = api_impl
        .as_ref()
        .multiget_get(&method, &host, &cookies)
        .await;

    let mut response = Response::builder();

    let resp = match result {
        Ok(rsp) => match rsp {
            apis::default::MultigetGetResponse::Status200_JSONRsp(body) => {
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
            apis::default::MultigetGetResponse::Status201_XMLRsp(body) => {
                let mut response = response.status(201);
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
            apis::default::MultigetGetResponse::Status202_OctetRsp(body) => {
                let mut response = response.status(202);
                {
                    let mut response_headers = response.headers_mut().unwrap();
                    response_headers.insert(
                        CONTENT_TYPE,
                        HeaderValue::from_str("application/octet-stream").map_err(|e| {
                            error!(error = ?e);
                            StatusCode::INTERNAL_SERVER_ERROR
                        })?,
                    );
                }

                let body_content = body.0;
                response.body(Body::from(body_content))
            }
            apis::default::MultigetGetResponse::Status203_StringRsp(body) => {
                let mut response = response.status(203);
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
            apis::default::MultigetGetResponse::Status204_DuplicateResponseLongText(body) => {
                let mut response = response.status(204);
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
            apis::default::MultigetGetResponse::Status205_DuplicateResponseLongText(body) => {
                let mut response = response.status(205);
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
            apis::default::MultigetGetResponse::Status206_DuplicateResponseLongText(body) => {
                let mut response = response.status(206);
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
fn multiple_auth_scheme_get_validation() -> std::result::Result<(), ValidationErrors> {
    Ok(())
}
/// MultipleAuthSchemeGet - GET /multiple_auth_scheme
#[tracing::instrument(skip_all)]
async fn multiple_auth_scheme_get<I, A, E>(
    method: Method,
    host: Host,
    cookies: CookieJar,
    State(api_impl): State<I>,
) -> Result<Response, StatusCode>
where
    I: AsRef<A> + Send + Sync,
    A: apis::default::Default<E> + Send + Sync,
    E: std::fmt::Debug + Send + Sync + 'static,
{
    let validation = multiple_auth_scheme_get_validation();

    let Ok(()) = validation else {
        return Response::builder()
            .status(StatusCode::BAD_REQUEST)
            .body(Body::from(validation.unwrap_err().to_string()))
            .map_err(|_| StatusCode::BAD_REQUEST);
    };

    let result = api_impl
        .as_ref()
        .multiple_auth_scheme_get(&method, &host, &cookies)
        .await;

    let mut response = Response::builder();

    let resp = match result {
                                            Ok(rsp) => match rsp {
                                                apis::default::MultipleAuthSchemeGetResponse::Status200_CheckThatLimitingToMultipleRequiredAuthSchemesWorks
                                                => {
                                                  let mut response = response.status(200);
                                                  response.body(Body::empty())
                                                },
                                            },
                                            Err(why) => {
                                                // Application code returned an error. This should not happen, as the implementation should
                                                // return a valid response.
                                                return api_impl.as_ref().handle_error(&method, &host, &cookies, why).await;
                                            },
                                        };

    resp.map_err(|e| {
        error!(error = ?e);
        StatusCode::INTERNAL_SERVER_ERROR
    })
}

#[tracing::instrument(skip_all)]
fn multiple_path_params_with_very_long_path_to_test_formatting_path_param_a_path_param_b_get_validation(
    path_params: models::MultiplePathParamsWithVeryLongPathToTestFormattingPathParamAPathParamBGetPathParams,
) -> std::result::Result<
    (models::MultiplePathParamsWithVeryLongPathToTestFormattingPathParamAPathParamBGetPathParams,),
    ValidationErrors,
> {
    path_params.validate()?;

    Ok((path_params,))
}
/// MultiplePathParamsWithVeryLongPathToTestFormattingPathParamAPathParamBGet - GET /multiple-path-params-with-very-long-path-to-test-formatting/{path_param_a}/{path_param_b}
#[tracing::instrument(skip_all)]
async fn multiple_path_params_with_very_long_path_to_test_formatting_path_param_a_path_param_b_get<
    I,
    A,
    E,
>(
    method: Method,
    host: Host,
    cookies: CookieJar,
    Path(path_params): Path<
        models::MultiplePathParamsWithVeryLongPathToTestFormattingPathParamAPathParamBGetPathParams,
    >,
    State(api_impl): State<I>,
) -> Result<Response, StatusCode>
where
    I: AsRef<A> + Send + Sync,
    A: apis::default::Default<E> + Send + Sync,
    E: std::fmt::Debug + Send + Sync + 'static,
{
    let validation =
    multiple_path_params_with_very_long_path_to_test_formatting_path_param_a_path_param_b_get_validation(
        path_params,
    )
  ;

    let Ok((path_params,)) = validation else {
        return Response::builder()
            .status(StatusCode::BAD_REQUEST)
            .body(Body::from(validation.unwrap_err().to_string()))
            .map_err(|_| StatusCode::BAD_REQUEST);
    };

    let result = api_impl
        .as_ref()
        .multiple_path_params_with_very_long_path_to_test_formatting_path_param_a_path_param_b_get(
            &method,
            &host,
            &cookies,
            &path_params,
        )
        .await;

    let mut response = Response::builder();

    let resp = match result {
                                            Ok(rsp) => match rsp {
                                                apis::default::MultiplePathParamsWithVeryLongPathToTestFormattingPathParamAPathParamBGetResponse::Status200_Success
                                                => {
                                                  let mut response = response.status(200);
                                                  response.body(Body::empty())
                                                },
                                            },
                                            Err(why) => {
                                                // Application code returned an error. This should not happen, as the implementation should
                                                // return a valid response.
                                                return api_impl.as_ref().handle_error(&method, &host, &cookies, why).await;
                                            },
                                        };

    resp.map_err(|e| {
        error!(error = ?e);
        StatusCode::INTERNAL_SERVER_ERROR
    })
}

#[tracing::instrument(skip_all)]
fn one_of_get_validation() -> std::result::Result<(), ValidationErrors> {
    Ok(())
}
/// OneOfGet - GET /one-of
#[tracing::instrument(skip_all)]
async fn one_of_get<I, A, E>(
    method: Method,
    host: Host,
    cookies: CookieJar,
    State(api_impl): State<I>,
) -> Result<Response, StatusCode>
where
    I: AsRef<A> + Send + Sync,
    A: apis::default::Default<E> + Send + Sync,
    E: std::fmt::Debug + Send + Sync + 'static,
{
    let validation = one_of_get_validation();

    let Ok(()) = validation else {
        return Response::builder()
            .status(StatusCode::BAD_REQUEST)
            .body(Body::from(validation.unwrap_err().to_string()))
            .map_err(|_| StatusCode::BAD_REQUEST);
    };

    let result = api_impl.as_ref().one_of_get(&method, &host, &cookies).await;

    let mut response = Response::builder();

    let resp = match result {
        Ok(rsp) => match rsp {
            apis::default::OneOfGetResponse::Status200_Success(body) => {
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
fn override_server_get_validation() -> std::result::Result<(), ValidationErrors> {
    Ok(())
}
/// OverrideServerGet - GET /override-server
#[tracing::instrument(skip_all)]
async fn override_server_get<I, A, E>(
    method: Method,
    host: Host,
    cookies: CookieJar,
    State(api_impl): State<I>,
) -> Result<Response, StatusCode>
where
    I: AsRef<A> + Send + Sync,
    A: apis::default::Default<E> + Send + Sync,
    E: std::fmt::Debug + Send + Sync + 'static,
{
    let validation = override_server_get_validation();

    let Ok(()) = validation else {
        return Response::builder()
            .status(StatusCode::BAD_REQUEST)
            .body(Body::from(validation.unwrap_err().to_string()))
            .map_err(|_| StatusCode::BAD_REQUEST);
    };

    let result = api_impl
        .as_ref()
        .override_server_get(&method, &host, &cookies)
        .await;

    let mut response = Response::builder();

    let resp = match result {
        Ok(rsp) => match rsp {
            apis::default::OverrideServerGetResponse::Status204_Success => {
                let mut response = response.status(204);
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
fn paramget_get_validation(
    query_params: models::ParamgetGetQueryParams,
) -> std::result::Result<(models::ParamgetGetQueryParams,), ValidationErrors> {
    query_params.validate()?;

    Ok((query_params,))
}
/// ParamgetGet - GET /paramget
#[tracing::instrument(skip_all)]
async fn paramget_get<I, A, E>(
    method: Method,
    host: Host,
    cookies: CookieJar,
    QueryExtra(query_params): QueryExtra<models::ParamgetGetQueryParams>,
    State(api_impl): State<I>,
) -> Result<Response, StatusCode>
where
    I: AsRef<A> + Send + Sync,
    A: apis::default::Default<E> + Send + Sync,
    E: std::fmt::Debug + Send + Sync + 'static,
{
    let validation = paramget_get_validation(query_params);

    let Ok((query_params,)) = validation else {
        return Response::builder()
            .status(StatusCode::BAD_REQUEST)
            .body(Body::from(validation.unwrap_err().to_string()))
            .map_err(|_| StatusCode::BAD_REQUEST);
    };

    let result = api_impl
        .as_ref()
        .paramget_get(&method, &host, &cookies, &query_params)
        .await;

    let mut response = Response::builder();

    let resp = match result {
        Ok(rsp) => match rsp {
            apis::default::ParamgetGetResponse::Status200_JSONRsp(body) => {
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
fn readonly_auth_scheme_get_validation() -> std::result::Result<(), ValidationErrors> {
    Ok(())
}
/// ReadonlyAuthSchemeGet - GET /readonly_auth_scheme
#[tracing::instrument(skip_all)]
async fn readonly_auth_scheme_get<I, A, E>(
    method: Method,
    host: Host,
    cookies: CookieJar,
    State(api_impl): State<I>,
) -> Result<Response, StatusCode>
where
    I: AsRef<A> + Send + Sync,
    A: apis::default::Default<E> + Send + Sync,
    E: std::fmt::Debug + Send + Sync + 'static,
{
    let validation = readonly_auth_scheme_get_validation();

    let Ok(()) = validation else {
        return Response::builder()
            .status(StatusCode::BAD_REQUEST)
            .body(Body::from(validation.unwrap_err().to_string()))
            .map_err(|_| StatusCode::BAD_REQUEST);
    };

    let result = api_impl
        .as_ref()
        .readonly_auth_scheme_get(&method, &host, &cookies)
        .await;

    let mut response = Response::builder();

    let resp = match result {
                                            Ok(rsp) => match rsp {
                                                apis::default::ReadonlyAuthSchemeGetResponse::Status200_CheckThatLimitingToASingleRequiredAuthSchemeWorks
                                                => {
                                                  let mut response = response.status(200);
                                                  response.body(Body::empty())
                                                },
                                            },
                                            Err(why) => {
                                                // Application code returned an error. This should not happen, as the implementation should
                                                // return a valid response.
                                                return api_impl.as_ref().handle_error(&method, &host, &cookies, why).await;
                                            },
                                        };

    resp.map_err(|e| {
        error!(error = ?e);
        StatusCode::INTERNAL_SERVER_ERROR
    })
}

#[tracing::instrument(skip_all)]
fn register_callback_post_validation(
    query_params: models::RegisterCallbackPostQueryParams,
) -> std::result::Result<(models::RegisterCallbackPostQueryParams,), ValidationErrors> {
    query_params.validate()?;

    Ok((query_params,))
}
/// RegisterCallbackPost - POST /register-callback
#[tracing::instrument(skip_all)]
async fn register_callback_post<I, A, E>(
    method: Method,
    host: Host,
    cookies: CookieJar,
    QueryExtra(query_params): QueryExtra<models::RegisterCallbackPostQueryParams>,
    State(api_impl): State<I>,
) -> Result<Response, StatusCode>
where
    I: AsRef<A> + Send + Sync,
    A: apis::default::Default<E> + Send + Sync,
    E: std::fmt::Debug + Send + Sync + 'static,
{
    let validation = register_callback_post_validation(query_params);

    let Ok((query_params,)) = validation else {
        return Response::builder()
            .status(StatusCode::BAD_REQUEST)
            .body(Body::from(validation.unwrap_err().to_string()))
            .map_err(|_| StatusCode::BAD_REQUEST);
    };

    let result = api_impl
        .as_ref()
        .register_callback_post(&method, &host, &cookies, &query_params)
        .await;

    let mut response = Response::builder();

    let resp = match result {
        Ok(rsp) => match rsp {
            apis::default::RegisterCallbackPostResponse::Status204_OK => {
                let mut response = response.status(204);
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
struct RequiredOctetStreamPutBodyValidator<'a> {
    body: &'a [u8],
}

#[tracing::instrument(skip_all)]
fn required_octet_stream_put_validation(
    body: Bytes,
) -> std::result::Result<(Bytes,), ValidationErrors> {
    Ok((body,))
}
/// RequiredOctetStreamPut - PUT /required_octet_stream
#[tracing::instrument(skip_all)]
async fn required_octet_stream_put<I, A, E>(
    method: Method,
    host: Host,
    cookies: CookieJar,
    State(api_impl): State<I>,
    body: Bytes,
) -> Result<Response, StatusCode>
where
    I: AsRef<A> + Send + Sync,
    A: apis::default::Default<E> + Send + Sync,
    E: std::fmt::Debug + Send + Sync + 'static,
{
    let validation = required_octet_stream_put_validation(body);

    let Ok((body,)) = validation else {
        return Response::builder()
            .status(StatusCode::BAD_REQUEST)
            .body(Body::from(validation.unwrap_err().to_string()))
            .map_err(|_| StatusCode::BAD_REQUEST);
    };

    let result = api_impl
        .as_ref()
        .required_octet_stream_put(&method, &host, &cookies, &body)
        .await;

    let mut response = Response::builder();

    let resp = match result {
        Ok(rsp) => match rsp {
            apis::default::RequiredOctetStreamPutResponse::Status200_OK => {
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
fn responses_with_headers_get_validation() -> std::result::Result<(), ValidationErrors> {
    Ok(())
}
/// ResponsesWithHeadersGet - GET /responses_with_headers
#[tracing::instrument(skip_all)]
async fn responses_with_headers_get<I, A, E>(
    method: Method,
    host: Host,
    cookies: CookieJar,
    State(api_impl): State<I>,
) -> Result<Response, StatusCode>
where
    I: AsRef<A> + Send + Sync,
    A: apis::default::Default<E> + Send + Sync,
    E: std::fmt::Debug + Send + Sync + 'static,
{
    let validation = responses_with_headers_get_validation();

    let Ok(()) = validation else {
        return Response::builder()
            .status(StatusCode::BAD_REQUEST)
            .body(Body::from(validation.unwrap_err().to_string()))
            .map_err(|_| StatusCode::BAD_REQUEST);
    };

    let result = api_impl
        .as_ref()
        .responses_with_headers_get(&method, &host, &cookies)
        .await;

    let mut response = Response::builder();

    let resp = match result {
        Ok(rsp) => match rsp {
            apis::default::ResponsesWithHeadersGetResponse::Status200_Success {
                body,
                success_info,
                bool_header,
                object_header,
            } => {
                let success_info = match header::IntoHeaderValue(success_info).try_into() {
                    Ok(val) => val,
                    Err(e) => {
                        return Response::builder()
                                                                    .status(StatusCode::INTERNAL_SERVER_ERROR)
                                                                    .body(Body::from(format!("An internal server error occurred handling success_info header - {}", e))).map_err(|e| { error!(error = ?e); StatusCode::INTERNAL_SERVER_ERROR });
                    }
                };

                {
                    let mut response_headers = response.headers_mut().unwrap();
                    response_headers.insert(HeaderName::from_static("success-info"), success_info);
                }
                if let Some(bool_header) = bool_header {
                    let bool_header = match header::IntoHeaderValue(bool_header).try_into() {
                        Ok(val) => val,
                        Err(e) => {
                            return Response::builder()
                                                                    .status(StatusCode::INTERNAL_SERVER_ERROR)
                                                                    .body(Body::from(format!("An internal server error occurred handling bool_header header - {}", e))).map_err(|e| { error!(error = ?e); StatusCode::INTERNAL_SERVER_ERROR });
                        }
                    };

                    {
                        let mut response_headers = response.headers_mut().unwrap();
                        response_headers
                            .insert(HeaderName::from_static("bool-header"), bool_header);
                    }
                }
                if let Some(object_header) = object_header {
                    let object_header = match header::IntoHeaderValue(object_header).try_into() {
                        Ok(val) => val,
                        Err(e) => {
                            return Response::builder()
                                                                    .status(StatusCode::INTERNAL_SERVER_ERROR)
                                                                    .body(Body::from(format!("An internal server error occurred handling object_header header - {}", e))).map_err(|e| { error!(error = ?e); StatusCode::INTERNAL_SERVER_ERROR });
                        }
                    };

                    {
                        let mut response_headers = response.headers_mut().unwrap();
                        response_headers
                            .insert(HeaderName::from_static("object-header"), object_header);
                    }
                }
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
            apis::default::ResponsesWithHeadersGetResponse::Status412_PreconditionFailed {
                further_info,
                failure_info,
            } => {
                if let Some(further_info) = further_info {
                    let further_info = match header::IntoHeaderValue(further_info).try_into() {
                        Ok(val) => val,
                        Err(e) => {
                            return Response::builder()
                                                                    .status(StatusCode::INTERNAL_SERVER_ERROR)
                                                                    .body(Body::from(format!("An internal server error occurred handling further_info header - {}", e))).map_err(|e| { error!(error = ?e); StatusCode::INTERNAL_SERVER_ERROR });
                        }
                    };

                    {
                        let mut response_headers = response.headers_mut().unwrap();
                        response_headers
                            .insert(HeaderName::from_static("further-info"), further_info);
                    }
                }
                if let Some(failure_info) = failure_info {
                    let failure_info = match header::IntoHeaderValue(failure_info).try_into() {
                        Ok(val) => val,
                        Err(e) => {
                            return Response::builder()
                                                                    .status(StatusCode::INTERNAL_SERVER_ERROR)
                                                                    .body(Body::from(format!("An internal server error occurred handling failure_info header - {}", e))).map_err(|e| { error!(error = ?e); StatusCode::INTERNAL_SERVER_ERROR });
                        }
                    };

                    {
                        let mut response_headers = response.headers_mut().unwrap();
                        response_headers
                            .insert(HeaderName::from_static("failure-info"), failure_info);
                    }
                }
                let mut response = response.status(412);
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
fn rfc7807_get_validation() -> std::result::Result<(), ValidationErrors> {
    Ok(())
}
/// Rfc7807Get - GET /rfc7807
#[tracing::instrument(skip_all)]
async fn rfc7807_get<I, A, E>(
    method: Method,
    host: Host,
    cookies: CookieJar,
    State(api_impl): State<I>,
) -> Result<Response, StatusCode>
where
    I: AsRef<A> + Send + Sync,
    A: apis::default::Default<E> + Send + Sync,
    E: std::fmt::Debug + Send + Sync + 'static,
{
    let validation = rfc7807_get_validation();

    let Ok(()) = validation else {
        return Response::builder()
            .status(StatusCode::BAD_REQUEST)
            .body(Body::from(validation.unwrap_err().to_string()))
            .map_err(|_| StatusCode::BAD_REQUEST);
    };

    let result = api_impl
        .as_ref()
        .rfc7807_get(&method, &host, &cookies)
        .await;

    let mut response = Response::builder();

    let resp = match result {
        Ok(rsp) => match rsp {
            apis::default::Rfc7807GetResponse::Status204_OK(body) => {
                let mut response = response.status(204);
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
            apis::default::Rfc7807GetResponse::Status404_NotFound(body) => {
                let mut response = response.status(404);
                {
                    let mut response_headers = response.headers_mut().unwrap();
                    response_headers.insert(
                        CONTENT_TYPE,
                        HeaderValue::from_str("application/problem+json").map_err(|e| {
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
            apis::default::Rfc7807GetResponse::Status406_NotAcceptable(body) => {
                let mut response = response.status(406);
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
fn two_first_letter_headers_validation(
    header_params: models::TwoFirstLetterHeadersHeaderParams,
) -> std::result::Result<(models::TwoFirstLetterHeadersHeaderParams,), ValidationErrors> {
    header_params.validate()?;

    Ok((header_params,))
}
/// TwoFirstLetterHeaders - POST /operation-two-first-letter-headers
#[tracing::instrument(skip_all)]
async fn two_first_letter_headers<I, A, E>(
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
        let header_x_header_one = headers.get(HeaderName::from_static("x-header-one"));

        let header_x_header_one = match header_x_header_one {
            Some(v) => match header::IntoHeaderValue::<bool>::try_from((*v).clone()) {
                Ok(result) => Some(result.0),
                Err(err) => {
                    return Response::builder()
                        .status(StatusCode::BAD_REQUEST)
                        .body(Body::from(format!("Invalid header x-header-one - {}", err)))
                        .map_err(|e| {
                            error!(error = ?e);
                            StatusCode::INTERNAL_SERVER_ERROR
                        });
                }
            },
            None => None,
        };
        let header_x_header_two = headers.get(HeaderName::from_static("x-header-two"));

        let header_x_header_two = match header_x_header_two {
            Some(v) => match header::IntoHeaderValue::<bool>::try_from((*v).clone()) {
                Ok(result) => Some(result.0),
                Err(err) => {
                    return Response::builder()
                        .status(StatusCode::BAD_REQUEST)
                        .body(Body::from(format!("Invalid header x-header-two - {}", err)))
                        .map_err(|e| {
                            error!(error = ?e);
                            StatusCode::INTERNAL_SERVER_ERROR
                        });
                }
            },
            None => None,
        };

        models::TwoFirstLetterHeadersHeaderParams {
            x_header_one: header_x_header_one,
            x_header_two: header_x_header_two,
        }
    };

    let validation = two_first_letter_headers_validation(header_params);

    let Ok((header_params,)) = validation else {
        return Response::builder()
            .status(StatusCode::BAD_REQUEST)
            .body(Body::from(validation.unwrap_err().to_string()))
            .map_err(|_| StatusCode::BAD_REQUEST);
    };

    let result = api_impl
        .as_ref()
        .two_first_letter_headers(&method, &host, &cookies, &header_params)
        .await;

    let mut response = Response::builder();

    let resp = match result {
        Ok(rsp) => match rsp {
            apis::default::TwoFirstLetterHeadersResponse::Status200_OK => {
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
struct UntypedPropertyGetBodyValidator<'a> {
    #[validate(nested)]
    body: &'a models::ObjectUntypedProps,
}

#[tracing::instrument(skip_all)]
fn untyped_property_get_validation(
    body: Option<models::ObjectUntypedProps>,
) -> std::result::Result<(Option<models::ObjectUntypedProps>,), ValidationErrors> {
    if let Some(body) = &body {
        let b = UntypedPropertyGetBodyValidator { body };
        b.validate()?;
    }

    Ok((body,))
}
/// UntypedPropertyGet - GET /untyped_property
#[tracing::instrument(skip_all)]
async fn untyped_property_get<I, A, E>(
    method: Method,
    host: Host,
    cookies: CookieJar,
    State(api_impl): State<I>,
    Json(body): Json<Option<models::ObjectUntypedProps>>,
) -> Result<Response, StatusCode>
where
    I: AsRef<A> + Send + Sync,
    A: apis::default::Default<E> + Send + Sync,
    E: std::fmt::Debug + Send + Sync + 'static,
{
    let validation = untyped_property_get_validation(body);

    let Ok((body,)) = validation else {
        return Response::builder()
            .status(StatusCode::BAD_REQUEST)
            .body(Body::from(validation.unwrap_err().to_string()))
            .map_err(|_| StatusCode::BAD_REQUEST);
    };

    let result = api_impl
        .as_ref()
        .untyped_property_get(&method, &host, &cookies, &body)
        .await;

    let mut response = Response::builder();

    let resp = match result {
                                            Ok(rsp) => match rsp {
                                                apis::default::UntypedPropertyGetResponse::Status200_CheckThatUntypedPropertiesWorks
                                                => {
                                                  let mut response = response.status(200);
                                                  response.body(Body::empty())
                                                },
                                            },
                                            Err(why) => {
                                                // Application code returned an error. This should not happen, as the implementation should
                                                // return a valid response.
                                                return api_impl.as_ref().handle_error(&method, &host, &cookies, why).await;
                                            },
                                        };

    resp.map_err(|e| {
        error!(error = ?e);
        StatusCode::INTERNAL_SERVER_ERROR
    })
}

#[tracing::instrument(skip_all)]
fn uuid_get_validation() -> std::result::Result<(), ValidationErrors> {
    Ok(())
}
/// UuidGet - GET /uuid
#[tracing::instrument(skip_all)]
async fn uuid_get<I, A, E>(
    method: Method,
    host: Host,
    cookies: CookieJar,
    State(api_impl): State<I>,
) -> Result<Response, StatusCode>
where
    I: AsRef<A> + Send + Sync,
    A: apis::default::Default<E> + Send + Sync,
    E: std::fmt::Debug + Send + Sync + 'static,
{
    let validation = uuid_get_validation();

    let Ok(()) = validation else {
        return Response::builder()
            .status(StatusCode::BAD_REQUEST)
            .body(Body::from(validation.unwrap_err().to_string()))
            .map_err(|_| StatusCode::BAD_REQUEST);
    };

    let result = api_impl.as_ref().uuid_get(&method, &host, &cookies).await;

    let mut response = Response::builder();

    let resp = match result {
        Ok(rsp) => match rsp {
            apis::default::UuidGetResponse::Status200_DuplicateResponseLongText(body) => {
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
struct XmlExtraPostBodyValidator<'a> {
    body: &'a [u8],
}

#[tracing::instrument(skip_all)]
fn xml_extra_post_validation(body: Bytes) -> std::result::Result<(Bytes,), ValidationErrors> {
    Ok((body,))
}
/// XmlExtraPost - POST /xml_extra
#[tracing::instrument(skip_all)]
async fn xml_extra_post<I, A, E>(
    method: Method,
    host: Host,
    cookies: CookieJar,
    State(api_impl): State<I>,
    body: Bytes,
) -> Result<Response, StatusCode>
where
    I: AsRef<A> + Send + Sync,
    A: apis::default::Default<E> + Send + Sync,
    E: std::fmt::Debug + Send + Sync + 'static,
{
    let validation = xml_extra_post_validation(body);

    let Ok((body,)) = validation else {
        return Response::builder()
            .status(StatusCode::BAD_REQUEST)
            .body(Body::from(validation.unwrap_err().to_string()))
            .map_err(|_| StatusCode::BAD_REQUEST);
    };

    let result = api_impl
        .as_ref()
        .xml_extra_post(&method, &host, &cookies, &body)
        .await;

    let mut response = Response::builder();

    let resp = match result {
        Ok(rsp) => match rsp {
            apis::default::XmlExtraPostResponse::Status201_OK => {
                let mut response = response.status(201);
                response.body(Body::empty())
            }
            apis::default::XmlExtraPostResponse::Status400_BadRequest => {
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
struct XmlOtherPostBodyValidator<'a> {
    body: &'a [u8],
}

#[tracing::instrument(skip_all)]
fn xml_other_post_validation(body: Bytes) -> std::result::Result<(Bytes,), ValidationErrors> {
    Ok((body,))
}
/// XmlOtherPost - POST /xml_other
#[tracing::instrument(skip_all)]
async fn xml_other_post<I, A, E>(
    method: Method,
    host: Host,
    cookies: CookieJar,
    State(api_impl): State<I>,
    body: Bytes,
) -> Result<Response, StatusCode>
where
    I: AsRef<A> + Send + Sync,
    A: apis::default::Default<E> + Send + Sync,
    E: std::fmt::Debug + Send + Sync + 'static,
{
    let validation = xml_other_post_validation(body);

    let Ok((body,)) = validation else {
        return Response::builder()
            .status(StatusCode::BAD_REQUEST)
            .body(Body::from(validation.unwrap_err().to_string()))
            .map_err(|_| StatusCode::BAD_REQUEST);
    };

    let result = api_impl
        .as_ref()
        .xml_other_post(&method, &host, &cookies, &body)
        .await;

    let mut response = Response::builder();

    let resp = match result {
        Ok(rsp) => match rsp {
            apis::default::XmlOtherPostResponse::Status201_OK(body) => {
                let mut response = response.status(201);
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
            apis::default::XmlOtherPostResponse::Status400_BadRequest => {
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
struct XmlOtherPutBodyValidator<'a> {
    body: &'a [u8],
}

#[tracing::instrument(skip_all)]
fn xml_other_put_validation(body: Bytes) -> std::result::Result<(Bytes,), ValidationErrors> {
    Ok((body,))
}
/// XmlOtherPut - PUT /xml_other
#[tracing::instrument(skip_all)]
async fn xml_other_put<I, A, E>(
    method: Method,
    host: Host,
    cookies: CookieJar,
    State(api_impl): State<I>,
    body: Bytes,
) -> Result<Response, StatusCode>
where
    I: AsRef<A> + Send + Sync,
    A: apis::default::Default<E> + Send + Sync,
    E: std::fmt::Debug + Send + Sync + 'static,
{
    let validation = xml_other_put_validation(body);

    let Ok((body,)) = validation else {
        return Response::builder()
            .status(StatusCode::BAD_REQUEST)
            .body(Body::from(validation.unwrap_err().to_string()))
            .map_err(|_| StatusCode::BAD_REQUEST);
    };

    let result = api_impl
        .as_ref()
        .xml_other_put(&method, &host, &cookies, &body)
        .await;

    let mut response = Response::builder();

    let resp = match result {
        Ok(rsp) => match rsp {
            apis::default::XmlOtherPutResponse::Status201_OK => {
                let mut response = response.status(201);
                response.body(Body::empty())
            }
            apis::default::XmlOtherPutResponse::Status400_BadRequest => {
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
struct XmlPostBodyValidator<'a> {
    body: &'a [u8],
}

#[tracing::instrument(skip_all)]
fn xml_post_validation(body: Bytes) -> std::result::Result<(Bytes,), ValidationErrors> {
    Ok((body,))
}
/// XmlPost - POST /xml
#[tracing::instrument(skip_all)]
async fn xml_post<I, A, E>(
    method: Method,
    host: Host,
    cookies: CookieJar,
    State(api_impl): State<I>,
    body: Bytes,
) -> Result<Response, StatusCode>
where
    I: AsRef<A> + Send + Sync,
    A: apis::default::Default<E> + Send + Sync,
    E: std::fmt::Debug + Send + Sync + 'static,
{
    let validation = xml_post_validation(body);

    let Ok((body,)) = validation else {
        return Response::builder()
            .status(StatusCode::BAD_REQUEST)
            .body(Body::from(validation.unwrap_err().to_string()))
            .map_err(|_| StatusCode::BAD_REQUEST);
    };

    let result = api_impl
        .as_ref()
        .xml_post(&method, &host, &cookies, &body)
        .await;

    let mut response = Response::builder();

    let resp = match result {
        Ok(rsp) => match rsp {
            apis::default::XmlPostResponse::Status201_OK => {
                let mut response = response.status(201);
                response.body(Body::empty())
            }
            apis::default::XmlPostResponse::Status400_BadRequest => {
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
struct XmlPutBodyValidator<'a> {
    body: &'a [u8],
}

#[tracing::instrument(skip_all)]
fn xml_put_validation(body: Bytes) -> std::result::Result<(Bytes,), ValidationErrors> {
    Ok((body,))
}
/// XmlPut - PUT /xml
#[tracing::instrument(skip_all)]
async fn xml_put<I, A, E>(
    method: Method,
    host: Host,
    cookies: CookieJar,
    State(api_impl): State<I>,
    body: Bytes,
) -> Result<Response, StatusCode>
where
    I: AsRef<A> + Send + Sync,
    A: apis::default::Default<E> + Send + Sync,
    E: std::fmt::Debug + Send + Sync + 'static,
{
    let validation = xml_put_validation(body);

    let Ok((body,)) = validation else {
        return Response::builder()
            .status(StatusCode::BAD_REQUEST)
            .body(Body::from(validation.unwrap_err().to_string()))
            .map_err(|_| StatusCode::BAD_REQUEST);
    };

    let result = api_impl
        .as_ref()
        .xml_put(&method, &host, &cookies, &body)
        .await;

    let mut response = Response::builder();

    let resp = match result {
        Ok(rsp) => match rsp {
            apis::default::XmlPutResponse::Status201_OK => {
                let mut response = response.status(201);
                response.body(Body::empty())
            }
            apis::default::XmlPutResponse::Status400_BadRequest => {
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
fn get_repo_info_validation(
    path_params: models::GetRepoInfoPathParams,
) -> std::result::Result<(models::GetRepoInfoPathParams,), ValidationErrors> {
    path_params.validate()?;

    Ok((path_params,))
}
/// GetRepoInfo - GET /repos/{repoId}
#[tracing::instrument(skip_all)]
async fn get_repo_info<I, A, E>(
    method: Method,
    host: Host,
    cookies: CookieJar,
    Path(path_params): Path<models::GetRepoInfoPathParams>,
    State(api_impl): State<I>,
) -> Result<Response, StatusCode>
where
    I: AsRef<A> + Send + Sync,
    A: apis::info_repo::InfoRepo<E> + Send + Sync,
    E: std::fmt::Debug + Send + Sync + 'static,
{
    let validation = get_repo_info_validation(path_params);

    let Ok((path_params,)) = validation else {
        return Response::builder()
            .status(StatusCode::BAD_REQUEST)
            .body(Body::from(validation.unwrap_err().to_string()))
            .map_err(|_| StatusCode::BAD_REQUEST);
    };

    let result = api_impl
        .as_ref()
        .get_repo_info(&method, &host, &cookies, &path_params)
        .await;

    let mut response = Response::builder();

    let resp = match result {
        Ok(rsp) => match rsp {
            apis::info_repo::GetRepoInfoResponse::Status200_OK(body) => {
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
struct CreateRepoBodyValidator<'a> {
    #[validate(nested)]
    body: &'a models::ObjectParam,
}

#[tracing::instrument(skip_all)]
fn create_repo_validation(
    body: models::ObjectParam,
) -> std::result::Result<(models::ObjectParam,), ValidationErrors> {
    let b = CreateRepoBodyValidator { body: &body };
    b.validate()?;

    Ok((body,))
}
/// CreateRepo - POST /repos
#[tracing::instrument(skip_all)]
async fn create_repo<I, A, E>(
    method: Method,
    host: Host,
    cookies: CookieJar,
    State(api_impl): State<I>,
    Json(body): Json<models::ObjectParam>,
) -> Result<Response, StatusCode>
where
    I: AsRef<A> + Send + Sync,
    A: apis::repo::Repo<E> + Send + Sync,
    E: std::fmt::Debug + Send + Sync + 'static,
{
    let validation = create_repo_validation(body);

    let Ok((body,)) = validation else {
        return Response::builder()
            .status(StatusCode::BAD_REQUEST)
            .body(Body::from(validation.unwrap_err().to_string()))
            .map_err(|_| StatusCode::BAD_REQUEST);
    };

    let result = api_impl
        .as_ref()
        .create_repo(&method, &host, &cookies, &body)
        .await;

    let mut response = Response::builder();

    let resp = match result {
        Ok(rsp) => match rsp {
            apis::repo::CreateRepoResponse::Status200_Success => {
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
