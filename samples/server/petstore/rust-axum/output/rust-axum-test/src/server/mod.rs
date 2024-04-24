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
    AllOfGetResponse, Api, DummyGetResponse, DummyPutResponse, FileResponseGetResponse,
    GetStructuredYamlResponse, HtmlPostResponse, PostYamlResponse, RawJsonGetResponse,
    SoloObjectPostResponse,
};

/// Setup API Server.
pub fn new<I, A>(api_impl: I) -> Router
where
    I: AsRef<A> + Clone + Send + Sync + 'static,
    A: Api + 'static,
{
    // build our application with a route
    Router::new()
        .route("/allOf", get(all_of_get::<I, A>))
        .route("/dummy", get(dummy_get::<I, A>).put(dummy_put::<I, A>))
        .route("/file_response", get(file_response_get::<I, A>))
        .route("/get-structured-yaml", get(get_structured_yaml::<I, A>))
        .route("/html", post(html_post::<I, A>))
        .route("/post-yaml", post(post_yaml::<I, A>))
        .route("/raw_json", get(raw_json_get::<I, A>))
        .route("/solo-object", post(solo_object_post::<I, A>))
        .with_state(api_impl)
}

#[tracing::instrument(skip_all)]
fn all_of_get_validation() -> std::result::Result<(), ValidationErrors> {
    Ok(())
}
/// AllOfGet - GET /allOf
#[tracing::instrument(skip_all)]
async fn all_of_get<I, A>(
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
    let validation = tokio::task::spawn_blocking(move || all_of_get_validation())
        .await
        .unwrap();

    let Ok(()) = validation else {
        return Response::builder()
            .status(StatusCode::BAD_REQUEST)
            .body(Body::from(validation.unwrap_err().to_string()))
            .map_err(|_| StatusCode::BAD_REQUEST);
    };

    let result = api_impl.as_ref().all_of_get(method, host, cookies).await;

    let mut response = Response::builder();

    let resp = match result {
        Ok(rsp) => match rsp {
            AllOfGetResponse::Status200_OK(body) => {
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
fn dummy_get_validation() -> std::result::Result<(), ValidationErrors> {
    Ok(())
}
/// DummyGet - GET /dummy
#[tracing::instrument(skip_all)]
async fn dummy_get<I, A>(
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
    let validation = tokio::task::spawn_blocking(move || dummy_get_validation())
        .await
        .unwrap();

    let Ok(()) = validation else {
        return Response::builder()
            .status(StatusCode::BAD_REQUEST)
            .body(Body::from(validation.unwrap_err().to_string()))
            .map_err(|_| StatusCode::BAD_REQUEST);
    };

    let result = api_impl.as_ref().dummy_get(method, host, cookies).await;

    let mut response = Response::builder();

    let resp = match result {
        Ok(rsp) => match rsp {
            DummyGetResponse::Status200_Success => {
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
struct DummyPutBodyValidator<'a> {
    #[validate(nested)]
    body: &'a models::DummyPutRequest,
}

#[tracing::instrument(skip_all)]
fn dummy_put_validation(
    body: models::DummyPutRequest,
) -> std::result::Result<(models::DummyPutRequest,), ValidationErrors> {
    let b = DummyPutBodyValidator { body: &body };
    b.validate()?;

    Ok((body,))
}
/// DummyPut - PUT /dummy
#[tracing::instrument(skip_all)]
async fn dummy_put<I, A>(
    method: Method,
    host: Host,
    cookies: CookieJar,
    State(api_impl): State<I>,
    Json(body): Json<models::DummyPutRequest>,
) -> Result<Response, StatusCode>
where
    I: AsRef<A> + Send + Sync,
    A: Api,
{
    #[allow(clippy::redundant_closure)]
    let validation = tokio::task::spawn_blocking(move || dummy_put_validation(body))
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
        .dummy_put(method, host, cookies, body)
        .await;

    let mut response = Response::builder();

    let resp = match result {
        Ok(rsp) => match rsp {
            DummyPutResponse::Status200_Success => {
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
fn file_response_get_validation() -> std::result::Result<(), ValidationErrors> {
    Ok(())
}
/// FileResponseGet - GET /file_response
#[tracing::instrument(skip_all)]
async fn file_response_get<I, A>(
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
    let validation = tokio::task::spawn_blocking(move || file_response_get_validation())
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
        .file_response_get(method, host, cookies)
        .await;

    let mut response = Response::builder();

    let resp = match result {
        Ok(rsp) => match rsp {
            FileResponseGetResponse::Status200_Success(body) => {
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
fn get_structured_yaml_validation() -> std::result::Result<(), ValidationErrors> {
    Ok(())
}
/// GetStructuredYaml - GET /get-structured-yaml
#[tracing::instrument(skip_all)]
async fn get_structured_yaml<I, A>(
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
    let validation = tokio::task::spawn_blocking(move || get_structured_yaml_validation())
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
        .get_structured_yaml(method, host, cookies)
        .await;

    let mut response = Response::builder();

    let resp = match result {
        Ok(rsp) => match rsp {
            GetStructuredYamlResponse::Status200_OK(body) => {
                let mut response = response.status(200);
                {
                    let mut response_headers = response.headers_mut().unwrap();
                    response_headers.insert(
                        CONTENT_TYPE,
                        HeaderValue::from_str("application/yaml").map_err(|e| {
                            error!(error = ?e);
                            StatusCode::INTERNAL_SERVER_ERROR
                        })?,
                    );
                }

                let body_content = body;
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
struct HtmlPostBodyValidator<'a> {
    body: &'a String,
}

#[tracing::instrument(skip_all)]
fn html_post_validation(body: String) -> std::result::Result<(String,), ValidationErrors> {
    Ok((body,))
}
/// HtmlPost - POST /html
#[tracing::instrument(skip_all)]
async fn html_post<I, A>(
    method: Method,
    host: Host,
    cookies: CookieJar,
    State(api_impl): State<I>,
    body: String,
) -> Result<Response, StatusCode>
where
    I: AsRef<A> + Send + Sync,
    A: Api,
{
    #[allow(clippy::redundant_closure)]
    let validation = tokio::task::spawn_blocking(move || html_post_validation(body))
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
        .html_post(method, host, cookies, body)
        .await;

    let mut response = Response::builder();

    let resp = match result {
        Ok(rsp) => match rsp {
            HtmlPostResponse::Status200_Success(body) => {
                let mut response = response.status(200);
                {
                    let mut response_headers = response.headers_mut().unwrap();
                    response_headers.insert(
                        CONTENT_TYPE,
                        HeaderValue::from_str("text/html").map_err(|e| {
                            error!(error = ?e);
                            StatusCode::INTERNAL_SERVER_ERROR
                        })?,
                    );
                }

                let body_content = body;
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
struct PostYamlBodyValidator<'a> {
    body: &'a String,
}

#[tracing::instrument(skip_all)]
fn post_yaml_validation(body: String) -> std::result::Result<(String,), ValidationErrors> {
    Ok((body,))
}
/// PostYaml - POST /post-yaml
#[tracing::instrument(skip_all)]
async fn post_yaml<I, A>(
    method: Method,
    host: Host,
    cookies: CookieJar,
    State(api_impl): State<I>,
    body: String,
) -> Result<Response, StatusCode>
where
    I: AsRef<A> + Send + Sync,
    A: Api,
{
    #[allow(clippy::redundant_closure)]
    let validation = tokio::task::spawn_blocking(move || post_yaml_validation(body))
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
        .post_yaml(method, host, cookies, body)
        .await;

    let mut response = Response::builder();

    let resp = match result {
        Ok(rsp) => match rsp {
            PostYamlResponse::Status204_OK => {
                let mut response = response.status(204);
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
fn raw_json_get_validation() -> std::result::Result<(), ValidationErrors> {
    Ok(())
}
/// RawJsonGet - GET /raw_json
#[tracing::instrument(skip_all)]
async fn raw_json_get<I, A>(
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
    let validation = tokio::task::spawn_blocking(move || raw_json_get_validation())
        .await
        .unwrap();

    let Ok(()) = validation else {
        return Response::builder()
            .status(StatusCode::BAD_REQUEST)
            .body(Body::from(validation.unwrap_err().to_string()))
            .map_err(|_| StatusCode::BAD_REQUEST);
    };

    let result = api_impl.as_ref().raw_json_get(method, host, cookies).await;

    let mut response = Response::builder();

    let resp = match result {
        Ok(rsp) => match rsp {
            RawJsonGetResponse::Status200_Success(body) => {
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
struct SoloObjectPostBodyValidator<'a> {
    body: &'a crate::types::Object,
}

#[tracing::instrument(skip_all)]
fn solo_object_post_validation(
    body: crate::types::Object,
) -> std::result::Result<(crate::types::Object,), ValidationErrors> {
    let b = SoloObjectPostBodyValidator { body: &body };
    b.validate()?;

    Ok((body,))
}
/// SoloObjectPost - POST /solo-object
#[tracing::instrument(skip_all)]
async fn solo_object_post<I, A>(
    method: Method,
    host: Host,
    cookies: CookieJar,
    State(api_impl): State<I>,
    Json(body): Json<crate::types::Object>,
) -> Result<Response, StatusCode>
where
    I: AsRef<A> + Send + Sync,
    A: Api,
{
    #[allow(clippy::redundant_closure)]
    let validation = tokio::task::spawn_blocking(move || solo_object_post_validation(body))
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
        .solo_object_post(method, host, cookies, body)
        .await;

    let mut response = Response::builder();

    let resp = match result {
        Ok(rsp) => match rsp {
            SoloObjectPostResponse::Status204_OK => {
                let mut response = response.status(204);
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
