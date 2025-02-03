use std::collections::HashMap;

use axum::{body::Body, extract::*, response::Response, routing::*};
use axum_extra::extract::{CookieJar, Host};
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
    A: apis::default::Default<E> + Send + Sync + 'static,
    E: std::fmt::Debug + Send + Sync + 'static,
{
    // build our application with a route
    Router::new()
        .route("/mail", put(mail_put::<I, A, E>))
        .with_state(api_impl)
}

/// MailPut - PUT /mail
#[tracing::instrument(skip_all)]
async fn mail_put<I, A, E>(
    method: Method,
    host: Host,
    cookies: CookieJar,
    State(api_impl): State<I>,
    Json(body): Json<models::Email>,
) -> Result<Response, StatusCode>
where
    I: AsRef<A> + Send + Sync,
    A: apis::default::Default<E> + Send + Sync,
    E: std::fmt::Debug + Send + Sync + 'static,
{
    let result = api_impl
        .as_ref()
<<<<<<< HEAD
        .mail_put(&method, &host, &cookies, &body)
=======
        .mail_put(method.clone(), host.clone(), cookies.clone(), body)
>>>>>>> 00e7ad2ac29 (Pass in method, host and cookies to error handler)
        .await;

    let mut response = Response::builder();

    let resp = match result {
        Ok(rsp) => match rsp {
            apis::default::MailPutResponse::Status204_OK => {
                let mut response = response.status(204);
                response.body(Body::empty())
            }
        },
        Err(why) => {
            // Application code returned an error. This should not happen, as the implementation should
            // return a valid response.
<<<<<<< HEAD
<<<<<<< HEAD

            return api_impl
                .as_ref()
                .handle_error(&method, &host, &cookies, why)
                .await;
=======
            return api_impl.as_ref().handle_error(why).await;
>>>>>>> 9841fa4dc2c (Implement a custom error handler for unhandled or generic endpoint errors)
=======

            return api_impl
                .as_ref()
                .handle_error(method, host, cookies, why)
                .await;
>>>>>>> 00e7ad2ac29 (Pass in method, host and cookies to error handler)
        }
    };

    resp.map_err(|e| {
        error!(error = ?e);
        StatusCode::INTERNAL_SERVER_ERROR
    })
}
