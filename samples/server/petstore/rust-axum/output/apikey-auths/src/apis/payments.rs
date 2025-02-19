use async_trait::async_trait;
use axum::extract::*;
use axum_extra::extract::{CookieJar, Multipart};
use bytes::Bytes;
use http::Method;
use serde::{Deserialize, Serialize};

use crate::{models, types::*};

#[derive(Debug, PartialEq, Serialize, Deserialize)]
#[must_use]
#[allow(clippy::large_enum_variant)]
pub enum GetPaymentMethodByIdResponse {
    /// OK - the request has succeeded.
    Status200_OK(models::PaymentMethod),
    /// Unprocessable Entity - a request validation error.
    Status422_UnprocessableEntity(models::CheckoutError),
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
#[must_use]
#[allow(clippy::large_enum_variant)]
pub enum GetPaymentMethodsResponse {
    /// OK - the request has succeeded.
    Status200_OK(Vec<models::PaymentMethod>),
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
#[must_use]
#[allow(clippy::large_enum_variant)]
pub enum PostMakePaymentResponse {
    /// OK - the request has succeeded.
    Status200_OK(models::PaymentResult),
    /// Unprocessable Entity - a request validation error.
    Status422_UnprocessableEntity(models::CheckoutError),
}

/// Payments
#[async_trait]
#[allow(clippy::ptr_arg)]
pub trait Payments {
    type Claims;

    /// Get payment method by id.
    ///
    /// GetPaymentMethodById - GET /v71/paymentMethods/{id}
    async fn get_payment_method_by_id(
        &self,
        method: Method,
        host: Host,
        cookies: CookieJar,
        path_params: models::GetPaymentMethodByIdPathParams,
    ) -> Result<GetPaymentMethodByIdResponse, ()>;

    /// Get payment methods.
    ///
    /// GetPaymentMethods - GET /v71/paymentMethods
    async fn get_payment_methods(
        &self,
        method: Method,
        host: Host,
        cookies: CookieJar,
    ) -> Result<GetPaymentMethodsResponse, ()>;

    /// Make a payment.
    ///
    /// PostMakePayment - POST /v71/payments
    async fn post_make_payment(
        &self,
        method: Method,
        host: Host,
        cookies: CookieJar,
        claims: Self::Claims,
        body: Option<models::Payment>,
    ) -> Result<PostMakePaymentResponse, ()>;
}
