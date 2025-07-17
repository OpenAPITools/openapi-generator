use async_trait::async_trait;
use axum::extract::*;
use axum_extra::extract::{CookieJar, Host};
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

/// Payments APIs - Authorization.
#[async_trait]
#[allow(clippy::ptr_arg)]
pub trait PaymentsAuthorization {
    type Claims;

    /// Authorization - Get payment method by id.
    /// GetPaymentMethodById - GET /v71/paymentMethods/{id}
    async fn get_payment_method_by_id_authorize(
        &self,
        method: &Method,
        host: &Host,
        cookies: &CookieJar,
        claims: &Self::Claims,
        path_params: &models::GetPaymentMethodByIdPathParams,
    ) -> Result<super::Authorization, ()> {
        Ok(super::Authorization::Authorized)
    }

    /// Authorization - Get payment methods.
    /// GetPaymentMethods - GET /v71/paymentMethods
    async fn get_payment_methods_authorize(
        &self,
        method: &Method,
        host: &Host,
        cookies: &CookieJar,
        claims: &Self::Claims,
    ) -> Result<super::Authorization, ()> {
        Ok(super::Authorization::Authorized)
    }

    /// Authorization - Make a payment.
    /// PostMakePayment - POST /v71/payments
    async fn post_make_payment_authorize(
        &self,
        method: &Method,
        host: &Host,
        cookies: &CookieJar,
        claims: &Self::Claims,
        body: &Option<models::Payment>,
    ) -> Result<super::Authorization, ()> {
        Ok(super::Authorization::Authorized)
    }
}

/// Payments
#[async_trait]
#[allow(clippy::ptr_arg)]
pub trait Payments<E: std::fmt::Debug + Send + Sync + 'static = ()>:
    super::ErrorHandler<E>
{
    type Claims;

    /// Get payment method by id.
    ///
    /// GetPaymentMethodById - GET /v71/paymentMethods/{id}
    async fn get_payment_method_by_id(
        &self,
        event: &mut super::event::Event,
        method: Method,
        host: Host,
        cookies: CookieJar,
        claims: Self::Claims,
        path_params: models::GetPaymentMethodByIdPathParams,
    ) -> Result<GetPaymentMethodByIdResponse, E>;

    /// Get payment methods.
    ///
    /// GetPaymentMethods - GET /v71/paymentMethods
    async fn get_payment_methods(
        &self,
        event: &mut super::event::Event,
        method: Method,
        host: Host,
        cookies: CookieJar,
        claims: Self::Claims,
    ) -> Result<GetPaymentMethodsResponse, E>;

    /// Make a payment.
    ///
    /// PostMakePayment - POST /v71/payments
    async fn post_make_payment(
        &self,
        event: &mut super::event::Event,
        method: Method,
        host: Host,
        cookies: CookieJar,
        claims: Self::Claims,
        body: Option<models::Payment>,
    ) -> Result<PostMakePaymentResponse, E>;
}
