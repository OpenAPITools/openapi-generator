use async_trait::async_trait;
use axum::extract::*;
<<<<<<< HEAD
use axum_extra::extract::{CookieJar, Host};
=======
use axum_extra::extract::{CookieJar, Host, Multipart};
>>>>>>> fb7dae12a7d (Update axum to 0.8)
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
pub trait Payments<E: std::fmt::Debug + Send + Sync + 'static = ()>:
    super::ErrorHandler<E>
{
    type Claims;

    /// Get payment method by id.
    ///
    /// GetPaymentMethodById - GET /v71/paymentMethods/{id}
    async fn get_payment_method_by_id(
        &self,
<<<<<<< HEAD
<<<<<<< HEAD
=======
>>>>>>> 3d833fd5ff9 (Make API methods take references instead of ownership)
        method: &Method,
        host: &Host,
        cookies: &CookieJar,
        path_params: &models::GetPaymentMethodByIdPathParams,
<<<<<<< HEAD
=======
        method: Method,
        host: Host,
        cookies: CookieJar,
        path_params: models::GetPaymentMethodByIdPathParams,
>>>>>>> 9841fa4dc2c (Implement a custom error handler for unhandled or generic endpoint errors)
=======
>>>>>>> 3d833fd5ff9 (Make API methods take references instead of ownership)
    ) -> Result<GetPaymentMethodByIdResponse, E>;

    /// Get payment methods.
    ///
    /// GetPaymentMethods - GET /v71/paymentMethods
    async fn get_payment_methods(
        &self,
<<<<<<< HEAD
<<<<<<< HEAD
        method: &Method,
        host: &Host,
        cookies: &CookieJar,
=======
        method: Method,
        host: Host,
        cookies: CookieJar,
>>>>>>> 9841fa4dc2c (Implement a custom error handler for unhandled or generic endpoint errors)
=======
        method: &Method,
        host: &Host,
        cookies: &CookieJar,
>>>>>>> 3d833fd5ff9 (Make API methods take references instead of ownership)
    ) -> Result<GetPaymentMethodsResponse, E>;

    /// Make a payment.
    ///
    /// PostMakePayment - POST /v71/payments
    async fn post_make_payment(
        &self,
<<<<<<< HEAD
<<<<<<< HEAD
=======
>>>>>>> 3d833fd5ff9 (Make API methods take references instead of ownership)
        method: &Method,
        host: &Host,
        cookies: &CookieJar,
        claims: &Self::Claims,
        body: &Option<models::Payment>,
<<<<<<< HEAD
=======
        method: Method,
        host: Host,
        cookies: CookieJar,
        claims: Self::Claims,
        body: Option<models::Payment>,
>>>>>>> 9841fa4dc2c (Implement a custom error handler for unhandled or generic endpoint errors)
=======
>>>>>>> 3d833fd5ff9 (Make API methods take references instead of ownership)
    ) -> Result<PostMakePaymentResponse, E>;
}
