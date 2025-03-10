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
pub enum DeleteOrderResponse {
    /// Invalid ID supplied
    Status400_InvalidIDSupplied,
    /// Order not found
    Status404_OrderNotFound,
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
#[must_use]
#[allow(clippy::large_enum_variant)]
pub enum GetInventoryResponse {
    /// successful operation
    Status200_SuccessfulOperation(std::collections::HashMap<String, i32>),
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
#[must_use]
#[allow(clippy::large_enum_variant)]
pub enum GetOrderByIdResponse {
    /// successful operation
    Status200_SuccessfulOperation(String),
    /// Invalid ID supplied
    Status400_InvalidIDSupplied,
    /// Order not found
    Status404_OrderNotFound,
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
#[must_use]
#[allow(clippy::large_enum_variant)]
pub enum PlaceOrderResponse {
    /// successful operation
    Status200_SuccessfulOperation(String),
    /// Invalid Order
    Status400_InvalidOrder,
}

/// Store
#[async_trait]
#[allow(clippy::ptr_arg)]
pub trait Store<E: std::fmt::Debug + Send + Sync + 'static = ()>: super::ErrorHandler<E> {
    type Claims;

    /// Delete purchase order by ID.
    ///
    /// DeleteOrder - DELETE /v2/store/order/{order_id}
    async fn delete_order(
        &self,
        method: &Method,
        host: &Host,
        cookies: &CookieJar,
        path_params: &models::DeleteOrderPathParams,
    ) -> Result<DeleteOrderResponse, E>;

    /// Returns pet inventories by status.
    ///
    /// GetInventory - GET /v2/store/inventory
    async fn get_inventory(
        &self,
        method: &Method,
        host: &Host,
        cookies: &CookieJar,
        claims: &Self::Claims,
    ) -> Result<GetInventoryResponse, E>;

    /// Find purchase order by ID.
    ///
    /// GetOrderById - GET /v2/store/order/{order_id}
    async fn get_order_by_id(
        &self,
        method: &Method,
        host: &Host,
        cookies: &CookieJar,
        path_params: &models::GetOrderByIdPathParams,
    ) -> Result<GetOrderByIdResponse, E>;

    /// Place an order for a pet.
    ///
    /// PlaceOrder - POST /v2/store/order
    async fn place_order(
        &self,
        method: &Method,
        host: &Host,
        cookies: &CookieJar,
        body: &models::Order,
    ) -> Result<PlaceOrderResponse, E>;
}
