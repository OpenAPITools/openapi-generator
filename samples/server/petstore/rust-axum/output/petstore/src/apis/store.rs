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
pub trait Store {
    /// Delete purchase order by ID.
    ///
    /// DeleteOrder - DELETE /v2/store/order/{orderId}
    async fn delete_order(
        &self,
        method: Method,
        host: Host,
        cookies: CookieJar,
        path_params: models::DeleteOrderPathParams,
    ) -> Result<DeleteOrderResponse, String>;

    /// Returns pet inventories by status.
    ///
    /// GetInventory - GET /v2/store/inventory
    async fn get_inventory(
        &self,
        method: Method,
        host: Host,
        cookies: CookieJar,
    ) -> Result<GetInventoryResponse, String>;

    /// Find purchase order by ID.
    ///
    /// GetOrderById - GET /v2/store/order/{orderId}
    async fn get_order_by_id(
        &self,
        method: Method,
        host: Host,
        cookies: CookieJar,
        path_params: models::GetOrderByIdPathParams,
    ) -> Result<GetOrderByIdResponse, String>;

    /// Place an order for a pet.
    ///
    /// PlaceOrder - POST /v2/store/order
    async fn place_order(
        &self,
        method: Method,
        host: Host,
        cookies: CookieJar,
        body: models::Order,
    ) -> Result<PlaceOrderResponse, String>;
}
