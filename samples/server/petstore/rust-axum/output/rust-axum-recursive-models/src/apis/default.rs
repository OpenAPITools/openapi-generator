use async_trait::async_trait;
use axum::extract::*;
use axum_extra::extract::CookieJar;
use bytes::Bytes;
use headers::Host;
use http::Method;
use serde::{Deserialize, Serialize};

use crate::{models, types::*};

#[derive(Debug, PartialEq, Serialize, Deserialize)]
#[must_use]
#[allow(clippy::large_enum_variant)]
pub enum GetEmployeeResponse {
    /// OK
    Status200_OK(models::Employee),
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
#[must_use]
#[allow(clippy::large_enum_variant)]
pub enum GetNodeResponse {
    /// OK
    Status200_OK(models::Node),
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
#[must_use]
#[allow(clippy::large_enum_variant)]
pub enum GetPetResponse {
    /// OK
    Status200_OK(models::Pet),
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
#[must_use]
#[allow(clippy::large_enum_variant)]
pub enum GetShapeResponse {
    /// OK
    Status200_OK(models::Shape),
}

/// Default
#[async_trait]
#[allow(clippy::ptr_arg)]
pub trait Default<E: std::fmt::Debug + Send + Sync + 'static = ()>: super::ErrorHandler<E> {
    /// GetEmployee - GET /employees
    async fn get_employee(
        &self,

        method: &Method,
        host: &Host,
        cookies: &CookieJar,
    ) -> Result<GetEmployeeResponse, E>;

    /// GetNode - GET /nodes
    async fn get_node(
        &self,

        method: &Method,
        host: &Host,
        cookies: &CookieJar,
    ) -> Result<GetNodeResponse, E>;

    /// GetPet - GET /pets
    async fn get_pet(
        &self,

        method: &Method,
        host: &Host,
        cookies: &CookieJar,
    ) -> Result<GetPetResponse, E>;

    /// GetShape - GET /shapes
    async fn get_shape(
        &self,

        method: &Method,
        host: &Host,
        cookies: &CookieJar,
    ) -> Result<GetShapeResponse, E>;
}
