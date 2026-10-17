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
pub enum DeletePetResponse {
    /// Deleted
    Status204_Deleted,
    /// Client error
    Status4XX_ClientError { status: u16 },
    /// Server error
    Status5XX_ServerError {
        status: u16,
        body: models::Error,
        x_request_id: String,
    },
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
#[must_use]
#[allow(clippy::large_enum_variant)]
pub enum GetPetResponse {
    /// The pet
    Status200_ThePet(models::Pet),
    /// Client error
    Status4XX_ClientError { status: u16, body: models::Error },
    /// Server error
    Status5XX_ServerError {
        status: u16,
        retry_after: Option<i32>,
    },
}

#[must_use]
#[allow(clippy::large_enum_variant)]
pub enum GetPetEventsResponse {
    /// Pet events
    Status2XX_PetEvents {
        status: u16,
        body: models::SSE,
        x_stream_id: Option<String>,
    },
    /// Server error
    Status5XX_ServerError { status: u16 },
}

/// Default
#[async_trait]
#[allow(clippy::ptr_arg)]
pub trait Default<E: std::fmt::Debug + Send + Sync + 'static = ()>: super::ErrorHandler<E> {
    /// DeletePet - DELETE /pets/{petId}
    async fn delete_pet(
        &self,

        method: &Method,
        host: &Host,
        cookies: &CookieJar,
        path_params: &models::DeletePetPathParams,
    ) -> Result<DeletePetResponse, E>;

    /// GetPet - GET /pets/{petId}
    async fn get_pet(
        &self,

        method: &Method,
        host: &Host,
        cookies: &CookieJar,
        path_params: &models::GetPetPathParams,
    ) -> Result<GetPetResponse, E>;

    /// GetPetEvents - GET /pets/events
    async fn get_pet_events(
        &self,

        method: &Method,
        host: &Host,
        cookies: &CookieJar,
    ) -> Result<GetPetEventsResponse, E>;
}
