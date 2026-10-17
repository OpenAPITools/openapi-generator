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
    /// Pet deleted
    Status204_PetDeleted,
    /// Unexpected error
    Status0_UnexpectedError(u16),
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
#[must_use]
#[allow(clippy::large_enum_variant)]
pub enum GetPetResponse {
    /// The pet
    Status200_ThePet(models::Pet),
    /// Unexpected error
    Status0_UnexpectedError(u16, models::Error),
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
#[must_use]
#[allow(clippy::large_enum_variant)]
pub enum ListPetsResponse {
    /// A list of pets
    Status200_AListOfPets {
        body: Vec<models::Pet>,
        x_rate_limit: Option<i32>,
    },
    /// Unexpected error
    Status0_UnexpectedError {
        status: u16,
        body: models::Error,
        x_rate_limit: Option<i32>,
    },
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

    /// ListPets - GET /pets
    async fn list_pets(
        &self,

        method: &Method,
        host: &Host,
        cookies: &CookieJar,
    ) -> Result<ListPetsResponse, E>;
}
