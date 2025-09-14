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
pub enum AddPetResponse {
    /// successful operation
    Status200_SuccessfulOperation(String),
    /// Invalid input
    Status405_InvalidInput,
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
#[must_use]
#[allow(clippy::large_enum_variant)]
pub enum DeletePetResponse {
    /// Invalid pet value
    Status400_InvalidPetValue,
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
#[must_use]
#[allow(clippy::large_enum_variant)]
pub enum FindPetsByStatusResponse {
    /// successful operation
    Status200_SuccessfulOperation(String),
    /// Invalid status value
    Status400_InvalidStatusValue,
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
#[must_use]
#[allow(clippy::large_enum_variant)]
pub enum FindPetsByTagsResponse {
    /// successful operation
    Status200_SuccessfulOperation(String),
    /// Invalid tag value
    Status400_InvalidTagValue,
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
#[must_use]
#[allow(clippy::large_enum_variant)]
pub enum GetPetByIdResponse {
    /// successful operation
    Status200_SuccessfulOperation(String),
    /// Invalid ID supplied
    Status400_InvalidIDSupplied,
    /// Pet not found
    Status404_PetNotFound,
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
#[must_use]
#[allow(clippy::large_enum_variant)]
pub enum UpdatePetResponse {
    /// successful operation
    Status200_SuccessfulOperation(String),
    /// Invalid ID supplied
    Status400_InvalidIDSupplied,
    /// Pet not found
    Status404_PetNotFound,
    /// Validation exception
    Status405_ValidationException,
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
#[must_use]
#[allow(clippy::large_enum_variant)]
pub enum UpdatePetWithFormResponse {
    /// Invalid input
    Status405_InvalidInput,
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
#[must_use]
#[allow(clippy::large_enum_variant)]
pub enum UploadFileResponse {
    /// successful operation
    Status200_SuccessfulOperation(models::ApiResponse),
}

/// Pet
#[async_trait]
#[allow(clippy::ptr_arg)]
pub trait Pet<E: std::fmt::Debug + Send + Sync + 'static = ()>: super::ErrorHandler<E> {
    type Claims;

    /// Add a new pet to the store.
    ///
    /// AddPet - POST /v2/pet
    async fn add_pet(
        &self,

        method: &Method,
        host: &Host,
        cookies: &CookieJar,
        body: &models::Pet,
    ) -> Result<AddPetResponse, E>;

    /// Deletes a pet.
    ///
    /// DeletePet - DELETE /v2/pet/{petId}
    async fn delete_pet(
        &self,

        method: &Method,
        host: &Host,
        cookies: &CookieJar,
        header_params: &models::DeletePetHeaderParams,
        path_params: &models::DeletePetPathParams,
    ) -> Result<DeletePetResponse, E>;

    /// Finds Pets by status.
    ///
    /// FindPetsByStatus - GET /v2/pet/findByStatus
    async fn find_pets_by_status(
        &self,

        method: &Method,
        host: &Host,
        cookies: &CookieJar,
        query_params: &models::FindPetsByStatusQueryParams,
    ) -> Result<FindPetsByStatusResponse, E>;

    /// Finds Pets by tags.
    ///
    /// FindPetsByTags - GET /v2/pet/findByTags
    async fn find_pets_by_tags(
        &self,

        method: &Method,
        host: &Host,
        cookies: &CookieJar,
        query_params: &models::FindPetsByTagsQueryParams,
    ) -> Result<FindPetsByTagsResponse, E>;

    /// Find pet by ID.
    ///
    /// GetPetById - GET /v2/pet/{petId}
    async fn get_pet_by_id(
        &self,

        method: &Method,
        host: &Host,
        cookies: &CookieJar,
        claims: &Self::Claims,
        path_params: &models::GetPetByIdPathParams,
    ) -> Result<GetPetByIdResponse, E>;

    /// Update an existing pet.
    ///
    /// UpdatePet - PUT /v2/pet
    async fn update_pet(
        &self,

        method: &Method,
        host: &Host,
        cookies: &CookieJar,
        body: &models::Pet,
    ) -> Result<UpdatePetResponse, E>;

    /// Updates a pet in the store with form data.
    ///
    /// UpdatePetWithForm - POST /v2/pet/{petId}
    async fn update_pet_with_form(
        &self,

        method: &Method,
        host: &Host,
        cookies: &CookieJar,
        path_params: &models::UpdatePetWithFormPathParams,
        body: &Option<models::UpdatePetWithFormRequest>,
    ) -> Result<UpdatePetWithFormResponse, E>;

    /// uploads an image.
    ///
    /// UploadFile - POST /v2/pet/{petId}/uploadImage
    async fn upload_file(
        &self,

        method: &Method,
        host: &Host,
        cookies: &CookieJar,
        path_params: &models::UploadFilePathParams,
        body: Multipart,
    ) -> Result<UploadFileResponse, E>;
}
