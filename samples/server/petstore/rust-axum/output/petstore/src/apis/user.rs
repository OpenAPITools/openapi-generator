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
pub enum CreateUserResponse {
    /// successful operation
    Status0_SuccessfulOperation,
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
#[must_use]
#[allow(clippy::large_enum_variant)]
pub enum CreateUsersWithArrayInputResponse {
    /// successful operation
    Status0_SuccessfulOperation,
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
#[must_use]
#[allow(clippy::large_enum_variant)]
pub enum CreateUsersWithListInputResponse {
    /// successful operation
    Status0_SuccessfulOperation,
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
#[must_use]
#[allow(clippy::large_enum_variant)]
pub enum DeleteUserResponse {
    /// Invalid username supplied
    Status400_InvalidUsernameSupplied,
    /// User not found
    Status404_UserNotFound,
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
#[must_use]
#[allow(clippy::large_enum_variant)]
pub enum GetUserByNameResponse {
    /// successful operation
    Status200_SuccessfulOperation(String),
    /// Invalid username supplied
    Status400_InvalidUsernameSupplied,
    /// User not found
    Status404_UserNotFound,
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
#[must_use]
#[allow(clippy::large_enum_variant)]
pub enum LoginUserResponse {
    /// successful operation
    Status200_SuccessfulOperation {
        body: String,
        set_cookie: Option<String>,
        x_rate_limit: Option<i32>,
        x_expires_after: Option<chrono::DateTime<chrono::Utc>>,
    },
    /// Invalid username/password supplied
    Status400_InvalidUsername,
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
#[must_use]
#[allow(clippy::large_enum_variant)]
pub enum LogoutUserResponse {
    /// successful operation
    Status0_SuccessfulOperation,
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
#[must_use]
#[allow(clippy::large_enum_variant)]
pub enum UpdateUserResponse {
    /// Invalid user supplied
    Status400_InvalidUserSupplied,
    /// User not found
    Status404_UserNotFound,
}

/// User
#[async_trait]
#[allow(clippy::ptr_arg)]
pub trait User<E: std::fmt::Debug + Send + Sync + 'static = ()>: super::ErrorHandler<E> {
    type Claims;

    /// Create user.
    ///
    /// CreateUser - POST /v2/user
    async fn create_user(
        &self,

        method: &Method,
        host: &Host,
        cookies: &CookieJar,
        claims: &Self::Claims,
        body: &models::User,
    ) -> Result<CreateUserResponse, E>;

    /// Creates list of users with given input array.
    ///
    /// CreateUsersWithArrayInput - POST /v2/user/createWithArray
    async fn create_users_with_array_input(
        &self,

        method: &Method,
        host: &Host,
        cookies: &CookieJar,
        claims: &Self::Claims,
        body: &Vec<models::User>,
    ) -> Result<CreateUsersWithArrayInputResponse, E>;

    /// Creates list of users with given input array.
    ///
    /// CreateUsersWithListInput - POST /v2/user/createWithList
    async fn create_users_with_list_input(
        &self,

        method: &Method,
        host: &Host,
        cookies: &CookieJar,
        claims: &Self::Claims,
        body: &Vec<models::User>,
    ) -> Result<CreateUsersWithListInputResponse, E>;

    /// Delete user.
    ///
    /// DeleteUser - DELETE /v2/user/{username}
    async fn delete_user(
        &self,

        method: &Method,
        host: &Host,
        cookies: &CookieJar,
        claims: &Self::Claims,
        path_params: &models::DeleteUserPathParams,
    ) -> Result<DeleteUserResponse, E>;

    /// Get user by user name.
    ///
    /// GetUserByName - GET /v2/user/{username}
    async fn get_user_by_name(
        &self,

        method: &Method,
        host: &Host,
        cookies: &CookieJar,
        path_params: &models::GetUserByNamePathParams,
    ) -> Result<GetUserByNameResponse, E>;

    /// Logs user into the system.
    ///
    /// LoginUser - GET /v2/user/login
    async fn login_user(
        &self,

        method: &Method,
        host: &Host,
        cookies: &CookieJar,
        query_params: &models::LoginUserQueryParams,
    ) -> Result<LoginUserResponse, E>;

    /// Logs out current logged in user session.
    ///
    /// LogoutUser - GET /v2/user/logout
    async fn logout_user(
        &self,

        method: &Method,
        host: &Host,
        cookies: &CookieJar,
        claims: &Self::Claims,
    ) -> Result<LogoutUserResponse, E>;

    /// Updated user.
    ///
    /// UpdateUser - PUT /v2/user/{username}
    async fn update_user(
        &self,

        method: &Method,
        host: &Host,
        cookies: &CookieJar,
        claims: &Self::Claims,
        path_params: &models::UpdateUserPathParams,
        body: &models::User,
    ) -> Result<UpdateUserResponse, E>;
}
