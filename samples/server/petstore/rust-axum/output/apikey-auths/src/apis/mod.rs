pub mod payments;

/// API Key Authentication - Header.
#[async_trait::async_trait]
pub trait ApiKeyAuthHeader {
    type Claims;

    /// Extracting Claims from Header. Return None if the Claims are invalid.
    async fn extract_claims_from_header(
        &self,
        headers: &axum::http::header::HeaderMap,
        key: &str,
    ) -> Option<Self::Claims>;
}

/// Cookie Authentication.
#[async_trait::async_trait]
pub trait CookieAuthentication {
    type Claims;

    /// Extracting Claims from Cookie. Return None if the Claims are invalid.
    async fn extract_claims_from_cookie(
        &self,
        cookies: &axum_extra::extract::CookieJar,
        key: &str,
    ) -> Option<Self::Claims>;
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[non_exhaustive]
pub enum BasicAuthKind {
    Basic,
    Bearer,
}

/// API Key Authentication - Authentication Header.
/// For `Basic token` and `Bearer token`
#[async_trait::async_trait]
pub trait ApiAuthBasic {
    type Claims;

    /// Extracting Claims from Header. Return None if the Claims are invalid.
    async fn extract_claims_from_auth_header(
        &self,
        kind: BasicAuthKind,
        headers: &axum::http::header::HeaderMap,
        key: &str,
    ) -> Option<Self::Claims>;
}

// Error handler for unhandled errors.
#[async_trait::async_trait]
pub trait ErrorHandler<E: std::fmt::Debug + Send + Sync + 'static = ()> {
<<<<<<< HEAD
<<<<<<< HEAD
=======
>>>>>>> 00e7ad2ac29 (Pass in method, host and cookies to error handler)
    #[allow(unused_variables)]
    #[tracing::instrument(skip_all)]
    async fn handle_error(
        &self,
<<<<<<< HEAD
<<<<<<< HEAD
<<<<<<< HEAD
<<<<<<< HEAD
=======
>>>>>>> 46691510f63 (Update deps)
=======
>>>>>>> 73be82180e8 (Rebase rust-axum-error-handling)
        method: &::http::Method,
        host: &axum_extra::extract::Host,
        cookies: &axum_extra::extract::CookieJar,
        error: E,
    ) -> Result<axum::response::Response, http::StatusCode> {
        tracing::error!("Unhandled error: {:?}", error);
        axum::response::Response::builder()
            .status(http::StatusCode::INTERNAL_SERVER_ERROR)
=======
    #[tracing::instrument(skip(self))]
    async fn handle_error(&self, error: E) -> Result<axum::response::Response, http::StatusCode> {
=======
        method: ::http::Method,
        host: axum_extra::extract::Host,
        cookies: axum_extra::extract::CookieJar,
=======
        method: &::http::Method,
        host: &axum_extra::extract::Host,
        cookies: &axum_extra::extract::CookieJar,
>>>>>>> 3d833fd5ff9 (Make API methods take references instead of ownership)
        error: E,
    ) -> Result<axum::response::Response, http::StatusCode> {
>>>>>>> 00e7ad2ac29 (Pass in method, host and cookies to error handler)
        tracing::error!("Unhandled error: {:?}", error);
        axum::response::Response::builder()
            .status(500)
>>>>>>> 9841fa4dc2c (Implement a custom error handler for unhandled or generic endpoint errors)
            .body(axum::body::Body::empty())
            .map_err(|_| http::StatusCode::INTERNAL_SERVER_ERROR)
    }
}
