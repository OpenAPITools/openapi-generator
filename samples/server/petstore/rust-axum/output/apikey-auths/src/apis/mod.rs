pub mod payments;

/// API Key Authentication - Header.
#[async_trait::async_trait]
pub trait ApiKeyAuthHeader {
    type Claims;

    /// Extracting Claims from Header. Return None if the Claims is invalid.
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

    /// Extracting Claims from Cookie. Return None if the Claims is invalid.
    async fn extract_claims_from_cookie(
        &self,
        cookies: &axum_extra::extract::CookieJar,
        key: &str,
    ) -> Option<Self::Claims>;
}
