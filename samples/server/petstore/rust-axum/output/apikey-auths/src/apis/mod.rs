pub mod payments;

/// API Key Authentication - Header.
#[async_trait::async_trait]
pub trait ApiKeyAuthHeader {
    type Claims;

    async fn extract_token_from_header(
        &self,
        headers: &axum::http::header::HeaderMap,
        key: &str,
    ) -> Option<Self::Claims>;
}
/// Cookie Authentication.
#[async_trait::async_trait]
pub trait CookieAuthentication {
    type Claims;

    async fn extract_token_from_cookie(
        &self,
        cookies: &axum_extra::extract::CookieJar,
        key: &str,
    ) -> Option<Self::Claims>;
}
