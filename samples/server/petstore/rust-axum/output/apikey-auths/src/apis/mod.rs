pub mod payments;

/// API Key Authentication - Header.
pub trait ApiKeyAuthHeader {
    fn extract_token_from_header(
        &self,
        headers: &axum::http::header::HeaderMap,
        key: &str,
    ) -> Option<String>;
}
/// Cookie Authentication.
pub trait CookieAuthentication {
    fn extract_token_from_cookie(
        &self,
        cookies: &axum_extra::extract::CookieJar,
        key: &str,
    ) -> Option<String>;
}
