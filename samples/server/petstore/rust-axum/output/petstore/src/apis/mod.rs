pub mod pet;
pub mod store;
pub mod user;

/// API Key Authentication - Header.
pub trait ApiKeyAuthHeader {
    fn extract_token_from_header(
        &self,
        headers: &axum::http::header::HeaderMap,
        key: &str,
    ) -> Option<String>;
}
