pub mod another_fake;
pub mod fake;
pub mod fake_classname_tags123;
pub mod pet;
pub mod store;
pub mod user;

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
