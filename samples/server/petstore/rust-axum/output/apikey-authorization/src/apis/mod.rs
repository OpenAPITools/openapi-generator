pub mod payments;

#[allow(dead_code)]
#[derive(Debug, Eq, PartialEq)]
pub enum Authorization {
    Authorized,
    Forbidden,
}

pub mod event {
    /// Anything to be recorded.
    pub type Event = std::collections::HashMap<String, String>;

    pub mod convention {
        pub const EVENT_SERVICE: &str = "_service_";
        pub const EVENT_ACTOR: &str = "_actor_";
        pub const EVENT_ACTION: &str = "_action_";
        pub const EVENT_RESOURCE_TYPE: &str = "_resource_type_";
        pub const EVENT_RESOURCE: &str = "_resource_";
        pub const EVENT_STATUS_CODE: &str = "_status_code_";
        pub const EVENT_LATENCY_SECS: &str = "_latency_secs_";
        pub const EVENT_TIMESTAMP: &str = "timestamp";
    }
}

#[async_trait::async_trait]
pub trait EventDispatcher {
    fn service_name(&self) -> String;
    async fn dispatch(&self, event: event::Event) {}
}

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
    #[allow(unused_variables)]
    #[tracing::instrument(skip_all)]
    async fn handle_error(
        &self,
        method: &::http::Method,
        host: &axum_extra::extract::Host,
        cookies: &axum_extra::extract::CookieJar,
        error: E,
    ) -> Result<axum::response::Response, http::StatusCode> {
        tracing::error!("Unhandled error: {:?}", error);
        axum::response::Response::builder()
            .status(http::StatusCode::INTERNAL_SERVER_ERROR)
            .body(axum::body::Body::empty())
            .map_err(|_| http::StatusCode::INTERNAL_SERVER_ERROR)
    }
}
