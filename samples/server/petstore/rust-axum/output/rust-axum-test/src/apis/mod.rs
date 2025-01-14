pub mod default;

// Error handler for unhandled errors.
#[async_trait::async_trait]
pub trait ErrorHandler<E: std::fmt::Debug + Send + Sync + 'static = ()> {
    #[tracing::instrument(skip(self))]
    async fn handle_error(&self, error: E) -> Result<axum::response::Response, http::StatusCode> {
        tracing::error!("Unhandled error: {:?}", error);
        axum::response::Response::builder()
            .status(500)
            .body(axum::body::Body::empty())
            .map_err(|_| http::StatusCode::INTERNAL_SERVER_ERROR)
    }
}
