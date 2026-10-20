use super::Service;
use crate::{Api, AuthenticationApi};
use headers::authorization::{Basic, Bearer};
use swagger::{
    ApiError,
    Authorization,
    Has,
    XSpanIdString
};

impl<T,C> AuthenticationApi for Service<T, C> where
T: Api<C> + Clone + Send + 'static + AuthenticationApi,
C: Has<XSpanIdString> + Has<Option<Authorization>> + Send + Sync + 'static {

    /// Passthrough of the task to the api-implementation
    fn bearer_authorization(&self, token: &Bearer) -> Result<Authorization, ApiError> {
        self.api_impl.bearer_authorization(token)
    }

    /// Passthrough of the task to the api-implementation
    fn apikey_authorization(&self, token: &str) -> Result<Authorization, ApiError> {
        self.api_impl.apikey_authorization(token)
    }

    /// Passthrough of the task to the api-implementation
    fn basic_authorization(&self, basic: &Basic) -> Result<Authorization, ApiError> {
        self.api_impl.basic_authorization(basic)
    }
}
