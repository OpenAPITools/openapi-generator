use std::collections::BTreeSet;
use crate::server::Authorization;
use serde::{Deserialize, Serialize};
use swagger::{ApiError, auth::{Basic, Bearer}};

#[derive(Debug, Serialize, Deserialize)]
pub struct Claims {
    pub sub: String,
    pub iss: String,
    pub aud: String,
    pub company: String,
    pub exp: u64,
    pub scopes: String,
}


pub trait AuthenticationApi {

    /// Method should be implemented (see example-code) to map Bearer-token to an Authorization
    fn bearer_authorization(&self, token: &Bearer) -> Result<Authorization, ApiError>;

    /// Method should be implemented (see example-code) to map ApiKey to an Authorization
    fn apikey_authorization(&self, token: &str) -> Result<Authorization, ApiError>;

    /// Method should be implemented (see example-code) to map Basic (Username:password) to an Authorization
    fn basic_authorization(&self, basic: &Basic) -> Result<Authorization, ApiError>;
} 

// Implement it for AllowAllAuthenticator (dummy is needed, but should not used as we have Bearer authorization)
use swagger::auth::{AllowAllAuthenticator, RcBound, Scopes};

fn dummy_authorization() -> Authorization {
    // Is called when MakeAllowAllAuthenticator is added to the stack. This is not needed as we have Bearer-authorization in the example-code.
    // However, if you want to use it anyway this can not be unimplemented, so dummy implementation added.
    // unimplemented!()
    Authorization{
        subject: "Dummy".to_owned(),
        scopes: Scopes::Some(BTreeSet::new()), // create an empty scope, as this should not be used
        issuer: None
    }
}

impl<T, RC> AuthenticationApi for AllowAllAuthenticator<T, RC>
where
    RC: RcBound,
    RC::Result: Send + 'static {

    /// Get method to map Bearer-token to an Authorization
    fn bearer_authorization(&self, _token: &Bearer) -> Result<Authorization, ApiError> {
        Ok(dummy_authorization())
    }

    /// Get method to map api-key to an Authorization
    fn apikey_authorization(&self, _apikey: &str) -> Result<Authorization, ApiError> {
        Ok(dummy_authorization())
    }

    /// Get method to map basic token to an Authorization
    fn basic_authorization(&self, _basic: &Basic) -> Result<Authorization, ApiError> {
        Ok(dummy_authorization())
    }
}
