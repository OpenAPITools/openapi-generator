use swagger::{
    ApiError,
    auth::{Basic, Bearer},
    Has,
    XSpanIdString};
use openapi_v3::{AuthenticationApi, Claims};
use crate::server::Server;
use jsonwebtoken::{decode, errors as JwtError, decode_header, DecodingKey, TokenData, Validation};
use swagger::auth::Authorization;
use log::{error, debug};

// NOTE: Set environment variable RUST_LOG to the name of the executable (or "cargo run") to activate console logging for all loglevels.
//     See https://docs.rs/env_logger/latest/env_logger/  for more details


/// Get a dummy claim with full permissions (all scopes) for testing purposes
fn full_permission_claim() -> Claims {
    // In this example code all available Scopes are added, so the current Bearer Token gets fully authorization.
    Claims {
        sub: "tester@acme.com".to_owned(),
        company: "ACME".to_owned(),
        iss: "mini-bank-IDP".to_owned(),
        aud: "org.acme.Resource_Server".to_string(),
        // added a very long expiry time
        exp: 10000000000,
        scopes:
          [
                    "test.read",
                    "test.write",
                    "additional.test.read",
                    "additional.test.write",
          ].join::<&str>(", ")
    }
}



/// Extract the data from a Bearer token using the provided Key (secret) and using the HS512-algorithm in this example.
fn extract_token_data(token: &str, key: &[u8]) -> Result<TokenData<Claims>, JwtError::Error> {

    // Ensure that you set the correct algorithm and correct key.
    // See https://github.com/Keats/jsonwebtoken for more information.
    let header = decode_header(token)?;
    let validation = {
        let mut validation = Validation::new(header.alg);
        validation.set_audience(&["org.acme.Resource_Server"]);
        validation.validate_exp = true;
        validation
    };

    let token_data = decode::<Claims>(
        &token,
        &DecodingKey::from_secret(key),
        &validation,
    )?;

    Ok(token_data)
}

/// Build a swagger-Authorization based on the claims (Assuming claims have been extracted from a validated token)
fn build_authorization(claims: Claims) -> Authorization {
    let mut scopes = std::collections::BTreeSet::<String>::new();
    claims
        .scopes
        .split(",")
        .map(|s| s.trim())
        .for_each(|s| {let _ = scopes.insert(s.to_string()); });
    let scopes = swagger::auth::Scopes::Some(scopes);

    Authorization{
        subject: claims.sub,
        scopes,
        issuer: Some(claims.iss)}
}

fn get_jwt_error_string(error: JwtError::Error) -> String {
    match error.kind() {
        JwtError::ErrorKind::InvalidSignature => "Incorrect token signature".to_owned(),
        JwtError::ErrorKind::InvalidAlgorithm => "The Algorithm is not correct".to_owned(),
        JwtError::ErrorKind::ExpiredSignature => "The token has expired".to_owned(),
        JwtError::ErrorKind::Base64(e) => format!("Base64 decode failed: {e}"),
        JwtError::ErrorKind::Json(e) => format!("JSON decoding: {e}"),
        JwtError::ErrorKind::Utf8(e) => format!("Invalid UTF-8: {e}"),
        _ => error.to_string()
    }
}


impl<C> AuthenticationApi for Server<C> where C: Has<XSpanIdString> + Send + Sync {

    /// Implementation of the method to map a Bearer-token to an Authorization
    fn bearer_authorization(&self, bearer: &Bearer) -> Result<Authorization, ApiError> {
        debug!("\tAuthorizationApi: Received Bearer-token, {bearer:#?}");

        match extract_token_data(&bearer.token, b"secret") {
            Ok(auth_data) => {
                debug!("\tUnpack auth_data as: {auth_data:#?}");
                let authorization = build_authorization(auth_data.claims);
                Ok(authorization)
            },
            Err(err) => {
                let msg = get_jwt_error_string(err);
                error!("Failed to unpack Bearer-token: {msg}");
                Err(ApiError(msg))
            }
        }
    }

    /// Implementation of the method to map an api-key to an Authorization
    fn apikey_authorization(&self, api_key: &str) -> Result<Authorization, ApiError> {
        debug!("\tAuthorizationApi: Received api-key, {api_key:#?}");

        // TODO: insert the logic to map received apikey to the set of claims
        let claims = full_permission_claim();

        // and build an authorization out of it
        Ok(build_authorization(claims))
    }

    /// Implementation of the method to map a basic authentication (username and password) to an Authorization
    fn basic_authorization(&self, basic: &Basic) -> Result<Authorization, ApiError> {
        debug!("\tAuthorizationApi: Received Basic-token, {basic:#?}");

        // TODO: insert the logic to map received apikey to the set of claims
        let claims = full_permission_claim();

        // and build an authorization out of it
        Ok(build_authorization(claims))
    }

}

