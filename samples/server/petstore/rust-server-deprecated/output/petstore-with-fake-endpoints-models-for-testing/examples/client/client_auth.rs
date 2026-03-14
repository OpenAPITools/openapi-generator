use petstore_with_fake_endpoints_models_for_testing::Claims;
use jsonwebtoken::{encode, errors::Error as JwtError, Algorithm, EncodingKey, Header};
use log::debug;

/// build an encrypted token with the provided claims.
pub fn build_token(my_claims: Claims, key: &[u8]) -> Result<String, JwtError> {

    // Ensure that you set the correct algorithm and correct key.
    // See https://github.com/Keats/jsonwebtoken for more information.
    let header =
        Header { kid: Some("signing_key".to_owned()), alg: Algorithm::HS512, ..Default::default() };

    let token = encode(&header, &my_claims, &EncodingKey::from_secret(key))?;
    debug!("Derived token: {:?}", token);

    Ok(token)
}
