use headers::{Header, HeaderName, HeaderValue};
use lazy_static::lazy_static;

pub struct Warning(pub String);

lazy_static! {
    static ref WarningHeader: HeaderName = HeaderName::from_static("Warning");
}

impl Header for Warning {
    fn name() -> &'static HeaderName {
        &WarningHeader
    }

    fn decode<'i, I>(values: &mut I) -> Result<Self, headers::Error>
        where
            I: Iterator<Item = &'i HeaderValue>,
    {
        let value = values
            .next()
            .ok_or_else(headers::Error::invalid)?;

        let value = value.to_str().map_err(|_| headers::Error::invalid())?;
        Ok(Warning(value.to_owned()))
    }

    fn encode<E>(&self, values: &mut E)
        where
            E: Extend<HeaderValue>,
    {
        let value = HeaderValue::from_str(&self.0).unwrap();

        values.extend(std::iter::once(value));
    }
}


    

    

    

    

    

    

    

    

    

/// Params Header EnumHeaderStringArray: enum_header_string_array => (String)*
pub struct RequestEnumHeaderStringArray(pub (String)*);

lazy_static! {
    static ref RequestEnumHeaderStringArrayHeader: HeaderName = HeaderName::from_static("enum_header_string_array");
}

impl Header for RequestEnumHeaderStringArray {
    fn name() -> &'static HeaderName {
        &RequestEnumHeaderStringArrayHeader
    }

    fn decode<'i, I>(values: &mut I) -> Result<Self, headers::Error>
        where
            I: Iterator<Item = &'i HeaderValue>,
    {
        let value = values
            .next()
            .ok_or_else(headers::Error::invalid)?;

        let value = value.to_str().map_err(|_| headers::Error::invalid())?;
        Ok(RequestEnumHeaderStringArray(value.parse().map_err(|_| headers::Error::invalid())?))
    }

    fn encode<E>(&self, values: &mut E)
        where
            E: Extend<HeaderValue>,
    {
        let value = HeaderValue::from_str(&self.0.to_string()).unwrap();

        values.extend(std::iter::once(value));
    }
}
/// Params Header EnumHeaderString: enum_header_string => [String]
pub struct RequestEnumHeaderString(pub String);

lazy_static! {
    static ref RequestEnumHeaderStringHeader: HeaderName = HeaderName::from_static("enum_header_string");
}

impl Header for RequestEnumHeaderString {
    fn name() -> &'static HeaderName {
        &RequestEnumHeaderStringHeader
    }

    fn decode<'i, I>(values: &mut I) -> Result<Self, headers::Error>
        where
            I: Iterator<Item = &'i HeaderValue>,
    {
        let value = values
            .next()
            .ok_or_else(headers::Error::invalid)?;

        let value = value.to_str().map_err(|_| headers::Error::invalid())?;
        Ok(RequestEnumHeaderString(value.parse().map_err(|_| headers::Error::invalid())?))
    }

    fn encode<E>(&self, values: &mut E)
        where
            E: Extend<HeaderValue>,
    {
        let value = HeaderValue::from_str(&self.0.to_string()).unwrap();

        values.extend(std::iter::once(value));
    }
}
    

    

    

    

    

/// Params Header ApiKey: api_key => [String]
pub struct RequestApiKey(pub String);

lazy_static! {
    static ref RequestApiKeyHeader: HeaderName = HeaderName::from_static("api_key");
}

impl Header for RequestApiKey {
    fn name() -> &'static HeaderName {
        &RequestApiKeyHeader
    }

    fn decode<'i, I>(values: &mut I) -> Result<Self, headers::Error>
        where
            I: Iterator<Item = &'i HeaderValue>,
    {
        let value = values
            .next()
            .ok_or_else(headers::Error::invalid)?;

        let value = value.to_str().map_err(|_| headers::Error::invalid())?;
        Ok(RequestApiKey(value.parse().map_err(|_| headers::Error::invalid())?))
    }

    fn encode<E>(&self, values: &mut E)
        where
            E: Extend<HeaderValue>,
    {
        let value = HeaderValue::from_str(&self.0.to_string()).unwrap();

        values.extend(std::iter::once(value));
    }
}
    

    

    

    
/// Vendor Extensions Auth Header ApiKeyGetPetById1: api_key => String
pub struct ApiKeyGetPetById1(pub String);

lazy_static! {
    static ref ApiKeyGetPetById1Header: HeaderName = HeaderName::from_static("api_key");
}

impl Header for ApiKeyGetPetById1 {
    fn name() -> &'static HeaderName {
        &ApiKeyGetPetById1Header
    }

    fn decode<'i, I>(values: &mut I) -> Result<Self, headers::Error>
        where
            I: Iterator<Item = &'i HeaderValue>,
    {
        let value = values
            .next()
            .ok_or_else(headers::Error::invalid)?;

        let value = value.to_str().map_err(|_| headers::Error::invalid())?;
        Ok(ApiKeyGetPetById1(value.to_owned()))
    }

    fn encode<E>(&self, values: &mut E)
        where
            E: Extend<HeaderValue>,
    {
        let value = HeaderValue::from_str(&self.0).unwrap();

        values.extend(std::iter::once(value));
    }
}
    

    

    

    

    

    
/// Vendor Extensions Auth Header ApiKeyGetInventory1: api_key => String
pub struct ApiKeyGetInventory1(pub String);

lazy_static! {
    static ref ApiKeyGetInventory1Header: HeaderName = HeaderName::from_static("api_key");
}

impl Header for ApiKeyGetInventory1 {
    fn name() -> &'static HeaderName {
        &ApiKeyGetInventory1Header
    }

    fn decode<'i, I>(values: &mut I) -> Result<Self, headers::Error>
        where
            I: Iterator<Item = &'i HeaderValue>,
    {
        let value = values
            .next()
            .ok_or_else(headers::Error::invalid)?;

        let value = value.to_str().map_err(|_| headers::Error::invalid())?;
        Ok(ApiKeyGetInventory1(value.to_owned()))
    }

    fn encode<E>(&self, values: &mut E)
        where
            E: Extend<HeaderValue>,
    {
        let value = HeaderValue::from_str(&self.0).unwrap();

        values.extend(std::iter::once(value));
    }
}
    

    

    

    

    

    

    

    

/// Responses Header XRateLimit: X-Rate-Limit => i32
pub struct ResponseXRateLimit(pub i32);

lazy_static! {
    static ref ResponseXRateLimitHeader: HeaderName = HeaderName::from_static("X-Rate-Limit");
}

impl Header for ResponseXRateLimit {
    fn name() -> &'static HeaderName {
        &ResponseXRateLimitHeader
    }

    fn decode<'i, I>(values: &mut I) -> Result<Self, headers::Error>
        where
            I: Iterator<Item = &'i HeaderValue>,
    {
        let value = values
            .next()
            .ok_or_else(headers::Error::invalid)?;

        let value = value.to_str().map_err(|_| headers::Error::invalid())?;
        Ok(ResponseXRateLimit(value.parse().map_err(|_| headers::Error::invalid())?))
    }

    fn encode<E>(&self, values: &mut E)
        where
            E: Extend<HeaderValue>,
    {
        let value = HeaderValue::from_str(&self.0.to_string()).unwrap();

        values.extend(std::iter::once(value));
    }
}
    
/// Responses Header XExpiresAfter: X-Expires-After => chrono::DateTime<chrono::Utc>
pub struct ResponseXExpiresAfter(pub chrono::DateTime<chrono::Utc>);

lazy_static! {
    static ref ResponseXExpiresAfterHeader: HeaderName = HeaderName::from_static("X-Expires-After");
}

impl Header for ResponseXExpiresAfter {
    fn name() -> &'static HeaderName {
        &ResponseXExpiresAfterHeader
    }

    fn decode<'i, I>(values: &mut I) -> Result<Self, headers::Error>
        where
            I: Iterator<Item = &'i HeaderValue>,
    {
        let value = values
            .next()
            .ok_or_else(headers::Error::invalid)?;

        let value = value.to_str().map_err(|_| headers::Error::invalid())?;
        Ok(ResponseXExpiresAfter(value.parse().map_err(|_| headers::Error::invalid())?))
    }

    fn encode<E>(&self, values: &mut E)
        where
            E: Extend<HeaderValue>,
    {
        let value = HeaderValue::from_str(&self.0.to_string()).unwrap();

        values.extend(std::iter::once(value));
    }
}
    
    

    

    



/// Auth Methods Indexed Header ApiKey1: api_key => String
pub struct ApiKey1(pub String);

lazy_static! {
    static ref ApiKey1Header: HeaderName = HeaderName::from_static("api_key");
}

impl Header for ApiKey1 {
    fn name() -> &'static HeaderName {
        &ApiKey1Header
    }

    fn decode<'i, I>(values: &mut I) -> Result<Self, headers::Error>
        where
            I: Iterator<Item = &'i HeaderValue>,
    {
        let value = values
            .next()
            .ok_or_else(headers::Error::invalid)?;

        let value = value.to_str().map_err(|_| headers::Error::invalid())?;
        Ok(ApiKey1(value.to_owned()))
    }

    fn encode<E>(&self, values: &mut E)
    where
    E: Extend<HeaderValue>,
    {
        let value = HeaderValue::from_str(&self.0).unwrap();

        values.extend(std::iter::once(value));
    }
}
