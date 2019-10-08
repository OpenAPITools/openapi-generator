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


    

    

    

    

/// Responses Header SuccessInfo: Success-Info => String
pub struct ResponseSuccessInfo(pub String);

lazy_static! {
    static ref ResponseSuccessInfoHeader: HeaderName = HeaderName::from_static("Success-Info");
}

impl Header for ResponseSuccessInfo {
    fn name() -> &'static HeaderName {
        &ResponseSuccessInfoHeader
    }

    fn decode<'i, I>(values: &mut I) -> Result<Self, headers::Error>
        where
            I: Iterator<Item = &'i HeaderValue>,
    {
        let value = values
            .next()
            .ok_or_else(headers::Error::invalid)?;

        let value = value.to_str().map_err(|_| headers::Error::invalid())?;
        Ok(ResponseSuccessInfo(value.parse().map_err(|_| headers::Error::invalid())?))
    }

    fn encode<E>(&self, values: &mut E)
        where
            E: Extend<HeaderValue>,
    {
        let value = HeaderValue::from_str(&self.0.to_string()).unwrap();

        values.extend(std::iter::once(value));
    }
}
    
/// Responses Header FurtherInfo: Further-Info => String
pub struct ResponseFurtherInfo(pub String);

lazy_static! {
    static ref ResponseFurtherInfoHeader: HeaderName = HeaderName::from_static("Further-Info");
}

impl Header for ResponseFurtherInfo {
    fn name() -> &'static HeaderName {
        &ResponseFurtherInfoHeader
    }

    fn decode<'i, I>(values: &mut I) -> Result<Self, headers::Error>
        where
            I: Iterator<Item = &'i HeaderValue>,
    {
        let value = values
            .next()
            .ok_or_else(headers::Error::invalid)?;

        let value = value.to_str().map_err(|_| headers::Error::invalid())?;
        Ok(ResponseFurtherInfo(value.parse().map_err(|_| headers::Error::invalid())?))
    }

    fn encode<E>(&self, values: &mut E)
        where
            E: Extend<HeaderValue>,
    {
        let value = HeaderValue::from_str(&self.0.to_string()).unwrap();

        values.extend(std::iter::once(value));
    }
}
    
/// Responses Header FailureInfo: Failure-Info => String
pub struct ResponseFailureInfo(pub String);

lazy_static! {
    static ref ResponseFailureInfoHeader: HeaderName = HeaderName::from_static("Failure-Info");
}

impl Header for ResponseFailureInfo {
    fn name() -> &'static HeaderName {
        &ResponseFailureInfoHeader
    }

    fn decode<'i, I>(values: &mut I) -> Result<Self, headers::Error>
        where
            I: Iterator<Item = &'i HeaderValue>,
    {
        let value = values
            .next()
            .ok_or_else(headers::Error::invalid)?;

        let value = value.to_str().map_err(|_| headers::Error::invalid())?;
        Ok(ResponseFailureInfo(value.parse().map_err(|_| headers::Error::invalid())?))
    }

    fn encode<E>(&self, values: &mut E)
        where
            E: Extend<HeaderValue>,
    {
        let value = HeaderValue::from_str(&self.0.to_string()).unwrap();

        values.extend(std::iter::once(value));
    }
}
    
    

    

    

    

    

    

    


