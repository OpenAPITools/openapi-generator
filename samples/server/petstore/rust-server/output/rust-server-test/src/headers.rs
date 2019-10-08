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


    

    

    

    

    


