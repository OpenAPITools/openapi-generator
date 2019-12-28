/// mime types for requests and responses

pub mod responses {


    /// Create &str objects for the response content types for MultigetGet
    pub static MULTIGET_GET_JSON_RSP: &str = "application/json";

    /// Create &str objects for the response content types for MultigetGet
    pub static MULTIGET_GET_XML_RSP: &str = "application/xml";

    /// Create &str objects for the response content types for MultigetGet
    pub static MULTIGET_GET_OCTET_RSP: &str = "application/octet-stream";

    /// Create &str objects for the response content types for MultigetGet
    pub static MULTIGET_GET_STRING_RSP: &str = "text/plain";

    /// Create &str objects for the response content types for MultigetGet
    pub static MULTIGET_GET_DUPLICATE_RESPONSE_LONG_TEXT: &str = "application/json";

    /// Create &str objects for the response content types for MultigetGet
    pub static MULTIGET_GET_DUPLICATE_RESPONSE_LONG_TEXT_2: &str = "application/json";

    /// Create &str objects for the response content types for MultigetGet
    pub static MULTIGET_GET_DUPLICATE_RESPONSE_LONG_TEXT_3: &str = "application/json";

    /// Create &str objects for the response content types for ResponsesWithHeadersGet
    pub static RESPONSES_WITH_HEADERS_GET_SUCCESS: &str = "application/json";

    /// Create &str objects for the response content types for UuidGet
    pub static UUID_GET_DUPLICATE_RESPONSE_LONG_TEXT: &str = "application/json";

}

pub mod requests {

    /// Create &str objects for the request content types for RequiredOctetStreamPut
    pub static REQUIRED_OCTET_STREAM_PUT: &str = "application/octet-stream";

    /// Create &str objects for the request content types for XmlExtraPost
    pub static XML_EXTRA_POST: &str = "application/xml";

    /// Create &str objects for the request content types for XmlOtherPost
    pub static XML_OTHER_POST: &str = "application/xml";

    /// Create &str objects for the request content types for XmlOtherPut
    pub static XML_OTHER_PUT: &str = "application/xml";

    /// Create &str objects for the request content types for XmlPost
    pub static XML_POST: &str = "application/xml";

    /// Create &str objects for the request content types for XmlPut
    pub static XML_PUT: &str = "application/xml";

}
