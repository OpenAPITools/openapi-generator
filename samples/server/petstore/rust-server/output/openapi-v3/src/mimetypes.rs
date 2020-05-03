/// mime types for requests and responses

pub mod responses {
    use hyper::mime::*;

    // The macro is called per-operation to beat the recursion limit

    lazy_static! {
        /// Create Mime objects for the response content types for MultigetGet
        pub static ref MULTIGET_GET_JSON_RSP: Mime = "application/json".parse().unwrap();
    }

    lazy_static! {
        /// Create Mime objects for the response content types for MultigetGet
        pub static ref MULTIGET_GET_XML_RSP: Mime = "application/xml".parse().unwrap();
    }

    lazy_static! {
        /// Create Mime objects for the response content types for MultigetGet
        pub static ref MULTIGET_GET_OCTET_RSP: Mime = "application/octet-stream".parse().unwrap();
    }

    lazy_static! {
        /// Create Mime objects for the response content types for MultigetGet
        pub static ref MULTIGET_GET_STRING_RSP: Mime = "text/plain".parse().unwrap();
    }

    lazy_static! {
        /// Create Mime objects for the response content types for MultigetGet
        pub static ref MULTIGET_GET_DUPLICATE_RESPONSE_LONG_TEXT: Mime = "application/json".parse().unwrap();
    }

    lazy_static! {
        /// Create Mime objects for the response content types for MultigetGet
        pub static ref MULTIGET_GET_DUPLICATE_RESPONSE_LONG_TEXT_2: Mime = "application/json".parse().unwrap();
    }

    lazy_static! {
        /// Create Mime objects for the response content types for MultigetGet
        pub static ref MULTIGET_GET_DUPLICATE_RESPONSE_LONG_TEXT_3: Mime = "application/json".parse().unwrap();
    }

    lazy_static! {
        /// Create Mime objects for the response content types for ResponsesWithHeadersGet
        pub static ref RESPONSES_WITH_HEADERS_GET_SUCCESS: Mime = "application/json".parse().unwrap();
    }

    lazy_static! {
        /// Create Mime objects for the response content types for UuidGet
        pub static ref UUID_GET_DUPLICATE_RESPONSE_LONG_TEXT: Mime = "application/json".parse().unwrap();
    }

    lazy_static! {
        /// Create Mime objects for the response content types for XmlOtherPost
        pub static ref XML_OTHER_POST_OK: Mime = "text/xml".parse().unwrap();
    }

}

pub mod requests {
    use hyper::mime::*;

    lazy_static! {
        /// Create Mime objects for the request content types for RequiredOctetStreamPut
        pub static ref REQUIRED_OCTET_STREAM_PUT: Mime = "application/octet-stream".parse().unwrap();
    }

    lazy_static! {
        /// Create Mime objects for the request content types for XmlExtraPost
        pub static ref XML_EXTRA_POST: Mime = "application/xml".parse().unwrap();
    }

    lazy_static! {
        /// Create Mime objects for the request content types for XmlOtherPost
        pub static ref XML_OTHER_POST: Mime = "text/xml".parse().unwrap();
    }

    lazy_static! {
        /// Create Mime objects for the request content types for XmlOtherPut
        pub static ref XML_OTHER_PUT: Mime = "application/xml".parse().unwrap();
    }

    lazy_static! {
        /// Create Mime objects for the request content types for XmlPost
        pub static ref XML_POST: Mime = "application/xml".parse().unwrap();
    }

    lazy_static! {
        /// Create Mime objects for the request content types for XmlPut
        pub static ref XML_PUT: Mime = "application/xml".parse().unwrap();
    }

}
