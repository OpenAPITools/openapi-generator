/// mime types for requests and responses

pub mod responses {
    use hyper::mime::*;

    // The macro is called per-operation to beat the recursion limit
    /// Create Mime objects for the response content types for UuidGet
    lazy_static! {
        pub static ref UUID_GET_DUPLICATE_RESPONSE_LONG_TEXT: Mime = "application/json".parse().unwrap();
    }

}

pub mod requests {
    use hyper::mime::*;
   /// Create Mime objects for the request content types for RequiredOctetStreamPut
    lazy_static! {
        pub static ref REQUIRED_OCTET_STREAM_PUT: Mime = "application/octet-stream".parse().unwrap();
    }
   /// Create Mime objects for the request content types for XmlExtraPost
    lazy_static! {
        pub static ref XML_EXTRA_POST: Mime = "application/xml".parse().unwrap();
    }
   /// Create Mime objects for the request content types for XmlOtherPost
    lazy_static! {
        pub static ref XML_OTHER_POST: Mime = "application/xml".parse().unwrap();
    }
   /// Create Mime objects for the request content types for XmlOtherPut
    lazy_static! {
        pub static ref XML_OTHER_PUT: Mime = "application/xml".parse().unwrap();
    }
   /// Create Mime objects for the request content types for XmlPost
    lazy_static! {
        pub static ref XML_POST: Mime = "application/xml".parse().unwrap();
    }
   /// Create Mime objects for the request content types for XmlPut
    lazy_static! {
        pub static ref XML_PUT: Mime = "application/xml".parse().unwrap();
    }

}
