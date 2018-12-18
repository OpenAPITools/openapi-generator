/// mime types for requests and responses

pub mod responses {
    use hyper::mime::*;

    // The macro is called per-operation to beat the recursion limit
    /// Create Mime objects for the response content types for XmlPost
    lazy_static! {
        pub static ref XML_POST_OK: Mime = "application/xml".parse().unwrap();
    }

}

pub mod requests {
    use hyper::mime::*;
   /// Create Mime objects for the request content types for XmlPost
    lazy_static! {
        pub static ref XML_POST: Mime = "application/xml".parse().unwrap();
    }

}
