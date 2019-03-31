/// mime types for requests and responses

pub mod responses {
    use hyper::mime::*;

    // The macro is called per-operation to beat the recursion limit
    /// Create Mime objects for the response content types for FileResponseGet
    lazy_static! {
        pub static ref FILE_RESPONSE_GET_SUCCESS: Mime = "application/json".parse().unwrap();
    }
    /// Create Mime objects for the response content types for HtmlPost
    lazy_static! {
        pub static ref HTML_POST_SUCCESS: Mime = "text/html".parse().unwrap();
    }
    /// Create Mime objects for the response content types for RawJsonGet
    lazy_static! {
        pub static ref RAW_JSON_GET_SUCCESS: Mime = "*/*".parse().unwrap();
    }

}

pub mod requests {
    use hyper::mime::*;
   /// Create Mime objects for the request content types for DummyPut
    lazy_static! {
        pub static ref DUMMY_PUT: Mime = "application/json".parse().unwrap();
    }
   /// Create Mime objects for the request content types for HtmlPost
    lazy_static! {
        pub static ref HTML_POST: Mime = "text/html".parse().unwrap();
    }

}
