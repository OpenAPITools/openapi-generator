/// mime types for requests and responses

pub mod responses {
    use mime::*;
    use lazy_static::lazy_static;

    // The macro is called per-operation to beat the recursion limit

    lazy_static! {
        /// Create Mime objects for the response content types for FileResponseGet
        pub static ref FILE_RESPONSE_GET_SUCCESS: Mime = "application/json".parse().unwrap();
    }

    lazy_static! {
        /// Create Mime objects for the response content types for HtmlPost
        pub static ref HTML_POST_SUCCESS: Mime = "text/html".parse().unwrap();
    }

    lazy_static! {
        /// Create Mime objects for the response content types for RawJsonGet
        pub static ref RAW_JSON_GET_SUCCESS: Mime = "*/*".parse().unwrap();
    }

}

pub mod requests {
    use mime::*;
    use lazy_static::lazy_static;


    lazy_static! {
        /// Create Mime objects for the request content types for DummyPut
        pub static ref DUMMY_PUT: Mime = "application/json".parse().unwrap();
    }

    lazy_static! {
        /// Create Mime objects for the request content types for HtmlPost
        pub static ref HTML_POST: Mime = "text/html".parse().unwrap();
    }

}
