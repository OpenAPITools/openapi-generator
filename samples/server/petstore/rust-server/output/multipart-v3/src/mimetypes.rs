/// mime types for requests and responses

pub mod responses {
    use hyper::mime::*;

    // The macro is called per-operation to beat the recursion limit

}

pub mod requests {
    use hyper::mime::*;

    lazy_static! {
        /// Create Mime objects for the request content types for MultipartRequestPost
        pub static ref MULTIPART_REQUEST_POST: Mime = "multipart/form-data".parse().unwrap();
    }

}
