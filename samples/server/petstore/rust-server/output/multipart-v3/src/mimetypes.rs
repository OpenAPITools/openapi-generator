/// mime types for requests and responses

pub mod responses {
    use mime::*;
    use lazy_static::lazy_static;

    // The macro is called per-operation to beat the recursion limit

}

pub mod requests {
    use mime::*;
    use lazy_static::lazy_static;


    lazy_static! {
        /// Create Mime objects for the request content types for MultipartRequestPost
        pub static ref MULTIPART_REQUEST_POST: Mime = "multipart/form-data".parse().unwrap();
    }

}
