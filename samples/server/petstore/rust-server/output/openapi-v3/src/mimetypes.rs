/// mime types for requests and responses

pub mod responses {
    use hyper::mime::*;

    // The macro is called per-operation to beat the recursion limit

}

pub mod requests {
    use hyper::mime::*;
   /// Create Mime objects for the request content types for XmlPost
    lazy_static! {
        pub static ref XML_POST: Mime = "application/xml".parse().unwrap();
    }

}
