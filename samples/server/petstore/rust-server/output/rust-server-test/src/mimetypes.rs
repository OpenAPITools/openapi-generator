/// mime types for requests and responses

pub mod responses {


    /// Create &str objects for the response content types for FileResponseGet
    pub static FILE_RESPONSE_GET_SUCCESS: &str = "application/json";

    /// Create &str objects for the response content types for HtmlPost
    pub static HTML_POST_SUCCESS: &str = "text/html";

    /// Create &str objects for the response content types for RawJsonGet
    pub static RAW_JSON_GET_SUCCESS: &str = "*/*";

}

pub mod requests {

    /// Create &str objects for the request content types for DummyPut
    pub static DUMMY_PUT: &str = "application/json";

    /// Create &str objects for the request content types for HtmlPost
    pub static HTML_POST: &str = "text/html";

}
