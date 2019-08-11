/// mime types for requests and responses

pub mod responses {
    use hyper::mime::*;

    // The macro is called per-operation to beat the recursion limit

    lazy_static! {
        /// Create Mime objects for the response content types for TestSpecialTags
        pub static ref TEST_SPECIAL_TAGS_SUCCESSFUL_OPERATION: Mime = "application/json".parse().unwrap();
    }

    lazy_static! {
        /// Create Mime objects for the response content types for FakeOuterBooleanSerialize
        pub static ref FAKE_OUTER_BOOLEAN_SERIALIZE_OUTPUT_BOOLEAN: Mime = "*/*".parse().unwrap();
    }

    lazy_static! {
        /// Create Mime objects for the response content types for FakeOuterCompositeSerialize
        pub static ref FAKE_OUTER_COMPOSITE_SERIALIZE_OUTPUT_COMPOSITE: Mime = "*/*".parse().unwrap();
    }

    lazy_static! {
        /// Create Mime objects for the response content types for FakeOuterNumberSerialize
        pub static ref FAKE_OUTER_NUMBER_SERIALIZE_OUTPUT_NUMBER: Mime = "*/*".parse().unwrap();
    }

    lazy_static! {
        /// Create Mime objects for the response content types for FakeOuterStringSerialize
        pub static ref FAKE_OUTER_STRING_SERIALIZE_OUTPUT_STRING: Mime = "*/*".parse().unwrap();
    }

    lazy_static! {
        /// Create Mime objects for the response content types for TestClientModel
        pub static ref TEST_CLIENT_MODEL_SUCCESSFUL_OPERATION: Mime = "application/json".parse().unwrap();
    }

    lazy_static! {
        /// Create Mime objects for the response content types for TestClassname
        pub static ref TEST_CLASSNAME_SUCCESSFUL_OPERATION: Mime = "application/json".parse().unwrap();
    }

    lazy_static! {
        /// Create Mime objects for the response content types for FindPetsByStatus
        pub static ref FIND_PETS_BY_STATUS_SUCCESSFUL_OPERATION: Mime = "application/json".parse().unwrap();
    }

    lazy_static! {
        /// Create Mime objects for the response content types for FindPetsByTags
        pub static ref FIND_PETS_BY_TAGS_SUCCESSFUL_OPERATION: Mime = "application/json".parse().unwrap();
    }

    lazy_static! {
        /// Create Mime objects for the response content types for GetPetById
        pub static ref GET_PET_BY_ID_SUCCESSFUL_OPERATION: Mime = "application/json".parse().unwrap();
    }

    lazy_static! {
        /// Create Mime objects for the response content types for UploadFile
        pub static ref UPLOAD_FILE_SUCCESSFUL_OPERATION: Mime = "application/json".parse().unwrap();
    }

    lazy_static! {
        /// Create Mime objects for the response content types for GetInventory
        pub static ref GET_INVENTORY_SUCCESSFUL_OPERATION: Mime = "application/json".parse().unwrap();
    }

    lazy_static! {
        /// Create Mime objects for the response content types for GetOrderById
        pub static ref GET_ORDER_BY_ID_SUCCESSFUL_OPERATION: Mime = "application/json".parse().unwrap();
    }

    lazy_static! {
        /// Create Mime objects for the response content types for PlaceOrder
        pub static ref PLACE_ORDER_SUCCESSFUL_OPERATION: Mime = "application/json".parse().unwrap();
    }

    lazy_static! {
        /// Create Mime objects for the response content types for GetUserByName
        pub static ref GET_USER_BY_NAME_SUCCESSFUL_OPERATION: Mime = "application/json".parse().unwrap();
    }

    lazy_static! {
        /// Create Mime objects for the response content types for LoginUser
        pub static ref LOGIN_USER_SUCCESSFUL_OPERATION: Mime = "application/json".parse().unwrap();
    }

}

pub mod requests {
    use hyper::mime::*;

    lazy_static! {
        /// Create Mime objects for the request content types for TestSpecialTags
        pub static ref TEST_SPECIAL_TAGS: Mime = "application/json".parse().unwrap();
    }

    lazy_static! {
        /// Create Mime objects for the request content types for FakeOuterBooleanSerialize
        pub static ref FAKE_OUTER_BOOLEAN_SERIALIZE: Mime = "application/json".parse().unwrap();
    }

    lazy_static! {
        /// Create Mime objects for the request content types for FakeOuterCompositeSerialize
        pub static ref FAKE_OUTER_COMPOSITE_SERIALIZE: Mime = "application/json".parse().unwrap();
    }

    lazy_static! {
        /// Create Mime objects for the request content types for FakeOuterNumberSerialize
        pub static ref FAKE_OUTER_NUMBER_SERIALIZE: Mime = "application/json".parse().unwrap();
    }

    lazy_static! {
        /// Create Mime objects for the request content types for FakeOuterStringSerialize
        pub static ref FAKE_OUTER_STRING_SERIALIZE: Mime = "application/json".parse().unwrap();
    }

    lazy_static! {
        /// Create Mime objects for the request content types for TestBodyWithQueryParams
        pub static ref TEST_BODY_WITH_QUERY_PARAMS: Mime = "application/json".parse().unwrap();
    }

    lazy_static! {
        /// Create Mime objects for the request content types for TestClientModel
        pub static ref TEST_CLIENT_MODEL: Mime = "application/json".parse().unwrap();
    }

    lazy_static! {
        /// Create Mime objects for the request content types for TestEndpointParameters
        pub static ref TEST_ENDPOINT_PARAMETERS: Mime = "application/x-www-form-urlencoded".parse().unwrap();
    }

    lazy_static! {
        /// Create Mime objects for the request content types for TestEnumParameters
        pub static ref TEST_ENUM_PARAMETERS: Mime = "application/x-www-form-urlencoded".parse().unwrap();
    }

    lazy_static! {
        /// Create Mime objects for the request content types for TestInlineAdditionalProperties
        pub static ref TEST_INLINE_ADDITIONAL_PROPERTIES: Mime = "application/json".parse().unwrap();
    }

    lazy_static! {
        /// Create Mime objects for the request content types for TestJsonFormData
        pub static ref TEST_JSON_FORM_DATA: Mime = "application/x-www-form-urlencoded".parse().unwrap();
    }

    lazy_static! {
        /// Create Mime objects for the request content types for TestClassname
        pub static ref TEST_CLASSNAME: Mime = "application/json".parse().unwrap();
    }

    lazy_static! {
        /// Create Mime objects for the request content types for AddPet
        pub static ref ADD_PET: Mime = "application/json".parse().unwrap();
    }

    lazy_static! {
        /// Create Mime objects for the request content types for UpdatePet
        pub static ref UPDATE_PET: Mime = "application/json".parse().unwrap();
    }

    lazy_static! {
        /// Create Mime objects for the request content types for UpdatePetWithForm
        pub static ref UPDATE_PET_WITH_FORM: Mime = "application/x-www-form-urlencoded".parse().unwrap();
    }

    lazy_static! {
        /// Create Mime objects for the request content types for UploadFile
        pub static ref UPLOAD_FILE: Mime = "multipart/form-data".parse().unwrap();
    }

    lazy_static! {
        /// Create Mime objects for the request content types for PlaceOrder
        pub static ref PLACE_ORDER: Mime = "application/json".parse().unwrap();
    }

    lazy_static! {
        /// Create Mime objects for the request content types for CreateUser
        pub static ref CREATE_USER: Mime = "application/json".parse().unwrap();
    }

    lazy_static! {
        /// Create Mime objects for the request content types for CreateUsersWithArrayInput
        pub static ref CREATE_USERS_WITH_ARRAY_INPUT: Mime = "application/json".parse().unwrap();
    }

    lazy_static! {
        /// Create Mime objects for the request content types for CreateUsersWithListInput
        pub static ref CREATE_USERS_WITH_LIST_INPUT: Mime = "application/json".parse().unwrap();
    }

    lazy_static! {
        /// Create Mime objects for the request content types for UpdateUser
        pub static ref UPDATE_USER: Mime = "application/json".parse().unwrap();
    }

}
