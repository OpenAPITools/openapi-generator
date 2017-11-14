/// mime types for requests and responses

pub mod responses {
    use hyper::mime::*;

    // The macro is called per-operation to beat the recursion limit
    /// Create Mime objects for the response content types for TestSpecialTags
    lazy_static! {
        pub static ref TEST_SPECIAL_TAGS_SUCCESSFUL_OPERATION: Mime = mime!(Application/Json);
    }
    /// Create Mime objects for the response content types for TestClientModel
    lazy_static! {
        pub static ref TEST_CLIENT_MODEL_SUCCESSFUL_OPERATION: Mime = mime!(Application/Json);
    }
    /// Create Mime objects for the response content types for TestClassname
    lazy_static! {
        pub static ref TEST_CLASSNAME_SUCCESSFUL_OPERATION: Mime = mime!(Application/Json);
    }
    /// Create Mime objects for the response content types for FindPetsByStatus
    lazy_static! {
        pub static ref FIND_PETS_BY_STATUS_SUCCESSFUL_OPERATION: Mime = mime!(Application/Xml);
    }
    /// Create Mime objects for the response content types for FindPetsByTags
    lazy_static! {
        pub static ref FIND_PETS_BY_TAGS_SUCCESSFUL_OPERATION: Mime = mime!(Application/Xml);
    }
    /// Create Mime objects for the response content types for GetPetById
    lazy_static! {
        pub static ref GET_PET_BY_ID_SUCCESSFUL_OPERATION: Mime = mime!(Application/Xml);
    }
    /// Create Mime objects for the response content types for UploadFile
    lazy_static! {
        pub static ref UPLOAD_FILE_SUCCESSFUL_OPERATION: Mime = mime!(Application/Json);
    }
    /// Create Mime objects for the response content types for GetInventory
    lazy_static! {
        pub static ref GET_INVENTORY_SUCCESSFUL_OPERATION: Mime = mime!(Application/Json);
    }
    /// Create Mime objects for the response content types for GetOrderById
    lazy_static! {
        pub static ref GET_ORDER_BY_ID_SUCCESSFUL_OPERATION: Mime = mime!(Application/Xml);
    }
    /// Create Mime objects for the response content types for PlaceOrder
    lazy_static! {
        pub static ref PLACE_ORDER_SUCCESSFUL_OPERATION: Mime = mime!(Application/Xml);
    }
    /// Create Mime objects for the response content types for GetUserByName
    lazy_static! {
        pub static ref GET_USER_BY_NAME_SUCCESSFUL_OPERATION: Mime = mime!(Application/Xml);
    }
    /// Create Mime objects for the response content types for LoginUser
    lazy_static! {
        pub static ref LOGIN_USER_SUCCESSFUL_OPERATION: Mime = mime!(Application/Xml);
    }

}

pub mod requests {
    use hyper::mime::*;
   /// Create Mime objects for the request content types for TestSpecialTags
    lazy_static! {
        pub static ref TEST_SPECIAL_TAGS: Mime = mime!(Application/Json);
    }
   /// Create Mime objects for the request content types for FakeOuterBooleanSerialize
    lazy_static! {
        pub static ref FAKE_OUTER_BOOLEAN_SERIALIZE: Mime = mime!(Application/Json);
    }
   /// Create Mime objects for the request content types for FakeOuterCompositeSerialize
    lazy_static! {
        pub static ref FAKE_OUTER_COMPOSITE_SERIALIZE: Mime = mime!(Application/Json);
    }
   /// Create Mime objects for the request content types for FakeOuterNumberSerialize
    lazy_static! {
        pub static ref FAKE_OUTER_NUMBER_SERIALIZE: Mime = mime!(Application/Json);
    }
   /// Create Mime objects for the request content types for FakeOuterStringSerialize
    lazy_static! {
        pub static ref FAKE_OUTER_STRING_SERIALIZE: Mime = mime!(Application/Json);
    }
   /// Create Mime objects for the request content types for TestClientModel
    lazy_static! {
        pub static ref TEST_CLIENT_MODEL: Mime = mime!(Application/Json);
    }
   /// Create Mime objects for the request content types for TestInlineAdditionalProperties
    lazy_static! {
        pub static ref TEST_INLINE_ADDITIONAL_PROPERTIES: Mime = mime!(Application/Json);
    }
   /// Create Mime objects for the request content types for TestClassname
    lazy_static! {
        pub static ref TEST_CLASSNAME: Mime = mime!(Application/Json);
    }
   /// Create Mime objects for the request content types for AddPet
    lazy_static! {
        pub static ref ADD_PET: Mime = mime!(Application/Json);
    }
   /// Create Mime objects for the request content types for UpdatePet
    lazy_static! {
        pub static ref UPDATE_PET: Mime = mime!(Application/Json);
    }
   /// Create Mime objects for the request content types for PlaceOrder
    lazy_static! {
        pub static ref PLACE_ORDER: Mime = mime!(Application/Json);
    }
   /// Create Mime objects for the request content types for CreateUser
    lazy_static! {
        pub static ref CREATE_USER: Mime = mime!(Application/Json);
    }
   /// Create Mime objects for the request content types for CreateUsersWithArrayInput
    lazy_static! {
        pub static ref CREATE_USERS_WITH_ARRAY_INPUT: Mime = mime!(Application/Json);
    }
   /// Create Mime objects for the request content types for CreateUsersWithListInput
    lazy_static! {
        pub static ref CREATE_USERS_WITH_LIST_INPUT: Mime = mime!(Application/Json);
    }
   /// Create Mime objects for the request content types for UpdateUser
    lazy_static! {
        pub static ref UPDATE_USER: Mime = mime!(Application/Json);
    }

}
