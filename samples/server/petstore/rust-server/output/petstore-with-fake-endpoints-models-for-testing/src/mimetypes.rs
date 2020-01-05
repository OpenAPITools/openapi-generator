/// mime types for requests and responses

pub mod responses {


    /// Create &str objects for the response content types for TestSpecialTags
    pub static TEST_SPECIAL_TAGS_SUCCESSFUL_OPERATION: &str = "application/json";

    /// Create &str objects for the response content types for FakeOuterBooleanSerialize
    pub static FAKE_OUTER_BOOLEAN_SERIALIZE_OUTPUT_BOOLEAN: &str = "*/*";

    /// Create &str objects for the response content types for FakeOuterCompositeSerialize
    pub static FAKE_OUTER_COMPOSITE_SERIALIZE_OUTPUT_COMPOSITE: &str = "*/*";

    /// Create &str objects for the response content types for FakeOuterNumberSerialize
    pub static FAKE_OUTER_NUMBER_SERIALIZE_OUTPUT_NUMBER: &str = "*/*";

    /// Create &str objects for the response content types for FakeOuterStringSerialize
    pub static FAKE_OUTER_STRING_SERIALIZE_OUTPUT_STRING: &str = "*/*";

    /// Create &str objects for the response content types for TestClientModel
    pub static TEST_CLIENT_MODEL_SUCCESSFUL_OPERATION: &str = "application/json";

    /// Create &str objects for the response content types for TestClassname
    pub static TEST_CLASSNAME_SUCCESSFUL_OPERATION: &str = "application/json";

    /// Create &str objects for the response content types for FindPetsByStatus
    pub static FIND_PETS_BY_STATUS_SUCCESSFUL_OPERATION: &str = "application/xml";

    /// Create &str objects for the response content types for FindPetsByTags
    pub static FIND_PETS_BY_TAGS_SUCCESSFUL_OPERATION: &str = "application/xml";

    /// Create &str objects for the response content types for GetPetById
    pub static GET_PET_BY_ID_SUCCESSFUL_OPERATION: &str = "application/xml";

    /// Create &str objects for the response content types for UploadFile
    pub static UPLOAD_FILE_SUCCESSFUL_OPERATION: &str = "application/json";

    /// Create &str objects for the response content types for GetInventory
    pub static GET_INVENTORY_SUCCESSFUL_OPERATION: &str = "application/json";

    /// Create &str objects for the response content types for GetOrderById
    pub static GET_ORDER_BY_ID_SUCCESSFUL_OPERATION: &str = "application/xml";

    /// Create &str objects for the response content types for PlaceOrder
    pub static PLACE_ORDER_SUCCESSFUL_OPERATION: &str = "application/xml";

    /// Create &str objects for the response content types for GetUserByName
    pub static GET_USER_BY_NAME_SUCCESSFUL_OPERATION: &str = "application/xml";

    /// Create &str objects for the response content types for LoginUser
    pub static LOGIN_USER_SUCCESSFUL_OPERATION: &str = "application/xml";

}

pub mod requests {

    /// Create &str objects for the request content types for TestSpecialTags
    pub static TEST_SPECIAL_TAGS: &str = "application/json";

    /// Create &str objects for the request content types for FakeOuterBooleanSerialize
    pub static FAKE_OUTER_BOOLEAN_SERIALIZE: &str = "application/json";

    /// Create &str objects for the request content types for FakeOuterCompositeSerialize
    pub static FAKE_OUTER_COMPOSITE_SERIALIZE: &str = "application/json";

    /// Create &str objects for the request content types for FakeOuterNumberSerialize
    pub static FAKE_OUTER_NUMBER_SERIALIZE: &str = "application/json";

    /// Create &str objects for the request content types for FakeOuterStringSerialize
    pub static FAKE_OUTER_STRING_SERIALIZE: &str = "application/json";

    /// Create &str objects for the request content types for TestBodyWithQueryParams
    pub static TEST_BODY_WITH_QUERY_PARAMS: &str = "application/json";

    /// Create &str objects for the request content types for TestClientModel
    pub static TEST_CLIENT_MODEL: &str = "application/json";

    /// Create &str objects for the request content types for TestEndpointParameters
    pub static TEST_ENDPOINT_PARAMETERS: &str = "application/x-www-form-urlencoded";

    /// Create &str objects for the request content types for TestEnumParameters
    pub static TEST_ENUM_PARAMETERS: &str = "application/x-www-form-urlencoded";

    /// Create &str objects for the request content types for TestInlineAdditionalProperties
    pub static TEST_INLINE_ADDITIONAL_PROPERTIES: &str = "application/json";

    /// Create &str objects for the request content types for TestJsonFormData
    pub static TEST_JSON_FORM_DATA: &str = "application/x-www-form-urlencoded";

    /// Create &str objects for the request content types for TestClassname
    pub static TEST_CLASSNAME: &str = "application/json";

    /// Create &str objects for the request content types for AddPet
    pub static ADD_PET: &str = "application/json";

    /// Create &str objects for the request content types for UpdatePet
    pub static UPDATE_PET: &str = "application/json";

    /// Create &str objects for the request content types for UpdatePetWithForm
    pub static UPDATE_PET_WITH_FORM: &str = "application/x-www-form-urlencoded";

    /// Create &str objects for the request content types for PlaceOrder
    pub static PLACE_ORDER: &str = "application/json";

    /// Create &str objects for the request content types for CreateUser
    pub static CREATE_USER: &str = "application/json";

    /// Create &str objects for the request content types for CreateUsersWithArrayInput
    pub static CREATE_USERS_WITH_ARRAY_INPUT: &str = "application/json";

    /// Create &str objects for the request content types for CreateUsersWithListInput
    pub static CREATE_USERS_WITH_LIST_INPUT: &str = "application/json";

    /// Create &str objects for the request content types for UpdateUser
    pub static UPDATE_USER: &str = "application/json";

}
