note
    description: "API tests for FAKE_API"
    date: "$Date$"
    revision: "$Revision$"


class FAKE_API_TEST

inherit

    EQA_TEST_SET

feature -- Test routines

    
    test_create_xml_item
            -- creates an XmlItem
            -- 
            -- this route creates an XmlItem 
        local
            l_xml_item: XML_ITEM
        do
            -- TODO: Initialize required params.
            -- l_xml_item
                      
            -- api.create_xml_item(l_xml_item)
            assert ("not_implemented", False)
        end
    
    test_fake_outer_boolean_serialize
            -- 
            -- 
            -- Test serialization of outer boolean types 
        local
            l_response: BOOLEAN
            l_body: BOOLEAN
        do
            -- TODO: Initialize required params.
                      
            -- l_response := api.fake_outer_boolean_serialize(l_body)
            assert ("not_implemented", False)
        end
    
    test_fake_outer_composite_serialize
            -- 
            -- 
            -- Test serialization of object with outer number type 
        local
            l_response: OUTER_COMPOSITE
            l_body: OUTER_COMPOSITE
        do
            -- TODO: Initialize required params.
                      
            -- l_response := api.fake_outer_composite_serialize(l_body)
            assert ("not_implemented", False)
        end
    
    test_fake_outer_number_serialize
            -- 
            -- 
            -- Test serialization of outer number types 
        local
            l_response: REAL_32
            l_body: REAL_32
        do
            -- TODO: Initialize required params.
                      
            -- l_response := api.fake_outer_number_serialize(l_body)
            assert ("not_implemented", False)
        end
    
    test_fake_outer_string_serialize
            -- 
            -- 
            -- Test serialization of outer string types 
        local
            l_response: STRING_32
            l_body: STRING_32
        do
            -- TODO: Initialize required params.
                      
            -- l_response := api.fake_outer_string_serialize(l_body)
            assert ("not_implemented", False)
        end
    
    test_test_body_with_file_schema
            -- 
            -- 
            -- For this test, the body for this request much reference a schema named &#x60;File&#x60;. 
        local
            l_body: FILE_SCHEMA_TEST_CLASS
        do
            -- TODO: Initialize required params.
            -- l_body
                      
            -- api.test_body_with_file_schema(l_body)
            assert ("not_implemented", False)
        end
    
    test_test_body_with_query_params
            -- 
            -- 
            --  
        local
            l_query: STRING_32
            l_body: USER
        do
            -- TODO: Initialize required params.
            -- l_query
            -- l_body
                      
            -- api.test_body_with_query_params(l_query, l_body)
            assert ("not_implemented", False)
        end
    
    test_test_client_model
            -- To test \&quot;client\&quot; model
            -- 
            -- To test \&quot;client\&quot; model 
        local
            l_response: CLIENT
            l_body: CLIENT
        do
            -- TODO: Initialize required params.
            -- l_body
                      
            -- l_response := api.test_client_model(l_body)
            assert ("not_implemented", False)
        end
    
    test_test_endpoint_parameters
            -- Fake endpoint for testing various parameters  假端點  偽のエンドポイント  가짜 엔드 포인트
            -- 
            -- Fake endpoint for testing various parameters  假端點  偽のエンドポイント  가짜 엔드 포인트 
        local
            l_number: REAL_32
            l_double: REAL_64
            l_pattern_without_delimiter: STRING_32
            l_byte: ARRAY [NATURAL_8]
            l_integer: INTEGER_32
            l_int32: INTEGER_32
            l_int64: INTEGER_64
            l_float: REAL_32
            l_string: STRING_32
            l_binary: FILE
            l_date: DATE
            l_date_time: DATE_TIME
            l_password: STRING
            l_callback: STRING_32
        do
            -- TODO: Initialize required params.
            -- l_number
            -- l_double
            -- l_pattern_without_delimiter
            -- l_byte
                      
            -- api.test_endpoint_parameters(l_number, l_double, l_pattern_without_delimiter, l_byte, l_integer, l_int32, l_int64, l_float, l_string, l_binary, l_date, l_date_time, l_password, l_callback)
            assert ("not_implemented", False)
        end
    
    test_test_enum_parameters
            -- To test enum parameters
            -- 
            -- To test enum parameters 
        local
            l_enum_header_string_array: LIST [STRING_32]
            l_enum_header_string: STRING_32
            l_enum_query_string_array: LIST [STRING_32]
            l_enum_query_string: STRING_32
            l_enum_query_integer: INTEGER_32
            l_enum_query_double: REAL_64
            l_enum_form_string_array: LIST [STRING_32]
            l_enum_form_string: STRING_32
        do
            -- TODO: Initialize required params.
                      
            -- api.test_enum_parameters(l_enum_header_string_array, l_enum_header_string, l_enum_query_string_array, l_enum_query_string, l_enum_query_integer, l_enum_query_double, l_enum_form_string_array, l_enum_form_string)
            assert ("not_implemented", False)
        end
    
    test_test_group_parameters
            -- Fake endpoint to test group parameters (optional)
            -- 
            -- Fake endpoint to test group parameters (optional) 
        local
            l_required_string_group: INTEGER_32
            l_required_boolean_group: BOOLEAN
            l_required_int64_group: INTEGER_64
            l_string_group: INTEGER_32
            l_boolean_group: BOOLEAN
            l_int64_group: INTEGER_64
        do
            -- TODO: Initialize required params.
            -- l_required_string_group
            -- l_required_boolean_group
            -- l_required_int64_group
                      
            -- api.test_group_parameters(l_required_string_group, l_required_boolean_group, l_required_int64_group, l_string_group, l_boolean_group, l_int64_group)
            assert ("not_implemented", False)
        end
    
    test_test_inline_additional_properties
            -- test inline additionalProperties
            -- 
            --  
        local
            l_param: STRING_TABLE [STRING_32]
        do
            -- TODO: Initialize required params.
                      
            -- api.test_inline_additional_properties(l_param)
            assert ("not_implemented", False)
        end
    
    test_test_json_form_data
            -- test json serialization of form data
            -- 
            --  
        local
            l_param: STRING_32
            l_param2: STRING_32
        do
            -- TODO: Initialize required params.
            -- l_param
            -- l_param2
                      
            -- api.test_json_form_data(l_param, l_param2)
            assert ("not_implemented", False)
        end
    
    test_test_query_parameter_collection_format
            -- 
            -- 
            -- To test the collection format in query parameters 
        local
            l_pipe: LIST [STRING_32]
            l_ioutil: LIST [STRING_32]
            l_http: LIST [STRING_32]
            l_url: LIST [STRING_32]
            l_context: LIST [STRING_32]
        do
            -- TODO: Initialize required params.
            -- create {ARRAYED_LIST [STRING_32]} l_pipe.make (2)
            -- create {ARRAYED_LIST [STRING_32]} l_ioutil.make (2)
            -- create {ARRAYED_LIST [STRING_32]} l_http.make (2)
            -- create {ARRAYED_LIST [STRING_32]} l_url.make (2)
            -- create {ARRAYED_LIST [STRING_32]} l_context.make (2)
                      
            -- api.test_query_parameter_collection_format(l_pipe, l_ioutil, l_http, l_url, l_context)
            assert ("not_implemented", False)
        end

feature {NONE} -- Implementation

    api: FAKE_API
            -- Create an object instance of `FAKE_API'.
        once            
            create { FAKE_API } Result
        end

end
