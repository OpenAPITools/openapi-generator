note
    description: "API tests for FAKE_API"
    date: "$Date$"
    revision: "$Revision$"


class FAKE_API_TEST

inherit

    EQA_TEST_SET

feature -- Test routines

    
    test_fake_outer_boolean_serialize
            -- 
            -- 
            -- Test serialization of outer boolean types 
        local
            l_response: OUTER_BOOLEAN
            l_body: OUTER_BOOLEAN
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
            l_response: OUTER_NUMBER
            l_body: OUTER_NUMBER
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
            l_response: OUTER_STRING
            l_body: OUTER_STRING
        do
            -- TODO: Initialize required params.
                      
            -- l_response := api.fake_outer_string_serialize(l_body)
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
            -- Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 
            -- 
            -- Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트  
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
            l_binary: STRING_32
            l_date: DATE
            l_date_time: DATE_TIME
            l_password: STRING_32
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
            l_enum_form_string_array: LIST [STRING_32]
            l_enum_form_string: STRING_32
            l_enum_header_string_array: LIST [STRING_32]
            l_enum_header_string: STRING_32
            l_enum_query_string_array: LIST [STRING_32]
            l_enum_query_string: STRING_32
            l_enum_query_integer: INTEGER_32
            l_enum_query_double: REAL_64
        do
            -- TODO: Initialize required params.
                      
            -- api.test_enum_parameters(l_enum_form_string_array, l_enum_form_string, l_enum_header_string_array, l_enum_header_string, l_enum_query_string_array, l_enum_query_string, l_enum_query_integer, l_enum_query_double)
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

feature {NONE} -- Implementation

    api: FAKE_API
            -- Create an object instance of `FAKE_API'.
        once            
            create { FAKE_API } Result
        end

end
