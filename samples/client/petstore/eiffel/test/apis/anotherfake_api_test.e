note
    description: "API tests for ANOTHERFAKE_API"
    date: "$Date$"
    revision: "$Revision$"


class ANOTHERFAKE_API_TEST

inherit

    EQA_TEST_SET

feature -- Test routines

    
    test_call123test_special_tags
            -- To test special tags
            -- 
            -- To test special tags and operation ID starting with number 
        local
            l_response: CLIENT
            l_body: CLIENT
        do
            -- TODO: Initialize required params.
            -- l_body
                      
            -- l_response := api.call123test_special_tags(l_body)
            assert ("not_implemented", False)
        end

feature {NONE} -- Implementation

    api: ANOTHERFAKE_API
            -- Create an object instance of `ANOTHERFAKE_API'.
        once            
            create { ANOTHERFAKE_API } Result
        end

end
