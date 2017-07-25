note
    description: "API tests for FAKE_CLASSNAME_TAGS123_API"
    date: "$Date$"
    revision: "$Revision$"


class FAKE_CLASSNAME_TAGS123_API_TEST

inherit

    EQA_TEST_SET

feature -- Test routines

    
    test_test_classname
            -- To test class name in snake case
            -- 
            --  
        local
            l_response: CLIENT
            l_body: CLIENT
        do
            -- TODO: Initialize required params.
            -- l_body
                      
            -- l_response := api.test_classname(l_body)
            assert ("not_implemented", False)
        end

feature {NONE} -- Implementation

    api: FAKE_CLASSNAME_TAGS123_API
            -- Create an object instance of `FAKE_CLASSNAME_TAGS123_API'.
        once            
            create { FAKE_CLASSNAME_TAGS123_API } Result
        end

end
