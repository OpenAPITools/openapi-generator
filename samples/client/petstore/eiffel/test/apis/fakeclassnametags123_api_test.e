note
    description: "API tests for FAKECLASSNAMETAGS123_API"
    date: "$Date$"
    revision: "$Revision$"


class FAKECLASSNAMETAGS123_API_TEST

inherit

    EQA_TEST_SET

feature -- Test routines

    
    test_test_classname
            -- To test class name in snake case
            -- 
            -- To test class name in snake case 
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

    api: FAKECLASSNAMETAGS123_API
            -- Create an object instance of `FAKECLASSNAMETAGS123_API'.
        once            
            create { FAKECLASSNAMETAGS123_API } Result
        end

end
