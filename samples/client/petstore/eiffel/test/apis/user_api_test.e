note
    description: "API tests for USER_API"
    date: "$Date$"
    revision: "$Revision$"


class USER_API_TEST

inherit

    EQA_TEST_SET

feature -- Test routines

    
    test_create_user
            -- Create user
            -- 
            -- This can only be done by the logged in user. 
        local
            l_body: USER
        do
            -- TODO: Initialize required params.
            -- l_body
                      
            -- api.create_user(l_body)
            assert ("not_implemented", False)
        end
    
    test_create_users_with_array_input
            -- Creates list of users with given input array
            -- 
            --  
        local
            l_body: LIST [USER]
        do
            -- TODO: Initialize required params.
            -- create {ARRAYED_LIST [USER]} l_body.make (2)
                      
            -- api.create_users_with_array_input(l_body)
            assert ("not_implemented", False)
        end
    
    test_create_users_with_list_input
            -- Creates list of users with given input array
            -- 
            --  
        local
            l_body: LIST [USER]
        do
            -- TODO: Initialize required params.
            -- create {ARRAYED_LIST [USER]} l_body.make (2)
                      
            -- api.create_users_with_list_input(l_body)
            assert ("not_implemented", False)
        end
    
    test_delete_user
            -- Delete user
            -- 
            -- This can only be done by the logged in user. 
        local
            l_username: STRING_32
        do
            -- TODO: Initialize required params.
            -- l_username
                      
            -- api.delete_user(l_username)
            assert ("not_implemented", False)
        end
    
    test_login_user
            -- Logs user into the system
            -- 
            --  
        local
            l_response: STRING_32
            l_username: STRING_32
            l_password: STRING_32
        do
            -- TODO: Initialize required params.
            -- l_username
            -- l_password
                      
            -- l_response := api.login_user(l_username, l_password)
            assert ("not_implemented", False)
        end
    
    test_logout_user
            -- Logs out current logged in user session
            -- 
            --  
        local
        do
            -- TODO: Initialize required params.
                      
            -- api.logout_user
            assert ("not_implemented", False)
        end
    
    test_update_user
            -- Updated user
            -- 
            -- This can only be done by the logged in user. 
        local
            l_username: STRING_32
            l_body: USER
        do
            -- TODO: Initialize required params.
            -- l_username
            -- l_body
                      
            -- api.update_user(l_username, l_body)
            assert ("not_implemented", False)
        end
    
    test_user_by_name
            -- Get user by user name
            -- 
            --  
        local
            l_response: USER
            l_username: STRING_32
        do
            -- TODO: Initialize required params.
            -- l_username
                      
            -- l_response := api.user_by_name(l_username)
            assert ("not_implemented", False)
        end

feature {NONE} -- Implementation

    api: USER_API
            -- Create an object instance of `USER_API'.
        once            
            create { USER_API } Result
        end

end
