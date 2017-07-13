note
    description: "API tests for PET_API"
    date: "$Date$"
    revision: "$Revision$"


class PET_API_TEST

inherit

    EQA_TEST_SET

feature -- Test routines

    
    test_add_pet
            -- Add a new pet to the store
            -- 
            --  
        local
            l_body: PET
        do
            -- TODO: Initialize required params.
            -- l_body
                      
            -- api.add_pet(l_body)
            assert ("not_implemented", False)
        end
    
    test_delete_pet
            -- Deletes a pet
            -- 
            --  
        local
            l_petid: INTEGER_64
            l_api_key: STRING_32
        do
            -- TODO: Initialize required params.
            -- l_petid
                      
            -- api.delete_pet(l_petid, l_api_key)
            assert ("not_implemented", False)
        end
    
    test_find_pets_by_status
            -- Finds Pets by status
            -- 
            -- Multiple status values can be provided with comma separated strings 
        local
            l_response: LIST [PET]
            l_status: LIST [STRING_32]
        do
            -- TODO: Initialize required params.
            -- create {ARRAYED_LIST [STRING_32]} l_status.make (2)
                      
            -- l_response := api.find_pets_by_status(l_status)
            assert ("not_implemented", False)
        end
    
    test_find_pets_by_tags
            -- Finds Pets by tags
            -- 
            -- Muliple tags can be provided with comma separated strings. Use tag1, tag2, tag3 for testing. 
        local
            l_response: LIST [PET]
            l_tags: LIST [STRING_32]
        do
            -- TODO: Initialize required params.
            -- create {ARRAYED_LIST [STRING_32]} l_tags.make (2)
                      
            -- l_response := api.find_pets_by_tags(l_tags)
            assert ("not_implemented", False)
        end
    
    test_pet_by_id
            -- Find pet by ID
            -- 
            -- Returns a single pet 
        local
            l_response: PET
            l_petid: INTEGER_64
        do
            -- TODO: Initialize required params.
            -- l_petid
                      
            -- l_response := api.pet_by_id(l_petid)
            assert ("not_implemented", False)
        end
    
    test_update_pet
            -- Update an existing pet
            -- 
            --  
        local
            l_body: PET
        do
            -- TODO: Initialize required params.
            -- l_body
                      
            -- api.update_pet(l_body)
            assert ("not_implemented", False)
        end
    
    test_update_pet_with_form
            -- Updates a pet in the store with form data
            -- 
            --  
        local
            l_petid: INTEGER_64
            l_name: STRING_32
            l_status: STRING_32
        do
            -- TODO: Initialize required params.
            -- l_petid
                      
            -- api.update_pet_with_form(l_petid, l_name, l_status)
            assert ("not_implemented", False)
        end
    
    test_upload_file
            -- uploads an image
            -- 
            --  
        local
            l_response: API_RESPONSE
            l_petid: INTEGER_64
            l_additionalmetadata: STRING_32
            l_file: FILE
        do
            -- TODO: Initialize required params.
            -- l_petid
                      
            -- l_response := api.upload_file(l_petid, l_additionalmetadata, l_file)
            assert ("not_implemented", False)
        end

feature {NONE} -- Implementation

    api: PET_API
            -- Create an object instance of `PET_API'.
        once            
            create { PET_API } Result
        end

end
