note
    description: "API tests for STORE_API"
    date: "$Date$"
    revision: "$Revision$"


class STORE_API_TEST

inherit

    EQA_TEST_SET

feature -- Test routines

    
    test_delete_order
            -- Delete purchase order by ID
            -- 
            -- For valid response try integer IDs with value &lt; 1000. Anything above 1000 or nonintegers will generate API errors 
        local
            l_order_id: STRING_32
        do
            -- TODO: Initialize required params.
            -- l_order_id
                      
            -- api.delete_order(l_order_id)
            assert ("not_implemented", False)
        end
    
    test_inventory
            -- Returns pet inventories by status
            -- 
            -- Returns a map of status codes to quantities 
        local
            l_response: STRING_TABLE[INTEGER_32]
        do
            -- TODO: Initialize required params.
                      
            -- l_response := api.inventory
            assert ("not_implemented", False)
        end
    
    test_order_by_id
            -- Find purchase order by ID
            -- 
            -- For valid response try integer IDs with value &lt;&#x3D; 5 or &gt; 10. Other values will generated exceptions 
        local
            l_response: ORDER
            l_order_id: INTEGER_64
        do
            -- TODO: Initialize required params.
            -- l_order_id
                      
            -- l_response := api.order_by_id(l_order_id)
            assert ("not_implemented", False)
        end
    
    test_place_order
            -- Place an order for a pet
            -- 
            --  
        local
            l_response: ORDER
            l_order: ORDER
        do
            -- TODO: Initialize required params.
            -- l_order
                      
            -- l_response := api.place_order(l_order)
            assert ("not_implemented", False)
        end

feature {NONE} -- Implementation

    api: STORE_API
            -- Create an object instance of `STORE_API'.
        once            
            create { STORE_API } Result
        end

end
