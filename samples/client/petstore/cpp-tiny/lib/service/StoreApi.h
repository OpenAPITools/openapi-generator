#ifndef TINY_CPP_CLIENT_StoreApi_H_
#define TINY_CPP_CLIENT_StoreApi_H_


#include "Response.h"
#include "Arduino.h"
#include "AbstractService.h"
#include "Helpers.h"
#include <list>

#include <map>
#include "Order.h"

namespace Tiny {

/**
 *  Class 
 * Generated with openapi::tiny-cpp-client
 */

class StoreApi : public AbstractService {
public:
    StoreApi() = default;

    virtual ~StoreApi() = default;

    /**
    * Delete purchase order by ID.
    *
    * For valid response try integer IDs with value < 1000. Anything above 1000 or nonintegers will generate API errors
    * \param orderId ID of the order that needs to be deleted *Required*
    */
    Response<
            String
        >
    deleteOrder(
            
            std::string orderId
            
    );
    /**
    * Returns pet inventories by status.
    *
    * Returns a map of status codes to quantities
    */
    Response<
                String
        >
    getInventory(
    );
    /**
    * Find purchase order by ID.
    *
    * For valid response try integer IDs with value <= 5 or > 10. Other values will generated exceptions
    * \param orderId ID of pet that needs to be fetched *Required*
    */
    Response<
                Order
        >
    getOrderById(
            
            long orderId
            
    );
    /**
    * Place an order for a pet.
    *
    * 
    * \param order order placed for purchasing the pet *Required*
    */
    Response<
                Order
        >
    placeOrder(
            
            Order order
            
    );
}; 

} 

#endif /* TINY_CPP_CLIENT_StoreApi_H_ */