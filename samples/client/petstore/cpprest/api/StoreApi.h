/*
 * StoreApi.h
 * 
 * 
 */
 
#ifndef StoreApi_H_
#define StoreApi_H_


#include "ApiClient.h"

#include <cpprest/details/basic_types.h>
#include <map>
#include "Order.h"

namespace io {
namespace swagger {
namespace client {
namespace api {

using namespace io::swagger::client::model;

class  StoreApi
{
public:
    StoreApi( std::shared_ptr<ApiClient> apiClient );
    virtual ~StoreApi();
    /// <summary>
    /// Delete purchase order by ID
    /// </summary>
    /// <remarks>
    /// For valid response try integer IDs with value &lt; 1000. Anything above 1000 or nonintegers will generate API errors
    /// </remarks>
    /// <param name="orderId">ID of the order that needs to be deleted</param>
    pplx::task<void> deleteOrder(utility::string_t orderId);
    /// <summary>
    /// Returns pet inventories by status
    /// </summary>
    /// <remarks>
    /// Returns a map of status codes to quantities
    /// </remarks>
    
    pplx::task<std::map<utility::string_t, int32_t>> getInventory();
    /// <summary>
    /// Find purchase order by ID
    /// </summary>
    /// <remarks>
    /// For valid response try integer IDs with value &lt;&#x3D; 5 or &gt; 10. Other values will generated exceptions
    /// </remarks>
    /// <param name="orderId">ID of pet that needs to be fetched</param>
    pplx::task<std::shared_ptr<Order>> getOrderById(int64_t orderId);
    /// <summary>
    /// Place an order for a pet
    /// </summary>
    /// <remarks>
    /// 
    /// </remarks>
    /// <param name="body">order placed for purchasing the pet</param>
    pplx::task<std::shared_ptr<Order>> placeOrder(std::shared_ptr<Order> body);
    
protected:
    std::shared_ptr<ApiClient> m_ApiClient;    
};
    
}
}
}
}

#endif /* StoreApi_H_ */

