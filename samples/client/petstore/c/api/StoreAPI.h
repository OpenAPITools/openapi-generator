#include <stdlib.h>
#include <stdio.h>
#include "../include/apiClient.h"
#include "../include/list.h"
#include "../external/cJSON.h"
#include "../include/keyValuePair.h"
#include "../include/binary.h"
#include "../model/order.h"


// Delete purchase order by ID
//
// For valid response try integer IDs with value < 1000. Anything above 1000 or nonintegers will generate API errors
//
void
StoreAPI_deleteOrder(apiClient_t *apiClient, char * orderId );


// Returns pet inventories by status
//
// Returns a map of status codes to quantities
//
list_t*
StoreAPI_getInventory(apiClient_t *apiClient);


// Find purchase order by ID
//
// For valid response try integer IDs with value <= 5 or > 10. Other values will generated exceptions
//
order_t*
StoreAPI_getOrderById(apiClient_t *apiClient, long orderId );


// Place an order for a pet
//
order_t*
StoreAPI_placeOrder(apiClient_t *apiClient, order_t * body );


