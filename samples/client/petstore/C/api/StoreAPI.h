#include <stdlib.h>
#include <stdio.h>
#include "apiClient.h"
#include "cJSON.h"
#include "map.h"
#include "order.h"


// Delete purchase order by ID
//
// For valid response try integer IDs with value < 1000. Anything above 1000 or nonintegers will generate API errors
//
void *StoreAPI_deleteOrder(apiClient_t *apiClient, char *OrderId);


// Returns pet inventories by status
//
// Returns a map of status codes to quantities
//
map *StoreAPI_getInventory(apiClient_t *apiClient);


// Find purchase order by ID
//
// For valid response try integer IDs with value <= 5 or > 10. Other values will generated exceptions
//
order_t *StoreAPI_getOrderById(apiClient_t *apiClient, long OrderId);


// Place an order for a pet
//
order_t *StoreAPI_placeOrder(apiClient_t *apiClient, order_t *Order);
