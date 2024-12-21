#include <stdlib.h>
#include <stdio.h>
#include "../include/apiClient.h"
#include "../include/list.h"
#include "../external/cJSON.h"
#include "../include/keyValuePair.h"
#include "../include/binary.h"
#include "../model/order.h"

// Enum RATING for StoreAPI_sendRating
typedef enum  { openapi_petstore_sendRating_RATING_NULL = 0, openapi_petstore_sendRating_RATING_Excellent, openapi_petstore_sendRating_RATING_Great, openapi_petstore_sendRating_RATING_Good, openapi_petstore_sendRating_RATING_Regular, openapi_petstore_sendRating_RATING_Bad, openapi_petstore_sendRating_RATING_Awful } openapi_petstore_sendRating_rating_e;


// Delete purchase order by ID
//
// For valid response try integer IDs with value < 1000. Anything above 1000 or nonintegers will generate API errors
//
void
StoreAPI_deleteOrder(apiClient_t *apiClient, char *orderId);


// Returns pet inventories by status
//
// Returns a map of status codes to quantities
//
list_t*
StoreAPI_getInventory(apiClient_t *apiClient);


// Find purchase order by ID
//
// For valid response try integer IDs with value <= 5 or > 10. Other values will generate exceptions
//
order_t*
StoreAPI_getOrderById(apiClient_t *apiClient, long orderId);


// Place an order for a pet
//
order_t*
StoreAPI_placeOrder(apiClient_t *apiClient, order_t *body);


// Send us a feedback message
//
char*
StoreAPI_sendFeedback(apiClient_t *apiClient, char *feedback);


// How would you rate our service?
//
char*
StoreAPI_sendRating(apiClient_t *apiClient, openapi_petstore_sendRating_rating_e rating);


// Would you recommend our service to a friend?
//
char*
StoreAPI_sendRecommend(apiClient_t *apiClient, int *recommend);


