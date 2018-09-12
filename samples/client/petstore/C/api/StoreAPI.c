#include <stdlib.h>
#include <stdio.h>
#include "apiClient.h"
#include "cJSON.h"
#include "map.h"
#include "order.h"

#define MAX_BUFFER_LENGTH 4096
#define intToStr(dst, src) \
	do { \
		char dst[64]; \
		snprintf(dst, 64, "%ld", (long int) (src)); \
	} while(0)

// Delete purchase order by ID
//
// For valid response try integer IDs with value < 1000. Anything above 1000 or nonintegers will generate API errors
//
void *StoreAPI_deleteOrder(apiClient_t *apiClient, char *OrderId) {
	list_t *localVarQueryParameters,
	       list_t *localVarHeaderParameters,
	       list_t *localVarFormParameters,
	char *localVarBodyParameters,

	// create the path
	char *localVarPath = malloc(MAX_BUFFER_LENGTH);
	snprintf(localVarPath, MAX_BUFFER_LENGTH, "/store/order/{orderId}");

	// TODO path parameter OrderId (orderId) not yet supported
	// TODO base path = http://petstore.swagger.io/v2
	char *baseNameMod = malloc(strlen(orderId) + 2); // baseNameMod free not yet implemented
	snprintf(baseNameMod, strlen(baseName) + 3, "%s%s%s", "{", orderId,
	         "}");
	localVarPath = strReplace(localVarPath, baseNameMod, OrderId);


	apiClient_invoke(apiClient,
	                 "TODO_UNKNOWN",
	                 localVarPath,
	                 localVarQueryParameters,
	                 localVarHeaderParameters,
	                 localVarFormParameters,
	                 localVarBodyParameters,
	                 "DELETE");

	free(apiClient->dataReceived);
	free(OrderId);
}

// Returns pet inventories by status
//
// Returns a map of status codes to quantities
//
map_t *StoreAPI_getInventory(apiClient_t *apiClient) {
	list_t *localVarQueryParameters,
	       list_t *localVarHeaderParameters,
	       list_t *localVarFormParameters,
	char *localVarBodyParameters,

	// create the path
	char *localVarPath = malloc(MAX_BUFFER_LENGTH);
	snprintf(localVarPath, MAX_BUFFER_LENGTH, "/store/inventory");



	apiClient_invoke(apiClient,
	                 "TODO_UNKNOWN",
	                 localVarPath,
	                 localVarQueryParameters,
	                 localVarHeaderParameters,
	                 localVarFormParameters,
	                 localVarBodyParameters,
	                 "GET");

	free(apiClient->dataReceived);
	localVarmap = _parseFromJSON(apiClient->dataReceived);
	if(localVarmap == NULL) {
		return 0;
	} else {
		cJSON *jsonObject = _convertToJSON();
		cJSON_Delete(jsonObject);
	}

	return localVarmap;
}

// Find purchase order by ID
//
// For valid response try integer IDs with value <= 5 or > 10. Other values will generated exceptions
//
order_t *StoreAPI_getOrderById(apiClient_t *apiClient, long OrderId) {
	list_t *localVarQueryParameters,
	       list_t *localVarHeaderParameters,
	       list_t *localVarFormParameters,
	char *localVarBodyParameters,

	// create the path
	char *localVarPath = malloc(MAX_BUFFER_LENGTH);
	snprintf(localVarPath, MAX_BUFFER_LENGTH, "/store/order/{orderId}");

	// TODO path parameter OrderId (orderId) not yet supported
	// TODO base path = http://petstore.swagger.io/v2
	char *baseNameMod = malloc(strlen(orderId) + 2); // baseNameMod free not yet implemented
	snprintf(baseNameMod, strlen(baseName) + 3, "%s%s%s", "{", orderId,
	         "}");
	char buff[64];
	intToStr(buf, OrderId);
	localVarPath = strReplace(localVarPath, baseNameMod, buff);


	apiClient_invoke(apiClient,
	                 "TODO_UNKNOWN",
	                 localVarPath,
	                 localVarQueryParameters,
	                 localVarHeaderParameters,
	                 localVarFormParameters,
	                 localVarBodyParameters,
	                 "GET");

	free(apiClient->dataReceived);
	free(OrderId);
	localVarorder = _parseFromJSON(apiClient->dataReceived);
	if(localVarorder == NULL) {
		return 0;
	} else {
		cJSON *jsonObject = _convertToJSON();
		cJSON_Delete(jsonObject);
	}

	return localVarorder;
}

// Place an order for a pet
//
order_t *StoreAPI_placeOrder(apiClient_t *apiClient, order_t *Order) {
	list_t *localVarQueryParameters,
	       list_t *localVarHeaderParameters,
	       list_t *localVarFormParameters,
	char *localVarBodyParameters,

	// create the path
	char *localVarPath = malloc(MAX_BUFFER_LENGTH);
	snprintf(localVarPath, MAX_BUFFER_LENGTH, "/store/order");



	// JSON HTTP Request - order
	cJSON *OrderJSONObject;
	OrderJSONObject = order_convertToJSON(Order);
	localVarBodyParameters = cJSON_Print(OrderJSONObject);

	apiClient_invoke(apiClient,
	                 "TODO_UNKNOWN",
	                 localVarPath,
	                 localVarQueryParameters,
	                 localVarHeaderParameters,
	                 localVarFormParameters,
	                 localVarBodyParameters,
	                 "POST");

	free(apiClient->dataReceived);
	free(localVarBodyParameters);
	cJSON_Delete(OrderJSONObject);
	localVarorder = _parseFromJSON(apiClient->dataReceived);
	if(localVarorder == NULL) {
		return 0;
	} else {
		cJSON *jsonObject = _convertToJSON();
		cJSON_Delete(jsonObject);
	}

	return localVarorder;
}
