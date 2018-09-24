#include <stdlib.h>
#include <stdio.h>
#include <ctype.h>
#include "apiClient.h"
#include "cJSON.h"
#include "keyValuePair.h"
//#include "map.h"
#include "order.h"

#define MAX_BUFFER_LENGTH 4096
#define intToStr(dst, src) \
	do { \
		char dst[256]; \
		snprintf(dst, 256, "%ld", (long int) (src)); \
	} while(0)

// Delete purchase order by ID
//
// For valid response try integer IDs with value < 1000. Anything above 1000 or nonintegers will generate API errors
//
void *StoreAPI_deleteOrder(apiClient_t *apiClient, char *orderId) {
	list_t *localVarQueryParameters = list_create();
	list_t *localVarHeaderParameters = list_create();
	list_t *localVarFormParameters = list_create();
	list_t *localVarHeaderType = list_create();
	list_t *localVarContentType = list_create();
	char *localVarBodyParameters = NULL;

	// create the path
	char *localVarPath = malloc(MAX_BUFFER_LENGTH);
	snprintf(localVarPath, MAX_BUFFER_LENGTH, "/store/order/{orderId}");

	// Path Params
	char *localVarToReplace = malloc(sizeof(orderId) + 2);
	snprintf(localVarToReplace, strlen(orderId) + 3, "%s%s%s", "{", "orderId", "}");

	localVarPath = strReplace(localVarPath, localVarToReplace, orderId);

	apiClient_invoke(apiClient,
	                 localVarPath,
	                 localVarQueryParameters,
	                 localVarHeaderParameters,
	                 localVarFormParameters,
	                 localVarHeaderType,
	                 localVarContentType,
	                 localVarBodyParameters,
	                 "DELETE");

	// No return type
	apiClient_free(apiClient);
	list_free(localVarQueryParameters);
	list_free(localVarHeaderParameters);
	list_free(localVarFormParameters);
	list_free(localVarHeaderType);
	list_free(localVarContentType);
	free(localVarPath);
	free(localVarToReplace);
}

// Returns pet inventories by status
//
// Returns a map of status codes to quantities
//
//map *StoreAPI_getInventory(apiClient_t *apiClient) {
//	list_t *localVarQueryParameters = list_create();
//	list_t *localVarHeaderParameters = list_create();
//	list_t *localVarFormParameters = list_create();
//	list_t *localVarHeaderType = list_create();
//	list_t *localVarContentType = list_create();
//	char *localVarBodyParameters = NULL;
//
//	// create the path
//	char *localVarPath = malloc(MAX_BUFFER_LENGTH);
//	snprintf(localVarPath, MAX_BUFFER_LENGTH, "/store/inventory");
//
//	list_addElement(localVarHeaderType, "application/json"); // produces
//
//	apiClient_invoke(apiClient,
//	                 localVarPath,
//	                 localVarQueryParameters,
//	                 localVarHeaderParameters,
//	                 localVarFormParameters,
//	                 localVarHeaderType,
//	                 localVarContentType,
//	                 localVarBodyParameters,
//	                 "GET");
//
//	// primitive reutrn type
//	char *elementToReturn = (char *) apiClient->dataReceived;
//	apiClient_free(apiClient);
//	list_free(localVarQueryParameters);
//	list_free(localVarHeaderParameters);
//	list_free(localVarFormParameters);
//	list_free(localVarHeaderType);
//	list_free(localVarContentType);
//	free(localVarPath);
//	return elementToReturn;
//}

// Find purchase order by ID
//
// For valid response try integer IDs with value <= 5 or > 10. Other values will generated exceptions
//
order_t *StoreAPI_getOrderById(apiClient_t *apiClient, long orderId) {
	list_t *localVarQueryParameters = list_create();
	list_t *localVarHeaderParameters = list_create();
	list_t *localVarFormParameters = list_create();
	list_t *localVarHeaderType = list_create();
	list_t *localVarContentType = list_create();
	char *localVarBodyParameters = NULL;

	// create the path
	char *localVarPath = malloc(MAX_BUFFER_LENGTH);
	snprintf(localVarPath, MAX_BUFFER_LENGTH, "/store/order/{orderId}");

	// Path Params
	char *localVarToReplace = malloc(sizeof(orderId) + 3);
	snprintf(localVarToReplace, strlen("orderId") + 3, "%s%s%s", "{", "orderId", "}");

	char localVarBuff[256];
	intToStr(localVarBuff, orderId);

	localVarPath = strReplace(localVarPath, localVarToReplace, localVarBuff);


	list_addElement(localVarHeaderType, "application/xml"); // produces

	list_addElement(localVarHeaderType, "application/json"); // produces

	apiClient_invoke(apiClient,
	                 localVarPath,
	                 localVarQueryParameters,
	                 localVarHeaderParameters,
	                 localVarFormParameters,
	                 localVarHeaderType,
	                 localVarContentType,
	                 localVarBodyParameters,
	                 "GET");

	// nonprimitive not container
	order_t *elementToReturn = order_parseFromJSON(apiClient->dataReceived);
	if(elementToReturn == NULL) {
		return 0;
	}

	// return type
	apiClient_free(apiClient);
	list_free(localVarQueryParameters);
	list_free(localVarHeaderParameters);
	list_free(localVarFormParameters);
	list_free(localVarHeaderType);
	list_free(localVarContentType);
	free(localVarPath);
	free(localVarToReplace);
	return elementToReturn;
}

// Place an order for a pet
//
order_t *StoreAPI_placeOrder(apiClient_t *apiClient, order_t *order) {
	list_t *localVarQueryParameters = list_create();
	list_t *localVarHeaderParameters = list_create();
	list_t *localVarFormParameters = list_create();
	list_t *localVarHeaderType = list_create();
	list_t *localVarContentType = list_create();
	char *localVarBodyParameters = NULL;

	// create the path
	char *localVarPath = malloc(MAX_BUFFER_LENGTH);
	snprintf(localVarPath, MAX_BUFFER_LENGTH, "/store/order");

	// Body Param
	// string
	cJSON *localVarSingleItemJSON_order;
	localVarSingleItemJSON_order = order_convertToJSON(order);
	localVarBodyParameters = cJSON_Print(localVarSingleItemJSON_order);

	list_addElement(localVarHeaderType, "application/xml"); // produces

	list_addElement(localVarHeaderType, "application/json"); // produces

	apiClient_invoke(apiClient,
	                 localVarPath,
	                 localVarQueryParameters,
	                 localVarHeaderParameters,
	                 localVarFormParameters,
	                 localVarHeaderType,
	                 localVarContentType,
	                 localVarBodyParameters,
	                 "POST");

	// nonprimitive not container
	order_t *elementToReturn = order_parseFromJSON(apiClient->dataReceived);
	if(elementToReturn == NULL) {
		return 0;
	}

	// return type
	apiClient_free(apiClient);
	list_free(localVarQueryParameters);
	list_free(localVarHeaderParameters);
	list_free(localVarFormParameters);
	list_free(localVarHeaderType);
	list_free(localVarContentType);
	free(localVarPath);
	return elementToReturn;
}
