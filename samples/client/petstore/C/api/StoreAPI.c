#include <stdlib.h>
#include <stdio.h>
#include <ctype.h>
#include "apiClient.h"
#include "cJSON.h"
#include "keyValuePair.h"
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

	// TODO path parameter OrderId (orderId) not yet supported
	// TODO base path = http://petstore.swagger.io/v2


	char *baseNameModToReplace = malloc(sizeof(orderId) + 2); // baseNameMod free not yet implemented
	snprintf(baseNameModToReplace,
	         strlen(orderId) + 3,
	         "%s%s%s",
	         "{",
	         "orderId",
	         "}");
	char *baseNameMod = malloc(strlen(orderId) + 2); // baseNameMod free not yet implemented
	snprintf(baseNameMod, strlen(orderId) + 3, "%s%s%s", "{", orderId, "}");
	localVarPath = strReplace(localVarPath, baseNameModToReplace, orderId);
	// localVarPath = "orderId";




	apiClient_invoke(apiClient,
	                 localVarPath,
	                 localVarQueryParameters,
	                 localVarHeaderParameters,
	                 localVarFormParameters,
	                 localVarHeaderType,
	                 localVarContentType,
	                 localVarBodyParameters,
	                 "DELETE");

	// free(apiClient->dataReceived);
	// free(localVarPath);
	// free(orderId);
}

// Returns pet inventories by status
//
// Returns a map of status codes to quantities
//
map *StoreAPI_getInventory(apiClient_t *apiClient) {
	list_t *localVarQueryParameters = list_create();
	list_t *localVarHeaderParameters = list_create();
	list_t *localVarFormParameters = list_create();
	list_t *localVarHeaderType = list_create();
	list_t *localVarContentType = list_create();
	char *localVarBodyParameters = NULL;


	// create the path
	char *localVarPath = malloc(MAX_BUFFER_LENGTH);
	snprintf(localVarPath, MAX_BUFFER_LENGTH, "/store/inventory");




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

	// primitive reutrn type
	cJSON *Store;
	cJSON *StoreAPIJSON = cJSON_Parse(apiClient->dataReceived);
	if(!cJSON_IsString(StoreAPIJSON)) {
		return 0; // primitive return type
	}
	char *elementToReturn = cJSON_Print(Store);

	// return elementToReturn;
	// free(apiClient->dataReceived);
	// free(localVarPath);

	return elementToReturn;
}

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

	// TODO path parameter OrderId (orderId) not yet supported
	// TODO base path = http://petstore.swagger.io/v2


	char *baseNameModToReplace = malloc(sizeof(orderId) + 3); // baseNameMod free not yet implemented
	snprintf(baseNameModToReplace,
	         strlen("orderId") + 3,
	         "%s%s%s",
	         "{",
	         "orderId",
	         "}");
	char *baseNameMod = malloc(sizeof(orderId) + 2); // baseNameMod free not yet implemented
	snprintf(baseNameMod, sizeof(orderId) + 3, "%s%li%s", "{", orderId,
	         "}");
	char buff[64];
	intToStr(buf, orderId);
	localVarPath = strReplace(localVarPath, baseNameModToReplace, buff);
	// localVarPath = buff;



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
	} else {
		// cJSON *jsonObject = Store_convertToJSON(localVarorder);
		// cJSON_Delete(jsonObject);
	}

	// return elementToReturn;
	// free(apiClient->dataReceived);
	// free(localVarPath);


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



	// JSON HTTP Request - order
	// string
	cJSON *OrderJSONObject;
	OrderJSONObject = order_convertToJSON(order);
	localVarBodyParameters = cJSON_Print(OrderJSONObject);

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
	} else {
		// cJSON *jsonObject = Store_convertToJSON(localVarorder);
		// cJSON_Delete(jsonObject);
	}

	// return elementToReturn;
	// free(apiClient->dataReceived);
	// free(localVarPath);

	// free(localVarBodyParameters);
	// cJSON_Delete(OrderJSONObject);

	return elementToReturn;
}
