#include <stdlib.h>
#include <stdio.h>
#include <ctype.h>
#include "apiClient.h"
#include "cJSON.h"
#include "keyValuePair.h"
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
void StoreAPI_deleteOrder(apiClient_t *apiClient, char *orderId) {
	list_t *localVarQueryParameters = NULL;
	list_t *localVarHeaderParameters = NULL;
	list_t *localVarFormParameters = NULL;
	list_t *localVarHeaderType = NULL;
	list_t *localVarContentType = NULL;
	char *localVarBodyParameters = NULL;

	// create the path
	long sizeOfPath = strlen("/store/order/{orderId}") + 1;
	char *localVarPath = malloc(sizeOfPath);
	snprintf(localVarPath, sizeOfPath, "/store/order/{orderId}");


	// Path Params
	long sizeOfPathParams = strlen(orderId) + 3 + strlen("{ orderId }");

	if(orderId == NULL) {
		goto end;
	}
	char *localVarToReplace = malloc(sizeOfPathParams);
	sprintf(localVarToReplace, "%s%s%s", "{", "orderId", "}");

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
end:    apiClient_free(apiClient);





	// free(localVarPathFinal);
	free(localVarToReplace);
	free(localVarPath);
}

// Returns pet inventories by status
//
// Returns a map of status codes to quantities
//
list_t *StoreAPI_getInventory(apiClient_t *apiClient) {
	list_t *localVarQueryParameters = NULL;
	list_t *localVarHeaderParameters = NULL;
	list_t *localVarFormParameters = NULL;
	list_t *localVarHeaderType = list_create();
	list_t *localVarContentType = NULL;
	char *localVarBodyParameters = NULL;

	// create the path
	long sizeOfPath = strlen("/store/inventory") + 1;
	char *localVarPath = malloc(sizeOfPath);
	snprintf(localVarPath, sizeOfPath, "/store/inventory");


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

	// primitive reutrn type not simple
	cJSON *StorelocalVarJSON = cJSON_Parse(apiClient->dataReceived);
	cJSON *StoreVarJSON;
	list_t *elementToReturn = list_create();
	cJSON_ArrayForEach(StoreVarJSON, StorelocalVarJSON) {
		keyValuePair_t *keyPair = keyValuePair_create(
			StoreVarJSON->string, cJSON_Print(StoreVarJSON));
		list_addElement(elementToReturn, keyPair);
		free(keyPair->value);
	}
	cJSON_Delete(StorelocalVarJSON);

	apiClient_free(apiClient);



	list_free(localVarHeaderType);

	free(localVarPath);
	return elementToReturn;
end:
	return NULL;
}

// Find purchase order by ID
//
// For valid response try integer IDs with value <= 5 or > 10. Other values will generated exceptions
//
order_t *StoreAPI_getOrderById(apiClient_t *apiClient, long orderId) {
	list_t *localVarQueryParameters = NULL;
	list_t *localVarHeaderParameters = NULL;
	list_t *localVarFormParameters = NULL;
	list_t *localVarHeaderType = list_create();
	list_t *localVarContentType = NULL;
	char *localVarBodyParameters = NULL;

	// create the path
	long sizeOfPath = strlen("/store/order/{orderId}") + 1;
	char *localVarPath = malloc(sizeOfPath);
	snprintf(localVarPath, sizeOfPath, "/store/order/{orderId}");


	// Path Params
	long sizeOfPathParams = sizeof(orderId) + 3 + strlen("{ orderId }");

	if(orderId == 0) {
		goto end;
	}
	char *localVarToReplace = malloc(sizeOfPathParams);
	snprintf(localVarToReplace, sizeOfPathParams, "%s%s%s", "{", "orderId",
	         "}");

	char localVarBuff[256];
	intToStr(localVarBuff, orderId);

	localVarPath =
		strReplace(localVarPath, localVarToReplace, localVarBuff);


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
		// return 0;
	}

	// return type
	apiClient_free(apiClient);



	list_free(localVarHeaderType);

	free(localVarPath);
	// free(localVarPathFinal);
	free(localVarToReplace);
	return elementToReturn;
end:
	return NULL;
}

// Place an order for a pet
//
order_t *StoreAPI_placeOrder(apiClient_t *apiClient, order_t *order) {
	list_t *localVarQueryParameters = NULL;
	list_t *localVarHeaderParameters = NULL;
	list_t *localVarFormParameters = NULL;
	list_t *localVarHeaderType = list_create();
	list_t *localVarContentType = NULL;
	char *localVarBodyParameters = NULL;

	// create the path
	long sizeOfPath = strlen("/store/order") + 1;
	char *localVarPath = malloc(sizeOfPath);
	snprintf(localVarPath, sizeOfPath, "/store/order");


	// Body Param
	cJSON *localVarSingleItemJSON_order;
	if(order != NULL) {
		// string
		localVarSingleItemJSON_order = order_convertToJSON(order);
		localVarBodyParameters = cJSON_Print(
			localVarSingleItemJSON_order);
	}

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
		// return 0;
	}

	// return type
	apiClient_free(apiClient);



	list_free(localVarHeaderType);

	free(localVarPath);
	cJSON_Delete(localVarSingleItemJSON_order);
	free(localVarBodyParameters);
	return elementToReturn;
end:
	return NULL;
}
