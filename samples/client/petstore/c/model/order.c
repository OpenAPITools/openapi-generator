#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include "cJSON.h"
#include "list.h"
#include "keyValuePair.h"
#include "order.h"

char *statusorder_ToString(status_e status) {
	char *statusArray[] = { "placed", "approved", "delivered" };
	return statusArray[status];
}

status_e statusorder_FromString(char *status) {
	int stringToReturn = 0;
	char *statusArray[] = { "placed", "approved", "delivered" };
	size_t sizeofArray = sizeof(statusArray) / sizeof(statusArray[0]);
	while(stringToReturn < sizeofArray) {
		if(strcmp(status, statusArray[stringToReturn]) == 0) {
			return stringToReturn;
		}
		stringToReturn++;
	}
}

order_t *order_create(long id, long petId, int quantity, char *shipDate,
                      status_e status, bool complete) {
	order_t *order = malloc(sizeof(order_t));
	order->id = id;
	order->petId = petId;
	order->quantity = quantity;
	order->shipDate = shipDate;
	order->status = status;
	order->complete = complete;

	return order;
}


void order_free(order_t *order) {
	listEntry_t *listEntry;
	free(order->shipDate);

	free(order);
}

cJSON *order_convertToJSON(order_t *order) {
	cJSON *item = cJSON_CreateObject();
	// order->id
	if(cJSON_AddNumberToObject(item, "id", order->id) == NULL) {
		goto fail; // Numeric
	}

	// order->petId
	if(cJSON_AddNumberToObject(item, "petId", order->petId) == NULL) {
		goto fail; // Numeric
	}

	// order->quantity
	if(cJSON_AddNumberToObject(item, "quantity", order->quantity) == NULL) {
		goto fail; // Numeric
	}

	// order->shipDate
	if(cJSON_AddStringToObject(item, "shipDate", order->shipDate) == NULL) {
		goto fail; // String
	}

	// order->status
	if(cJSON_AddStringToObject(item, "status",
	                           statusorder_ToString(order->status)) == NULL)
	{
		goto fail; // String
	}

	// order->complete
	if(cJSON_AddBoolToObject(item, "complete", order->complete) == NULL) {
		goto fail; // Numeric
	}

	return item;
fail:
	cJSON_Delete(item);
	return NULL;
}

order_t *order_parseFromJSON(char *jsonString) {
	order_t *order = NULL;
	cJSON *orderJSON = cJSON_Parse(jsonString);
	if(orderJSON == NULL) {
		const char *error_ptr = cJSON_GetErrorPtr();
		if(error_ptr != NULL) {
			fprintf(stderr, "Error Before: %s\n", error_ptr);
			goto end;
		}
	}

	// order->id
	cJSON *id = cJSON_GetObjectItemCaseSensitive(orderJSON, "id");
	if(!cJSON_IsNumber(id)) {
		goto end; // Numeric
	}

	// order->petId
	cJSON *petId = cJSON_GetObjectItemCaseSensitive(orderJSON, "petId");
	if(!cJSON_IsNumber(petId)) {
		goto end; // Numeric
	}

	// order->quantity
	cJSON *quantity =
		cJSON_GetObjectItemCaseSensitive(orderJSON, "quantity");
	if(!cJSON_IsNumber(quantity)) {
		goto end; // Numeric
	}

	// order->shipDate
	cJSON *shipDate =
		cJSON_GetObjectItemCaseSensitive(orderJSON, "shipDate");
	if(!cJSON_IsString(shipDate) ||
	   (shipDate->valuestring == NULL))
	{
		goto end; // String
	}

	// order->status
	status_e statusVariable;
	cJSON *status = cJSON_GetObjectItemCaseSensitive(orderJSON, "status");
	if(!cJSON_IsString(status) ||
	   (status->valuestring == NULL))
	{
		goto end; // String
	}

	statusVariable = statusorder_FromString(status->valuestring);

	// order->complete
	cJSON *complete =
		cJSON_GetObjectItemCaseSensitive(orderJSON, "complete");
	if(!cJSON_IsBool(complete)) {
		goto end; // Numeric
	}


	order = order_create(
		id->valuedouble,
		petId->valuedouble,
		quantity->valuedouble,
		strdup(shipDate->valuestring),
		statusVariable,
		complete->valueint
		);
	cJSON_Delete(orderJSON);
	return order;
end:
	cJSON_Delete(orderJSON);
	return NULL;
}
