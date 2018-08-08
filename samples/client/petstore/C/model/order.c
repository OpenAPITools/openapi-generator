#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include "cJSON.h"


order_t *order_create(
		long		*id,
		long		*petId,
		int		*quantity,
		char		*shipDate,
		char		*status,
		int		*complete
		) {
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




	free(order->status)


	free(order);
}

cJSON *order_convertToJSON(order_t *order) {
	cJSON *item = cJSON_CreateObject();
	// order->id
	if(cJSON_AddNumberToObject(item, "id", order->id) == NULL) {
    	goto fail;
    }


	// order->petId
	if(cJSON_AddNumberToObject(item, "petId", order->petId) == NULL) {
    	goto fail;
    }


	// order->quantity
	if(cJSON_AddNumberToObject(item, "quantity", order->quantity) == NULL) {
    	goto fail;
    }


	// order->shipDate

	// order->status
	if(cJSON_AddStringToObject(item, "status", order->status) == NULL) {
		goto fail;
	}

	// order->complete

	return item;
fail:
	cJSON_Delete(item);
	return NULL;
}
