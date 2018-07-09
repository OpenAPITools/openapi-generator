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
	listEntry_t *listEntry;

	free(order->id)

	free(order->petId)

	free(order->quantity)

	free(order->shipDate)

	free(order->status)

	free(order->complete)

	free(order);
}
