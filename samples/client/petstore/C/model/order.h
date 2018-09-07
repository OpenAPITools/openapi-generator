/*
 * order.h
 *
 * An order for a pets from the pet store
 */

#ifndef _order_H_
#define _order_H_

#include <string.h>

typedef struct order_t {
	long id;
	long petId;
	int quantity;
	char *shipDate;
	char *status;
	int *complete;
} order_t;

order_t *order_create(
		long id,
		long petId,
		int quantity,
		char *shipDate,
		char *status,
		int *complete
		);
		
void order_free(order_t *order);

order_t *order_parseFromJSON(char *jsonString)

cJSON *order_convertToJSON(order_t *order);

#endif /* _order_H_ */
