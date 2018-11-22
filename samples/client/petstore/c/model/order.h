/*
 * order.h
 *
 * An order for a pets from the pet store
 */

#ifndef _order_H_
#define _order_H_

#include <string.h>
#include "cJSON.h"

typedef int bool;
#define true 1
#define false 0

typedef enum  {  placed, approved, delivered } status_e;

char *statusorder_ToString(status_e status);

status_e statusorder_FromString(char *status);

typedef struct order_t {
	long id; // numeric
	long petId; // numeric
	int quantity; // numeric
	char *shipDate; // date time string
	status_e status; // enum string
	bool complete; // boolean
} order_t;

order_t *order_create(long id, long petId, int quantity, char *shipDate,
                      status_e status, bool complete);

void order_free(order_t *order);

order_t *order_parseFromJSON(char *jsonString);

cJSON *order_convertToJSON(order_t *order);

#endif /* _order_H_ */
