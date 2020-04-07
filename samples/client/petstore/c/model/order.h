/*
 * order.h
 *
 * An order for a pets from the pet store
 */

#ifndef _order_H_
#define _order_H_

#include <string.h>
#include "../external/cJSON.h"
#include "../include/list.h"
#include "../include/keyValuePair.h"
#include "../include/binary.h"

typedef struct order_t order_t;


// Enum STATUS for order

typedef enum  { openapi_petstore_order_STATUS_NULL = 0, openapi_petstore_order_STATUS_placed, openapi_petstore_order_STATUS_approved, openapi_petstore_order_STATUS_delivered } openapi_petstore_order_STATUS_e;

char* order_status_ToString(openapi_petstore_order_STATUS_e status);

openapi_petstore_order_STATUS_e order_status_FromString(char* status);



typedef struct order_t {
    long id; //numeric
    long pet_id; //numeric
    int quantity; //numeric
    char *ship_date; //date time
    openapi_petstore_order_STATUS_e status; //enum
    int complete; //boolean

} order_t;

order_t *order_create(
    long id,
    long pet_id,
    int quantity,
    char *ship_date,
    openapi_petstore_order_STATUS_e status,
    int complete
);

void order_free(order_t *order);

order_t *order_parseFromJSON(cJSON *orderJSON);

cJSON *order_convertToJSON(order_t *order);

#endif /* _order_H_ */

