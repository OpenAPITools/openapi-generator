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

                typedef enum  {  order_STATUS_placed, order_STATUS_approved, order_STATUS_delivered } order_status_e;

        char* order_status_ToString(order_status_e status);

        order_status_e order_status_FromString(char* status);


typedef struct order_t {
    long id; //numeric
    long pet_id; //numeric
    int quantity; //numeric
    char *ship_date; //date time
    order_status_e status; //enum
    int complete; //boolean

} order_t;

order_t *order_create(
    long id,
    long pet_id,
    int quantity,
    char *ship_date,
    order_status_e status,
    int complete
);

void order_free(order_t *order);

order_t *order_parseFromJSON(cJSON *orderJSON);

cJSON *order_convertToJSON(order_t *order);

#endif /* _order_H_ */

