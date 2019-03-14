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

                typedef enum  {  placed, approved, delivered } status_e;

        char* status_ToString(status_e status);

        status_e status_FromString(char* status);


typedef struct order_t {
    long id; //numeric
    long petId; //numeric
    int quantity; //numeric
    char *shipDate; //date time
    status_e status; //enum
    int complete; //boolean

} order_t;

order_t *order_create(
    long id,
    long petId,
    int quantity,
    char *shipDate,
    status_e status,
    int complete
);

void order_free(order_t *order);

order_t *order_parseFromJSON(cJSON *orderJSON);

cJSON *order_convertToJSON(order_t *order);

#endif /* _order_H_ */

