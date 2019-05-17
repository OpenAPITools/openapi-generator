/*
 * pet.h
 *
 * A pet for sale in the pet store
 */

#ifndef _pet_H_
#define _pet_H_

#include <string.h>
#include "../external/cJSON.h"
#include "../include/list.h"
#include "../include/keyValuePair.h"
#include "category.h"
#include "tag.h"

                typedef enum  {  available, pending, sold } status_e;

        char* status_ToString(status_e status);

        status_e status_FromString(char* status);


typedef struct pet_t {
    long id; //numeric
    category_t *category; //model
    char *name; // string
    list_t *photoUrls; //primitive container
    list_t *tags; //nonprimitive container
    status_e status; //enum

} pet_t;

pet_t *pet_create(
    long id,
    category_t *category,
    char *name,
    list_t *photoUrls,
    list_t *tags,
    status_e status
);

void pet_free(pet_t *pet);

pet_t *pet_parseFromJSON(cJSON *petJSON);

cJSON *pet_convertToJSON(pet_t *pet);

#endif /* _pet_H_ */

