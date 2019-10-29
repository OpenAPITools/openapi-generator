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

                typedef enum  {  pet_AVAILABLE, pet_PENDING, pet_SOLD } pet_status_e;

        char* pet_status_ToString(pet_status_e status);

        pet_status_e pet_status_FromString(char* status);


typedef struct pet_t {
    long id; //numeric
    category_t *category; //model
    char *name; // string
    list_t *photoUrls; //primitive container
    list_t *tags; //nonprimitive container
    pet_status_e status; //enum

} pet_t;

pet_t *pet_create(
    long id,
    category_t *category,
    char *name,
    list_t *photoUrls,
    list_t *tags,
    pet_status_e status
);

void pet_free(pet_t *pet);

pet_t *pet_parseFromJSON(cJSON *petJSON);

cJSON *pet_convertToJSON(pet_t *pet);

#endif /* _pet_H_ */

