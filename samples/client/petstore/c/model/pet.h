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
#include "../include/binary.h"

typedef struct pet_t pet_t;

#include "category.h"
#include "tag.h"

// Enum STATUS for pet

typedef enum  { openapi_petstore_pet_STATUS_NULL = 0, openapi_petstore_pet_STATUS_available, openapi_petstore_pet_STATUS_pending, openapi_petstore_pet_STATUS_sold } openapi_petstore_pet_STATUS_e;

char* pet_status_ToString(openapi_petstore_pet_STATUS_e status);

openapi_petstore_pet_STATUS_e pet_status_FromString(char* status);



typedef struct pet_t {
    long id; //numeric
    struct category_t *category; //model
    char *name; // string
    list_t *photo_urls; //primitive container
    list_t *tags; //nonprimitive container
    openapi_petstore_pet_STATUS_e status; //enum

} pet_t;

pet_t *pet_create(
    long id,
    category_t *category,
    char *name,
    list_t *photo_urls,
    list_t *tags,
    openapi_petstore_pet_STATUS_e status
);

void pet_free(pet_t *pet);

pet_t *pet_parseFromJSON(cJSON *petJSON);

cJSON *pet_convertToJSON(pet_t *pet);

#endif /* _pet_H_ */

