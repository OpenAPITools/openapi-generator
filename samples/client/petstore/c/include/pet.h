#ifndef INCLUDE_PET_H
#define INCLUDE_PET_H

#include "cJSON.h"
#include "list.h"
#include "category.h"

typedef enum status_t {available, pending, sold } status_t;

char* status_ToString(status_t status);
status_t status_FromString(char *status);

typedef struct pet_t {
    long id;
    category_t *category;
    char *name;
    list_t *photoUrls;
    list_t *tags;
    status_t status;
} pet_t;

pet_t *pet_create(long id, category_t *category, char *name, list_t *photoUrls, list_t *tags, status_t status);
void pet_free(pet_t* pet);

pet_t *pet_parseFromJSON(char *jsonString);
cJSON *pet_convertToJSON(pet_t *pet);

#endif // INCLUDE_PET_H