#ifndef INCLUDE_CATEGORY_H
#define INCLUDE_CATEGORY_H
#include "cJSON.h"

typedef struct category_t {
    long id;
    char *name;
} category_t;

category_t *category_create(long id, char *name);
void category_free(category_t *category);

cJSON *category_convertToJSON(category_t *category);
#endif // INCLUDE_CATEGORY_H