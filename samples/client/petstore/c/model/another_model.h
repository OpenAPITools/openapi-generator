/*
 * another_model.h
 *
 * to test mapping features
 */

#ifndef _another_model_H_
#define _another_model_H_

#include <string.h>
#include "../external/cJSON.h"
#include "../include/list.h"
#include "../include/keyValuePair.h"
#include "../include/binary.h"

typedef struct another_model_t another_model_t;




typedef struct another_model_t {
    int another_property; //numeric
    char *uuid_property; // string

} another_model_t;

another_model_t *another_model_create(
    int another_property,
    char *uuid_property
);

void another_model_free(another_model_t *another_model);

another_model_t *another_model_parseFromJSON(cJSON *another_modelJSON);

cJSON *another_model_convertToJSON(another_model_t *another_model);

#endif /* _another_model_H_ */

