/*
 * MappedModel.h
 *
 * to test mapping features
 */

#ifndef _MappedModel_H_
#define _MappedModel_H_

#include <string.h>
#include "../external/cJSON.h"
#include "../include/list.h"
#include "../include/keyValuePair.h"
#include "../include/binary.h"

typedef struct MappedModel_t MappedModel_t;




typedef struct MappedModel_t {
    int another_property; //numeric
    char *uuid_property; // string

} MappedModel_t;

MappedModel_t *MappedModel_create(
    int another_property,
    char *uuid_property
);

void MappedModel_free(MappedModel_t *MappedModel);

MappedModel_t *MappedModel_parseFromJSON(cJSON *MappedModelJSON);

cJSON *MappedModel_convertToJSON(MappedModel_t *MappedModel);

#endif /* _MappedModel_H_ */

