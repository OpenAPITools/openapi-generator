/*
 * model_with_set_properties.h
 *
 * to test set properties
 */

#ifndef _model_with_set_properties_H_
#define _model_with_set_properties_H_

#include <string.h>
#include "../external/cJSON.h"
#include "../include/list.h"
#include "../include/keyValuePair.h"
#include "../include/binary.h"

typedef struct model_with_set_properties_t model_with_set_properties_t;

#include "tag.h"



typedef struct model_with_set_properties_t {
    list_t *tag_set; //nonprimitive container
    list_t *string_set; //primitive container

} model_with_set_properties_t;

model_with_set_properties_t *model_with_set_properties_create(
    list_t *tag_set,
    list_t *string_set
);

void model_with_set_properties_free(model_with_set_properties_t *model_with_set_properties);

model_with_set_properties_t *model_with_set_properties_parseFromJSON(cJSON *model_with_set_propertiesJSON);

cJSON *model_with_set_properties_convertToJSON(model_with_set_properties_t *model_with_set_properties);

#endif /* _model_with_set_properties_H_ */

