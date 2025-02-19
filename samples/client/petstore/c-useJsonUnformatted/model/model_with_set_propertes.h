/*
 * model_with_set_propertes.h
 *
 * to test set properties
 */

#ifndef _model_with_set_propertes_H_
#define _model_with_set_propertes_H_

#include <string.h>
#include "../external/cJSON.h"
#include "../include/list.h"
#include "../include/keyValuePair.h"
#include "../include/binary.h"

typedef struct model_with_set_propertes_t model_with_set_propertes_t;

#include "tag.h"



typedef struct model_with_set_propertes_t {
    list_t *tag_set; //nonprimitive container
    list_t *string_set; //primitive container

} model_with_set_propertes_t;

model_with_set_propertes_t *model_with_set_propertes_create(
    list_t *tag_set,
    list_t *string_set
);

void model_with_set_propertes_free(model_with_set_propertes_t *model_with_set_propertes);

model_with_set_propertes_t *model_with_set_propertes_parseFromJSON(cJSON *model_with_set_propertesJSON);

cJSON *model_with_set_propertes_convertToJSON(model_with_set_propertes_t *model_with_set_propertes);

#endif /* _model_with_set_propertes_H_ */

