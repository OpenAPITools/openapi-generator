#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include "object.h"

object_t *object_create() {
    object_t *object = malloc(sizeof(object_t));

    return object;
}

void object_free(object_t *object) {
    if (object->temporary) {
        free(object->temporary);
    }
    free (object);
}

cJSON *object_convertToJSON(object_t *object) {
    if (object){
        if (object->temporary == NULL) {
            object->temporary = "{}";
        }
        // need to process the object somehow?
        return cJSON_Parse(object->temporary);
    } else {
        return NULL;
    }
}

object_t *object_parseFromJSON(cJSON *json){
    object_t *object = object_create();
    object->temporary = cJSON_Print(json);

    return object;
end:
    return NULL;
}
