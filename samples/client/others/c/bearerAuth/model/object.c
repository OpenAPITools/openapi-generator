#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include "object.h"

object_t *object_create() {
    object_t *object = calloc(1, sizeof(object_t));

    return object;
}

void object_free(object_t *object) {
    if (!object) {
        return ;
    }

    if (object->temporary) {
        free(object->temporary);
        object->temporary = NULL;
    }

    free (object);
}

cJSON *object_convertToJSON(object_t *object) {
    if (!object) {
        return NULL;
    }

    if (!object->temporary) {
        return cJSON_Parse("null");
    }

    return cJSON_Parse(object->temporary);
}

object_t *object_parseFromJSON(cJSON *json){
    if (!json) {
        goto end;
    }

    object_t *object = object_create();
    if (!object) {
        goto end;
    }
    object->temporary = cJSON_Print(json);
    return object;

end:
    return NULL;
}
