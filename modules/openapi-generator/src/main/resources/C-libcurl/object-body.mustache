#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include "object.h"

object_t *object_create() {
    object_t *object;

    object = malloc(sizeof(object_t));
    if (!object)
        return NULL;

    object->json = cJSON_CreateObject();
    if (!object->json)
        goto fail;
    return object;

fail:
    free(object);
    return NULL;
}

void object_free(object_t *object) {
    if (object->json)
        cJSON_Delete(object->json);
    free(object);
}

cJSON *object_convertToJSON(object_t *object) {
    return cJSON_Duplicate(object->json, 1);
}

object_t *object_parseFromJSON(cJSON *json) {
    object_t *object;

    object = malloc(sizeof(object_t));
    if (!object)
        return NULL;
    object->json = cJSON_Duplicate(json, 1);
    if (!object->json) {
        free(object);
        return NULL;
    }
    return object;
}
