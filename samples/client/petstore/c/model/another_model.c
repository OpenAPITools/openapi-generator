#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include "another_model.h"



another_model_t *another_model_create(
    int another_property,
    char *uuid_property
    ) {
    another_model_t *another_model_local_var = malloc(sizeof(another_model_t));
    if (!another_model_local_var) {
        return NULL;
    }
    another_model_local_var->another_property = another_property;
    another_model_local_var->uuid_property = uuid_property;

    return another_model_local_var;
}


void another_model_free(another_model_t *another_model) {
    if(NULL == another_model){
        return ;
    }
    listEntry_t *listEntry;
    if (another_model->uuid_property) {
        free(another_model->uuid_property);
        another_model->uuid_property = NULL;
    }
    free(another_model);
}

cJSON *another_model_convertToJSON(another_model_t *another_model) {
    cJSON *item = cJSON_CreateObject();

    // another_model->another_property
    if(another_model->another_property) {
    if(cJSON_AddNumberToObject(item, "another_property", another_model->another_property) == NULL) {
    goto fail; //Numeric
    }
    }


    // another_model->uuid_property
    if(another_model->uuid_property) {
    if(cJSON_AddStringToObject(item, "uuid_property", another_model->uuid_property) == NULL) {
    goto fail; //String
    }
    }

    return item;
fail:
    if (item) {
        cJSON_Delete(item);
    }
    return NULL;
}

another_model_t *another_model_parseFromJSON(cJSON *another_modelJSON){

    another_model_t *another_model_local_var = NULL;

    // another_model->another_property
    cJSON *another_property = cJSON_GetObjectItemCaseSensitive(another_modelJSON, "another_property");
    if (another_property) { 
    if(!cJSON_IsNumber(another_property))
    {
    goto end; //Numeric
    }
    }

    // another_model->uuid_property
    cJSON *uuid_property = cJSON_GetObjectItemCaseSensitive(another_modelJSON, "uuid_property");
    if (uuid_property) { 
    if(!cJSON_IsString(uuid_property) && !cJSON_IsNull(uuid_property))
    {
    goto end; //String
    }
    }


    another_model_local_var = another_model_create (
        another_property ? another_property->valuedouble : 0,
        uuid_property && !cJSON_IsNull(uuid_property) ? strdup(uuid_property->valuestring) : NULL
        );

    return another_model_local_var;
end:
    return NULL;

}
