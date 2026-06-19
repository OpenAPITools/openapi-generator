#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include "mapped_model.h"



static MappedModel_t *MappedModel_create_internal(
    int another_property,
    char *uuid_property
    ) {
    MappedModel_t *MappedModel_local_var = malloc(sizeof(MappedModel_t));
    if (!MappedModel_local_var) {
        return NULL;
    }
    MappedModel_local_var->another_property = another_property;
    MappedModel_local_var->uuid_property = uuid_property;

    MappedModel_local_var->_library_owned = 1;
    return MappedModel_local_var;
}

__attribute__((deprecated)) MappedModel_t *MappedModel_create(
    int another_property,
    char *uuid_property
    ) {
    return MappedModel_create_internal (
        another_property,
        uuid_property
        );
}

void MappedModel_free(MappedModel_t *MappedModel) {
    if(NULL == MappedModel){
        return ;
    }
    if(MappedModel->_library_owned != 1){
        fprintf(stderr, "WARNING: %s() does NOT free objects allocated by the user\n", "MappedModel_free");
        return ;
    }
    listEntry_t *listEntry;
    if (MappedModel->uuid_property) {
        free(MappedModel->uuid_property);
        MappedModel->uuid_property = NULL;
    }
    free(MappedModel);
}

cJSON *MappedModel_convertToJSON(MappedModel_t *MappedModel) {
    cJSON *item = cJSON_CreateObject();

    // MappedModel->another_property
    if(MappedModel->another_property) {
    if(cJSON_AddNumberToObject(item, "another_property", MappedModel->another_property) == NULL) {
    goto fail; //Numeric
    }
    }


    // MappedModel->uuid_property
    if(MappedModel->uuid_property) {
    if(cJSON_AddStringToObject(item, "uuid_property", MappedModel->uuid_property) == NULL) {
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

MappedModel_t *MappedModel_parseFromJSON(cJSON *MappedModelJSON){

    MappedModel_t *MappedModel_local_var = NULL;

    // MappedModel->another_property
    cJSON *another_property = cJSON_GetObjectItemCaseSensitive(MappedModelJSON, "another_property");
    if (cJSON_IsNull(another_property)) {
        another_property = NULL;
    }
    if (another_property) { 
    if(!cJSON_IsNumber(another_property))
    {
    goto end; //Numeric
    }
    }

    // MappedModel->uuid_property
    cJSON *uuid_property = cJSON_GetObjectItemCaseSensitive(MappedModelJSON, "uuid_property");
    if (cJSON_IsNull(uuid_property)) {
        uuid_property = NULL;
    }
    if (uuid_property) { 
    if(!cJSON_IsString(uuid_property) && !cJSON_IsNull(uuid_property))
    {
    goto end; //String
    }
    }


    MappedModel_local_var = MappedModel_create_internal (
        another_property ? another_property->valuedouble : 0,
        uuid_property && !cJSON_IsNull(uuid_property) ? strdup(uuid_property->valuestring) : NULL
        );

    return MappedModel_local_var;
end:
    return NULL;

}
