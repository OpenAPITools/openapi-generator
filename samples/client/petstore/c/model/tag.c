#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include "tag.h"



static tag_t *tag_create_internal(
    long id,
    char *name
    ) {
    tag_t *tag_local_var = malloc(sizeof(tag_t));
    if (!tag_local_var) {
        return NULL;
    }
    tag_local_var->id = id;
    tag_local_var->name = name;

    tag_local_var->_library_owned = 1;
    return tag_local_var;
}

__attribute__((deprecated)) tag_t *tag_create(
    long id,
    char *name
    ) {
    return tag_create_internal (
        id,
        name
        );
}

void tag_free(tag_t *tag) {
    if(NULL == tag){
        return ;
    }
    if(tag->_library_owned != 1){
        fprintf(stderr, "WARNING: %s() does NOT free objects allocated by the user\n", "tag_free");
        return ;
    }
    listEntry_t *listEntry;
    if (tag->name) {
        free(tag->name);
        tag->name = NULL;
    }
    free(tag);
}

cJSON *tag_convertToJSON(tag_t *tag) {
    cJSON *item = cJSON_CreateObject();

    // tag->id
    if(tag->id) {
    if(cJSON_AddNumberToObject(item, "id", tag->id) == NULL) {
    goto fail; //Numeric
    }
    }


    // tag->name
    if(tag->name) {
    if(cJSON_AddStringToObject(item, "name", tag->name) == NULL) {
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

tag_t *tag_parseFromJSON(cJSON *tagJSON){

    tag_t *tag_local_var = NULL;

    // tag->id
    cJSON *id = cJSON_GetObjectItemCaseSensitive(tagJSON, "id");
    if (cJSON_IsNull(id)) {
        id = NULL;
    }
    if (id) { 
    if(!cJSON_IsNumber(id))
    {
    goto end; //Numeric
    }
    }

    // tag->name
    cJSON *name = cJSON_GetObjectItemCaseSensitive(tagJSON, "name");
    if (cJSON_IsNull(name)) {
        name = NULL;
    }
    if (name) { 
    if(!cJSON_IsString(name) && !cJSON_IsNull(name))
    {
    goto end; //String
    }
    }


    tag_local_var = tag_create_internal (
        id ? id->valuedouble : 0,
        name && !cJSON_IsNull(name) ? strdup(name->valuestring) : NULL
        );

    return tag_local_var;
end:
    return NULL;

}
