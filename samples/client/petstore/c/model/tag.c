#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include "tag.h"



static tag_t *tag_create_internal(
    long *id,
    char *name
    ) {
    tag_t *tag_local_var = malloc(sizeof(tag_t));
    if (!tag_local_var) {
        return NULL;
    }
    memset(tag_local_var, 0, sizeof(tag_t));
    tag_local_var->_library_owned = 1;
    tag_local_var->id = id;
    tag_local_var->name = name;
    return tag_local_var;
}

__attribute__((deprecated)) tag_t *tag_create(
    long *id,
    char *name
    ) {
    long *id_copy = NULL;
    if (id) {
        id_copy = malloc(sizeof(long));
        if (id_copy) *id_copy = *id;
    }
    tag_t *result = tag_create_internal (
        id_copy,
        name
        );
    if (!result) {
        free(id_copy);
    }
    return result;
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
    if (tag->id) {
        free(tag->id);
        tag->id = NULL;
    }
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
    if(cJSON_AddNumberToObject(item, "id", *tag->id) == NULL) {
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

    // define the local variable for tag->id
    long *id_local_var = NULL;

    char *name_local_str = NULL;

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
    id_local_var = malloc(sizeof(long));
    if(!id_local_var)
    {
        goto end;
    }
    *id_local_var = id->valuedouble;
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


    if (name && !cJSON_IsNull(name)) name_local_str = strdup(name->valuestring);

    tag_local_var = tag_create_internal (
        id_local_var,
        name_local_str
        );

    if (!tag_local_var) {
        goto end;
    }

    return tag_local_var;
end:
    if (id_local_var) {
        free(id_local_var);
        id_local_var = NULL;
    }
    if (name_local_str) {
        free(name_local_str);
        name_local_str = NULL;
    }
    return NULL;

}
