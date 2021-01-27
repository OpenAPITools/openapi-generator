#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include "tag.h"



tag_t *tag_create(
    long id,
    char *name
    ) {
    tag_t *tag_local_var = malloc(sizeof(tag_t));
    if (!tag_local_var) {
        return NULL;
    }
    tag_local_var->id = id;
    tag_local_var->name = name;

    return tag_local_var;
}


void tag_free(tag_t *tag) {
    if(NULL == tag){
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
    if (id) { 
    if(!cJSON_IsNumber(id))
    {
    goto end; //Numeric
    }
    }

    // tag->name
    cJSON *name = cJSON_GetObjectItemCaseSensitive(tagJSON, "name");
    if (name) { 
    if(!cJSON_IsString(name))
    {
    goto end; //String
    }
    }


    tag_local_var = tag_create (
        id ? id->valuedouble : 0,
        name ? strdup(name->valuestring) : NULL
        );

    return tag_local_var;
end:
    return NULL;

}
