#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include "model_with_set_properties.h"



model_with_set_properties_t *model_with_set_properties_create(
    list_t *tag_set,
    list_t *string_set
    ) {
    model_with_set_properties_t *model_with_set_properties_local_var = malloc(sizeof(model_with_set_properties_t));
    if (!model_with_set_properties_local_var) {
        return NULL;
    }
    model_with_set_properties_local_var->tag_set = tag_set;
    model_with_set_properties_local_var->string_set = string_set;

    return model_with_set_properties_local_var;
}


void model_with_set_properties_free(model_with_set_properties_t *model_with_set_properties) {
    if(NULL == model_with_set_properties){
        return ;
    }
    listEntry_t *listEntry;
    if (model_with_set_properties->tag_set) {
        list_ForEach(listEntry, model_with_set_properties->tag_set) {
            tag_free(listEntry->data);
        }
        list_freeList(model_with_set_properties->tag_set);
        model_with_set_properties->tag_set = NULL;
    }
    if (model_with_set_properties->string_set) {
        list_ForEach(listEntry, model_with_set_properties->string_set) {
            free(listEntry->data);
        }
        list_freeList(model_with_set_properties->string_set);
        model_with_set_properties->string_set = NULL;
    }
    free(model_with_set_properties);
}

cJSON *model_with_set_properties_convertToJSON(model_with_set_properties_t *model_with_set_properties) {
    cJSON *item = cJSON_CreateObject();

    // model_with_set_properties->tag_set
    if(model_with_set_properties->tag_set) {
    cJSON *tag_set = cJSON_AddArrayToObject(item, "tag_set");
    if(tag_set == NULL) {
    goto fail; //nonprimitive container
    }

    listEntry_t *tag_setListEntry;
    if (model_with_set_properties->tag_set) {
    list_ForEach(tag_setListEntry, model_with_set_properties->tag_set) {
    cJSON *itemLocal = tag_convertToJSON(tag_setListEntry->data);
    if(itemLocal == NULL) {
    goto fail;
    }
    cJSON_AddItemToArray(tag_set, itemLocal);
    }
    }
    }


    // model_with_set_properties->string_set
    if(model_with_set_properties->string_set) {
    cJSON *string_set = cJSON_AddArrayToObject(item, "string_set");
    if(string_set == NULL) {
        goto fail; //primitive container
    }

    listEntry_t *string_setListEntry;
    list_ForEach(string_setListEntry, model_with_set_properties->string_set) {
    if(cJSON_AddStringToObject(string_set, "", (char*)string_setListEntry->data) == NULL)
    {
        goto fail;
    }
    }
    }

    return item;
fail:
    if (item) {
        cJSON_Delete(item);
    }
    return NULL;
}

model_with_set_properties_t *model_with_set_properties_parseFromJSON(cJSON *model_with_set_propertiesJSON){

    model_with_set_properties_t *model_with_set_properties_local_var = NULL;

    // define the local list for model_with_set_properties->tag_set
    list_t *tag_setList = NULL;

    // define the local list for model_with_set_properties->string_set
    list_t *string_setList = NULL;

    // model_with_set_properties->tag_set
    cJSON *tag_set = cJSON_GetObjectItemCaseSensitive(model_with_set_propertiesJSON, "tag_set");
    if (tag_set) { 
    cJSON *tag_set_local_nonprimitive = NULL;
    if(!cJSON_IsArray(tag_set)){
        goto end; //nonprimitive container
    }

    tag_setList = list_createList();

    cJSON_ArrayForEach(tag_set_local_nonprimitive,tag_set )
    {
        if(!cJSON_IsObject(tag_set_local_nonprimitive)){
            goto end;
        }
        tag_t *tag_setItem = tag_parseFromJSON(tag_set_local_nonprimitive);

        list_addElement(tag_setList, tag_setItem);
    }
    }

    // model_with_set_properties->string_set
    cJSON *string_set = cJSON_GetObjectItemCaseSensitive(model_with_set_propertiesJSON, "string_set");
    if (string_set) { 
    cJSON *string_set_local = NULL;
    if(!cJSON_IsArray(string_set)) {
        goto end;//primitive container
    }
    string_setList = list_createList();

    cJSON_ArrayForEach(string_set_local, string_set)
    {
        if(!cJSON_IsString(string_set_local))
        {
            goto end;
        }
        list_addElement(string_setList , strdup(string_set_local->valuestring));
    }
    }


    model_with_set_properties_local_var = model_with_set_properties_create (
        tag_set ? tag_setList : NULL,
        string_set ? string_setList : NULL
        );

    return model_with_set_properties_local_var;
end:
    if (tag_setList) {
        listEntry_t *listEntry = NULL;
        list_ForEach(listEntry, tag_setList) {
            tag_free(listEntry->data);
            listEntry->data = NULL;
        }
        list_freeList(tag_setList);
        tag_setList = NULL;
    }
    if (string_setList) {
        listEntry_t *listEntry = NULL;
        list_ForEach(listEntry, string_setList) {
            free(listEntry->data);
            listEntry->data = NULL;
        }
        list_freeList(string_setList);
        string_setList = NULL;
    }
    return NULL;

}
