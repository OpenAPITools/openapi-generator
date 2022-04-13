#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include "pet.h"


char* statuspet_ToString(openapi_petstore_pet_STATUS_e status) {
    char* statusArray[] =  { "NULL", "available", "pending", "sold" };
	return statusArray[status];
}

openapi_petstore_pet_STATUS_e statuspet_FromString(char* status){
    int stringToReturn = 0;
    char *statusArray[] =  { "NULL", "available", "pending", "sold" };
    size_t sizeofArray = sizeof(statusArray) / sizeof(statusArray[0]);
    while(stringToReturn < sizeofArray) {
        if(strcmp(status, statusArray[stringToReturn]) == 0) {
            return stringToReturn;
        }
        stringToReturn++;
    }
    return 0;
}

pet_t *pet_create(
    long id,
    category_t *category,
    char *name,
    list_t *photo_urls,
    list_t *tags,
    openapi_petstore_pet_STATUS_e status
    ) {
    pet_t *pet_local_var = malloc(sizeof(pet_t));
    if (!pet_local_var) {
        return NULL;
    }
    pet_local_var->id = id;
    pet_local_var->category = category;
    pet_local_var->name = name;
    pet_local_var->photo_urls = photo_urls;
    pet_local_var->tags = tags;
    pet_local_var->status = status;

    return pet_local_var;
}


void pet_free(pet_t *pet) {
    if(NULL == pet){
        return ;
    }
    listEntry_t *listEntry;
    if (pet->category) {
        category_free(pet->category);
        pet->category = NULL;
    }
    if (pet->name) {
        free(pet->name);
        pet->name = NULL;
    }
    if (pet->photo_urls) {
        list_ForEach(listEntry, pet->photo_urls) {
            free(listEntry->data);
        }
        list_freeList(pet->photo_urls);
        pet->photo_urls = NULL;
    }
    if (pet->tags) {
        list_ForEach(listEntry, pet->tags) {
            tag_free(listEntry->data);
        }
        list_freeList(pet->tags);
        pet->tags = NULL;
    }
    free(pet);
}

cJSON *pet_convertToJSON(pet_t *pet) {
    cJSON *item = cJSON_CreateObject();

    // pet->id
    if(pet->id) {
    if(cJSON_AddNumberToObject(item, "id", pet->id) == NULL) {
    goto fail; //Numeric
    }
    }


    // pet->category
    if(pet->category) {
    cJSON *category_local_JSON = category_convertToJSON(pet->category);
    if(category_local_JSON == NULL) {
    goto fail; //model
    }
    cJSON_AddItemToObject(item, "category", category_local_JSON);
    if(item->child == NULL) {
    goto fail;
    }
    }


    // pet->name
    if (!pet->name) {
        goto fail;
    }
    if(cJSON_AddStringToObject(item, "name", pet->name) == NULL) {
    goto fail; //String
    }


    // pet->photo_urls
    if (!pet->photo_urls) {
        goto fail;
    }
    cJSON *photo_urls = cJSON_AddArrayToObject(item, "photoUrls");
    if(photo_urls == NULL) {
        goto fail; //primitive container
    }

    listEntry_t *photo_urlsListEntry;
    list_ForEach(photo_urlsListEntry, pet->photo_urls) {
    if(cJSON_AddStringToObject(photo_urls, "", (char*)photo_urlsListEntry->data) == NULL)
    {
        goto fail;
    }
    }


    // pet->tags
    if(pet->tags) {
    cJSON *tags = cJSON_AddArrayToObject(item, "tags");
    if(tags == NULL) {
    goto fail; //nonprimitive container
    }

    listEntry_t *tagsListEntry;
    if (pet->tags) {
    list_ForEach(tagsListEntry, pet->tags) {
    cJSON *itemLocal = tag_convertToJSON(tagsListEntry->data);
    if(itemLocal == NULL) {
    goto fail;
    }
    cJSON_AddItemToArray(tags, itemLocal);
    }
    }
    }


    // pet->status
    if(pet->status != openapi_petstore_pet_STATUS_NULL) {
    if(cJSON_AddStringToObject(item, "status", statuspet_ToString(pet->status)) == NULL)
    {
    goto fail; //Enum
    }
    }

    return item;
fail:
    if (item) {
        cJSON_Delete(item);
    }
    return NULL;
}

pet_t *pet_parseFromJSON(cJSON *petJSON){

    pet_t *pet_local_var = NULL;

    // define the local variable for pet->category
    category_t *category_local_nonprim = NULL;

    // define the local list for pet->photo_urls
    list_t *photo_urlsList = NULL;

    // define the local list for pet->tags
    list_t *tagsList = NULL;

    // pet->id
    cJSON *id = cJSON_GetObjectItemCaseSensitive(petJSON, "id");
    if (id) { 
    if(!cJSON_IsNumber(id))
    {
    goto end; //Numeric
    }
    }

    // pet->category
    cJSON *category = cJSON_GetObjectItemCaseSensitive(petJSON, "category");
    if (category) { 
    category_local_nonprim = category_parseFromJSON(category); //nonprimitive
    }

    // pet->name
    cJSON *name = cJSON_GetObjectItemCaseSensitive(petJSON, "name");
    if (!name) {
        goto end;
    }

    
    if(!cJSON_IsString(name))
    {
    goto end; //String
    }

    // pet->photo_urls
    cJSON *photo_urls = cJSON_GetObjectItemCaseSensitive(petJSON, "photoUrls");
    if (!photo_urls) {
        goto end;
    }

    
    cJSON *photo_urls_local = NULL;
    if(!cJSON_IsArray(photo_urls)) {
        goto end;//primitive container
    }
    photo_urlsList = list_createList();

    cJSON_ArrayForEach(photo_urls_local, photo_urls)
    {
        if(!cJSON_IsString(photo_urls_local))
        {
            goto end;
        }
        list_addElement(photo_urlsList , strdup(photo_urls_local->valuestring));
    }

    // pet->tags
    cJSON *tags = cJSON_GetObjectItemCaseSensitive(petJSON, "tags");
    if (tags) { 
    cJSON *tags_local_nonprimitive = NULL;
    if(!cJSON_IsArray(tags)){
        goto end; //nonprimitive container
    }

    tagsList = list_createList();

    cJSON_ArrayForEach(tags_local_nonprimitive,tags )
    {
        if(!cJSON_IsObject(tags_local_nonprimitive)){
            goto end;
        }
        tag_t *tagsItem = tag_parseFromJSON(tags_local_nonprimitive);

        list_addElement(tagsList, tagsItem);
    }
    }

    // pet->status
    cJSON *status = cJSON_GetObjectItemCaseSensitive(petJSON, "status");
    openapi_petstore_pet_STATUS_e statusVariable;
    if (status) { 
    if(!cJSON_IsString(status))
    {
    goto end; //Enum
    }
    statusVariable = statuspet_FromString(status->valuestring);
    }


    pet_local_var = pet_create (
        id ? id->valuedouble : 0,
        category ? category_local_nonprim : NULL,
        strdup(name->valuestring),
        photo_urlsList,
        tags ? tagsList : NULL,
        status ? statusVariable : -1
        );

    return pet_local_var;
end:
    if (category_local_nonprim) {
        category_free(category_local_nonprim);
        category_local_nonprim = NULL;
    }
    if (photo_urlsList) {
        listEntry_t *listEntry = NULL;
        list_ForEach(listEntry, photo_urlsList) {
            free(listEntry->data);
            listEntry->data = NULL;
        }
        list_freeList(photo_urlsList);
        photo_urlsList = NULL;
    }
    if (tagsList) {
        listEntry_t *listEntry = NULL;
        list_ForEach(listEntry, tagsList) {
            tag_free(listEntry->data);
            listEntry->data = NULL;
        }
        list_freeList(tagsList);
        tagsList = NULL;
    }
    return NULL;

}
