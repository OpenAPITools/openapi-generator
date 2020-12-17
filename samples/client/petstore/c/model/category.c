#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include "category.h"



category_t *category_create(
    long id,
    char *name
    ) {
    category_t *category_local_var = malloc(sizeof(category_t));
    if (!category_local_var) {
        return NULL;
    }
    category_local_var->id = id;
    category_local_var->name = name;

    return category_local_var;
}


void category_free(category_t *category) {
    if(NULL == category){
        return ;
    }
    listEntry_t *listEntry;
    if (category->name) {
        free(category->name);
        category->name = NULL;
    }
    free(category);
}

cJSON *category_convertToJSON(category_t *category) {
    cJSON *item = cJSON_CreateObject();

    // category->id
    if(category->id) { 
    if(cJSON_AddNumberToObject(item, "id", category->id) == NULL) {
    goto fail; //Numeric
    }
     } 


    // category->name
    if(category->name) { 
    if(cJSON_AddStringToObject(item, "name", category->name) == NULL) {
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

category_t *category_parseFromJSON(cJSON *categoryJSON){

    category_t *category_local_var = NULL;

    // category->id
    cJSON *id = cJSON_GetObjectItemCaseSensitive(categoryJSON, "id");
    if (id) { 
    if(!cJSON_IsNumber(id))
    {
    goto end; //Numeric
    }
    }

    // category->name
    cJSON *name = cJSON_GetObjectItemCaseSensitive(categoryJSON, "name");
    if (name) { 
    if(!cJSON_IsString(name))
    {
    goto end; //String
    }
    }


    category_local_var = category_create (
        id ? id->valuedouble : 0,
        name ? strdup(name->valuestring) : NULL
        );

    return category_local_var;
end:
    return NULL;

}
