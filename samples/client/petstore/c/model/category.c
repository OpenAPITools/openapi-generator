#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include "category.h"



static category_t *category_create_internal(
    long *id,
    char *name
    ) {
    category_t *category_local_var = malloc(sizeof(category_t));
    if (!category_local_var) {
        return NULL;
    }
    memset(category_local_var, 0, sizeof(category_t));
    category_local_var->_library_owned = 1;
    category_local_var->id = id;
    category_local_var->name = name;
    return category_local_var;
}

__attribute__((deprecated)) category_t *category_create(
    long *id,
    char *name
    ) {
    long *id_copy = NULL;
    if (id) {
        id_copy = malloc(sizeof(long));
        if (id_copy) *id_copy = *id;
    }
    return category_create_internal (
        id_copy,
        name
        );
}

void category_free(category_t *category) {
    if(NULL == category){
        return ;
    }
    if(category->_library_owned != 1){
        fprintf(stderr, "WARNING: %s() does NOT free objects allocated by the user\n", "category_free");
        return ;
    }
    listEntry_t *listEntry;
    if (category->id) {
        free(category->id);
        category->id = NULL;
    }
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
    if(cJSON_AddNumberToObject(item, "id", *category->id) == NULL) {
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

    // define the local variable for category->id
    long *id_local_var = NULL;

    // category->id
    cJSON *id = cJSON_GetObjectItemCaseSensitive(categoryJSON, "id");
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

    // category->name
    cJSON *name = cJSON_GetObjectItemCaseSensitive(categoryJSON, "name");
    if (cJSON_IsNull(name)) {
        name = NULL;
    }
    if (name) { 
    if(!cJSON_IsString(name) && !cJSON_IsNull(name))
    {
    goto end; //String
    }
    }


    category_local_var = category_create_internal (
        id_local_var,
        name && !cJSON_IsNull(name) ? strdup(name->valuestring) : NULL
        );

    return category_local_var;
end:
    if (id_local_var) {
        free(id_local_var);
        id_local_var = NULL;
    }
    return NULL;

}
