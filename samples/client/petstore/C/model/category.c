#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include "cJSON.h"


category_t *category_create(
		long		*id,
		char		*name
		) {
	category_t *category = malloc(sizeof(category_t));
	category->id = id;
	category->name = name;

	return category;
}


void category_free(category_t *category) {

	free(category->name)

	free(category);
}

cJSON *category_convertToJSON(category_t *category) {
	cJSON *item = cJSON_CreateObject();
	// category->id
	if(cJSON_AddNumberToObject(item, "id", category->id) == NULL) {
    	goto fail;
    }


	// category->name
	if(cJSON_AddStringToObject(item, "name", category->name) == NULL) {
		goto fail;
	}

	return item;
fail:
	cJSON_Delete(item);
	return NULL;
}
