#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include "cJSON.h"
#include "list.h"
#include "keyValuePair.h"
#include "category.h"


category_t *category_create(long id, char *name) {
	category_t *category = malloc(sizeof(category_t));
	category->id = id;
	category->name = name;

	return category;
}


void category_free(category_t *category) {
	listEntry_t *listEntry;
	free(category->name);

	free(category);
}

cJSON *category_convertToJSON(category_t *category) {
	cJSON *item = cJSON_CreateObject();
	// category->id
	if(cJSON_AddNumberToObject(item, "id", category->id) == NULL) {
		goto fail; // Numeric
	}

	// category->name
	if(cJSON_AddStringToObject(item, "name", category->name) == NULL) {
		goto fail; // String
	}

	return item;
fail:
	cJSON_Delete(item);
	return NULL;
}

category_t *category_parseFromJSON(char *jsonString) {
	category_t *category = NULL;
	cJSON *categoryJSON = cJSON_Parse(jsonString);
	if(categoryJSON == NULL) {
		const char *error_ptr = cJSON_GetErrorPtr();
		if(error_ptr != NULL) {
			fprintf(stderr, "Error Before: %s\n", error_ptr);
			goto end;
		}
	}

	// category->id
	cJSON *id = cJSON_GetObjectItemCaseSensitive(categoryJSON, "id");
	if(!cJSON_IsNumber(id)) {
		goto end; // Numeric
	}

	// category->name
	cJSON *name = cJSON_GetObjectItemCaseSensitive(categoryJSON, "name");
	if(!cJSON_IsString(name) ||
	   (name->valuestring == NULL))
	{
		goto end; // String
	}


	category = category_create(
		id->valuedouble,
		strdup(name->valuestring)
		);
	cJSON_Delete(categoryJSON);
	return category;
end:
	cJSON_Delete(categoryJSON);
	return NULL;
}
