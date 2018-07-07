#include <stdlib.h>
#include "category.h"
#include "cJSON.h"

category_t *category_create(long id, char *name) {
	category_t *category = malloc(sizeof(category_t));
	category->id = id;
	category->name = name;

	return category;
}

void category_free(category_t *category) {
	free(category->name);
	free(category);
}

cJSON *category_convertToJSON(category_t *category) {
	cJSON *categoryJSONObject = cJSON_CreateObject();

	// Category->id
	if(cJSON_AddNumberToObject(categoryJSONObject, "id",
	                           category->id) == NULL)
	{
		goto fail;
	}
	// Category->name
	if(cJSON_AddStringToObject(categoryJSONObject, "name",
	                           category->name) == NULL)
	{
		goto fail;
	}

	return categoryJSONObject;

fail:
	cJSON_Delete(categoryJSONObject);
	return NULL;
}