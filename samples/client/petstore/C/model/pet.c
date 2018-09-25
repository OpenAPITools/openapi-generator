#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include "cJSON.h"
#include "pet.h"
#include "category.h"
#include "list.h"
#include "tag.h"


pet_t *pet_create(long id, category_t *category, char *name, list_t *photoUrls, list_t *tags, char *status) {
	pet_t *pet = malloc(sizeof(pet_t));
	pet->id = id;
	pet->category = category;
	pet->name = name;
	pet->photoUrls = photoUrls;
	pet->tags = tags;
	pet->status = status;

	return pet;
}


void pet_free(pet_t *pet) {
	// free(pet->id);
	category_free(pet->category);
	// free(pet->name);
	list_free(pet->photoUrls);
	list_free(pet->tags);
	// free(pet->status);

	free(pet);
}

cJSON *pet_convertToJSON(pet_t *pet) {
	cJSON *item = cJSON_CreateObject();
	// pet->id
	if(cJSON_AddNumberToObject(item, "id", pet->id) == NULL) {
		goto fail; // Numeric
	}

	// pet->category
	cJSON *category = category_convertToJSON(pet->category);
	if(category == NULL) {
		goto fail; // nonprimitive
	}
	cJSON_AddItemToObject(item, "category", category);
	if(item->child == NULL) {
		goto fail;
	}

	// pet->name
	if(cJSON_AddStringToObject(item, "name", pet->name) == NULL) {
		goto fail; // String
	}

	// pet->photoUrls
	cJSON *photoUrls = cJSON_AddArrayToObject(item, "photoUrls");
	if(photoUrls == NULL) {
		goto fail; // primitive container
	}

	listEntry_t *photoUrlsListEntry;
	list_ForEach(photoUrlsListEntry, pet->photoUrls) {
		if(cJSON_AddStringToObject(photoUrls, "", photoUrlsListEntry->data) == NULL) {
			goto fail;
		}
	}

	// pet->tags
	cJSON *tags = cJSON_AddArrayToObject(item, "tags");
	if(tags == NULL) {
		goto fail; // nonprimitive container
	}

	listEntry_t *tagsListEntry;
	list_ForEach(tagsListEntry, pet->tags) {
		cJSON *item = tag_convertToJSON(tagsListEntry->data);
		if(item == NULL) {
			goto fail;
		}
		cJSON_AddItemToArray(tags, item);
	}

	// pet->status
	if(cJSON_AddStringToObject(item, "status", pet->status) == NULL) {
		goto fail; // String
	}

	return item;
fail:
	cJSON_Delete(item);
	return NULL;
}

pet_t *pet_parseFromJSON(char *jsonString) {
	pet_t *pet = NULL;
	cJSON *petJSON = cJSON_Parse(jsonString);
	if(petJSON == NULL) {
		const char *error_ptr = cJSON_GetErrorPtr();
		if(error_ptr != NULL) {
			fprintf(stderr, "Error Before: %s\n", error_ptr);
			goto end;
		}
	}

	// pet->id
	cJSON *id = cJSON_GetObjectItemCaseSensitive(petJSON, "id");
	if(!cJSON_IsNumber(id)) {
		goto end; // Numeric
	}

	// pet->category
	category_t *category;
	cJSON *categoryJSON = cJSON_GetObjectItemCaseSensitive(petJSON, "category");
	if(petJSON == NULL) {
		const char *error_ptr = cJSON_GetErrorPtr();
		if(error_ptr != NULL) {
			fprintf(stderr, "Error Before: %s\n", error_ptr);
		}
		goto end; // nonprimitive
	}
	char *JSONData = cJSON_Print(categoryJSON);
	category = category_parseFromJSON(JSONData);

	// pet->name
	cJSON *name = cJSON_GetObjectItemCaseSensitive(petJSON, "name");
	if(!cJSON_IsString(name) ||
	   (name->valuestring == NULL))
	{
		goto end; // String
	}

	// pet->photoUrls
	cJSON *photoUrls;
	cJSON *photoUrlsJSON = cJSON_GetObjectItemCaseSensitive(petJSON, "photoUrls");
	if(!cJSON_IsArray(photoUrlsJSON)) {
		goto end; // primitive container
	}
	list_t *photoUrlsList = list_create();

	cJSON_ArrayForEach(photoUrls, photoUrlsJSON)
	{
		if(!cJSON_IsString(photoUrls) ||
		   (photoUrls->valuestring == NULL))
		{
			goto end;
		}
		list_addElement(photoUrlsList, strdup(photoUrls->valuestring));
	}

	// pet->tags
	cJSON *tags;
	cJSON *tagsJSON = cJSON_GetObjectItemCaseSensitive(petJSON, "tags");
	if(!cJSON_IsArray(tagsJSON)) {
		goto end; // nonprimitive container
	}

	list_t *tagsList = list_create();

	cJSON_ArrayForEach(tags, tagsJSON)
	{
		if(!cJSON_IsObject(tags)) {
			goto end;
		}
		char *JSONData = cJSON_Print(tags);
		tag_t *tagsItem = tag_parseFromJSON(JSONData);

		list_addElement(tagsList, tagsItem);
	}

	// pet->status
	cJSON *status = cJSON_GetObjectItemCaseSensitive(petJSON, "status");
	if(!cJSON_IsString(status) ||
	   (status->valuestring == NULL))
	{
		goto end; // String
	}


	pet = pet_create(
		id->valuedouble,
		category,
		strdup(name->valuestring),
		photoUrlsList,
		tagsList,
		strdup(status->valuestring)
		);

	return pet;
end:
	cJSON_Delete(petJSON);
	return NULL;
}
