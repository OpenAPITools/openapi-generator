#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include "cJSON.h"
#include "list.h"
#include "keyValuePair.h"
#include "pet.h"
#include "category.h"
#include "list.h"
#include "tag.h"

char *statuspet_ToString(status_e status) {
	char *statusArray[] = { "available", "pending", "sold" };
	return statusArray[status];
}

status_e statuspet_FromString(char *status) {
	int stringToReturn = 0;
	char *statusArray[] = { "available", "pending", "sold" };
	size_t sizeofArray = sizeof(statusArray) / sizeof(statusArray[0]);
	while(stringToReturn < sizeofArray) {
		if(strcmp(status, statusArray[stringToReturn]) == 0) {
			return stringToReturn;
		}
		stringToReturn++;
	}
}

pet_t *pet_create(long id, category_t *category, char *name, list_t *photoUrls,
                  list_t *tags, status_e status) {
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
	listEntry_t *listEntry;
	category_free(pet->category);
	free(pet->name);
	list_ForEach(listEntry, pet->photoUrls) {
		free(listEntry->data);
	}
	list_free(pet->photoUrls);
	list_ForEach(listEntry, pet->tags) {
		tag_free(listEntry->data);
	}
	list_free(pet->tags);

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
	cJSON *photo_urls = cJSON_AddArrayToObject(item, "photoUrls");
	if(photo_urls == NULL) {
		goto fail; // primitive container
	}

	listEntry_t *photo_urlsListEntry;
	list_ForEach(photo_urlsListEntry, pet->photoUrls) {
		if(cJSON_AddStringToObject(photo_urls, "",
		                           (char *) photo_urlsListEntry->data)
		   == NULL)
		{
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
	if(cJSON_AddStringToObject(item, "status",
	                           statuspet_ToString(pet->status)) == NULL)
	{
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
	cJSON *categoryJSON = cJSON_GetObjectItemCaseSensitive(petJSON,
	                                                       "category");
	if(petJSON == NULL) {
		const char *error_ptr = cJSON_GetErrorPtr();
		if(error_ptr != NULL) {
			fprintf(stderr, "Error Before: %s\n", error_ptr);
		}
		goto end; // nonprimitive
	}
	char *categoryJSONData = cJSON_Print(categoryJSON);
	category = category_parseFromJSON(categoryJSONData);

	// pet->name
	cJSON *name = cJSON_GetObjectItemCaseSensitive(petJSON, "name");
	if(!cJSON_IsString(name) ||
	   (name->valuestring == NULL))
	{
		goto end; // String
	}

	// pet->photoUrls
	cJSON *photo_urls;
	cJSON *photoUrlsJSON = cJSON_GetObjectItemCaseSensitive(petJSON,
	                                                        "photoUrls");
	if(!cJSON_IsArray(photoUrlsJSON)) {
		goto end; // primitive container
	}
	list_t *photo_urlsList = list_create();

	cJSON_ArrayForEach(photo_urls, photoUrlsJSON)
	{
		if(!cJSON_IsString(photo_urls)) {
			goto end;
		}
		list_addElement(photo_urlsList,
		                strdup(photo_urls->valuestring));
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
		free(JSONData);
	}

	// pet->status
	status_e statusVariable;
	cJSON *status = cJSON_GetObjectItemCaseSensitive(petJSON, "status");
	if(!cJSON_IsString(status) ||
	   (status->valuestring == NULL))
	{
		goto end; // String
	}

	statusVariable = statuspet_FromString(status->valuestring);


	pet = pet_create(
		id->valuedouble,
		category,
		strdup(name->valuestring),
		photo_urlsList,
		tagsList,
		statusVariable
		);
	free(categoryJSONData);
	cJSON_Delete(petJSON);
	return pet;
end:
	cJSON_Delete(petJSON);
	return NULL;
}
