#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include "pet.h"
#include "cJSON.h"
#include "tag.h"
#include "category.h"

char *status_ToString(status_t status) {
	switch(status) {
	case 0:
		return "available";

	case 1:
		return "pending";

	default:
		return "sold";
	}
}

status_t status_FromString(char *status) {
	if(strcmp(status, "available") == 0) {
		return 0;
	} else if(strcmp(status, "pending") == 0) {
		return 1;
	} else {
		return 2;
	}
}

pet_t *pet_create(long		id,
                  category_t	*category,
                  char		*name,
                  list_t	*photoUrls,
                  list_t	*tags,
                  status_t	status) {
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

pet_t *pet_parseFromJSON(char *jsonString) {
	pet_t *pet = NULL;
	cJSON *petId;
	cJSON *categoryJSON;
	cJSON *categoryId;
	cJSON *categoryName;
	cJSON *petName;
	cJSON *petPhotoUrls;
	cJSON *petPhotoUrl;
	cJSON *tags;
	cJSON *tag;
	cJSON *tagId;
	cJSON *tagName;
	cJSON *petStatus;

	// Pet
	cJSON *petJSON = cJSON_Parse(jsonString);
	if(petJSON == NULL) {
		const char *error_ptr = cJSON_GetErrorPtr();
		if(error_ptr != NULL) {
			fprintf(stderr, "Error before: %s\n", error_ptr);
		}
		goto end;
	}

	// Pet->id
	petId = cJSON_GetObjectItemCaseSensitive(petJSON, "id");
	if(!cJSON_IsNumber(petId)) {
		goto end;
	}

	// Pet->category
	category_t *category;
	categoryJSON = cJSON_GetObjectItemCaseSensitive(petJSON, "category");
	if(categoryJSON == NULL) {
		const char *error_ptr = cJSON_GetErrorPtr();
		if(error_ptr != NULL) {
			fprintf(stderr, "Error before: %s\n", error_ptr);
		}
		goto end;
	}

	// Category->id
	categoryId = cJSON_GetObjectItemCaseSensitive(categoryJSON, "id");
	if(!cJSON_IsNumber(categoryId)) {
		goto end;
	}

	// Category->name
	categoryName = cJSON_GetObjectItemCaseSensitive(categoryJSON, "name");
	if(!cJSON_IsString(categoryName) ||
	   (categoryName->valuestring == NULL) )
	{
		goto end;
	}

	category = category_create(categoryId->valuedouble,
	                           strdup(categoryName->valuestring));

	// Pet->name
	petName = cJSON_GetObjectItemCaseSensitive(petJSON, "name");
	if(!cJSON_IsString(petName) ||
	   (petName->valuestring == NULL) )
	{
		goto end;
	}

	// Pet->photoUrls
	petPhotoUrls = cJSON_GetObjectItemCaseSensitive(petJSON, "photoUrls");
	if(!cJSON_IsArray(petPhotoUrls)) {
		goto end;
	}

	list_t *photoUrlList = list_create();

	cJSON_ArrayForEach(petPhotoUrl, petPhotoUrls)
	{
		if(!cJSON_IsString(petPhotoUrl) ||
		   (petPhotoUrl->valuestring == NULL) )
		{
			goto end;
		}

		list_addElement(photoUrlList, strdup(petPhotoUrl->valuestring));
	}

	// Pet->tags
	tags = cJSON_GetObjectItemCaseSensitive(petJSON, "tags");
	if(!cJSON_IsArray(tags)) {
		goto end;
	}

	list_t *tagList = list_create();

	cJSON_ArrayForEach(tag, tags)
	{
		if(!cJSON_IsObject(tag)) {
			goto end;
		}

		tagId = cJSON_GetObjectItemCaseSensitive(tag, "id");
		if(!cJSON_IsNumber(tagId)) {
			goto end;
		}
		tagName = cJSON_GetObjectItemCaseSensitive(tag, "name");
		if(!cJSON_IsString(tagName) ||
		   (tagName->valuestring == NULL) )
		{
			goto end;
		}
		tag_t *tag =
			tag_create(tagId->valuedouble,
			           strdup(tagName->valuestring));
		list_addElement(tagList, tag);
	}

	// Pet->status
	status_t status;
	petStatus = cJSON_GetObjectItemCaseSensitive(petJSON, "status");
	if(!cJSON_IsString(petStatus) ||
	   (petStatus->valuestring == NULL) )
	{
		goto end;
	}

	status = status_FromString(petStatus->valuestring);

	pet = pet_create(petId->valuedouble,
	                 category,
	                 strdup(petName->valuestring),
	                 photoUrlList,
	                 tagList,
	                 status);

end:
	cJSON_Delete(petJSON);
	return pet;
}

cJSON *pet_convertToJSON(pet_t *pet) {
	listEntry_t *listEntry;

	cJSON *petJSONObject = cJSON_CreateObject();

	// Pet->id
	if(cJSON_AddNumberToObject(petJSONObject, "id", pet->id) == NULL) {
		goto fail;
	}

	cJSON *category = category_convertToJSON(pet->category);

	if(category == NULL) {
		goto fail;
	}

	// Pet->category
	cJSON_AddItemToObject(petJSONObject, "category", category);
	if(petJSONObject->child == NULL) {
		goto fail;
	}

	// Pet->name
	if(cJSON_AddStringToObject(petJSONObject, "name", pet->name) == NULL) {
		goto fail;
	}

	// Pet->photoUrls
	cJSON *photoUrls = cJSON_AddArrayToObject(petJSONObject, "photoUrls");

	if(photoUrls == NULL) {
		goto fail;
	}

	list_ForEach(listEntry, pet->photoUrls) {
		if(cJSON_AddStringToObject(photoUrls, "",
		                           listEntry->data) == NULL)
		{
			goto fail;
		}
	}

	// Pet->tags
	cJSON *tags = cJSON_AddArrayToObject(petJSONObject, "tags");

	if(tags == NULL) {
		goto fail;
	}

	list_ForEach(listEntry, pet->tags) {
		cJSON *item = tag_convertToJSON(listEntry->data);
		if(item == NULL) {
			goto fail;
		}
		cJSON_AddItemToArray(tags, item);
	}

	// Pet->status
	cJSON_AddStringToObject(petJSONObject, "status",
	                        status_ToString(pet->status));

	return petJSONObject;

fail:
	// frees memory
	cJSON_Delete(petJSONObject);
	return NULL;
}