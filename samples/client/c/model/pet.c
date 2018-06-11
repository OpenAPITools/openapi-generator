#include <stdlib.h>
#include "pet.h"
#include "cJSON.h"
#include <stdio.h>

pet_t *pet_create(long		id,
                  category_t	*category,
                  char		**name,
                  list_t	*photoUrls,
                  list_t	*tags,
                  status_t	*status) {
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
	return free(pet);
}

char *pet_convertToJson(pet_t *pet) {
	char *string;

	cJSON *petJSONObject = cJSON_CreateObject();

	// Pet->id
	if(cJSON_AddNumberToObject(petJSONObject, "id", pet->id) == NULL) {
		goto end;
	}
	// Pet->category
	if(cJSON_AddObjectToObject(petJSONObject, "category") == NULL) {
		goto end;
	}

	cJSON *categoryJSONObject = cJSON_GetObjectItem(petJSONObject,
	                                                "category");

	// Category->id
	if(cJSON_AddNumberToObject(categoryJSONObject, "id",
	                           pet->category->id) == NULL)
	{
		goto end;
	}
	// Category->name
	if(cJSON_AddStringToObject(categoryJSONObject, "name",
	                           *pet->category->name) == NULL)
	{
		goto end;
	}

	// Pet->name
	if(cJSON_AddStringToObject(petJSONObject, "name", *pet->name) == NULL) {
		goto end;
	}

	// Pet->photoUrls
	cJSON *photoUrls = cJSON_AddArrayToObject(petJSONObject, "photoUrls");

	if(photoUrls == NULL) {
		goto end;
	}

	list_iterateThroughListForward(pet->photoUrls,
	                               listEntry_addAsItemToJSONArray,
	                               photoUrls);

	// Pet->tags
	cJSON *tags = cJSON_AddArrayToObject(petJSONObject, "tags");

	if(tags == NULL) {
		goto end;
	}

	list_iterateThroughListForward(pet->tags,
	                               listEntry_addAsItemToJSONArray,
	                               tags);

	// Pet->status
	cJSON_AddNumberToObject(petJSONObject, "status", *pet->status);

	// Outputs the assembles JSON string into a variable
	string = cJSON_Print(petJSONObject);
	if(string == NULL) {
		fprintf(stderr, "Failed to print petJSONObject.\n");
	}

end:
	// frees memory
	cJSON_Delete(petJSONObject);
	// actually prints the JSON string
	puts(string);
	return string;
}