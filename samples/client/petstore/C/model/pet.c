#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include "cJSON.h"
#include "arrayh"
#include "categoryh"
#include "tagh"


pet_t *pet_create(
		long		*id,
		category		*category,
		char		*name,
		array		*photoUrls,
		array		*tags,
		char		*status
		) {
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

	category_free(pet->category)

	free(pet->name)

	listEntry_t *photoUrlsListEntry;
	list_ForEach(photoUrlsListEntry, pet->photoUrls) {
		free(photoUrlsListEntry->data);
	}
	list_free(pet->photoUrls);

	list_ForEach(tagsListEntry, pet->tags) {
		tags_free(tagsListEntry->data);
	}
	list_free(pet->tags);

	free(pet->status)

	free(pet);
}

cJSON *pet_convertToJSON(pet_t *pet) {
	cJSON *item = cJSON_CreateObject();
	// pet->id
	if(cJSON_AddNumberToObject(item, "id", pet->id) == NULL) {
    	goto fail;
    }


	// pet->category
	if(cJSON_AddStringToObject(item, "category", pet->category) == NULL) {
		goto fail;
	}

	// pet->name
	if(cJSON_AddStringToObject(item, "name", pet->name) == NULL) {
		goto fail;
	}

	// pet->photoUrls
	list_ForEach(listEntry, pet->photoUrls) {
		free(listEntry->data);
	}

	cJSON *photoUrls = cJSON_AddArrayToObject(item, "photoUrls");
	if(photoUrls == NULL) {
		goto fail;
	}

	listEntry_t *photoUrlsListEntry;
    list_ForEach(photoUrlsListEntry, pet->photoUrls) {
        if(cJSON_AddStringToObject(photoUrls, "", photoUrlsListEntry->data) == NULL)
        {
            goto fail;
        }
    }

	// pet->tags
	cJSON *tags = cJSON_AddArrayToObject(item, "tags");
	if(tags == NULL) {
		goto fail;
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
		goto fail;
	}

	return item;
fail:
	cJSON_Delete(item);
	return NULL;
}
