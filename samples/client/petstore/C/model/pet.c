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
	listEntry_t *listEntry;

	free(pet->id)

	category_free(pet->category)

	free(pet->name)

	list_ForEach(listEntry, pet->photoUrls) {
		free(listEntry->data);
	}
	list_free(pet->photoUrls);

	list_ForEach(listEntry, pet->tags) {
		tags_free(listEntry->data);
	}
	list_free(pet->tags);

	free(pet->status)

	free(pet);
}
