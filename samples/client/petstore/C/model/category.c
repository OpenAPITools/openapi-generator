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
	listEntry_t *listEntry;

	free(category->id)

	free(category->name)

	free(category);
}
