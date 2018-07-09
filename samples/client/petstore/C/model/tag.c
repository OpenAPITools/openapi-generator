#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include "cJSON.h"


tag_t *tag_create(
		long		*id,
		char		*name
		) {
	tag_t *tag = malloc(sizeof(tag_t));
	tag->id = id;
	tag->name = name;

	return tag;
}


void tag_free(tag_t *tag) {
	listEntry_t *listEntry;

	free(tag->id)

	free(tag->name)

	free(tag);
}
