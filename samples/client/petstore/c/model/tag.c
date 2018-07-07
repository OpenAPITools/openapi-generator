#include <stdlib.h>
#include "tag.h"

tag_t *tag_create(long id, char *name) {
	tag_t *tag = malloc(sizeof(tag_t));
	tag->id = id;
	tag->name = name;
	return tag;
}

void tag_free(tag_t *tag) {
	free(tag->name);
	free(tag);
}

cJSON *tag_convertToJSON(tag_t *tag) {
	cJSON *item = cJSON_CreateObject();
	if(cJSON_AddNumberToObject(item, "id", tag->id) == NULL) {
		goto fail;
	}
	if(cJSON_AddStringToObject(item, "name", tag->name) == NULL) {
		goto fail;
	}

	return item;
fail:
	cJSON_Delete(item);
	return NULL;
}