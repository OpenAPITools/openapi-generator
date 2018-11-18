#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include "cJSON.h"
#include "list.h"
#include "keyValuePair.h"
#include "tag.h"


tag_t *tag_create(long id, char *name) {
	tag_t *tag = malloc(sizeof(tag_t));
	tag->id = id;
	tag->name = name;

	return tag;
}


void tag_free(tag_t *tag) {
	listEntry_t *listEntry;
	free(tag->name);

	free(tag);
}

cJSON *tag_convertToJSON(tag_t *tag) {
	cJSON *item = cJSON_CreateObject();
	// tag->id
	if(cJSON_AddNumberToObject(item, "id", tag->id) == NULL) {
		goto fail; // Numeric
	}

	// tag->name
	if(cJSON_AddStringToObject(item, "name", tag->name) == NULL) {
		goto fail; // String
	}

	return item;
fail:
	cJSON_Delete(item);
	return NULL;
}

tag_t *tag_parseFromJSON(char *jsonString) {
	tag_t *tag = NULL;
	cJSON *tagJSON = cJSON_Parse(jsonString);
	if(tagJSON == NULL) {
		const char *error_ptr = cJSON_GetErrorPtr();
		if(error_ptr != NULL) {
			fprintf(stderr, "Error Before: %s\n", error_ptr);
			goto end;
		}
	}

	// tag->id
	cJSON *id = cJSON_GetObjectItemCaseSensitive(tagJSON, "id");
	if(!cJSON_IsNumber(id)) {
		goto end; // Numeric
	}

	// tag->name
	cJSON *name = cJSON_GetObjectItemCaseSensitive(tagJSON, "name");
	if(!cJSON_IsString(name) ||
	   (name->valuestring == NULL))
	{
		goto end; // String
	}


	tag = tag_create(
		id->valuedouble,
		strdup(name->valuestring)
		);
	cJSON_Delete(tagJSON);
	return tag;
end:
	cJSON_Delete(tagJSON);
	return NULL;
}
