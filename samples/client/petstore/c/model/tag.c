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