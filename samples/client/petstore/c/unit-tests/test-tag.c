#include <stdlib.h>
#include <string.h>
#include "tag.h"

#define EXAMPLE_ID_1 1
#define EXAMPLE_TAG_NAME_1 "example tag name"


int main() {
	long id = EXAMPLE_ID_1;
	char *exampleTagName1 = malloc(strlen(EXAMPLE_TAG_NAME_1) + 1);
	strcpy(exampleTagName1, EXAMPLE_TAG_NAME_1);

	tag_t *tag = tag_create(id, exampleTagName1);

	tag_free(tag);
}