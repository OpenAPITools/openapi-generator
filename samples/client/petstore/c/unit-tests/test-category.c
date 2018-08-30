#include <stdlib.h>
#include <string.h>
#include "category.h"

#define EXAMPLE_CATEGORY_NAME "Cats"
#define EXAMPLE_CATEGORY_ID 5

int main() {
	char *exampelCategoryName1 = malloc(strlen(EXAMPLE_CATEGORY_NAME) + 1);

	category_t *category = category_create(EXAMPLE_CATEGORY_ID,
	                                       exampelCategoryName1);
	category_free(category);
}