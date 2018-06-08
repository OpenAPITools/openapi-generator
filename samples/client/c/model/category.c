#include <stdlib.h>
#include "category.h"

category_t *category_create(long id, char **name) {
	category_t *category = malloc(sizeof(category_t));
	category->id = id;
	category->name = name;

	return category;
}

void category_free(category_t *category) {
	return free(category);
}