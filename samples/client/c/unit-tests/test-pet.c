#include "pet.h"
#include "category.h"
#include <stdlib.h>
#include <string.h>

#define EXAMPLE_CATEGORY_NAME "Example Category"
#define EXAMPLE_PET_NAME "Example Pet"

int main() {
	char *categoryName = malloc(strlen(EXAMPLE_CATEGORY_NAME) + 1);
	strcpy(categoryName, EXAMPLE_CATEGORY_NAME);

	category_t *category = category_create(5, &categoryName);

	char *petName = malloc(strlen(EXAMPLE_PET_NAME) + 1);
	strcpy(petName, EXAMPLE_PET_NAME);

	list_t *photoUrls = list_create();
	list_t *tags = list_create();

	status_t status = sold;

	pet_t *pet =
		pet_create(1, category, &petName, photoUrls, tags, &status);
	free(pet_convertToJson(pet));

	free(categoryName);
	category_free(category);
	free(petName);
	pet_free(pet);
	list_free(photoUrls);
	list_free(tags);
}