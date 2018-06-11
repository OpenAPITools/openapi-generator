#include "pet.h"
#include "category.h"
#include <stdlib.h>
#include <string.h>

#define EXAMPLE_CATEGORY_NAME "Example Category"
#define EXAMPLE_PET_NAME "Example Pet"
#define EXAMPLE_URL_1 "http://www.github.com"
#define EXAMPLE_URL_2 "http://www.gitter.im"
#define EXAMPLE_TAG_1 "beautiful code"
#define EXAMPLE_TAG_2 "at least I tried"

int main() {
	char *categoryName = malloc(strlen(EXAMPLE_CATEGORY_NAME) + 1);
	strcpy(categoryName, EXAMPLE_CATEGORY_NAME);

	category_t *category = category_create(5, &categoryName);

	char *petName = malloc(strlen(EXAMPLE_PET_NAME) + 1);
	strcpy(petName, EXAMPLE_PET_NAME);

	char *exampleUrl1 = malloc(strlen(EXAMPLE_URL_1) + 1);
	strcpy(exampleUrl1, EXAMPLE_URL_1);

	char *exampleUrl2 = malloc(strlen(EXAMPLE_URL_2) + 1);
	strcpy(exampleUrl2, EXAMPLE_URL_2);

	list_t *photoUrls = list_create();

	list_addElement(photoUrls, exampleUrl1);
	list_addElement(photoUrls, exampleUrl2);

	char *exampleTag1 = malloc(strlen(EXAMPLE_TAG_1) + 1);
	strcpy(exampleTag1, EXAMPLE_TAG_1);

	char *exampleTag2 = malloc(strlen(EXAMPLE_TAG_2) + 1);
	strcpy(exampleTag2, EXAMPLE_TAG_2);

	list_t *tags = list_create();

	list_addElement(tags, exampleTag1);
	list_addElement(tags, exampleTag2);

	status_t status = sold;

	pet_t *pet =
		pet_create(1, category, &petName, photoUrls, tags, &status);
	free(pet_convertToJson(pet));

	free(categoryName);
	category_free(category);

	free(petName);
	pet_free(pet);
	free(exampleUrl1);
	free(exampleUrl2);
	free(exampleTag1);
	free(exampleTag2);
	list_free(photoUrls);

	list_free(tags);
}