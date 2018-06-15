#include "pet.h"
#include "category.h"
#include "tag.h"
#include <stdlib.h>
#include <string.h>

#define EXAMPLE_CATEGORY_NAME "Example Category"
#define EXAMPLE_PET_NAME "Example Pet"
#define EXAMPLE_URL_1 "http://www.github.com"
#define EXAMPLE_URL_2 "http://www.gitter.im"
#define EXAMPLE_TAG_1_NAME "beautiful code"
#define EXAMPLE_TAG_2_NAME "at least I tried"
#define EXAMPLE_TAG_1_ID 1
#define EXAMPLE_TAG_2_ID 542353

int main() {
	char *categoryName = malloc(strlen(EXAMPLE_CATEGORY_NAME) + 1);
	strcpy(categoryName, EXAMPLE_CATEGORY_NAME);

	category_t *category = category_create(5, categoryName);

	char *petName = malloc(strlen(EXAMPLE_PET_NAME) + 1);
	strcpy(petName, EXAMPLE_PET_NAME);

	char *exampleUrl1 = malloc(strlen(EXAMPLE_URL_1) + 1);
	strcpy(exampleUrl1, EXAMPLE_URL_1);

	char *exampleUrl2 = malloc(strlen(EXAMPLE_URL_2) + 1);
	strcpy(exampleUrl2, EXAMPLE_URL_2);

	list_t *photoUrls = list_create();

	list_addElement(photoUrls, exampleUrl1);
	list_addElement(photoUrls, exampleUrl2);

	char *exampleTag1Name = malloc(strlen(EXAMPLE_TAG_1_NAME) + 1);
	strcpy(exampleTag1Name, EXAMPLE_TAG_1_NAME);
	tag_t *exampleTag1 = tag_create(EXAMPLE_TAG_1_ID, exampleTag1Name);

	char *exampleTag2Name = malloc(strlen(EXAMPLE_TAG_2_NAME) + 1);
	strcpy(exampleTag2Name, EXAMPLE_TAG_2_NAME);
	tag_t *exampleTag2 = tag_create(EXAMPLE_TAG_2_ID, exampleTag2Name);

	list_t *tags = list_create();

	list_addElement(tags, exampleTag1);
	list_addElement(tags, exampleTag2);

	status_t status = sold;

	pet_t *pet =
		pet_create(1, category, petName, photoUrls, tags, status);
	cJSON_Delete(pet_convertToJSON(pet));

	pet_free(pet);
}