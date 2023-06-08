#include <stdio.h>
    #include <stdlib.h>
    #include <string.h>
    #include <assert.h>
    #include "../api/PetAPI.h"


    #define EXAMPLE_CATEGORY_NAME "Example Category"
    #define EXAMPLE_CATEGORY_ID 5
    #define EXAMPLE_PET_NAME "Example Pet"
    #define EXAMPLE_URL_1 "http://www.github.com"
    #define EXAMPLE_URL_2 "http://www.gitter.im"
    #define EXAMPLE_TAG_1_NAME "beautiful code"
    #define EXAMPLE_TAG_2_NAME "at least I tried"
    #define EXAMPLE_TAG_1_ID 1
    #define EXAMPLE_TAG_2_ID 542353
    #define EXAMPLE_PET_ID 1234 // Set to 0 to generate a new pet


int main() {
// Add pet test
	apiClient_t *apiClient = apiClient_create();

	char *categoryName = malloc(strlen(EXAMPLE_CATEGORY_NAME) + 1);
	strcpy(categoryName, EXAMPLE_CATEGORY_NAME);

	category_t *category =
		category_create(EXAMPLE_CATEGORY_ID, categoryName);

	char *petName = malloc(strlen(EXAMPLE_PET_NAME) + 1);
	strcpy(petName, EXAMPLE_PET_NAME);

	char *exampleUrl1 = malloc(strlen(EXAMPLE_URL_1) + 1);
	strcpy(exampleUrl1, EXAMPLE_URL_1);

	char *exampleUrl2 = malloc(strlen(EXAMPLE_URL_2) + 1);
	strcpy(exampleUrl2, EXAMPLE_URL_2);

	list_t *photoUrls = list_createList();

	list_addElement(photoUrls, exampleUrl1);
	list_addElement(photoUrls, exampleUrl2);

	char *exampleTag1Name = malloc(strlen(EXAMPLE_TAG_1_NAME) + 1);
	strcpy(exampleTag1Name, EXAMPLE_TAG_1_NAME);
	tag_t *exampleTag1 = tag_create(EXAMPLE_TAG_1_ID, exampleTag1Name);

	char *exampleTag2Name = malloc(strlen(EXAMPLE_TAG_2_NAME) + 1);
	strcpy(exampleTag2Name, EXAMPLE_TAG_2_NAME);
	tag_t *exampleTag2 = tag_create(EXAMPLE_TAG_2_ID, exampleTag2Name);

	list_t *tags = list_createList();

	list_addElement(tags, exampleTag1);
	list_addElement(tags, exampleTag2);


	status_e status = available;
	pet_t *pet =
		pet_create(EXAMPLE_PET_ID,
		           category,
		           petName,
		           photoUrls,
		           tags,
		           status);

	PetAPI_addPet(apiClient, pet);
	cJSON *JSONR_local = pet_convertToJSON(pet);
	char *toPrint = cJSON_Print(JSONR_local);
	printf("Data is:%s\n", toPrint);
	free(toPrint);
	pet_free(pet);
	cJSON_Delete(JSONR_local);
	apiClient_free(apiClient);

// Pet update with form test
	char *petName1 = "Rocky Handsome";

	char *petName2 = "sold";

	apiClient_t *apiClient1 = apiClient_create();
	PetAPI_updatePetWithForm(apiClient1, EXAMPLE_PET_ID, petName1,
	                         petName2);
	apiClient_free(apiClient1);

// Get pet by id test
	apiClient_t *apiClient2 = apiClient_create();
	pet_t *mypet = PetAPI_getPetById(apiClient2, EXAMPLE_PET_ID);

	cJSON *JSONR = pet_convertToJSON(mypet);
	char *petJson = cJSON_Print(JSONR);
	printf("Data is:%s\n", petJson);

	assert(strcmp(mypet->name, "Rocky Handsome") == 0);
	assert(mypet->id == EXAMPLE_PET_ID);
	assert(strcmp(mypet->category->name, EXAMPLE_CATEGORY_NAME) == 0);
	assert(mypet->category->id == EXAMPLE_CATEGORY_ID);
	assert(strcmp(list_getElementAt(mypet->photoUrls,
	                                0)->data, EXAMPLE_URL_1) == 0);
	assert(strcmp(list_getElementAt(mypet->photoUrls,
	                                1)->data, EXAMPLE_URL_2) == 0);
	assert(((tag_t *) list_getElementAt(mypet->tags,
	                                    0)->data)->id == EXAMPLE_TAG_1_ID);
	assert(((tag_t *) list_getElementAt(mypet->tags,
	                                    1)->data)->id == EXAMPLE_TAG_2_ID);
	assert(strcmp(((tag_t *) list_getElementAt(mypet->tags, 0)->data)->name,
	              EXAMPLE_TAG_1_NAME) == 0);
	assert(strcmp(((tag_t *) list_getElementAt(mypet->tags, 1)->data)->name,
	              EXAMPLE_TAG_2_NAME) == 0);

	free(petJson);
	cJSON_Delete(JSONR);
	pet_free(mypet);
	apiClient_free(apiClient2);

// Pet upload file Test
	apiClient_t *apiClient3 = apiClient_create();
	FILE *file = fopen("/opt/image.png", "r");
	char *buff;
	int read_size, len;
	binary_t *data = malloc(sizeof(binary_t));
	if(file) {
		fseek(file, 0, SEEK_END);
		read_size = 2 * ftell(file);
		rewind(file);
		data->data = (char *) malloc(read_size + 1);
		data->len = fread((void *) data->data, 1, read_size, file);
		data->data[read_size] = '\0';
	}
	if(file != NULL) {
		api_response_t *respo = PetAPI_uploadFile(apiClient3,
		                                          EXAMPLE_PET_ID,
		                                          "dec",
		                                          data);

		api_response_free(respo);
		free(data->data);
		free(data);
		fclose(file);
	}
	apiClient_free(apiClient3);

	apiClient_unsetupGlobalEnv();
}
