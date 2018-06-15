#include <stdlib.h>
#include <stdio.h>
#include "apiClient.h"
#include "cJSON.h"
#include "pet.h"

#define MAX_BUFFER_LENGTH 9

pet_t *petApi_getPetById(long petId) {
	pet_t *pet;
	apiClient_t *apiClient;
	char *petIdString = malloc(MAX_BUFFER_LENGTH);

	snprintf(petIdString, MAX_BUFFER_LENGTH, "%li", petId);

	apiClient = apiClient_create();
	apiClient_invoke(apiClient,
	                 "pet",
	                 petIdString,
	                 NULL);
	pet = pet_parseFromJSON(apiClient->dataReceived);
	if(pet == NULL) {
		return 0;
	} else {
		cJSON *jsonObject = pet_convertToJSON(pet);
		cJSON_Delete(jsonObject);
	}
	apiClient_free(apiClient);
	free(petIdString);

	return pet;
}

void *petApi_addPet(pet_t *pet) {
	apiClient_t *apiClient;
	cJSON *petJSONObject;
	char *petJSONString;

	petJSONObject = pet_convertToJSON(pet);
	petJSONString = cJSON_Print(petJSONObject);
	apiClient = apiClient_create();
	apiClient_invoke(apiClient,
	                 "pet",
	                 NULL,
	                 petJSONString);
	apiClient_free(apiClient);
	free(petJSONString);
	cJSON_Delete(petJSONObject);

	return pet;
}