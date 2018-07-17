#include <stdlib.h>
#include <stdio.h>
#include "apiClient.h"
#include "cJSON.h"
#include "pet.h"

#define MAX_BUFFER_LENGTH 9

pet_t *petApi_getPetById(apiClient_t *apiClient, long petId) {
	pet_t *pet;
	char *petIdString = malloc(MAX_BUFFER_LENGTH);

	snprintf(petIdString, MAX_BUFFER_LENGTH, "%li", petId);

	apiClient_invoke(apiClient,
	                 "pet",
	                 petIdString,
	                 NULL,
	                 NULL,
	                 NULL,
	                 NULL,
	                 NULL);
	pet = pet_parseFromJSON(apiClient->dataReceived);
	free(apiClient->dataReceived);
	if(pet == NULL) {
		return 0;
	} else {
		cJSON *jsonObject = pet_convertToJSON(pet);
		cJSON_Delete(jsonObject);
	}
	free(petIdString);

	return pet;
}

void *petApi_addPet(apiClient_t *apiClient, pet_t *pet) {
	cJSON *petJSONObject;
	char *petJSONString;

	petJSONObject = pet_convertToJSON(pet);
	petJSONString = cJSON_Print(petJSONObject);
	apiClient_invoke(apiClient,
	                 "pet",
	                 NULL,
	                 NULL,
	                 NULL,
	                 NULL,
	                 petJSONString,
	                 "POST");
	free(apiClient->dataReceived);
	free(petJSONString);
	cJSON_Delete(petJSONObject);

	return pet;
}