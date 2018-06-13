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
	                 petIdString);
	pet = pet_parseFromJSON(apiClient->dataReceived);
	if(pet == NULL) {
		return 0;
	} else {
		char *jsonString = pet_convertToJSON(pet);
		free(jsonString);
	}
	apiClient_free(apiClient);
	free(petIdString);

	return pet;
}