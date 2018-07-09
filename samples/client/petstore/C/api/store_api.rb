#include <stdlib.h>
#include <stdio.h>
#include "apiClient.h"
#include "cJSON.h"
#include "{import=models.Map}.h"
#include "{import=models.Order}.h"

#define MAX_BUFFER_LENGTH 256

void *StoreApi_(apiClient_t *apiClient) {
	pet_t *pet;


	char *petIdString = malloc(MAX_BUFFER_LENGTH);

	snprintf(petIdString, MAX_BUFFER_LENGTH, "%li", petId);

	apiClient_invoke(apiClient,
	                 "pet",
	                 petIdString,
	                 NULL);
	pet = pet_parseFromJSON(apiClient->dataReceived);
	free(apiClient->dataReceived);
	if(pet == NULL) {
		return 0;
	} else {
		cJSON *jsonObject = pet_convertToJSON(pet);
		cJSON_Delete(jsonObject);
	}

	return;

}