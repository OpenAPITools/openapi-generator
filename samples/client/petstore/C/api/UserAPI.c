#include <stdlib.h>
#include <stdio.h>
#include "apiClient.h"
#include "cJSON.h"
#include "{import=models.array}.h"
#include "{import=models.user}.h"

#define MAX_BUFFER_LENGTH 4096

void *UserAPI_(apiClient_t *apiClient) {
	pet_t *pet;


	char *petIdString = malloc(MAX_BUFFER_LENGTH);

	snprintf(petIdString, MAX_BUFFER_LENGTH, "%li", petId);



    // TODO header parameters

    // TODO query parameters

    // TODO form parameters

    // http body (model)
    post_body = nil

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

	return;

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