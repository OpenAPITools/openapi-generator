#include <stdlib.h>
#include "apiClient.h"
#include "cJSON.h"
#include "pet.h"
#ifdef API_KEY
#include "list.h"
#include "apiKey.h"
#endif // API_KEY
#ifdef DEBUG
#include <stdio.h>
#endif // DEBUG


#define EXAMPLE_OPERATION_NAME "pet"
#define EXAMPLE_OPERATION_PARAMETER "3"

int main() {
	apiClient_t *apiClient = apiClient_create();
	#ifdef OAUTH2
	apiClient->accessToken = "thisIsMyExampleAccessToken";
	#endif // OAUTH2
	#ifdef API_KEY
	apiClient->apiKeys = list_create();
	apiKey_t *apiKey = apiKey_create("X-API-Key", "abcdef12345");
	list_addElement(apiClient->apiKeys, apiKey);
	#endif // API_KEY

	apiClient_invoke(apiClient,
	                 EXAMPLE_OPERATION_NAME,
	                 EXAMPLE_OPERATION_PARAMETER,
	                 NULL);
	pet_t *pet = pet_parseFromJSON(apiClient->dataReceived);
	if(pet == NULL) {
		free(apiClient);
		return 0;
	} else {
		cJSON *petJSONObject = pet_convertToJSON(pet);

		#ifdef DEBUG
		char *jsonString = cJSON_Print(petJSONObject);
		puts(jsonString);
		free(jsonString);
		#endif
		cJSON_Delete(petJSONObject);
	}
	free(apiClient->dataReceived);

	#ifdef API_KEY
	free(apiKey);
	list_free(apiClient->apiKeys);
	#endif // API_KEY

	apiClient_free(apiClient);
	pet_free(pet);
}