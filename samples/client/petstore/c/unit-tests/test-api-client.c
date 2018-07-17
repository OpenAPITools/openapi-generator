#include <stdlib.h>
#include <string.h>
#include "apiClient.h"
#include "cJSON.h"
#include "pet.h"
#include "list.h"
#include "keyValuePair.h"
#ifdef DEBUG
#include <stdio.h>
#endif // DEBUG


#define EXAMPLE_OPERATION_NAME "pet"
#define EXAMPLE_OPERATION_PARAMETER "5"
#define EXAMPLE_KEYNAME_1 "MyExampleKey"
#define EXAMPLE_VALUENAME_1 "MyExampleValue"
#define EXAMPLE_KEYNAME_2 "MyExampleKeyTwo"
#define EXAMPLE_VALUENAME_2 "MyExampleValueTwo"

int main() {
	apiClient_t *apiClient = apiClient_create();
	#ifdef OAUTH2
	apiClient->accessToken = "thisIsMyExampleAccessToken";
	#endif // OAUTH2
	#ifdef API_KEY
	apiClient->apiKeys = list_create();
	keyValuePair_t *apiKey = apiKey_create("X-API-Key", "abcdef12345");
	list_addElement(apiClient->apiKeys, apiKey);
	#endif // API_KEY

	list_t *customHeaderFields = list_create();
	char *keyOne = malloc(strlen(EXAMPLE_KEYNAME_1) + 1);
	char *valueOne = malloc(strlen(EXAMPLE_VALUENAME_1) + 1);
	strcpy(keyOne, EXAMPLE_KEYNAME_1);
	strcpy(valueOne, EXAMPLE_VALUENAME_1);

	char *keyTwo = malloc(strlen(EXAMPLE_KEYNAME_2) + 1);
	char *valueTwo = malloc(strlen(EXAMPLE_VALUENAME_2) + 1);
	strcpy(keyTwo, EXAMPLE_KEYNAME_2);
	strcpy(valueTwo, EXAMPLE_VALUENAME_2);

	keyValuePair_t *firstCustomField =
		keyValuePair_create(keyOne, valueOne);
	keyValuePair_t *secondCustomField =
		keyValuePair_create(keyTwo, valueTwo);

	list_addElement(customHeaderFields, firstCustomField);
	list_addElement(customHeaderFields, secondCustomField);

	apiClient_invoke(apiClient,
	                 EXAMPLE_OPERATION_NAME,
	                 EXAMPLE_OPERATION_PARAMETER,
	                 NULL,
	                 customHeaderFields,
	                 NULL,
	                 NULL,
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
	free(keyOne);
	free(valueOne);
	free(keyTwo);
	free(valueTwo);
	keyValuePair_free(firstCustomField);
	keyValuePair_free(secondCustomField);
	list_free(customHeaderFields);
	apiClient_free(apiClient);
	pet_free(pet);
}