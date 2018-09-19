#include <stdlib.h>
#include <stdio.h>
#include <ctype.h>
#include "apiClient.h"
#include "cJSON.h"
#include "keyValuePair.h"
#include "api_response.h"
#include "pet.h"

#define MAX_BUFFER_LENGTH 4096
#define intToStr(dst, src) \
	do { \
		char dst[64]; \
		snprintf(dst, 64, "%ld", (long int) (src)); \
	} while(0)

// Add a new pet to the store
//
void *PetAPI_addPet(apiClient_t *apiClient, pet_t *pet) {
	list_t *localVarQueryParameters = list_create();
	list_t *localVarHeaderParameters = list_create();
	list_t *localVarFormParameters = list_create();
	list_t *localVarHeaderType = list_create();
	list_t *localVarContentType = list_create();
	char *localVarBodyParameters = NULL;


	// create the path
	char *localVarPath = malloc(MAX_BUFFER_LENGTH);
	snprintf(localVarPath, MAX_BUFFER_LENGTH, "/pet");



	// JSON HTTP Request - pet
	// string
	cJSON *PetJSONObject;
	PetJSONObject = pet_convertToJSON(pet);
	localVarBodyParameters = cJSON_Print(PetJSONObject);

	list_addElement(localVarContentType, "application/json"); // consumes
	list_addElement(localVarContentType, "application/xml"); // consumes

	apiClient_invoke(apiClient,
	                 localVarPath,
	                 localVarQueryParameters,
	                 localVarHeaderParameters,
	                 localVarFormParameters,
	                 localVarHeaderType,
	                 localVarContentType,
	                 localVarBodyParameters,
	                 "POST");

	// free(apiClient->dataReceived);
	// free(localVarPath);
	free(localVarBodyParameters);
	// cJSON_Delete(PetJSONObject);
}

// Deletes a pet
//
void *PetAPI_deletePet(apiClient_t *apiClient, long petId, char *api_key) {
	list_t *localVarQueryParameters = list_create();
	list_t *localVarHeaderParameters = list_create();
	list_t *localVarFormParameters = list_create();
	list_t *localVarHeaderType = list_create();
	list_t *localVarContentType = list_create();
	char *localVarBodyParameters = NULL;


	// create the path
	char *localVarPath = malloc(MAX_BUFFER_LENGTH);
	snprintf(localVarPath, MAX_BUFFER_LENGTH, "/pet/{petId}");

	// TODO path parameter PetId (petId) not yet supported
	// TODO base path = http://petstore.swagger.io/v2


	char *baseNameModToReplace = malloc(sizeof(petId) + 3); // baseNameMod free not yet implemented
	snprintf(baseNameModToReplace,
	         strlen("petId") + 3,
	         "%s%s%s",
	         "{",
	         "petId",
	         "}");
	char *baseNameMod = malloc(sizeof(petId) + 2); // baseNameMod free not yet implemented
	snprintf(baseNameMod, sizeof(petId) + 3, "%s%li%s", "{", petId, "}");
	char buff[64];
	intToStr(buf, petId);
	localVarPath = strReplace(localVarPath, baseNameModToReplace, buff);
	// localVarPath = buff;

	// header parameters (TODO free function to implement)
	if(api_key) {
		char *key = malloc(strlen("api_key") + 1);
		char *value = malloc(sizeof(api_key) + 1);
		key = "api_key";
		value = api_key;
		keyValuePair_t *keyPair = keyValuePair_create(key, value);
		list_addElement(localVarHeaderParameters, keyPair);
	}



	apiClient_invoke(apiClient,
	                 localVarPath,
	                 localVarQueryParameters,
	                 localVarHeaderParameters,
	                 localVarFormParameters,
	                 localVarHeaderType,
	                 localVarContentType,
	                 localVarBodyParameters,
	                 "DELETE");

	// free(apiClient->dataReceived);
	// free(localVarPath);
	// free(api_key);
}

// Finds Pets by status
//
// Multiple status values can be provided with comma separated strings
//
list_t *PetAPI_findPetsByStatus(apiClient_t *apiClient, list_t *status) {
	list_t *localVarQueryParameters = list_create();
	list_t *localVarHeaderParameters = list_create();
	list_t *localVarFormParameters = list_create();
	list_t *localVarHeaderType = list_create();
	list_t *localVarContentType = list_create();
	char *localVarBodyParameters = NULL;


	// create the path
	char *localVarPath = malloc(MAX_BUFFER_LENGTH);
	snprintf(localVarPath, MAX_BUFFER_LENGTH, "/pet/findByStatus");



	// query parameters (TODO free function to implement)
	if(status) {
		// notstring
		localVarQueryParameters = status;
	}

	list_addElement(localVarHeaderType, "application/xml"); // produces
	list_addElement(localVarHeaderType, "application/json"); // produces

	apiClient_invoke(apiClient,
	                 localVarPath,
	                 localVarQueryParameters,
	                 localVarHeaderParameters,
	                 localVarFormParameters,
	                 localVarHeaderType,
	                 localVarContentType,
	                 localVarBodyParameters,
	                 "GET");

	cJSON *Pet;
	cJSON *PetAPIJSON = cJSON_Parse(apiClient->dataReceived);
	// cJSON *PetJSON = cJSON_GetObjectItemCaseSensitive(PetAPIJSON, "Pet");
	if(!cJSON_IsArray(PetAPIJSON)) {
		return 0; // nonprimitive container
	}
	list_t *elementToReturn = list_create();

	cJSON_ArrayForEach(Pet, PetAPIJSON)
	{
		if(!cJSON_IsObject(Pet)) {
			return 0;
		}
		char *JSONToChar = cJSON_Print(Pet);
		list_addElement(elementToReturn, JSONToChar);
	}
	// return elementToReturn;
	// free(apiClient->dataReceived);
	// free(localVarPath);


	return elementToReturn;
}

// Finds Pets by tags
//
// Multiple tags can be provided with comma separated strings. Use tag1, tag2, tag3 for testing.
//
list_t *PetAPI_findPetsByTags(apiClient_t *apiClient, list_t *tags) {
	list_t *localVarQueryParameters = list_create();
	list_t *localVarHeaderParameters = list_create();
	list_t *localVarFormParameters = list_create();
	list_t *localVarHeaderType = list_create();
	list_t *localVarContentType = list_create();
	char *localVarBodyParameters = NULL;


	// create the path
	char *localVarPath = malloc(MAX_BUFFER_LENGTH);
	snprintf(localVarPath, MAX_BUFFER_LENGTH, "/pet/findByTags");



	// query parameters (TODO free function to implement)
	if(tags) {
		// notstring
		localVarQueryParameters = tags;
	}

	list_addElement(localVarHeaderType, "application/xml"); // produces
	list_addElement(localVarHeaderType, "application/json"); // produces

	apiClient_invoke(apiClient,
	                 localVarPath,
	                 localVarQueryParameters,
	                 localVarHeaderParameters,
	                 localVarFormParameters,
	                 localVarHeaderType,
	                 localVarContentType,
	                 localVarBodyParameters,
	                 "GET");

	cJSON *Pet;
	cJSON *PetAPIJSON = cJSON_Parse(apiClient->dataReceived);
	// cJSON *PetJSON = cJSON_GetObjectItemCaseSensitive(PetAPIJSON, "Pet");
	if(!cJSON_IsArray(PetAPIJSON)) {
		return 0; // nonprimitive container
	}
	list_t *elementToReturn = list_create();

	cJSON_ArrayForEach(Pet, PetAPIJSON)
	{
		if(!cJSON_IsObject(Pet)) {
			return 0;
		}
		char *JSONToChar = cJSON_Print(Pet);
		list_addElement(elementToReturn, JSONToChar);
	}
	// return elementToReturn;
	// free(apiClient->dataReceived);
	// free(localVarPath);


	return elementToReturn;
}

// Find pet by ID
//
// Returns a single pet
//
pet_t *PetAPI_getPetById(apiClient_t *apiClient, long petId) {
	list_t *localVarQueryParameters = list_create();
	list_t *localVarHeaderParameters = list_create();
	list_t *localVarFormParameters = list_create();
	list_t *localVarHeaderType = list_create();
	list_t *localVarContentType = list_create();
	char *localVarBodyParameters = NULL;


	// create the path
	char *localVarPath = malloc(MAX_BUFFER_LENGTH);
	snprintf(localVarPath, MAX_BUFFER_LENGTH, "/pet/{petId}");

	// TODO path parameter PetId (petId) not yet supported
	// TODO base path = http://petstore.swagger.io/v2


	char *baseNameModToReplace = malloc(sizeof(petId) + 3); // baseNameMod free not yet implemented
	snprintf(baseNameModToReplace,
	         strlen("petId") + 3,
	         "%s%s%s",
	         "{",
	         "petId",
	         "}");
	char *baseNameMod = malloc(sizeof(petId) + 2); // baseNameMod free not yet implemented
	snprintf(baseNameMod, sizeof(petId) + 3, "%s%li%s", "{", petId, "}");
	char buff[64];
	intToStr(buf, petId);
	localVarPath = strReplace(localVarPath, baseNameModToReplace, buff);
	// localVarPath = buff;



	list_addElement(localVarHeaderType, "application/xml"); // produces
	list_addElement(localVarHeaderType, "application/json"); // produces

	apiClient_invoke(apiClient,
	                 localVarPath,
	                 localVarQueryParameters,
	                 localVarHeaderParameters,
	                 localVarFormParameters,
	                 localVarHeaderType,
	                 localVarContentType,
	                 localVarBodyParameters,
	                 "GET");

	// nonprimitive not container
	pet_t *elementToReturn = pet_parseFromJSON(apiClient->dataReceived);
	if(elementToReturn == NULL) {
		return 0;
	} else {
		// cJSON *jsonObject = Pet_convertToJSON(localVarpet);
		// cJSON_Delete(jsonObject);
	}

	// return elementToReturn;
	// free(apiClient->dataReceived);
	// free(localVarPath);


	return elementToReturn;
}

// Update an existing pet
//
void *PetAPI_updatePet(apiClient_t *apiClient, pet_t *pet) {
	list_t *localVarQueryParameters = list_create();
	list_t *localVarHeaderParameters = list_create();
	list_t *localVarFormParameters = list_create();
	list_t *localVarHeaderType = list_create();
	list_t *localVarContentType = list_create();
	char *localVarBodyParameters = NULL;


	// create the path
	char *localVarPath = malloc(MAX_BUFFER_LENGTH);
	snprintf(localVarPath, MAX_BUFFER_LENGTH, "/pet");



	// JSON HTTP Request - pet
	// string
	cJSON *PetJSONObject;
	PetJSONObject = pet_convertToJSON(pet);
	localVarBodyParameters = cJSON_Print(PetJSONObject);

	list_addElement(localVarContentType, "application/json"); // consumes
	list_addElement(localVarContentType, "application/xml"); // consumes

	apiClient_invoke(apiClient,
	                 localVarPath,
	                 localVarQueryParameters,
	                 localVarHeaderParameters,
	                 localVarFormParameters,
	                 localVarHeaderType,
	                 localVarContentType,
	                 localVarBodyParameters,
	                 "PUT");

	// free(apiClient->dataReceived);
	// free(localVarPath);
	free(localVarBodyParameters);
	// cJSON_Delete(PetJSONObject);
}

// Updates a pet in the store with form data
//
void *PetAPI_updatePetWithForm(apiClient_t	*apiClient,
                               long		petId,
                               char		*name,
                               char		*status) {
	list_t *localVarQueryParameters = list_create();
	list_t *localVarHeaderParameters = list_create();
	list_t *localVarFormParameters = list_create();
	list_t *localVarHeaderType = list_create();
	list_t *localVarContentType = list_create();
	char *localVarBodyParameters = NULL;


	// create the path
	char *localVarPath = malloc(MAX_BUFFER_LENGTH);
	snprintf(localVarPath, MAX_BUFFER_LENGTH, "/pet/{petId}");

	// TODO path parameter PetId (petId) not yet supported
	// TODO base path = http://petstore.swagger.io/v2


	char *baseNameModToReplace = malloc(sizeof(petId) + 3); // baseNameMod free not yet implemented
	snprintf(baseNameModToReplace,
	         strlen("petId") + 3,
	         "%s%s%s",
	         "{",
	         "petId",
	         "}");
	char *baseNameMod = malloc(sizeof(petId) + 2); // baseNameMod free not yet implemented
	snprintf(baseNameMod, sizeof(petId) + 3, "%s%li%s", "{", petId, "}");
	char buff[64];
	intToStr(buf, petId);
	localVarPath = strReplace(localVarPath, baseNameModToReplace, buff);
	// localVarPath = buff;


	// form parameters (TODO free function to implement)
	if(name) {
		char *key = malloc(strlen("name") + 1);
		char *value = malloc(sizeof(name) + 1);
		key = "name";
		value = name;
		keyValuePair_t *keyPair = keyValuePair_create(key, value);
		list_addElement(localVarFormParameters, keyPair); // String
		// keyValuePair_free(key);
	}
	// form parameters (TODO free function to implement)
	if(status) {
		char *key = malloc(strlen("status") + 1);
		char *value = malloc(sizeof(status) + 1);
		key = "status";
		value = status;
		keyValuePair_t *keyPair = keyValuePair_create(key, value);
		list_addElement(localVarFormParameters, keyPair); // String
		// keyValuePair_free(key);
	}

	list_addElement(localVarContentType,
	                "application/x-www-form-urlencoded");                      // consumes

	apiClient_invoke(apiClient,
	                 localVarPath,
	                 localVarQueryParameters,
	                 localVarHeaderParameters,
	                 localVarFormParameters,
	                 localVarHeaderType,
	                 localVarContentType,
	                 localVarBodyParameters,
	                 "POST");

	// free(apiClient->dataReceived);
	// free(localVarPath);
	// free(name);
	// free(status);
}

// uploads an image
//
api_response_t *PetAPI_uploadFile(apiClient_t	*apiClient,
                                  long		petId,
                                  char		*additionalMetadata,
                                  FILE		*file) {
	list_t *localVarQueryParameters = list_create();
	list_t *localVarHeaderParameters = list_create();
	list_t *localVarFormParameters = list_create();
	list_t *localVarHeaderType = list_create();
	list_t *localVarContentType = list_create();
	char *localVarBodyParameters = NULL;


	// create the path
	char *localVarPath = malloc(MAX_BUFFER_LENGTH);
	snprintf(localVarPath, MAX_BUFFER_LENGTH, "/pet/{petId}/uploadImage");

	// TODO path parameter PetId (petId) not yet supported
	// TODO base path = http://petstore.swagger.io/v2


	char *baseNameModToReplace = malloc(sizeof(petId) + 3); // baseNameMod free not yet implemented
	snprintf(baseNameModToReplace,
	         strlen("petId") + 3,
	         "%s%s%s",
	         "{",
	         "petId",
	         "}");
	char *baseNameMod = malloc(sizeof(petId) + 2); // baseNameMod free not yet implemented
	snprintf(baseNameMod, sizeof(petId) + 3, "%s%li%s", "{", petId, "}");
	char buff[64];
	intToStr(buf, petId);
	localVarPath = strReplace(localVarPath, baseNameModToReplace, buff);
	// localVarPath = buff;


	// form parameters (TODO free function to implement)
	if(additionalMetadata) {
		char *key = malloc(strlen("additionalMetadata") + 1);
		char *value = malloc(sizeof(additionalMetadata) + 1);
		key = "additionalMetadata";
		value = additionalMetadata;
		keyValuePair_t *keyPair = keyValuePair_create(key, value);
		list_addElement(localVarFormParameters, keyPair); // String
		// keyValuePair_free(key);
	}
	// form parameters (TODO free function to implement)
	if(file != NULL) {
		// list_addElement(localVarFormParameters,file); //notstring
	}

	list_addElement(localVarHeaderType, "application/json"); // produces
	list_addElement(localVarContentType, "multipart/form-data"); // consumes

	apiClient_invoke(apiClient,
	                 localVarPath,
	                 localVarQueryParameters,
	                 localVarHeaderParameters,
	                 localVarFormParameters,
	                 localVarHeaderType,
	                 localVarContentType,
	                 localVarBodyParameters,
	                 "POST");

	// nonprimitive not container
	api_response_t *elementToReturn = api_response_parseFromJSON(
		apiClient->dataReceived);
	if(elementToReturn == NULL) {
		return 0;
	} else {
		// cJSON *jsonObject = Pet_convertToJSON(localVarapi_response);
		// cJSON_Delete(jsonObject);
	}

	// return elementToReturn;
	// free(apiClient->dataReceived);
	// free(localVarPath);


	// free(additionalMetadata);


	return elementToReturn;
}
