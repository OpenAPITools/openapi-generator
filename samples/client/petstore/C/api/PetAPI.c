#include <stdlib.h>
#include <stdio.h>
#include "apiClient.h"
#include "cJSON.h"
#include "models.api_response.h" // TODO will fix the import later
#include "models.pet.h" // TODO will fix the import later

#define MAX_BUFFER_LENGTH 4096
#define intToStr(dst, src) \
    do {\
    char dst[64];\
    snprintf(dst, 64, "%ld", (long int)(src));\
}while(0)

// Add a new pet to the store
//
void *PetAPI_addPet(apiClient_t *apiClient, pet Pet) {
    list_t    *localVarQueryParameters,
    list_t    *localVarHeaderParameters,
    list_t    *localVarFormParameters,
    char      *localVarBodyParameters,

    // create the path
   char *localVarPath = malloc(MAX_BUFFER_LENGTH);
   snprintf(localVarPath, MAX_BUFFER_LENGTH, "/pet");



    // JSON HTTP Request - pet
    cJSON *PetJSONObject;
    PetJSONObject = pet_convertToJSON(Pet);
    localVarBodyParameters = cJSON_Print(PetJSONObject);

   apiClient_invoke(apiClient,
                    "pet",
                    localVarPath,
                    localVarQueryParameters,
                    localVarHeaderParameters,
                    localVarFormParameters,
                    localVarBodyParameters,
                    "POST");

   free(apiClient->dataReceived);
    free(localVarBodyParameters);
   cJSON_Delete()
   return;

}

// Deletes a pet
//
void *PetAPI_deletePet(apiClient_t *apiClient, long PetId, char ApiKey) {
    list_t    *localVarQueryParameters,
    list_t    *localVarHeaderParameters,
    list_t    *localVarFormParameters,
    char      *localVarBodyParameters,

    // create the path
   char *localVarPath = malloc(MAX_BUFFER_LENGTH);
   snprintf(localVarPath, MAX_BUFFER_LENGTH, "/pet/{petId}");

    // TODO path parameter PetId (petId) not yet supported
    // TODO base path = http://petstore.swagger.io/v2
    char* baseNameMod = malloc(strlen(petId)+2); //baseNameMod free not yet implemented
    snprintf(baseNameMod, strlen(baseName)+3, "%s%s%s", "{", petId, "}");
    char buff[64];
    intToStr(buf, PetId);
    localVarPath = strReplace(localVarPath, baseNameMod, buff);

    // TODO header parameters
    // header parameter ApiKey (api_key) not yet supported

   apiClient_invoke(apiClient,
                    "pet",
                    localVarPath,
                    localVarQueryParameters,
                    localVarHeaderParameters,
                    localVarFormParameters,
                    localVarBodyParameters,
                    "DELETE");

   free(apiClient->dataReceived);
   free(PetIdString);
   free(ApiKeyString);
   return;

}

// Finds Pets by status
//
// Multiple status values can be provided with comma separated strings
//
list_t *PetAPI_findPetsByStatus(apiClient_t *apiClient, list Status) {
    list_t    *localVarQueryParameters,
    list_t    *localVarHeaderParameters,
    list_t    *localVarFormParameters,
    char      *localVarBodyParameters,

    // create the path
   char *localVarPath = malloc(MAX_BUFFER_LENGTH);
   snprintf(localVarPath, MAX_BUFFER_LENGTH, "/pet/findByStatus");



    // TODO query parameters
    // query parameter Status (status) not yet supported
    char *status = malloc(MAX_BUFFER_LENGTH);

   apiClient_invoke(apiClient,
                    "pet",
                    localVarPath,
                    localVarQueryParameters,
                    localVarHeaderParameters,
                    localVarFormParameters,
                    localVarBodyParameters,
                    "GET");

   free(apiClient->dataReceived);
   free(StatusString);
    localVarlist = _parseFromJSON(apiClient->dataReceived);
    if(localVarlist == NULL) {
        return 0;
    } else {
        cJSON *jsonObject = _convertToJSON();
        cJSON_Delete(jsonObject);
    }

   return localVarlist;

}

// Finds Pets by tags
//
// Multiple tags can be provided with comma separated strings. Use tag1, tag2, tag3 for testing.
//
list_t *PetAPI_findPetsByTags(apiClient_t *apiClient, list Tags) {
    list_t    *localVarQueryParameters,
    list_t    *localVarHeaderParameters,
    list_t    *localVarFormParameters,
    char      *localVarBodyParameters,

    // create the path
   char *localVarPath = malloc(MAX_BUFFER_LENGTH);
   snprintf(localVarPath, MAX_BUFFER_LENGTH, "/pet/findByTags");



    // TODO query parameters
    // query parameter Tags (tags) not yet supported
    char *tags = malloc(MAX_BUFFER_LENGTH);

   apiClient_invoke(apiClient,
                    "pet",
                    localVarPath,
                    localVarQueryParameters,
                    localVarHeaderParameters,
                    localVarFormParameters,
                    localVarBodyParameters,
                    "GET");

   free(apiClient->dataReceived);
   free(TagsString);
    localVarlist = _parseFromJSON(apiClient->dataReceived);
    if(localVarlist == NULL) {
        return 0;
    } else {
        cJSON *jsonObject = _convertToJSON();
        cJSON_Delete(jsonObject);
    }

   return localVarlist;

}

// Find pet by ID
//
// Returns a single pet
//
pet_t *PetAPI_getPetById(apiClient_t *apiClient, long PetId) {
    list_t    *localVarQueryParameters,
    list_t    *localVarHeaderParameters,
    list_t    *localVarFormParameters,
    char      *localVarBodyParameters,

    // create the path
   char *localVarPath = malloc(MAX_BUFFER_LENGTH);
   snprintf(localVarPath, MAX_BUFFER_LENGTH, "/pet/{petId}");

    // TODO path parameter PetId (petId) not yet supported
    // TODO base path = http://petstore.swagger.io/v2
    char* baseNameMod = malloc(strlen(petId)+2); //baseNameMod free not yet implemented
    snprintf(baseNameMod, strlen(baseName)+3, "%s%s%s", "{", petId, "}");
    char buff[64];
    intToStr(buf, PetId);
    localVarPath = strReplace(localVarPath, baseNameMod, buff);


   apiClient_invoke(apiClient,
                    "pet",
                    localVarPath,
                    localVarQueryParameters,
                    localVarHeaderParameters,
                    localVarFormParameters,
                    localVarBodyParameters,
                    "GET");

   free(apiClient->dataReceived);
   free(PetIdString);
    localVarpet = _parseFromJSON(apiClient->dataReceived);
    if(localVarpet == NULL) {
        return 0;
    } else {
        cJSON *jsonObject = _convertToJSON();
        cJSON_Delete(jsonObject);
    }

   return localVarpet;

}

// Update an existing pet
//
void *PetAPI_updatePet(apiClient_t *apiClient, pet Pet) {
    list_t    *localVarQueryParameters,
    list_t    *localVarHeaderParameters,
    list_t    *localVarFormParameters,
    char      *localVarBodyParameters,

    // create the path
   char *localVarPath = malloc(MAX_BUFFER_LENGTH);
   snprintf(localVarPath, MAX_BUFFER_LENGTH, "/pet");



    // JSON HTTP Request - pet
    cJSON *PetJSONObject;
    PetJSONObject = pet_convertToJSON(Pet);
    localVarBodyParameters = cJSON_Print(PetJSONObject);

   apiClient_invoke(apiClient,
                    "pet",
                    localVarPath,
                    localVarQueryParameters,
                    localVarHeaderParameters,
                    localVarFormParameters,
                    localVarBodyParameters,
                    "PUT");

   free(apiClient->dataReceived);
    free(localVarBodyParameters);
   cJSON_Delete()
   return;

}

// Updates a pet in the store with form data
//
void *PetAPI_updatePetWithForm(apiClient_t *apiClient, long PetId, char Name, char Status) {
    list_t    *localVarQueryParameters,
    list_t    *localVarHeaderParameters,
    list_t    *localVarFormParameters,
    char      *localVarBodyParameters,

    // create the path
   char *localVarPath = malloc(MAX_BUFFER_LENGTH);
   snprintf(localVarPath, MAX_BUFFER_LENGTH, "/pet/{petId}");

    // TODO path parameter PetId (petId) not yet supported
    // TODO base path = http://petstore.swagger.io/v2
    char* baseNameMod = malloc(strlen(petId)+2); //baseNameMod free not yet implemented
    snprintf(baseNameMod, strlen(baseName)+3, "%s%s%s", "{", petId, "}");
    char buff[64];
    intToStr(buf, PetId);
    localVarPath = strReplace(localVarPath, baseNameMod, buff);


    // TODO form parameters
    // form parameter Name (name) not yet supported

    // TODO form parameters
    // form parameter Status (status) not yet supported

   apiClient_invoke(apiClient,
                    "pet",
                    localVarPath,
                    localVarQueryParameters,
                    localVarHeaderParameters,
                    localVarFormParameters,
                    localVarBodyParameters,
                    "POST");

   free(apiClient->dataReceived);
   free(PetIdString);
   free(NameString);
   free(StatusString);
   return;

}

// uploads an image
//
api_response_t *PetAPI_uploadFile(apiClient_t *apiClient, long PetId, char AdditionalMetadata, FILE File) {
    list_t    *localVarQueryParameters,
    list_t    *localVarHeaderParameters,
    list_t    *localVarFormParameters,
    char      *localVarBodyParameters,

    // create the path
   char *localVarPath = malloc(MAX_BUFFER_LENGTH);
   snprintf(localVarPath, MAX_BUFFER_LENGTH, "/pet/{petId}/uploadImage");

    // TODO path parameter PetId (petId) not yet supported
    // TODO base path = http://petstore.swagger.io/v2
    char* baseNameMod = malloc(strlen(petId)+2); //baseNameMod free not yet implemented
    snprintf(baseNameMod, strlen(baseName)+3, "%s%s%s", "{", petId, "}");
    char buff[64];
    intToStr(buf, PetId);
    localVarPath = strReplace(localVarPath, baseNameMod, buff);


    // TODO form parameters
    // form parameter AdditionalMetadata (additionalMetadata) not yet supported

    // TODO form parameters
    // form parameter File (file) not yet supported

   apiClient_invoke(apiClient,
                    "pet",
                    localVarPath,
                    localVarQueryParameters,
                    localVarHeaderParameters,
                    localVarFormParameters,
                    localVarBodyParameters,
                    "POST");

   free(apiClient->dataReceived);
   free(PetIdString);
   free(AdditionalMetadataString);
   free(FileString);
    localVarapi_response = _parseFromJSON(apiClient->dataReceived);
    if(localVarapi_response == NULL) {
        return 0;
    } else {
        cJSON *jsonObject = _convertToJSON();
        cJSON_Delete(jsonObject);
    }

   return localVarapi_response;

}



