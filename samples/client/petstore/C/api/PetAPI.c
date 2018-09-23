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
    do {\
    char dst[256];\
    snprintf(dst, 256, "%ld", (long int)(src));\
}while(0)

// Add a new pet to the store
//
void *PetAPI_addPet(apiClient_t *apiClient, pet_t* pet) {
    list_t    *localVarQueryParameters = list_create();
    list_t    *localVarHeaderParameters = list_create();
    list_t    *localVarFormParameters = list_create();
    list_t *localVarHeaderType = list_create();
    list_t *localVarContentType = list_create();
    char      *localVarBodyParameters = NULL;

    // create the path
    char *localVarPath = malloc(MAX_BUFFER_LENGTH);
    snprintf(localVarPath, MAX_BUFFER_LENGTH, "/pet");

    // Body Param
    //string
    cJSON *localVarSingleItemJSON_pet;
    localVarSingleItemJSON_pet = pet_convertToJSON(pet);
    localVarBodyParameters = cJSON_Print(localVarSingleItemJSON_pet);

        list_addElement(localVarContentType,"application/json"); //consumes

        list_addElement(localVarContentType,"application/xml"); //consumes

    apiClient_invoke(apiClient,
                    localVarPath,
                    localVarQueryParameters,
                    localVarHeaderParameters,
                    localVarFormParameters,
                    localVarHeaderType,
                    localVarContentType,
                    localVarBodyParameters,
                    "POST");

    //No return type
    apiClient_free(apiClient);
    list_free(localVarQueryParameters);
    list_free(localVarHeaderParameters);
    list_free(localVarFormParameters);
    list_free(localVarHeaderType);
    list_free(localVarContentType);
    free(localVarPath);

}

// Deletes a pet
//
void *PetAPI_deletePet(apiClient_t *apiClient, long petId, char* api_key) {
    list_t    *localVarQueryParameters = list_create();
    list_t    *localVarHeaderParameters = list_create();
    list_t    *localVarFormParameters = list_create();
    list_t *localVarHeaderType = list_create();
    list_t *localVarContentType = list_create();
    char      *localVarBodyParameters = NULL;

    // create the path
    char *localVarPath = malloc(MAX_BUFFER_LENGTH);
    snprintf(localVarPath, MAX_BUFFER_LENGTH, "/pet/{petId}");

    // Path Params
    char* localVarToReplace = malloc(sizeof(petId)+3);
    snprintf(localVarToReplace, strlen("petId")+3, "%s%s%s", "{", "petId", "}");

    char localVarBuff[256];
    intToStr(localVarBuff, petId);

    localVarPath = strReplace(localVarPath, localVarToReplace, localVarBuff);


    // header parameters (TODO free function to implement)
    char *keyHeader_api_key;
    char *valueHeader_api_key;
    keyValuePair_t *keyPairHeader_api_key = 0;
    if (api_key)
    {
        keyHeader_api_key = strdup("api_key");
        valueHeader_api_key = strdup(api_key);
        keyPairHeader_api_key = keyValuePair_create(keyHeader_api_key, valueHeader_api_key);
        list_addElement(localVarHeaderParameters,keyPairHeader_api_key);
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

    //No return type
    apiClient_free(apiClient);
    list_free(localVarQueryParameters);
    list_free(localVarHeaderParameters);
    list_free(localVarFormParameters);
    list_free(localVarHeaderType);
    list_free(localVarContentType);
    free(localVarPath);
    free(localVarToReplace);
    free(keyPairHeader_api_key);
    free(keyHeader_api_key);
    free(valueHeader_api_key);

}

// Finds Pets by status
//
// Multiple status values can be provided with comma separated strings
//
list_t *PetAPI_findPetsByStatus(apiClient_t *apiClient, list_t* status) {
    list_t    *localVarQueryParameters = list_create();
    list_t    *localVarHeaderParameters = list_create();
    list_t    *localVarFormParameters = list_create();
    list_t *localVarHeaderType = list_create();
    list_t *localVarContentType = list_create();
    char      *localVarBodyParameters = NULL;

    // create the path
    char *localVarPath = malloc(MAX_BUFFER_LENGTH);
    snprintf(localVarPath, MAX_BUFFER_LENGTH, "/pet/findByStatus");

    // query parameters
        if (status)
    {
       //listContainer
       localVarQueryParameters = status;
    }

        list_addElement(localVarHeaderType,"application/xml"); //produces

        list_addElement(localVarHeaderType,"application/json"); //produces

    apiClient_invoke(apiClient,
                    localVarPath,
                    localVarQueryParameters,
                    localVarHeaderParameters,
                    localVarFormParameters,
                    localVarHeaderType,
                    localVarContentType,
                    localVarBodyParameters,
                    "GET");

    cJSON *PetAPIlocalVarJSON = cJSON_Parse(apiClient->dataReceived);
    if(!cJSON_IsArray(PetAPIlocalVarJSON)) {
        return 0;//nonprimitive container
    }
    list_t *elementToReturn = list_create();
    cJSON *PetVarJSON;
    cJSON_ArrayForEach(PetVarJSON, PetAPIlocalVarJSON)
    {
    if(!cJSON_IsObject(PetVarJSON))
    {
        return 0;;
    }
    char *localVarJSONToChar = cJSON_Print(PetVarJSON);
    list_addElement(elementToReturn , localVarJSONToChar);
    }

    cJSON_Delete( PetAPIlocalVarJSON);
    cJSON_Delete( PetVarJSON);
    //return type
    apiClient_free(apiClient);
    list_free(localVarQueryParameters);
    list_free(localVarHeaderParameters);
    list_free(localVarFormParameters);
    list_free(localVarHeaderType);
    list_free(localVarContentType);
    free(localVarPath);
   return elementToReturn;



}

// Finds Pets by tags
//
// Multiple tags can be provided with comma separated strings. Use tag1, tag2, tag3 for testing.
//
list_t *PetAPI_findPetsByTags(apiClient_t *apiClient, list_t* tags) {
    list_t    *localVarQueryParameters = list_create();
    list_t    *localVarHeaderParameters = list_create();
    list_t    *localVarFormParameters = list_create();
    list_t *localVarHeaderType = list_create();
    list_t *localVarContentType = list_create();
    char      *localVarBodyParameters = NULL;

    // create the path
    char *localVarPath = malloc(MAX_BUFFER_LENGTH);
    snprintf(localVarPath, MAX_BUFFER_LENGTH, "/pet/findByTags");

    // query parameters
        if (tags)
    {
       //listContainer
       localVarQueryParameters = tags;
    }

        list_addElement(localVarHeaderType,"application/xml"); //produces

        list_addElement(localVarHeaderType,"application/json"); //produces

    apiClient_invoke(apiClient,
                    localVarPath,
                    localVarQueryParameters,
                    localVarHeaderParameters,
                    localVarFormParameters,
                    localVarHeaderType,
                    localVarContentType,
                    localVarBodyParameters,
                    "GET");

    cJSON *PetAPIlocalVarJSON = cJSON_Parse(apiClient->dataReceived);
    if(!cJSON_IsArray(PetAPIlocalVarJSON)) {
        return 0;//nonprimitive container
    }
    list_t *elementToReturn = list_create();
    cJSON *PetVarJSON;
    cJSON_ArrayForEach(PetVarJSON, PetAPIlocalVarJSON)
    {
    if(!cJSON_IsObject(PetVarJSON))
    {
        return 0;;
    }
    char *localVarJSONToChar = cJSON_Print(PetVarJSON);
    list_addElement(elementToReturn , localVarJSONToChar);
    }

    cJSON_Delete( PetAPIlocalVarJSON);
    cJSON_Delete( PetVarJSON);
    //return type
    apiClient_free(apiClient);
    list_free(localVarQueryParameters);
    list_free(localVarHeaderParameters);
    list_free(localVarFormParameters);
    list_free(localVarHeaderType);
    list_free(localVarContentType);
    free(localVarPath);
   return elementToReturn;



}

// Find pet by ID
//
// Returns a single pet
//
pet_t *PetAPI_getPetById(apiClient_t *apiClient, long petId) {
    list_t    *localVarQueryParameters = list_create();
    list_t    *localVarHeaderParameters = list_create();
    list_t    *localVarFormParameters = list_create();
    list_t *localVarHeaderType = list_create();
    list_t *localVarContentType = list_create();
    char      *localVarBodyParameters = NULL;

    // create the path
    char *localVarPath = malloc(MAX_BUFFER_LENGTH);
    snprintf(localVarPath, MAX_BUFFER_LENGTH, "/pet/{petId}");

    // Path Params
    char* localVarToReplace = malloc(sizeof(petId)+3);
    snprintf(localVarToReplace, strlen("petId")+3, "%s%s%s", "{", "petId", "}");

    char localVarBuff[256];
    intToStr(localVarBuff, petId);

    localVarPath = strReplace(localVarPath, localVarToReplace, localVarBuff);


        list_addElement(localVarHeaderType,"application/xml"); //produces

        list_addElement(localVarHeaderType,"application/json"); //produces

    apiClient_invoke(apiClient,
                    localVarPath,
                    localVarQueryParameters,
                    localVarHeaderParameters,
                    localVarFormParameters,
                    localVarHeaderType,
                    localVarContentType,
                    localVarBodyParameters,
                    "GET");

    //nonprimitive not container
    pet_t *elementToReturn = pet_parseFromJSON(apiClient->dataReceived);
    if(elementToReturn == NULL) {
        return 0;
    }

    //return type
    apiClient_free(apiClient);
    list_free(localVarQueryParameters);
    list_free(localVarHeaderParameters);
    list_free(localVarFormParameters);
    list_free(localVarHeaderType);
    list_free(localVarContentType);
    free(localVarPath);
    free(localVarToReplace);
   return elementToReturn;



}

// Update an existing pet
//
void *PetAPI_updatePet(apiClient_t *apiClient, pet_t* pet) {
    list_t    *localVarQueryParameters = list_create();
    list_t    *localVarHeaderParameters = list_create();
    list_t    *localVarFormParameters = list_create();
    list_t *localVarHeaderType = list_create();
    list_t *localVarContentType = list_create();
    char      *localVarBodyParameters = NULL;

    // create the path
    char *localVarPath = malloc(MAX_BUFFER_LENGTH);
    snprintf(localVarPath, MAX_BUFFER_LENGTH, "/pet");

    // Body Param
    //string
    cJSON *localVarSingleItemJSON_pet;
    localVarSingleItemJSON_pet = pet_convertToJSON(pet);
    localVarBodyParameters = cJSON_Print(localVarSingleItemJSON_pet);

        list_addElement(localVarContentType,"application/json"); //consumes

        list_addElement(localVarContentType,"application/xml"); //consumes

    apiClient_invoke(apiClient,
                    localVarPath,
                    localVarQueryParameters,
                    localVarHeaderParameters,
                    localVarFormParameters,
                    localVarHeaderType,
                    localVarContentType,
                    localVarBodyParameters,
                    "PUT");

    //No return type
    apiClient_free(apiClient);
    list_free(localVarQueryParameters);
    list_free(localVarHeaderParameters);
    list_free(localVarFormParameters);
    list_free(localVarHeaderType);
    list_free(localVarContentType);
    free(localVarPath);

}

// Updates a pet in the store with form data
//
void *PetAPI_updatePetWithForm(apiClient_t *apiClient, long petId, char* name, char* status) {
    list_t    *localVarQueryParameters = list_create();
    list_t    *localVarHeaderParameters = list_create();
    list_t    *localVarFormParameters = list_create();
    list_t *localVarHeaderType = list_create();
    list_t *localVarContentType = list_create();
    char      *localVarBodyParameters = NULL;

    // create the path
    char *localVarPath = malloc(MAX_BUFFER_LENGTH);
    snprintf(localVarPath, MAX_BUFFER_LENGTH, "/pet/{petId}");

    // Path Params
    char* localVarToReplace = malloc(sizeof(petId)+3);
    snprintf(localVarToReplace, strlen("petId")+3, "%s%s%s", "{", "petId", "}");

    char localVarBuff[256];
    intToStr(localVarBuff, petId);

    localVarPath = strReplace(localVarPath, localVarToReplace, localVarBuff);


    // form parameters
    char *keyForm_name;
    char *valueForm_name;
    keyValuePair_t *keyPairForm_name = 0;
    if (name)
    {
        keyForm_name = strdup("name");
        valueForm_name = strdup(name);
        keyPairForm_name = keyValuePair_create(keyForm_name,valueForm_name);
        list_addElement(localVarFormParameters,keyPairForm_name); //String
    }

    // form parameters
    char *keyForm_status;
    char *valueForm_status;
    keyValuePair_t *keyPairForm_status = 0;
    if (status)
    {
        keyForm_status = strdup("status");
        valueForm_status = strdup(status);
        keyPairForm_status = keyValuePair_create(keyForm_status,valueForm_status);
        list_addElement(localVarFormParameters,keyPairForm_status); //String
    }

        list_addElement(localVarContentType,"application/x-www-form-urlencoded"); //consumes

    apiClient_invoke(apiClient,
                    localVarPath,
                    localVarQueryParameters,
                    localVarHeaderParameters,
                    localVarFormParameters,
                    localVarHeaderType,
                    localVarContentType,
                    localVarBodyParameters,
                    "POST");

    //No return type
    apiClient_free(apiClient);
    list_free(localVarQueryParameters);
    list_free(localVarHeaderParameters);
    list_free(localVarFormParameters);
    list_free(localVarHeaderType);
    list_free(localVarContentType);
    free(localVarPath);
    free(localVarToReplace);
    free(keyForm_name);
    free(valueForm_name);
    free(keyForm_status);
    free(valueForm_status);

}

// uploads an image
//
api_response_t *PetAPI_uploadFile(apiClient_t *apiClient, long petId, char* additionalMetadata, FILE* file) {
    list_t    *localVarQueryParameters = list_create();
    list_t    *localVarHeaderParameters = list_create();
    list_t    *localVarFormParameters = list_create();
    list_t *localVarHeaderType = list_create();
    list_t *localVarContentType = list_create();
    char      *localVarBodyParameters = NULL;

    // create the path
    char *localVarPath = malloc(MAX_BUFFER_LENGTH);
    snprintf(localVarPath, MAX_BUFFER_LENGTH, "/pet/{petId}/uploadImage");

    // Path Params
    char* localVarToReplace = malloc(sizeof(petId)+3);
    snprintf(localVarToReplace, strlen("petId")+3, "%s%s%s", "{", "petId", "}");

    char localVarBuff[256];
    intToStr(localVarBuff, petId);

    localVarPath = strReplace(localVarPath, localVarToReplace, localVarBuff);


    // form parameters
    char *keyForm_additionalMetadata;
    char *valueForm_additionalMetadata;
    keyValuePair_t *keyPairForm_additionalMetadata = 0;
    if (additionalMetadata)
    {
        keyForm_additionalMetadata = strdup("additionalMetadata");
        valueForm_additionalMetadata = strdup(additionalMetadata);
        keyPairForm_additionalMetadata = keyValuePair_create(keyForm_additionalMetadata,valueForm_additionalMetadata);
        list_addElement(localVarFormParameters,keyPairForm_additionalMetadata); //String
    }

    // form parameters
    fseek(file, 0, SEEK_END);
    long f_size = ftell(file);
    fseek(file, 0, SEEK_SET);
    char *keyForm_file;
    FileStruct *fileVar_file = malloc(sizeof(FileStruct));
    keyValuePair_t *keyPairForm_file = 0;
    if (file != NULL)
    {
        keyForm_file = strdup("file");
        fileVar_file->fileData = malloc((f_size)* sizeof(char*));

        fread(fileVar_file->fileData, f_size, 1, file);
        fileVar_file->fileData[f_size] = '\0';

        fileVar_file->fileSize = f_size;
        char valueFile[sizeof(fileVar_file)];

        memcpy(valueFile,&fileVar_file, sizeof(fileVar_file));

        keyPairForm_file = keyValuePair_create(keyForm_file,valueFile);
        list_addElement(localVarFormParameters,keyPairForm_file); //file adding
    }

        list_addElement(localVarHeaderType,"application/json"); //produces

        list_addElement(localVarContentType,"multipart/form-data"); //consumes

    apiClient_invoke(apiClient,
                    localVarPath,
                    localVarQueryParameters,
                    localVarHeaderParameters,
                    localVarFormParameters,
                    localVarHeaderType,
                    localVarContentType,
                    localVarBodyParameters,
                    "POST");

    //nonprimitive not container
    api_response_t *elementToReturn = api_response_parseFromJSON(apiClient->dataReceived);
    if(elementToReturn == NULL) {
        return 0;
    }

    //return type
    apiClient_free(apiClient);
    list_free(localVarQueryParameters);
    list_free(localVarHeaderParameters);
    list_free(localVarFormParameters);
    list_free(localVarHeaderType);
    list_free(localVarContentType);
    free(localVarPath);
    free(localVarToReplace);
    free(keyForm_additionalMetadata);
    free(valueForm_additionalMetadata);
    free(keyPairForm_additionalMetadata);
    free(keyForm_file);
    free(fileVar_file->fileData);
    free(fileVar_file);
    free(keyPairForm_file);
   return elementToReturn;



}



