#include <stdlib.h>
#include <stdio.h>
#include <ctype.h>
#include "PetAPI.h"


#define MAX_BUFFER_LENGTH 4096
#define intToStr(dst, src) \
    do {\
    char dst[256];\
    snprintf(dst, 256, "%ld", (long int)(src));\
}while(0)

// Add a new pet to the store
//
void
PetAPI_addPet(apiClient_t *apiClient ,pet_t * body)
{
    list_t    *localVarQueryParameters = NULL;
    list_t    *localVarHeaderParameters = NULL;
    list_t    *localVarFormParameters = NULL;
    list_t *localVarHeaderType = NULL;
    list_t *localVarContentType = list_create();
    char      *localVarBodyParameters = NULL;

    // create the path
    long sizeOfPath = strlen("/pet")+1;
    char *localVarPath = malloc(sizeOfPath);
    snprintf(localVarPath, sizeOfPath, "/pet");




    // Body Param
    cJSON *localVarSingleItemJSON_body;
    if (body != NULL)
    {
        //string
        localVarSingleItemJSON_body = pet_convertToJSON(body);
        localVarBodyParameters = cJSON_Print(localVarSingleItemJSON_body);
    }
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

    if (apiClient->response_code == 405) {
        printf("%s\n","Invalid input");
    }
    //No return type
end:
    if (apiClient->dataReceived) {
        free(apiClient->dataReceived);
    }
    
    
    
    
    list_free(localVarContentType);
    free(localVarPath);
    cJSON_Delete(localVarSingleItemJSON_body);
    free(localVarBodyParameters);

}

// Deletes a pet
//
void
PetAPI_deletePet(apiClient_t *apiClient ,long petId ,char * api_key)
{
    list_t    *localVarQueryParameters = NULL;
    list_t    *localVarHeaderParameters = list_create();
    list_t    *localVarFormParameters = NULL;
    list_t *localVarHeaderType = NULL;
    list_t *localVarContentType = NULL;
    char      *localVarBodyParameters = NULL;

    // create the path
    long sizeOfPath = strlen("/pet/{petId}")+1;
    char *localVarPath = malloc(sizeOfPath);
    snprintf(localVarPath, sizeOfPath, "/pet/{petId}");


    // Path Params
    long sizeOfPathParams_petId = sizeof(petId)+3 + strlen("{ petId }");
    if(petId == 0){
        goto end;
    }
    char* localVarToReplace_petId = malloc(sizeOfPathParams_petId);
    snprintf(localVarToReplace_petId, sizeOfPathParams_petId, "{%s}", "petId");

    char localVarBuff_petId[256];
    intToStr(localVarBuff_petId, petId);

    localVarPath = strReplace(localVarPath, localVarToReplace_petId, localVarBuff_petId);




    // header parameters
    char *keyHeader_api_key;
    char * valueHeader_api_key;
    keyValuePair_t *keyPairHeader_api_key = 0;
    if (api_key) {
        keyHeader_api_key = strdup("api_key");
        valueHeader_api_key = strdup((api_key));
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

    if (apiClient->response_code == 400) {
        printf("%s\n","Invalid pet value");
    }
    //No return type
end:
    if (apiClient->dataReceived) {
        free(apiClient->dataReceived);
    }
    
    list_free(localVarHeaderParameters);
    
    
    
    free(localVarPath);
    free(localVarToReplace_petId);
    free(keyHeader_api_key);
    free(valueHeader_api_key);
    free(keyPairHeader_api_key);

}

// Finds Pets by status
//
// Multiple status values can be provided with comma separated strings
//
list_t*
PetAPI_findPetsByStatus(apiClient_t *apiClient ,list_t * status)
{
    list_t    *localVarQueryParameters = list_create();
    list_t    *localVarHeaderParameters = NULL;
    list_t    *localVarFormParameters = NULL;
    list_t *localVarHeaderType = list_create();
    list_t *localVarContentType = NULL;
    char      *localVarBodyParameters = NULL;

    // create the path
    long sizeOfPath = strlen("/pet/findByStatus")+1;
    char *localVarPath = malloc(sizeOfPath);
    snprintf(localVarPath, sizeOfPath, "/pet/findByStatus");




    // query parameters
    if (status)
    {
        list_addElement(localVarQueryParameters,status);
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

    if (apiClient->response_code == 200) {
        printf("%s\n","successful operation");
    }
    if (apiClient->response_code == 400) {
        printf("%s\n","Invalid status value");
    }
    cJSON *PetAPIlocalVarJSON = cJSON_Parse(apiClient->dataReceived);
    if(!cJSON_IsArray(PetAPIlocalVarJSON)) {
        return 0;//nonprimitive container
    }
    list_t *elementToReturn = list_create();
    cJSON *VarJSON;
    cJSON_ArrayForEach(VarJSON, PetAPIlocalVarJSON)
    {
        if(!cJSON_IsObject(VarJSON))
        {
           // return 0;
        }
        char *localVarJSONToChar = cJSON_Print(VarJSON);
        list_addElement(elementToReturn , localVarJSONToChar);
    }

    cJSON_Delete( PetAPIlocalVarJSON);
    cJSON_Delete( VarJSON);
    //return type
    if (apiClient->dataReceived) {
        free(apiClient->dataReceived);
    }
    list_free(localVarQueryParameters);
    
    
    list_free(localVarHeaderType);
    
    free(localVarPath);
    return elementToReturn;
end:
    return NULL;

}

// Finds Pets by tags
//
// Multiple tags can be provided with comma separated strings. Use tag1, tag2, tag3 for testing.
//
list_t*
PetAPI_findPetsByTags(apiClient_t *apiClient ,list_t * tags)
{
    list_t    *localVarQueryParameters = list_create();
    list_t    *localVarHeaderParameters = NULL;
    list_t    *localVarFormParameters = NULL;
    list_t *localVarHeaderType = list_create();
    list_t *localVarContentType = NULL;
    char      *localVarBodyParameters = NULL;

    // create the path
    long sizeOfPath = strlen("/pet/findByTags")+1;
    char *localVarPath = malloc(sizeOfPath);
    snprintf(localVarPath, sizeOfPath, "/pet/findByTags");




    // query parameters
    if (tags)
    {
        list_addElement(localVarQueryParameters,tags);
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

    if (apiClient->response_code == 200) {
        printf("%s\n","successful operation");
    }
    if (apiClient->response_code == 400) {
        printf("%s\n","Invalid tag value");
    }
    cJSON *PetAPIlocalVarJSON = cJSON_Parse(apiClient->dataReceived);
    if(!cJSON_IsArray(PetAPIlocalVarJSON)) {
        return 0;//nonprimitive container
    }
    list_t *elementToReturn = list_create();
    cJSON *VarJSON;
    cJSON_ArrayForEach(VarJSON, PetAPIlocalVarJSON)
    {
        if(!cJSON_IsObject(VarJSON))
        {
           // return 0;
        }
        char *localVarJSONToChar = cJSON_Print(VarJSON);
        list_addElement(elementToReturn , localVarJSONToChar);
    }

    cJSON_Delete( PetAPIlocalVarJSON);
    cJSON_Delete( VarJSON);
    //return type
    if (apiClient->dataReceived) {
        free(apiClient->dataReceived);
    }
    list_free(localVarQueryParameters);
    
    
    list_free(localVarHeaderType);
    
    free(localVarPath);
    return elementToReturn;
end:
    return NULL;

}

// Find pet by ID
//
// Returns a single pet
//
pet_t*
PetAPI_getPetById(apiClient_t *apiClient ,long petId)
{
    list_t    *localVarQueryParameters = NULL;
    list_t    *localVarHeaderParameters = NULL;
    list_t    *localVarFormParameters = NULL;
    list_t *localVarHeaderType = list_create();
    list_t *localVarContentType = NULL;
    char      *localVarBodyParameters = NULL;

    // create the path
    long sizeOfPath = strlen("/pet/{petId}")+1;
    char *localVarPath = malloc(sizeOfPath);
    snprintf(localVarPath, sizeOfPath, "/pet/{petId}");


    // Path Params
    long sizeOfPathParams_petId = sizeof(petId)+3 + strlen("{ petId }");
    if(petId == 0){
        goto end;
    }
    char* localVarToReplace_petId = malloc(sizeOfPathParams_petId);
    snprintf(localVarToReplace_petId, sizeOfPathParams_petId, "{%s}", "petId");

    char localVarBuff_petId[256];
    intToStr(localVarBuff_petId, petId);

    localVarPath = strReplace(localVarPath, localVarToReplace_petId, localVarBuff_petId);



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

    if (apiClient->response_code == 200) {
        printf("%s\n","successful operation");
    }
    if (apiClient->response_code == 400) {
        printf("%s\n","Invalid ID supplied");
    }
    if (apiClient->response_code == 404) {
        printf("%s\n","Pet not found");
    }
    //nonprimitive not container
    cJSON *PetAPIlocalVarJSON = cJSON_Parse(apiClient->dataReceived);
    pet_t *elementToReturn = pet_parseFromJSON(PetAPIlocalVarJSON);
    cJSON_Delete(PetAPIlocalVarJSON);
    if(elementToReturn == NULL) {
        // return 0;
    }

    //return type
    if (apiClient->dataReceived) {
        free(apiClient->dataReceived);
    }
    
    
    
    list_free(localVarHeaderType);
    
    free(localVarPath);
    free(localVarToReplace_petId);
    return elementToReturn;
end:
    return NULL;

}

// Update an existing pet
//
void
PetAPI_updatePet(apiClient_t *apiClient ,pet_t * body)
{
    list_t    *localVarQueryParameters = NULL;
    list_t    *localVarHeaderParameters = NULL;
    list_t    *localVarFormParameters = NULL;
    list_t *localVarHeaderType = NULL;
    list_t *localVarContentType = list_create();
    char      *localVarBodyParameters = NULL;

    // create the path
    long sizeOfPath = strlen("/pet")+1;
    char *localVarPath = malloc(sizeOfPath);
    snprintf(localVarPath, sizeOfPath, "/pet");




    // Body Param
    cJSON *localVarSingleItemJSON_body;
    if (body != NULL)
    {
        //string
        localVarSingleItemJSON_body = pet_convertToJSON(body);
        localVarBodyParameters = cJSON_Print(localVarSingleItemJSON_body);
    }
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

    if (apiClient->response_code == 400) {
        printf("%s\n","Invalid ID supplied");
    }
    if (apiClient->response_code == 404) {
        printf("%s\n","Pet not found");
    }
    if (apiClient->response_code == 405) {
        printf("%s\n","Validation exception");
    }
    //No return type
end:
    if (apiClient->dataReceived) {
        free(apiClient->dataReceived);
    }
    
    
    
    
    list_free(localVarContentType);
    free(localVarPath);
    cJSON_Delete(localVarSingleItemJSON_body);
    free(localVarBodyParameters);

}

// Updates a pet in the store with form data
//
void
PetAPI_updatePetWithForm(apiClient_t *apiClient ,long petId ,char * name ,char * status)
{
    list_t    *localVarQueryParameters = NULL;
    list_t    *localVarHeaderParameters = NULL;
    list_t    *localVarFormParameters = list_create();
    list_t *localVarHeaderType = NULL;
    list_t *localVarContentType = list_create();
    char      *localVarBodyParameters = NULL;

    // create the path
    long sizeOfPath = strlen("/pet/{petId}")+1;
    char *localVarPath = malloc(sizeOfPath);
    snprintf(localVarPath, sizeOfPath, "/pet/{petId}");


    // Path Params
    long sizeOfPathParams_petId = sizeof(petId)+3 + strlen("{ petId }");
    if(petId == 0){
        goto end;
    }
    char* localVarToReplace_petId = malloc(sizeOfPathParams_petId);
    snprintf(localVarToReplace_petId, sizeOfPathParams_petId, "{%s}", "petId");

    char localVarBuff_petId[256];
    intToStr(localVarBuff_petId, petId);

    localVarPath = strReplace(localVarPath, localVarToReplace_petId, localVarBuff_petId);




    // form parameters
    char *keyForm_name;
    char * valueForm_name;
    keyValuePair_t *keyPairForm_name = 0;
    if (name != NULL)
    {
        keyForm_name = strdup("name");
        valueForm_name = strdup((name));
        keyPairForm_name = keyValuePair_create(keyForm_name,valueForm_name);
        list_addElement(localVarFormParameters,keyPairForm_name);
    }

    // form parameters
    char *keyForm_status;
    char * valueForm_status;
    keyValuePair_t *keyPairForm_status = 0;
    if (status != NULL)
    {
        keyForm_status = strdup("status");
        valueForm_status = strdup((status));
        keyPairForm_status = keyValuePair_create(keyForm_status,valueForm_status);
        list_addElement(localVarFormParameters,keyPairForm_status);
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

    if (apiClient->response_code == 405) {
        printf("%s\n","Invalid input");
    }
    //No return type
end:
    if (apiClient->dataReceived) {
        free(apiClient->dataReceived);
    }
    
    
    list_free(localVarFormParameters);
    
    list_free(localVarContentType);
    free(localVarPath);
    free(localVarToReplace_petId);
    free(keyForm_name);
    free(valueForm_name);
    keyValuePair_free(keyPairForm_name);
    free(keyForm_status);
    free(valueForm_status);
    keyValuePair_free(keyPairForm_status);

}

// uploads an image
//
api_response_t*
PetAPI_uploadFile(apiClient_t *apiClient ,long petId ,char * additionalMetadata ,binary_t* file)
{
    list_t    *localVarQueryParameters = NULL;
    list_t    *localVarHeaderParameters = NULL;
    list_t    *localVarFormParameters = list_create();
    list_t *localVarHeaderType = list_create();
    list_t *localVarContentType = list_create();
    char      *localVarBodyParameters = NULL;

    // create the path
    long sizeOfPath = strlen("/pet/{petId}/uploadImage")+1;
    char *localVarPath = malloc(sizeOfPath);
    snprintf(localVarPath, sizeOfPath, "/pet/{petId}/uploadImage");


    // Path Params
    long sizeOfPathParams_petId = sizeof(petId)+3 + strlen("{ petId }");
    if(petId == 0){
        goto end;
    }
    char* localVarToReplace_petId = malloc(sizeOfPathParams_petId);
    snprintf(localVarToReplace_petId, sizeOfPathParams_petId, "{%s}", "petId");

    char localVarBuff_petId[256];
    intToStr(localVarBuff_petId, petId);

    localVarPath = strReplace(localVarPath, localVarToReplace_petId, localVarBuff_petId);




    // form parameters
    char *keyForm_additionalMetadata;
    char * valueForm_additionalMetadata;
    keyValuePair_t *keyPairForm_additionalMetadata = 0;
    if (additionalMetadata != NULL)
    {
        keyForm_additionalMetadata = strdup("additionalMetadata");
        valueForm_additionalMetadata = strdup((additionalMetadata));
        keyPairForm_additionalMetadata = keyValuePair_create(keyForm_additionalMetadata,valueForm_additionalMetadata);
        list_addElement(localVarFormParameters,keyPairForm_additionalMetadata);
    }

    // form parameters
    char *keyForm_file;
    binary_t* valueForm_file;
    keyValuePair_t *keyPairForm_file = 0;
    if (file != NULL)
    {
        keyForm_file = strdup("file");
        valueForm_file = file;
        keyPairForm_file = keyValuePair_create(keyForm_file, &valueForm_file);
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

    if (apiClient->response_code == 200) {
        printf("%s\n","successful operation");
    }
    //nonprimitive not container
    cJSON *PetAPIlocalVarJSON = cJSON_Parse(apiClient->dataReceived);
    api_response_t *elementToReturn = api_response_parseFromJSON(PetAPIlocalVarJSON);
    cJSON_Delete(PetAPIlocalVarJSON);
    if(elementToReturn == NULL) {
        // return 0;
    }

    //return type
    if (apiClient->dataReceived) {
        free(apiClient->dataReceived);
    }
    
    
    list_free(localVarFormParameters);
    list_free(localVarHeaderType);
    list_free(localVarContentType);
    free(localVarPath);
    free(localVarToReplace_petId);
    free(keyForm_additionalMetadata);
    free(valueForm_additionalMetadata);
    free(keyPairForm_additionalMetadata);
    free(keyForm_file);
//    free(fileVar_file->data);
//    free(fileVar_file);
    free(keyPairForm_file);
    return elementToReturn;
end:
    return NULL;

}

