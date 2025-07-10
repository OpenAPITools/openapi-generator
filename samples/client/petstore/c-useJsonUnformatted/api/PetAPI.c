#include <stdlib.h>
#include <stdio.h>
#include <ctype.h>

#include "PetAPI.h"

#define MAX_NUMBER_LENGTH 16
#define MAX_BUFFER_LENGTH 4096
#define MAX_NUMBER_LENGTH_LONG 21

// Functions for enum STATUS for PetAPI_findPetsByStatus

static char* findPetsByStatus_STATUS_ToString(openapi_petstore_findPetsByStatus_status_e STATUS){
    char *STATUSArray[] =  { "NULL", "available", "pending", "sold" };
    return STATUSArray[STATUS];
}

static openapi_petstore_findPetsByStatus_status_e findPetsByStatus_STATUS_FromString(char* STATUS){
    int stringToReturn = 0;
    char *STATUSArray[] =  { "NULL", "available", "pending", "sold" };
    size_t sizeofArray = sizeof(STATUSArray) / sizeof(STATUSArray[0]);
    while(stringToReturn < sizeofArray) {
        if(strcmp(STATUS, STATUSArray[stringToReturn]) == 0) {
            return stringToReturn;
        }
        stringToReturn++;
    }
    return 0;
}

/*
// Function findPetsByStatus_STATUS_convertToJSON is not currently used,
// since conversion to JSON passes through the conversion of the model, and ToString. The function is kept for future reference.
//
static cJSON *findPetsByStatus_STATUS_convertToJSON(openapi_petstore_findPetsByStatus_status_e STATUS) {
    cJSON *item = cJSON_CreateObject();
    return item;
    fail:
    cJSON_Delete(item);
    return NULL;
}

// Function findPetsByStatus_STATUS_parseFromJSON is not currently used,
// since conversion from JSON passes through the conversion of the model, and FromString. The function is kept for future reference.
//
static openapi_petstore_findPetsByStatus_status_e findPetsByStatus_STATUS_parseFromJSON(cJSON* STATUSJSON) {
    openapi_petstore_findPetsByStatus_status_e STATUSVariable = 0;
    return STATUSVariable;
end:
    return 0;
}
*/


// Add a new pet to the store
//
void
PetAPI_addPet(apiClient_t *apiClient, pet_t *body)
{
    list_t    *localVarQueryParameters = NULL;
    list_t    *localVarHeaderParameters = NULL;
    list_t    *localVarFormParameters = NULL;
    list_t *localVarHeaderType = NULL;
    list_t *localVarContentType = list_createList();
    char      *localVarBodyParameters = NULL;
    size_t     localVarBodyLength = 0;

    // clear the error code from the previous api call
    apiClient->response_code = 0;

    // create the path
    char *localVarPath = strdup("/pet");





    // Body Param
    cJSON *localVarSingleItemJSON_body = NULL;
    if (body != NULL)
    {
        //not string, not binary
        localVarSingleItemJSON_body = pet_convertToJSON(body);
        localVarBodyParameters = cJSON_PrintUnformatted(localVarSingleItemJSON_body);
        localVarBodyLength = strlen(localVarBodyParameters);
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
                    localVarBodyLength,
                    "POST");

    // uncomment below to debug the error response
    //if (apiClient->response_code == 405) {
    //    printf("%s\n","Invalid input");
    //}
    //No return type
end:
    if (apiClient->dataReceived) {
        free(apiClient->dataReceived);
        apiClient->dataReceived = NULL;
        apiClient->dataReceivedLen = 0;
    }
    
    
    
    
    list_freeList(localVarContentType);
    free(localVarPath);
    if (localVarSingleItemJSON_body) {
        cJSON_Delete(localVarSingleItemJSON_body);
        localVarSingleItemJSON_body = NULL;
    }
    free(localVarBodyParameters);

}

// Deletes a pet
//
void
PetAPI_deletePet(apiClient_t *apiClient, long petId, char *api_key)
{
    list_t    *localVarQueryParameters = NULL;
    list_t    *localVarHeaderParameters = list_createList();
    list_t    *localVarFormParameters = NULL;
    list_t *localVarHeaderType = NULL;
    list_t *localVarContentType = NULL;
    char      *localVarBodyParameters = NULL;
    size_t     localVarBodyLength = 0;

    // clear the error code from the previous api call
    apiClient->response_code = 0;

    // create the path
    char *localVarPath = strdup("/pet/{petId}");



    // Path Params
    long sizeOfPathParams_petId = sizeof(petId)+3 + sizeof("{ petId }") - 1;
    if(petId == 0){
        goto end;
    }
    char* localVarToReplace_petId = malloc(sizeOfPathParams_petId);
    snprintf(localVarToReplace_petId, sizeOfPathParams_petId, "{%s}", "petId");

    char localVarBuff_petId[256];
    snprintf(localVarBuff_petId, sizeof localVarBuff_petId, "%ld", petId);

    localVarPath = strReplace(localVarPath, localVarToReplace_petId, localVarBuff_petId);




    // header parameters
    char *keyHeader_api_key = NULL;
    char * valueHeader_api_key = 0;
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
                    localVarBodyLength,
                    "DELETE");

    // uncomment below to debug the error response
    //if (apiClient->response_code == 400) {
    //    printf("%s\n","Invalid pet value");
    //}
    //No return type
end:
    if (apiClient->dataReceived) {
        free(apiClient->dataReceived);
        apiClient->dataReceived = NULL;
        apiClient->dataReceivedLen = 0;
    }
    
    list_freeList(localVarHeaderParameters);
    
    
    
    free(localVarPath);
    free(localVarToReplace_petId);
    if (keyHeader_api_key) {
        free(keyHeader_api_key);
        keyHeader_api_key = NULL;
    }
    if (valueHeader_api_key) {
        free(valueHeader_api_key);
        valueHeader_api_key = NULL;
    }
    free(keyPairHeader_api_key);

}

// Finds Pets by status
//
// Multiple status values can be provided with comma separated strings
//
list_t*
PetAPI_findPetsByStatus(apiClient_t *apiClient, list_t *status)
{
    list_t    *localVarQueryParameters = list_createList();
    list_t    *localVarHeaderParameters = NULL;
    list_t    *localVarFormParameters = NULL;
    list_t *localVarHeaderType = list_createList();
    list_t *localVarContentType = NULL;
    char      *localVarBodyParameters = NULL;
    size_t     localVarBodyLength = 0;

    // clear the error code from the previous api call
    apiClient->response_code = 0;

    // create the path
    char *localVarPath = strdup("/pet/findByStatus");





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
                    localVarBodyLength,
                    "GET");

    // uncomment below to debug the error response
    //if (apiClient->response_code == 200) {
    //    printf("%s\n","successful operation");
    //}
    // uncomment below to debug the error response
    //if (apiClient->response_code == 400) {
    //    printf("%s\n","Invalid status value");
    //}
    list_t *elementToReturn = NULL;
    if(apiClient->response_code >= 200 && apiClient->response_code < 300) {
        cJSON *PetAPIlocalVarJSON = cJSON_Parse(apiClient->dataReceived);
        if(!cJSON_IsArray(PetAPIlocalVarJSON)) {
            return 0;//nonprimitive container
        }
        elementToReturn = list_createList();
        cJSON *VarJSON;
        cJSON_ArrayForEach(VarJSON, PetAPIlocalVarJSON)
        {
            if(!cJSON_IsObject(VarJSON))
            {
               // return 0;
            }
            char *localVarJSONToChar = cJSON_PrintUnformatted(VarJSON);
            list_addElement(elementToReturn , localVarJSONToChar);
        }

        cJSON_Delete( PetAPIlocalVarJSON);
        cJSON_Delete( VarJSON);
    }
    //return type
    if (apiClient->dataReceived) {
        free(apiClient->dataReceived);
        apiClient->dataReceived = NULL;
        apiClient->dataReceivedLen = 0;
    }
    list_freeList(localVarQueryParameters);
    
    
    list_freeList(localVarHeaderType);
    
    free(localVarPath);
    return elementToReturn;
end:
    free(localVarPath);
    return NULL;

}

// Finds Pets by tags
//
// Multiple tags can be provided with comma separated strings. Use tag1, tag2, tag3 for testing.
//
list_t*
PetAPI_findPetsByTags(apiClient_t *apiClient, list_t *tags)
{
    list_t    *localVarQueryParameters = list_createList();
    list_t    *localVarHeaderParameters = NULL;
    list_t    *localVarFormParameters = NULL;
    list_t *localVarHeaderType = list_createList();
    list_t *localVarContentType = NULL;
    char      *localVarBodyParameters = NULL;
    size_t     localVarBodyLength = 0;

    // clear the error code from the previous api call
    apiClient->response_code = 0;

    // create the path
    char *localVarPath = strdup("/pet/findByTags");





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
                    localVarBodyLength,
                    "GET");

    // uncomment below to debug the error response
    //if (apiClient->response_code == 200) {
    //    printf("%s\n","successful operation");
    //}
    // uncomment below to debug the error response
    //if (apiClient->response_code == 400) {
    //    printf("%s\n","Invalid tag value");
    //}
    list_t *elementToReturn = NULL;
    if(apiClient->response_code >= 200 && apiClient->response_code < 300) {
        cJSON *PetAPIlocalVarJSON = cJSON_Parse(apiClient->dataReceived);
        if(!cJSON_IsArray(PetAPIlocalVarJSON)) {
            return 0;//nonprimitive container
        }
        elementToReturn = list_createList();
        cJSON *VarJSON;
        cJSON_ArrayForEach(VarJSON, PetAPIlocalVarJSON)
        {
            if(!cJSON_IsObject(VarJSON))
            {
               // return 0;
            }
            char *localVarJSONToChar = cJSON_PrintUnformatted(VarJSON);
            list_addElement(elementToReturn , localVarJSONToChar);
        }

        cJSON_Delete( PetAPIlocalVarJSON);
        cJSON_Delete( VarJSON);
    }
    //return type
    if (apiClient->dataReceived) {
        free(apiClient->dataReceived);
        apiClient->dataReceived = NULL;
        apiClient->dataReceivedLen = 0;
    }
    list_freeList(localVarQueryParameters);
    
    
    list_freeList(localVarHeaderType);
    
    free(localVarPath);
    return elementToReturn;
end:
    free(localVarPath);
    return NULL;

}

// Number of days since the last time a pet maimed someone at the store
//
int
PetAPI_getDaysWithoutIncident(apiClient_t *apiClient)
{
    list_t    *localVarQueryParameters = NULL;
    list_t    *localVarHeaderParameters = NULL;
    list_t    *localVarFormParameters = NULL;
    list_t *localVarHeaderType = list_createList();
    list_t *localVarContentType = NULL;
    char      *localVarBodyParameters = NULL;
    size_t     localVarBodyLength = 0;

    // clear the error code from the previous api call
    apiClient->response_code = 0;

    // create the path
    char *localVarPath = strdup("/store/daysWithoutIncident");




    list_addElement(localVarHeaderType,"*/*"); //produces
    apiClient_invoke(apiClient,
                    localVarPath,
                    localVarQueryParameters,
                    localVarHeaderParameters,
                    localVarFormParameters,
                    localVarHeaderType,
                    localVarContentType,
                    localVarBodyParameters,
                    localVarBodyLength,
                    "GET");

    // uncomment below to debug the error response
    //if (apiClient->response_code == 200) {
    //    printf("%s\n","successful operation");
    //}
    //primitive return type simple integer
    int elementToReturn = 0;
    if(apiClient->response_code >= 200 && apiClient->response_code < 300)
        elementToReturn = atoi(apiClient->dataReceived);

    if (apiClient->dataReceived) {
        free(apiClient->dataReceived);
        apiClient->dataReceived = NULL;
        apiClient->dataReceivedLen = 0;
    }
    
    
    
    list_freeList(localVarHeaderType);
    
    free(localVarPath);
    return elementToReturn;
end:
    free(localVarPath);
    return 0;

}

// Find pet by ID
//
// Returns a single pet
//
pet_t*
PetAPI_getPetById(apiClient_t *apiClient, long petId)
{
    list_t    *localVarQueryParameters = NULL;
    list_t    *localVarHeaderParameters = NULL;
    list_t    *localVarFormParameters = NULL;
    list_t *localVarHeaderType = list_createList();
    list_t *localVarContentType = NULL;
    char      *localVarBodyParameters = NULL;
    size_t     localVarBodyLength = 0;

    // clear the error code from the previous api call
    apiClient->response_code = 0;

    // create the path
    char *localVarPath = strdup("/pet/{petId}");



    // Path Params
    long sizeOfPathParams_petId = sizeof(petId)+3 + sizeof("{ petId }") - 1;
    if(petId == 0){
        goto end;
    }
    char* localVarToReplace_petId = malloc(sizeOfPathParams_petId);
    snprintf(localVarToReplace_petId, sizeOfPathParams_petId, "{%s}", "petId");

    char localVarBuff_petId[256];
    snprintf(localVarBuff_petId, sizeof localVarBuff_petId, "%ld", petId);

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
                    localVarBodyLength,
                    "GET");

    // uncomment below to debug the error response
    //if (apiClient->response_code == 200) {
    //    printf("%s\n","successful operation");
    //}
    // uncomment below to debug the error response
    //if (apiClient->response_code == 400) {
    //    printf("%s\n","Invalid ID supplied");
    //}
    // uncomment below to debug the error response
    //if (apiClient->response_code == 404) {
    //    printf("%s\n","Pet not found");
    //}
    //nonprimitive not container
    pet_t *elementToReturn = NULL;
    if(apiClient->response_code >= 200 && apiClient->response_code < 300) {
        cJSON *PetAPIlocalVarJSON = cJSON_Parse(apiClient->dataReceived);
        elementToReturn = pet_parseFromJSON(PetAPIlocalVarJSON);
        cJSON_Delete(PetAPIlocalVarJSON);
        if(elementToReturn == NULL) {
            // return 0;
        }
    }

    //return type
    if (apiClient->dataReceived) {
        free(apiClient->dataReceived);
        apiClient->dataReceived = NULL;
        apiClient->dataReceivedLen = 0;
    }
    
    
    
    list_freeList(localVarHeaderType);
    
    free(localVarPath);
    free(localVarToReplace_petId);
    return elementToReturn;
end:
    free(localVarPath);
    return NULL;

}

// Get a random picture of someone else's pet
//
binary_t*
PetAPI_getPicture(apiClient_t *apiClient)
{
    list_t    *localVarQueryParameters = NULL;
    list_t    *localVarHeaderParameters = NULL;
    list_t    *localVarFormParameters = NULL;
    list_t *localVarHeaderType = list_createList();
    list_t *localVarContentType = NULL;
    char      *localVarBodyParameters = NULL;
    size_t     localVarBodyLength = 0;

    // clear the error code from the previous api call
    apiClient->response_code = 0;

    // create the path
    char *localVarPath = strdup("/pet/picture");




    list_addElement(localVarHeaderType,"*/*"); //produces
    apiClient_invoke(apiClient,
                    localVarPath,
                    localVarQueryParameters,
                    localVarHeaderParameters,
                    localVarFormParameters,
                    localVarHeaderType,
                    localVarContentType,
                    localVarBodyParameters,
                    localVarBodyLength,
                    "GET");

    // uncomment below to debug the error response
    //if (apiClient->response_code == 200) {
    //    printf("%s\n","successful operation");
    //}
    //primitive return type simple binary
    binary_t* elementToReturn = NULL;
    if(apiClient->response_code >= 200 && apiClient->response_code < 300)
        elementToReturn = instantiate_binary_t(apiClient->dataReceived, apiClient->dataReceivedLen);

    if (apiClient->dataReceived) {
        free(apiClient->dataReceived);
        apiClient->dataReceived = NULL;
        apiClient->dataReceivedLen = 0;
    }
    
    
    
    list_freeList(localVarHeaderType);
    
    free(localVarPath);
    return elementToReturn;
end:
    free(localVarPath);
    return NULL;

}

// Is this pet still available?
//
openapi_petstore_bit__e
PetAPI_isPetAvailable(apiClient_t *apiClient, long petId)
{
    list_t    *localVarQueryParameters = NULL;
    list_t    *localVarHeaderParameters = NULL;
    list_t    *localVarFormParameters = NULL;
    list_t *localVarHeaderType = list_createList();
    list_t *localVarContentType = NULL;
    char      *localVarBodyParameters = NULL;
    size_t     localVarBodyLength = 0;

    // clear the error code from the previous api call
    apiClient->response_code = 0;

    // create the path
    char *localVarPath = strdup("/pet/{petId}/isAvailable");



    // Path Params
    long sizeOfPathParams_petId = sizeof(petId)+3 + sizeof("{ petId }") - 1;
    if(petId == 0){
        goto end;
    }
    char* localVarToReplace_petId = malloc(sizeOfPathParams_petId);
    snprintf(localVarToReplace_petId, sizeOfPathParams_petId, "{%s}", "petId");

    char localVarBuff_petId[256];
    snprintf(localVarBuff_petId, sizeof localVarBuff_petId, "%ld", petId);

    localVarPath = strReplace(localVarPath, localVarToReplace_petId, localVarBuff_petId);



    list_addElement(localVarHeaderType,"application/json"); //produces
    apiClient_invoke(apiClient,
                    localVarPath,
                    localVarQueryParameters,
                    localVarHeaderParameters,
                    localVarFormParameters,
                    localVarHeaderType,
                    localVarContentType,
                    localVarBodyParameters,
                    localVarBodyLength,
                    "POST");

    // uncomment below to debug the error response
    //if (apiClient->response_code == 200) {
    //    printf("%s\n","successful operation");
    //}
    //nonprimitive not container
    openapi_petstore_bit__e elementToReturn = 0;
    if(apiClient->response_code >= 200 && apiClient->response_code < 300) {
        cJSON *PetAPIlocalVarJSON = cJSON_Parse(apiClient->dataReceived);
        elementToReturn = bit_parseFromJSON(PetAPIlocalVarJSON);
        cJSON_Delete(PetAPIlocalVarJSON);
        if(elementToReturn == 0) {
            // return 0;
        }
    }

    //return type
    if (apiClient->dataReceived) {
        free(apiClient->dataReceived);
        apiClient->dataReceived = NULL;
        apiClient->dataReceivedLen = 0;
    }
    
    
    
    list_freeList(localVarHeaderType);
    
    free(localVarPath);
    free(localVarToReplace_petId);
    return elementToReturn;
end:
    free(localVarPath);
    return 0;

}

// Send a picture of your happy pet
//
char*
PetAPI_sharePicture(apiClient_t *apiClient, binary_t* picture)
{
    list_t    *localVarQueryParameters = NULL;
    list_t    *localVarHeaderParameters = NULL;
    list_t    *localVarFormParameters = NULL;
    list_t *localVarHeaderType = list_createList();
    list_t *localVarContentType = NULL;
    char      *localVarBodyParameters = NULL;
    size_t     localVarBodyLength = 0;

    // clear the error code from the previous api call
    apiClient->response_code = 0;

    // create the path
    char *localVarPath = strdup("/pet/picture");





    // Body Param
    localVarBodyParameters = malloc(picture->len);
    memcpy(localVarBodyParameters, picture->data, picture->len);
    localVarBodyLength = picture->len;
    list_addElement(localVarHeaderType,"*/*"); //produces
    apiClient_invoke(apiClient,
                    localVarPath,
                    localVarQueryParameters,
                    localVarHeaderParameters,
                    localVarFormParameters,
                    localVarHeaderType,
                    localVarContentType,
                    localVarBodyParameters,
                    localVarBodyLength,
                    "POST");

    // uncomment below to debug the error response
    //if (apiClient->response_code == 200) {
    //    printf("%s\n","successful operation");
    //}
    //primitive return type simple string
    char* elementToReturn = NULL;
    if(apiClient->response_code >= 200 && apiClient->response_code < 300)
        elementToReturn = strdup((char*)apiClient->dataReceived);

    if (apiClient->dataReceived) {
        free(apiClient->dataReceived);
        apiClient->dataReceived = NULL;
        apiClient->dataReceivedLen = 0;
    }
    
    
    
    list_freeList(localVarHeaderType);
    
    free(localVarPath);
    free(localVarBodyParameters);
    return elementToReturn;
end:
    free(localVarPath);
    return NULL;

}

// Specialty of the shop
//
// Returns the kind of pet the store specializes in
//
openapi_petstore_preference__e
PetAPI_specialtyPet(apiClient_t *apiClient)
{
    list_t    *localVarQueryParameters = NULL;
    list_t    *localVarHeaderParameters = NULL;
    list_t    *localVarFormParameters = NULL;
    list_t *localVarHeaderType = list_createList();
    list_t *localVarContentType = NULL;
    char      *localVarBodyParameters = NULL;
    size_t     localVarBodyLength = 0;

    // clear the error code from the previous api call
    apiClient->response_code = 0;

    // create the path
    char *localVarPath = strdup("/pet/specialty");




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
                    localVarBodyLength,
                    "GET");

    // uncomment below to debug the error response
    //if (apiClient->response_code == 200) {
    //    printf("%s\n","successful operation");
    //}
    //nonprimitive not container
    openapi_petstore_preference__e elementToReturn = 0;
    if(apiClient->response_code >= 200 && apiClient->response_code < 300) {
        cJSON *PetAPIlocalVarJSON = cJSON_Parse(apiClient->dataReceived);
        elementToReturn = preference_parseFromJSON(PetAPIlocalVarJSON);
        cJSON_Delete(PetAPIlocalVarJSON);
        if(elementToReturn == 0) {
            // return 0;
        }
    }

    //return type
    if (apiClient->dataReceived) {
        free(apiClient->dataReceived);
        apiClient->dataReceived = NULL;
        apiClient->dataReceivedLen = 0;
    }
    
    
    
    list_freeList(localVarHeaderType);
    
    free(localVarPath);
    return elementToReturn;
end:
    free(localVarPath);
    return 0;

}

// Update an existing pet
//
void
PetAPI_updatePet(apiClient_t *apiClient, pet_t *body)
{
    list_t    *localVarQueryParameters = NULL;
    list_t    *localVarHeaderParameters = NULL;
    list_t    *localVarFormParameters = NULL;
    list_t *localVarHeaderType = NULL;
    list_t *localVarContentType = list_createList();
    char      *localVarBodyParameters = NULL;
    size_t     localVarBodyLength = 0;

    // clear the error code from the previous api call
    apiClient->response_code = 0;

    // create the path
    char *localVarPath = strdup("/pet");





    // Body Param
    cJSON *localVarSingleItemJSON_body = NULL;
    if (body != NULL)
    {
        //not string, not binary
        localVarSingleItemJSON_body = pet_convertToJSON(body);
        localVarBodyParameters = cJSON_PrintUnformatted(localVarSingleItemJSON_body);
        localVarBodyLength = strlen(localVarBodyParameters);
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
                    localVarBodyLength,
                    "PUT");

    // uncomment below to debug the error response
    //if (apiClient->response_code == 400) {
    //    printf("%s\n","Invalid ID supplied");
    //}
    // uncomment below to debug the error response
    //if (apiClient->response_code == 404) {
    //    printf("%s\n","Pet not found");
    //}
    // uncomment below to debug the error response
    //if (apiClient->response_code == 405) {
    //    printf("%s\n","Validation exception");
    //}
    //No return type
end:
    if (apiClient->dataReceived) {
        free(apiClient->dataReceived);
        apiClient->dataReceived = NULL;
        apiClient->dataReceivedLen = 0;
    }
    
    
    
    
    list_freeList(localVarContentType);
    free(localVarPath);
    if (localVarSingleItemJSON_body) {
        cJSON_Delete(localVarSingleItemJSON_body);
        localVarSingleItemJSON_body = NULL;
    }
    free(localVarBodyParameters);

}

// Updates a pet in the store with form data
//
void
PetAPI_updatePetWithForm(apiClient_t *apiClient, long petId, char *name, char *status)
{
    list_t    *localVarQueryParameters = NULL;
    list_t    *localVarHeaderParameters = NULL;
    list_t    *localVarFormParameters = list_createList();
    list_t *localVarHeaderType = NULL;
    list_t *localVarContentType = list_createList();
    char      *localVarBodyParameters = NULL;
    size_t     localVarBodyLength = 0;

    // clear the error code from the previous api call
    apiClient->response_code = 0;

    // create the path
    char *localVarPath = strdup("/pet/{petId}");



    // Path Params
    long sizeOfPathParams_petId = sizeof(petId)+3 + sizeof("{ petId }") - 1;
    if(petId == 0){
        goto end;
    }
    char* localVarToReplace_petId = malloc(sizeOfPathParams_petId);
    snprintf(localVarToReplace_petId, sizeOfPathParams_petId, "{%s}", "petId");

    char localVarBuff_petId[256];
    snprintf(localVarBuff_petId, sizeof localVarBuff_petId, "%ld", petId);

    localVarPath = strReplace(localVarPath, localVarToReplace_petId, localVarBuff_petId);




    // form parameters
    char *keyForm_name = NULL;
    char * valueForm_name = 0;
    keyValuePair_t *keyPairForm_name = 0;
    if (name != NULL)
    {
        keyForm_name = strdup("name");
        valueForm_name = strdup((name));
        keyPairForm_name = keyValuePair_create(keyForm_name,valueForm_name);
        list_addElement(localVarFormParameters,keyPairForm_name);
    }

    // form parameters
    char *keyForm_status = NULL;
    char * valueForm_status = 0;
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
                    localVarBodyLength,
                    "POST");

    // uncomment below to debug the error response
    //if (apiClient->response_code == 405) {
    //    printf("%s\n","Invalid input");
    //}
    //No return type
end:
    if (apiClient->dataReceived) {
        free(apiClient->dataReceived);
        apiClient->dataReceived = NULL;
        apiClient->dataReceivedLen = 0;
    }
    
    
    list_freeList(localVarFormParameters);
    
    list_freeList(localVarContentType);
    free(localVarPath);
    free(localVarToReplace_petId);
    if (keyForm_name) {
        free(keyForm_name);
        keyForm_name = NULL;
    }
    if (valueForm_name) {
        free(valueForm_name);
        valueForm_name = NULL;
    }
    keyValuePair_free(keyPairForm_name);
    if (keyForm_status) {
        free(keyForm_status);
        keyForm_status = NULL;
    }
    if (valueForm_status) {
        free(valueForm_status);
        valueForm_status = NULL;
    }
    keyValuePair_free(keyPairForm_status);

}

// uploads an image
//
api_response_t*
PetAPI_uploadFile(apiClient_t *apiClient, long petId, char *additionalMetadata, binary_t* file)
{
    list_t    *localVarQueryParameters = NULL;
    list_t    *localVarHeaderParameters = NULL;
    list_t    *localVarFormParameters = list_createList();
    list_t *localVarHeaderType = list_createList();
    list_t *localVarContentType = list_createList();
    char      *localVarBodyParameters = NULL;
    size_t     localVarBodyLength = 0;

    // clear the error code from the previous api call
    apiClient->response_code = 0;

    // create the path
    char *localVarPath = strdup("/pet/{petId}/uploadImage");



    // Path Params
    long sizeOfPathParams_petId = sizeof(petId)+3 + sizeof("{ petId }") - 1;
    if(petId == 0){
        goto end;
    }
    char* localVarToReplace_petId = malloc(sizeOfPathParams_petId);
    snprintf(localVarToReplace_petId, sizeOfPathParams_petId, "{%s}", "petId");

    char localVarBuff_petId[256];
    snprintf(localVarBuff_petId, sizeof localVarBuff_petId, "%ld", petId);

    localVarPath = strReplace(localVarPath, localVarToReplace_petId, localVarBuff_petId);




    // form parameters
    char *keyForm_additionalMetadata = NULL;
    char * valueForm_additionalMetadata = 0;
    keyValuePair_t *keyPairForm_additionalMetadata = 0;
    if (additionalMetadata != NULL)
    {
        keyForm_additionalMetadata = strdup("additionalMetadata");
        valueForm_additionalMetadata = strdup((additionalMetadata));
        keyPairForm_additionalMetadata = keyValuePair_create(keyForm_additionalMetadata,valueForm_additionalMetadata);
        list_addElement(localVarFormParameters,keyPairForm_additionalMetadata);
    }

    // form parameters
    char *keyForm_file = NULL;
    binary_t* valueForm_file = 0;
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
                    localVarBodyLength,
                    "POST");

    // uncomment below to debug the error response
    //if (apiClient->response_code == 200) {
    //    printf("%s\n","successful operation");
    //}
    //nonprimitive not container
    api_response_t *elementToReturn = NULL;
    if(apiClient->response_code >= 200 && apiClient->response_code < 300) {
        cJSON *PetAPIlocalVarJSON = cJSON_Parse(apiClient->dataReceived);
        elementToReturn = api_response_parseFromJSON(PetAPIlocalVarJSON);
        cJSON_Delete(PetAPIlocalVarJSON);
        if(elementToReturn == NULL) {
            // return 0;
        }
    }

    //return type
    if (apiClient->dataReceived) {
        free(apiClient->dataReceived);
        apiClient->dataReceived = NULL;
        apiClient->dataReceivedLen = 0;
    }
    
    
    list_freeList(localVarFormParameters);
    list_freeList(localVarHeaderType);
    list_freeList(localVarContentType);
    free(localVarPath);
    free(localVarToReplace_petId);
    if (keyForm_additionalMetadata) {
        free(keyForm_additionalMetadata);
        keyForm_additionalMetadata = NULL;
    }
    if (valueForm_additionalMetadata) {
        free(valueForm_additionalMetadata);
        valueForm_additionalMetadata = NULL;
    }
    free(keyPairForm_additionalMetadata);
    if (keyForm_file) {
        free(keyForm_file);
        keyForm_file = NULL;
    }
//    free(fileVar_file->data);
//    free(fileVar_file);
    free(keyPairForm_file);
    return elementToReturn;
end:
    free(localVarPath);
    return NULL;

}

