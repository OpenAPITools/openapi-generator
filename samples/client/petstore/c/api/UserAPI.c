#include <stdlib.h>
#include <stdio.h>
#include <ctype.h>
#include "UserAPI.h"

#define MAX_NUMBER_LENGTH 16
#define MAX_BUFFER_LENGTH 4096
#define intToStr(dst, src) \
    do {\
    char dst[256];\
    snprintf(dst, 256, "%ld", (long int)(src));\
}while(0)


// Create user
//
// This can only be done by the logged in user.
//
void
UserAPI_createUser(apiClient_t *apiClient, user_t * body )
{
    list_t    *localVarQueryParameters = NULL;
    list_t    *localVarHeaderParameters = NULL;
    list_t    *localVarFormParameters = NULL;
    list_t *localVarHeaderType = NULL;
    list_t *localVarContentType = NULL;
    char      *localVarBodyParameters = NULL;

    // create the path
    long sizeOfPath = strlen("/user")+1;
    char *localVarPath = malloc(sizeOfPath);
    snprintf(localVarPath, sizeOfPath, "/user");




    // Body Param
    cJSON *localVarSingleItemJSON_body = NULL;
    if (body != NULL)
    {
        //string
        localVarSingleItemJSON_body = user_convertToJSON(body);
        localVarBodyParameters = cJSON_Print(localVarSingleItemJSON_body);
    }
    apiClient_invoke(apiClient,
                    localVarPath,
                    localVarQueryParameters,
                    localVarHeaderParameters,
                    localVarFormParameters,
                    localVarHeaderType,
                    localVarContentType,
                    localVarBodyParameters,
                    "POST");

    if (apiClient->response_code == 0) {
        printf("%s\n","successful operation");
    }
    //No return type
end:
    if (apiClient->dataReceived) {
        free(apiClient->dataReceived);
        apiClient->dataReceived = NULL;
        apiClient->dataReceivedLen = 0;
    }
    
    
    
    
    
    free(localVarPath);
    if (localVarSingleItemJSON_body) {
        cJSON_Delete(localVarSingleItemJSON_body);
        localVarSingleItemJSON_body = NULL;
    }
    free(localVarBodyParameters);

}

// Creates list of users with given input array
//
void
UserAPI_createUsersWithArrayInput(apiClient_t *apiClient, list_t * body )
{
    list_t    *localVarQueryParameters = NULL;
    list_t    *localVarHeaderParameters = NULL;
    list_t    *localVarFormParameters = NULL;
    list_t *localVarHeaderType = NULL;
    list_t *localVarContentType = NULL;
    char      *localVarBodyParameters = NULL;

    // create the path
    long sizeOfPath = strlen("/user/createWithArray")+1;
    char *localVarPath = malloc(sizeOfPath);
    snprintf(localVarPath, sizeOfPath, "/user/createWithArray");




    // Body Param
    //notstring
    cJSON *localVar_body = NULL;
    cJSON *localVarItemJSON_body = NULL;
    cJSON *localVarSingleItemJSON_body = NULL;
    if (body != NULL)
    {
        localVarItemJSON_body = cJSON_CreateObject();
        localVarSingleItemJSON_body = cJSON_AddArrayToObject(localVarItemJSON_body, "body");
        if (localVarSingleItemJSON_body == NULL)
        {
            // nonprimitive container

            goto end;
        }
    }

    listEntry_t *bodyBodyListEntry;
    list_ForEach(bodyBodyListEntry, body)
    {
        localVar_body = user_convertToJSON(bodyBodyListEntry->data);
        if(localVar_body == NULL)
        {
            goto end;
        }
        cJSON_AddItemToArray(localVarSingleItemJSON_body, localVar_body);
        localVarBodyParameters = cJSON_Print(localVarItemJSON_body);
    }
    apiClient_invoke(apiClient,
                    localVarPath,
                    localVarQueryParameters,
                    localVarHeaderParameters,
                    localVarFormParameters,
                    localVarHeaderType,
                    localVarContentType,
                    localVarBodyParameters,
                    "POST");

    if (apiClient->response_code == 0) {
        printf("%s\n","successful operation");
    }
    //No return type
end:
    if (apiClient->dataReceived) {
        free(apiClient->dataReceived);
        apiClient->dataReceived = NULL;
        apiClient->dataReceivedLen = 0;
    }
    
    
    
    
    
    free(localVarPath);
    if (localVarItemJSON_body) {
        cJSON_Delete(localVarItemJSON_body);
        localVarItemJSON_body = NULL;
    }
    if (localVarSingleItemJSON_body) {
        cJSON_Delete(localVarSingleItemJSON_body);
        localVarSingleItemJSON_body = NULL;
    }
    if (localVar_body) {
        cJSON_Delete(localVar_body);
        localVar_body = NULL;
    }
    free(localVarBodyParameters);

}

// Creates list of users with given input array
//
void
UserAPI_createUsersWithListInput(apiClient_t *apiClient, list_t * body )
{
    list_t    *localVarQueryParameters = NULL;
    list_t    *localVarHeaderParameters = NULL;
    list_t    *localVarFormParameters = NULL;
    list_t *localVarHeaderType = NULL;
    list_t *localVarContentType = NULL;
    char      *localVarBodyParameters = NULL;

    // create the path
    long sizeOfPath = strlen("/user/createWithList")+1;
    char *localVarPath = malloc(sizeOfPath);
    snprintf(localVarPath, sizeOfPath, "/user/createWithList");




    // Body Param
    //notstring
    cJSON *localVar_body = NULL;
    cJSON *localVarItemJSON_body = NULL;
    cJSON *localVarSingleItemJSON_body = NULL;
    if (body != NULL)
    {
        localVarItemJSON_body = cJSON_CreateObject();
        localVarSingleItemJSON_body = cJSON_AddArrayToObject(localVarItemJSON_body, "body");
        if (localVarSingleItemJSON_body == NULL)
        {
            // nonprimitive container

            goto end;
        }
    }

    listEntry_t *bodyBodyListEntry;
    list_ForEach(bodyBodyListEntry, body)
    {
        localVar_body = user_convertToJSON(bodyBodyListEntry->data);
        if(localVar_body == NULL)
        {
            goto end;
        }
        cJSON_AddItemToArray(localVarSingleItemJSON_body, localVar_body);
        localVarBodyParameters = cJSON_Print(localVarItemJSON_body);
    }
    apiClient_invoke(apiClient,
                    localVarPath,
                    localVarQueryParameters,
                    localVarHeaderParameters,
                    localVarFormParameters,
                    localVarHeaderType,
                    localVarContentType,
                    localVarBodyParameters,
                    "POST");

    if (apiClient->response_code == 0) {
        printf("%s\n","successful operation");
    }
    //No return type
end:
    if (apiClient->dataReceived) {
        free(apiClient->dataReceived);
        apiClient->dataReceived = NULL;
        apiClient->dataReceivedLen = 0;
    }
    
    
    
    
    
    free(localVarPath);
    if (localVarItemJSON_body) {
        cJSON_Delete(localVarItemJSON_body);
        localVarItemJSON_body = NULL;
    }
    if (localVarSingleItemJSON_body) {
        cJSON_Delete(localVarSingleItemJSON_body);
        localVarSingleItemJSON_body = NULL;
    }
    if (localVar_body) {
        cJSON_Delete(localVar_body);
        localVar_body = NULL;
    }
    free(localVarBodyParameters);

}

// Delete user
//
// This can only be done by the logged in user.
//
void
UserAPI_deleteUser(apiClient_t *apiClient, char * username )
{
    list_t    *localVarQueryParameters = NULL;
    list_t    *localVarHeaderParameters = NULL;
    list_t    *localVarFormParameters = NULL;
    list_t *localVarHeaderType = NULL;
    list_t *localVarContentType = NULL;
    char      *localVarBodyParameters = NULL;

    // create the path
    long sizeOfPath = strlen("/user/{username}")+1;
    char *localVarPath = malloc(sizeOfPath);
    snprintf(localVarPath, sizeOfPath, "/user/{username}");


    // Path Params
    long sizeOfPathParams_username = strlen(username)+3 + strlen("{ username }");
    if(username == NULL) {
        goto end;
    }
    char* localVarToReplace_username = malloc(sizeOfPathParams_username);
    sprintf(localVarToReplace_username, "{%s}", "username");

    localVarPath = strReplace(localVarPath, localVarToReplace_username, username);


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
        printf("%s\n","Invalid username supplied");
    }
    if (apiClient->response_code == 404) {
        printf("%s\n","User not found");
    }
    //No return type
end:
    if (apiClient->dataReceived) {
        free(apiClient->dataReceived);
        apiClient->dataReceived = NULL;
        apiClient->dataReceivedLen = 0;
    }
    
    
    
    
    
    free(localVarPath);
    free(localVarToReplace_username);

}

// Get user by user name
//
user_t*
UserAPI_getUserByName(apiClient_t *apiClient, char * username )
{
    list_t    *localVarQueryParameters = NULL;
    list_t    *localVarHeaderParameters = NULL;
    list_t    *localVarFormParameters = NULL;
    list_t *localVarHeaderType = list_createList();
    list_t *localVarContentType = NULL;
    char      *localVarBodyParameters = NULL;

    // create the path
    long sizeOfPath = strlen("/user/{username}")+1;
    char *localVarPath = malloc(sizeOfPath);
    snprintf(localVarPath, sizeOfPath, "/user/{username}");


    // Path Params
    long sizeOfPathParams_username = strlen(username)+3 + strlen("{ username }");
    if(username == NULL) {
        goto end;
    }
    char* localVarToReplace_username = malloc(sizeOfPathParams_username);
    sprintf(localVarToReplace_username, "{%s}", "username");

    localVarPath = strReplace(localVarPath, localVarToReplace_username, username);


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
        printf("%s\n","Invalid username supplied");
    }
    if (apiClient->response_code == 404) {
        printf("%s\n","User not found");
    }
    //nonprimitive not container
    cJSON *UserAPIlocalVarJSON = cJSON_Parse(apiClient->dataReceived);
    user_t *elementToReturn = user_parseFromJSON(UserAPIlocalVarJSON);
    cJSON_Delete(UserAPIlocalVarJSON);
    if(elementToReturn == NULL) {
        // return 0;
    }

    //return type
    if (apiClient->dataReceived) {
        free(apiClient->dataReceived);
        apiClient->dataReceived = NULL;
        apiClient->dataReceivedLen = 0;
    }
    
    
    
    list_freeList(localVarHeaderType);
    
    free(localVarPath);
    free(localVarToReplace_username);
    return elementToReturn;
end:
    free(localVarPath);
    return NULL;

}

// Logs user into the system
//
char*
UserAPI_loginUser(apiClient_t *apiClient, char * username , char * password )
{
    list_t    *localVarQueryParameters = list_createList();
    list_t    *localVarHeaderParameters = NULL;
    list_t    *localVarFormParameters = NULL;
    list_t *localVarHeaderType = list_createList();
    list_t *localVarContentType = NULL;
    char      *localVarBodyParameters = NULL;

    // create the path
    long sizeOfPath = strlen("/user/login")+1;
    char *localVarPath = malloc(sizeOfPath);
    snprintf(localVarPath, sizeOfPath, "/user/login");




    // query parameters
    char *keyQuery_username = NULL;
    char * valueQuery_username = NULL;
    keyValuePair_t *keyPairQuery_username = 0;
    if (username)
    {
        keyQuery_username = strdup("username");
        valueQuery_username = strdup((username));
        keyPairQuery_username = keyValuePair_create(keyQuery_username, valueQuery_username);
        list_addElement(localVarQueryParameters,keyPairQuery_username);
    }

    // query parameters
    char *keyQuery_password = NULL;
    char * valueQuery_password = NULL;
    keyValuePair_t *keyPairQuery_password = 0;
    if (password)
    {
        keyQuery_password = strdup("password");
        valueQuery_password = strdup((password));
        keyPairQuery_password = keyValuePair_create(keyQuery_password, valueQuery_password);
        list_addElement(localVarQueryParameters,keyPairQuery_password);
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
        printf("%s\n","Invalid username/password supplied");
    }
    //primitive return type simple
    char* elementToReturn =  strdup((char*)apiClient->dataReceived);

    if (apiClient->dataReceived) {
        free(apiClient->dataReceived);
        apiClient->dataReceived = NULL;
        apiClient->dataReceivedLen = 0;
    }
    list_freeList(localVarQueryParameters);
    
    
    list_freeList(localVarHeaderType);
    
    free(localVarPath);
    if(keyQuery_username){
        free(keyQuery_username);
        keyQuery_username = NULL;
    }
    if(valueQuery_username){
        free(valueQuery_username);
        valueQuery_username = NULL;
    }
    if(keyPairQuery_username){
        keyValuePair_free(keyPairQuery_username);
        keyPairQuery_username = NULL;
    }
    if(keyQuery_password){
        free(keyQuery_password);
        keyQuery_password = NULL;
    }
    if(valueQuery_password){
        free(valueQuery_password);
        valueQuery_password = NULL;
    }
    if(keyPairQuery_password){
        keyValuePair_free(keyPairQuery_password);
        keyPairQuery_password = NULL;
    }
    return elementToReturn;
end:
    free(localVarPath);
    return NULL;

}

// Logs out current logged in user session
//
void
UserAPI_logoutUser(apiClient_t *apiClient)
{
    list_t    *localVarQueryParameters = NULL;
    list_t    *localVarHeaderParameters = NULL;
    list_t    *localVarFormParameters = NULL;
    list_t *localVarHeaderType = NULL;
    list_t *localVarContentType = NULL;
    char      *localVarBodyParameters = NULL;

    // create the path
    long sizeOfPath = strlen("/user/logout")+1;
    char *localVarPath = malloc(sizeOfPath);
    snprintf(localVarPath, sizeOfPath, "/user/logout");



    apiClient_invoke(apiClient,
                    localVarPath,
                    localVarQueryParameters,
                    localVarHeaderParameters,
                    localVarFormParameters,
                    localVarHeaderType,
                    localVarContentType,
                    localVarBodyParameters,
                    "GET");

    if (apiClient->response_code == 0) {
        printf("%s\n","successful operation");
    }
    //No return type
end:
    if (apiClient->dataReceived) {
        free(apiClient->dataReceived);
        apiClient->dataReceived = NULL;
        apiClient->dataReceivedLen = 0;
    }
    
    
    
    
    
    free(localVarPath);

}

// Updated user
//
// This can only be done by the logged in user.
//
void
UserAPI_updateUser(apiClient_t *apiClient, char * username , user_t * body )
{
    list_t    *localVarQueryParameters = NULL;
    list_t    *localVarHeaderParameters = NULL;
    list_t    *localVarFormParameters = NULL;
    list_t *localVarHeaderType = NULL;
    list_t *localVarContentType = NULL;
    char      *localVarBodyParameters = NULL;

    // create the path
    long sizeOfPath = strlen("/user/{username}")+1;
    char *localVarPath = malloc(sizeOfPath);
    snprintf(localVarPath, sizeOfPath, "/user/{username}");


    // Path Params
    long sizeOfPathParams_username = strlen(username)+3 + strlen("{ username }");
    if(username == NULL) {
        goto end;
    }
    char* localVarToReplace_username = malloc(sizeOfPathParams_username);
    sprintf(localVarToReplace_username, "{%s}", "username");

    localVarPath = strReplace(localVarPath, localVarToReplace_username, username);



    // Body Param
    cJSON *localVarSingleItemJSON_body = NULL;
    if (body != NULL)
    {
        //string
        localVarSingleItemJSON_body = user_convertToJSON(body);
        localVarBodyParameters = cJSON_Print(localVarSingleItemJSON_body);
    }
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
        printf("%s\n","Invalid user supplied");
    }
    if (apiClient->response_code == 404) {
        printf("%s\n","User not found");
    }
    //No return type
end:
    if (apiClient->dataReceived) {
        free(apiClient->dataReceived);
        apiClient->dataReceived = NULL;
        apiClient->dataReceivedLen = 0;
    }
    
    
    
    
    
    free(localVarPath);
    free(localVarToReplace_username);
    if (localVarSingleItemJSON_body) {
        cJSON_Delete(localVarSingleItemJSON_body);
        localVarSingleItemJSON_body = NULL;
    }
    free(localVarBodyParameters);

}

