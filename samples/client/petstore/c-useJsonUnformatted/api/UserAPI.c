#include <stdlib.h>
#include <stdio.h>
#include <ctype.h>

#include "UserAPI.h"

#define MAX_NUMBER_LENGTH 16
#define MAX_BUFFER_LENGTH 4096
#define MAX_NUMBER_LENGTH_LONG 21


// Create user
//
// This can only be done by the logged in user.
//
void
UserAPI_createUser(apiClient_t *apiClient, user_t *body)
{
    list_t    *localVarQueryParameters = NULL;
    list_t    *localVarHeaderParameters = NULL;
    list_t    *localVarFormParameters = NULL;
    list_t *localVarHeaderType = NULL;
    list_t *localVarContentType = NULL;
    char      *localVarBodyParameters = NULL;
    size_t     localVarBodyLength = 0;

    // clear the error code from the previous api call
    apiClient->response_code = 0;

    // create the path
    char *localVarPath = strdup("/user");





    // Body Param
    cJSON *localVarSingleItemJSON_body = NULL;
    if (body != NULL)
    {
        //not string, not binary
        localVarSingleItemJSON_body = user_convertToJSON(body);
        localVarBodyParameters = cJSON_PrintUnformatted(localVarSingleItemJSON_body);
        localVarBodyLength = strlen(localVarBodyParameters);
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
                    "POST");

    // uncomment below to debug the error response
    //if (apiClient->response_code == 0) {
    //    printf("%s\n","successful operation");
    //}
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
UserAPI_createUsersWithArrayInput(apiClient_t *apiClient, list_t *body)
{
    list_t    *localVarQueryParameters = NULL;
    list_t    *localVarHeaderParameters = NULL;
    list_t    *localVarFormParameters = NULL;
    list_t *localVarHeaderType = NULL;
    list_t *localVarContentType = NULL;
    char      *localVarBodyParameters = NULL;
    size_t     localVarBodyLength = 0;

    // clear the error code from the previous api call
    apiClient->response_code = 0;

    // create the path
    char *localVarPath = strdup("/user/createWithArray");





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
        localVarBodyParameters = cJSON_PrintUnformatted(localVarItemJSON_body);
        localVarBodyLength = strlen(localVarBodyParameters);
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
                    "POST");

    // uncomment below to debug the error response
    //if (apiClient->response_code == 0) {
    //    printf("%s\n","successful operation");
    //}
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
UserAPI_createUsersWithListInput(apiClient_t *apiClient, list_t *body)
{
    list_t    *localVarQueryParameters = NULL;
    list_t    *localVarHeaderParameters = NULL;
    list_t    *localVarFormParameters = NULL;
    list_t *localVarHeaderType = NULL;
    list_t *localVarContentType = NULL;
    char      *localVarBodyParameters = NULL;
    size_t     localVarBodyLength = 0;

    // clear the error code from the previous api call
    apiClient->response_code = 0;

    // create the path
    char *localVarPath = strdup("/user/createWithList");





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
        localVarBodyParameters = cJSON_PrintUnformatted(localVarItemJSON_body);
        localVarBodyLength = strlen(localVarBodyParameters);
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
                    "POST");

    // uncomment below to debug the error response
    //if (apiClient->response_code == 0) {
    //    printf("%s\n","successful operation");
    //}
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
UserAPI_deleteUser(apiClient_t *apiClient, char *username)
{
    list_t    *localVarQueryParameters = NULL;
    list_t    *localVarHeaderParameters = NULL;
    list_t    *localVarFormParameters = NULL;
    list_t *localVarHeaderType = NULL;
    list_t *localVarContentType = NULL;
    char      *localVarBodyParameters = NULL;
    size_t     localVarBodyLength = 0;

    // clear the error code from the previous api call
    apiClient->response_code = 0;

    // create the path
    char *localVarPath = strdup("/user/{username}");

    if(!username)
        goto end;


    // Path Params
    long sizeOfPathParams_username = strlen(username)+3 + sizeof("{ username }") - 1;
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
                    localVarBodyLength,
                    "DELETE");

    // uncomment below to debug the error response
    //if (apiClient->response_code == 400) {
    //    printf("%s\n","Invalid username supplied");
    //}
    // uncomment below to debug the error response
    //if (apiClient->response_code == 404) {
    //    printf("%s\n","User not found");
    //}
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
UserAPI_getUserByName(apiClient_t *apiClient, char *username)
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
    char *localVarPath = strdup("/user/{username}");

    if(!username)
        goto end;


    // Path Params
    long sizeOfPathParams_username = strlen(username)+3 + sizeof("{ username }") - 1;
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
                    localVarBodyLength,
                    "GET");

    // uncomment below to debug the error response
    //if (apiClient->response_code == 200) {
    //    printf("%s\n","successful operation");
    //}
    // uncomment below to debug the error response
    //if (apiClient->response_code == 400) {
    //    printf("%s\n","Invalid username supplied");
    //}
    // uncomment below to debug the error response
    //if (apiClient->response_code == 404) {
    //    printf("%s\n","User not found");
    //}
    //nonprimitive not container
    user_t *elementToReturn = NULL;
    if(apiClient->response_code >= 200 && apiClient->response_code < 300) {
        cJSON *UserAPIlocalVarJSON = cJSON_Parse(apiClient->dataReceived);
        elementToReturn = user_parseFromJSON(UserAPIlocalVarJSON);
        cJSON_Delete(UserAPIlocalVarJSON);
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
    free(localVarToReplace_username);
    return elementToReturn;
end:
    free(localVarPath);
    return NULL;

}

// Logs user into the system
//
char*
UserAPI_loginUser(apiClient_t *apiClient, char *username, char *password)
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
    char *localVarPath = strdup("/user/login");





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
                    localVarBodyLength,
                    "GET");

    // uncomment below to debug the error response
    //if (apiClient->response_code == 200) {
    //    printf("%s\n","successful operation");
    //}
    // uncomment below to debug the error response
    //if (apiClient->response_code == 400) {
    //    printf("%s\n","Invalid username/password supplied");
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
    size_t     localVarBodyLength = 0;

    // clear the error code from the previous api call
    apiClient->response_code = 0;

    // create the path
    char *localVarPath = strdup("/user/logout");




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
    //if (apiClient->response_code == 0) {
    //    printf("%s\n","successful operation");
    //}
    //No return type
end:
    if (apiClient->dataReceived) {
        free(apiClient->dataReceived);
        apiClient->dataReceived = NULL;
        apiClient->dataReceivedLen = 0;
    }
    
    
    
    
    
    free(localVarPath);

}

// test int32, int64 float and double query parameters in API
//
// This can test int32, int64 float and double query parameters in API.
//
void
UserAPI_testInt32Int64FloatDouble(apiClient_t *apiClient, float floatnum, double doublenum, int *int32num, long int64num)
{
    list_t    *localVarQueryParameters = list_createList();
    list_t    *localVarHeaderParameters = NULL;
    list_t    *localVarFormParameters = NULL;
    list_t *localVarHeaderType = NULL;
    list_t *localVarContentType = NULL;
    char      *localVarBodyParameters = NULL;
    size_t     localVarBodyLength = 0;

    // clear the error code from the previous api call
    apiClient->response_code = 0;

    // create the path
    char *localVarPath = strdup("/user/test_int32_int64_float_double");





    // query parameters
    char *keyQuery_floatnum = NULL;
    char * valueQuery_floatnum = NULL;
    keyValuePair_t *keyPairQuery_floatnum = 0;
    {
        keyQuery_floatnum = strdup("floatnum");
        int s = snprintf(NULL, 0, "%.7e", floatnum);
        if (s >= 0)
        {
            valueQuery_floatnum = calloc(1,s+1);
            snprintf(valueQuery_floatnum, s+1, "%.7e", floatnum);
        }
        keyPairQuery_floatnum = keyValuePair_create(keyQuery_floatnum, valueQuery_floatnum);
        list_addElement(localVarQueryParameters,keyPairQuery_floatnum);
    }

    // query parameters
    char *keyQuery_doublenum = NULL;
    char * valueQuery_doublenum = NULL;
    keyValuePair_t *keyPairQuery_doublenum = 0;
    {
        keyQuery_doublenum = strdup("doublenum");
        int s = snprintf(NULL, 0, "%.16e", doublenum);
        if (s >= 0)
        {
            valueQuery_doublenum = calloc(1,s+1);
            snprintf(valueQuery_doublenum, s+1, "%.16e", doublenum);
        }
        keyPairQuery_doublenum = keyValuePair_create(keyQuery_doublenum, valueQuery_doublenum);
        list_addElement(localVarQueryParameters,keyPairQuery_doublenum);
    }

    // query parameters
    char *keyQuery_int32num = NULL;
    char * valueQuery_int32num = NULL;
    keyValuePair_t *keyPairQuery_int32num = 0;
    if (int32num)
    {
        keyQuery_int32num = strdup("int32num");
        valueQuery_int32num = calloc(1,MAX_NUMBER_LENGTH);
        snprintf(valueQuery_int32num, MAX_NUMBER_LENGTH, "%d", *int32num);
        keyPairQuery_int32num = keyValuePair_create(keyQuery_int32num, valueQuery_int32num);
        list_addElement(localVarQueryParameters,keyPairQuery_int32num);
    }

    // query parameters
    char *keyQuery_int64num = NULL;
    char * valueQuery_int64num ;
    keyValuePair_t *keyPairQuery_int64num = 0;
    {
        keyQuery_int64num = strdup("int64num");
        valueQuery_int64num = calloc(1,MAX_NUMBER_LENGTH_LONG);
        snprintf(valueQuery_int64num, MAX_NUMBER_LENGTH_LONG, "%d", int64num);
        keyPairQuery_int64num = keyValuePair_create(keyQuery_int64num, valueQuery_int64num);
        list_addElement(localVarQueryParameters,keyPairQuery_int64num);
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
                    "GET");

    // uncomment below to debug the error response
    //if (apiClient->response_code == 200) {
    //    printf("%s\n","successful operation");
    //}
    //No return type
end:
    if (apiClient->dataReceived) {
        free(apiClient->dataReceived);
        apiClient->dataReceived = NULL;
        apiClient->dataReceivedLen = 0;
    }
    list_freeList(localVarQueryParameters);
    
    
    
    
    free(localVarPath);

}

// test integer and boolean query parameters in API
//
// This can test integer and boolean query parameters in API.
//
void
UserAPI_testIntAndBool(apiClient_t *apiClient, int *keep, int *keepDay)
{
    list_t    *localVarQueryParameters = list_createList();
    list_t    *localVarHeaderParameters = NULL;
    list_t    *localVarFormParameters = NULL;
    list_t *localVarHeaderType = NULL;
    list_t *localVarContentType = NULL;
    char      *localVarBodyParameters = NULL;
    size_t     localVarBodyLength = 0;

    // clear the error code from the previous api call
    apiClient->response_code = 0;

    // create the path
    char *localVarPath = strdup("/user/testIntAndBool");





    // query parameters
    char *keyQuery_keep = NULL;
    char * valueQuery_keep = NULL;
    keyValuePair_t *keyPairQuery_keep = 0;
    if (keep)
    {
        keyQuery_keep = strdup("keep");
        valueQuery_keep = calloc(1,MAX_NUMBER_LENGTH);
        snprintf(valueQuery_keep, MAX_NUMBER_LENGTH, "%d", *keep);
        keyPairQuery_keep = keyValuePair_create(keyQuery_keep, valueQuery_keep);
        list_addElement(localVarQueryParameters,keyPairQuery_keep);
    }

    // query parameters
    char *keyQuery_keepDay = NULL;
    char * valueQuery_keepDay = NULL;
    keyValuePair_t *keyPairQuery_keepDay = 0;
    if (keepDay)
    {
        keyQuery_keepDay = strdup("keepDay");
        valueQuery_keepDay = calloc(1,MAX_NUMBER_LENGTH);
        snprintf(valueQuery_keepDay, MAX_NUMBER_LENGTH, "%d", *keepDay);
        keyPairQuery_keepDay = keyValuePair_create(keyQuery_keepDay, valueQuery_keepDay);
        list_addElement(localVarQueryParameters,keyPairQuery_keepDay);
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
                    "GET");

    // uncomment below to debug the error response
    //if (apiClient->response_code == 200) {
    //    printf("%s\n","successful operation");
    //}
    //No return type
end:
    if (apiClient->dataReceived) {
        free(apiClient->dataReceived);
        apiClient->dataReceived = NULL;
        apiClient->dataReceivedLen = 0;
    }
    list_freeList(localVarQueryParameters);
    
    
    
    
    free(localVarPath);

}

// Updated user
//
// This can only be done by the logged in user.
//
void
UserAPI_updateUser(apiClient_t *apiClient, char *username, user_t *body)
{
    list_t    *localVarQueryParameters = NULL;
    list_t    *localVarHeaderParameters = NULL;
    list_t    *localVarFormParameters = NULL;
    list_t *localVarHeaderType = NULL;
    list_t *localVarContentType = NULL;
    char      *localVarBodyParameters = NULL;
    size_t     localVarBodyLength = 0;

    // clear the error code from the previous api call
    apiClient->response_code = 0;

    // create the path
    char *localVarPath = strdup("/user/{username}");

    if(!username)
        goto end;


    // Path Params
    long sizeOfPathParams_username = strlen(username)+3 + sizeof("{ username }") - 1;
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
        //not string, not binary
        localVarSingleItemJSON_body = user_convertToJSON(body);
        localVarBodyParameters = cJSON_PrintUnformatted(localVarSingleItemJSON_body);
        localVarBodyLength = strlen(localVarBodyParameters);
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
                    "PUT");

    // uncomment below to debug the error response
    //if (apiClient->response_code == 400) {
    //    printf("%s\n","Invalid user supplied");
    //}
    // uncomment below to debug the error response
    //if (apiClient->response_code == 404) {
    //    printf("%s\n","User not found");
    //}
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

