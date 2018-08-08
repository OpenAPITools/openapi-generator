#include <stdlib.h>
#include <stdio.h>
#include "apiClient.h"
#include "cJSON.h"
#include "models.array.h" // TODO will fix the import later
#include "models.user.h" // TODO will fix the import later

#define MAX_BUFFER_LENGTH 4096

// Create user
//
// This can only be done by the logged in user.
//
void *UserAPI_createUser(apiClient_t *apiClient, user User) {
    list_t		*localVarQueryParameters,
    list_t		*localVarHeaderParameters,
    list_t		*localVarFormParameters,
    char		*localVarBodyParameters,

    // create the path
	char *localVarPath = malloc(MAX_BUFFER_LENGTH);
	snprintf(localVarPath, MAX_BUFFER_LENGTH, "/user");



    // JSON HTTP Request - user
    cJSON *UserJSONObject;
    UserJSONObject = user_convertToJSON(User);
    localVarBodyParameters = cJSON_Print(UserJSONObject);

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

// Creates list of users with given input array
//
void *UserAPI_createUsersWithArrayInput(apiClient_t *apiClient, array User) {
    list_t		*localVarQueryParameters,
    list_t		*localVarHeaderParameters,
    list_t		*localVarFormParameters,
    char		*localVarBodyParameters,

    // create the path
	char *localVarPath = malloc(MAX_BUFFER_LENGTH);
	snprintf(localVarPath, MAX_BUFFER_LENGTH, "/user/createWithArray");



    // JSON HTTP Request - user
    cJSON *UserJSONObject;
    UserJSONObject = array_convertToJSON(User);
    localVarBodyParameters = cJSON_Print(UserJSONObject);

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

// Creates list of users with given input array
//
void *UserAPI_createUsersWithListInput(apiClient_t *apiClient, array User) {
    list_t		*localVarQueryParameters,
    list_t		*localVarHeaderParameters,
    list_t		*localVarFormParameters,
    char		*localVarBodyParameters,

    // create the path
	char *localVarPath = malloc(MAX_BUFFER_LENGTH);
	snprintf(localVarPath, MAX_BUFFER_LENGTH, "/user/createWithList");



    // JSON HTTP Request - user
    cJSON *UserJSONObject;
    UserJSONObject = array_convertToJSON(User);
    localVarBodyParameters = cJSON_Print(UserJSONObject);

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

// Delete user
//
// This can only be done by the logged in user.
//
void *UserAPI_deleteUser(apiClient_t *apiClient, char Username) {
    list_t		*localVarQueryParameters,
    list_t		*localVarHeaderParameters,
    list_t		*localVarFormParameters,
    char		*localVarBodyParameters,

    // create the path
	char *localVarPath = malloc(MAX_BUFFER_LENGTH);
	snprintf(localVarPath, MAX_BUFFER_LENGTH, "/user/{username}");

    // TODO path parameter Username (username) not yet supported
    // TODO base path = http://petstore.swagger.io/v2
    replace_str(localVarPath, "{" + "username" + "}", Username)// TODO need to revise


	apiClient_invoke(apiClient,
	                 "pet",
	                 localVarPath,
	                 localVarQueryParameters,
	                 localVarHeaderParameters,
	                 localVarFormParameters,
	                 localVarBodyParameters,
	                 "DELETE");

	free(apiClient->dataReceived);
	free(UsernameString);
	return;

}

// Get user by user name
//
user_t *UserAPI_getUserByName(apiClient_t *apiClient, char Username) {
    list_t		*localVarQueryParameters,
    list_t		*localVarHeaderParameters,
    list_t		*localVarFormParameters,
    char		*localVarBodyParameters,

    // create the path
	char *localVarPath = malloc(MAX_BUFFER_LENGTH);
	snprintf(localVarPath, MAX_BUFFER_LENGTH, "/user/{username}");

    // TODO path parameter Username (username) not yet supported
    // TODO base path = http://petstore.swagger.io/v2
    replace_str(localVarPath, "{" + "username" + "}", Username)// TODO need to revise


	apiClient_invoke(apiClient,
	                 "pet",
	                 localVarPath,
	                 localVarQueryParameters,
	                 localVarHeaderParameters,
	                 localVarFormParameters,
	                 localVarBodyParameters,
	                 "GET");

	free(apiClient->dataReceived);
	free(UsernameString);
    localVaruser = _parseFromJSON(apiClient->dataReceived);
    if(localVaruser == NULL) {
        return 0;
    } else {
        cJSON *jsonObject = _convertToJSON();
        cJSON_Delete(jsonObject);
    }

	return localVaruser;

}

// Logs user into the system
//
char_t *UserAPI_loginUser(apiClient_t *apiClient, char Username, char Password) {
    list_t		*localVarQueryParameters,
    list_t		*localVarHeaderParameters,
    list_t		*localVarFormParameters,
    char		*localVarBodyParameters,

    // create the path
	char *localVarPath = malloc(MAX_BUFFER_LENGTH);
	snprintf(localVarPath, MAX_BUFFER_LENGTH, "/user/login");



    // TODO query parameters
    // query parameter Username (username) not yet supported
    char *username = malloc(MAX_BUFFER_LENGTH);

    // TODO query parameters
    // query parameter Password (password) not yet supported
    char *password = malloc(MAX_BUFFER_LENGTH);

	apiClient_invoke(apiClient,
	                 "pet",
	                 localVarPath,
	                 localVarQueryParameters,
	                 localVarHeaderParameters,
	                 localVarFormParameters,
	                 localVarBodyParameters,
	                 "GET");

	free(apiClient->dataReceived);
	free(UsernameString);
	free(PasswordString);
    localVarchar = _parseFromJSON(apiClient->dataReceived);
    if(localVarchar == NULL) {
        return 0;
    } else {
        cJSON *jsonObject = _convertToJSON();
        cJSON_Delete(jsonObject);
    }

	return localVarchar;

}

// Logs out current logged in user session
//
void *UserAPI_logoutUser(apiClient_t *apiClient) {
    list_t		*localVarQueryParameters,
    list_t		*localVarHeaderParameters,
    list_t		*localVarFormParameters,
    char		*localVarBodyParameters,

    // create the path
	char *localVarPath = malloc(MAX_BUFFER_LENGTH);
	snprintf(localVarPath, MAX_BUFFER_LENGTH, "/user/logout");



	apiClient_invoke(apiClient,
	                 "pet",
	                 localVarPath,
	                 localVarQueryParameters,
	                 localVarHeaderParameters,
	                 localVarFormParameters,
	                 localVarBodyParameters,
	                 "GET");

	free(apiClient->dataReceived);
	return;

}

// Updated user
//
// This can only be done by the logged in user.
//
void *UserAPI_updateUser(apiClient_t *apiClient, char Username, user User) {
    list_t		*localVarQueryParameters,
    list_t		*localVarHeaderParameters,
    list_t		*localVarFormParameters,
    char		*localVarBodyParameters,

    // create the path
	char *localVarPath = malloc(MAX_BUFFER_LENGTH);
	snprintf(localVarPath, MAX_BUFFER_LENGTH, "/user/{username}");

    // TODO path parameter Username (username) not yet supported
    // TODO base path = http://petstore.swagger.io/v2
    replace_str(localVarPath, "{" + "username" + "}", Username)// TODO need to revise


    // JSON HTTP Request - user
    cJSON *UserJSONObject;
    UserJSONObject = user_convertToJSON(User);
    localVarBodyParameters = cJSON_Print(UserJSONObject);

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
    free(localVarBodyParameters);
	cJSON_Delete()
	return;

}

