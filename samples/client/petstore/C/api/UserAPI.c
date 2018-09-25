#include <stdlib.h>
#include <stdio.h>
#include <ctype.h>
#include "apiClient.h"
#include "cJSON.h"
#include "keyValuePair.h"
#include "list.h"
#include "user.h"

#define MAX_BUFFER_LENGTH 4096
#define intToStr(dst, src) \
	do { \
		char dst[256]; \
		snprintf(dst, 256, "%ld", (long int) (src)); \
	} while(0)

// Create user
//
// This can only be done by the logged in user.
//
void *UserAPI_createUser(apiClient_t *apiClient, user_t *user) {
	list_t *localVarQueryParameters = list_create();
	list_t *localVarHeaderParameters = list_create();
	list_t *localVarFormParameters = list_create();
	list_t *localVarHeaderType = list_create();
	list_t *localVarContentType = list_create();
	char *localVarBodyParameters = NULL;

	// create the path
	char *localVarPath = malloc(MAX_BUFFER_LENGTH);
	snprintf(localVarPath, MAX_BUFFER_LENGTH, "/user");

	// Body Param
	// string
	cJSON *localVarSingleItemJSON_user;
	localVarSingleItemJSON_user = user_convertToJSON(user);
	localVarBodyParameters = cJSON_Print(localVarSingleItemJSON_user);

	apiClient_invoke(apiClient,
	                 localVarPath,
	                 localVarQueryParameters,
	                 localVarHeaderParameters,
	                 localVarFormParameters,
	                 localVarHeaderType,
	                 localVarContentType,
	                 localVarBodyParameters,
	                 "POST");

	// No return type
	apiClient_free(apiClient);
	list_free(localVarQueryParameters);
	list_free(localVarHeaderParameters);
	list_free(localVarFormParameters);
	list_free(localVarHeaderType);
	list_free(localVarContentType);
	free(localVarPath);
}

// Creates list of users with given input array
//
void *UserAPI_createUsersWithArrayInput(apiClient_t *apiClient, list_t *user) {
	list_t *localVarQueryParameters = list_create();
	list_t *localVarHeaderParameters = list_create();
	list_t *localVarFormParameters = list_create();
	list_t *localVarHeaderType = list_create();
	list_t *localVarContentType = list_create();
	char *localVarBodyParameters = NULL;

	// create the path
	char *localVarPath = malloc(MAX_BUFFER_LENGTH);
	snprintf(localVarPath, MAX_BUFFER_LENGTH, "/user/createWithArray");

	// Body Param
	// notstring
	cJSON *localVarItemJSON_user = cJSON_CreateObject();
	cJSON *localVarSingleItemJSON_user = cJSON_AddArrayToObject(localVarItemJSON_user, "user");
	if(localVarSingleItemJSON_user == NULL) {
		return 0; // nonprimitive container
	}
	listEntry_t *userBodyListEntry;
	cJSON *localVar_user;
	list_ForEach(userBodyListEntry, user) {
		localVar_user = user_convertToJSON(userBodyListEntry->data);
		if(localVar_user == NULL) {
			return 0;
		}
		cJSON_AddItemToArray(localVarSingleItemJSON_user, localVar_user);
	}

	localVarBodyParameters = cJSON_Print(localVarItemJSON_user);

	apiClient_invoke(apiClient,
	                 localVarPath,
	                 localVarQueryParameters,
	                 localVarHeaderParameters,
	                 localVarFormParameters,
	                 localVarHeaderType,
	                 localVarContentType,
	                 localVarBodyParameters,
	                 "POST");

	// No return type
	apiClient_free(apiClient);
	list_free(localVarQueryParameters);
	list_free(localVarHeaderParameters);
	list_free(localVarFormParameters);
	list_free(localVarHeaderType);
	list_free(localVarContentType);
	free(localVarPath);
	cJSON_Delete(localVarItemJSON_user);
	cJSON_Delete(localVarSingleItemJSON_user);
	cJSON_Delete(localVar_user);
	free(localVarBodyParameters);
}

// Creates list of users with given input array
//
void *UserAPI_createUsersWithListInput(apiClient_t *apiClient, list_t *user) {
	list_t *localVarQueryParameters = list_create();
	list_t *localVarHeaderParameters = list_create();
	list_t *localVarFormParameters = list_create();
	list_t *localVarHeaderType = list_create();
	list_t *localVarContentType = list_create();
	char *localVarBodyParameters = NULL;

	// create the path
	char *localVarPath = malloc(MAX_BUFFER_LENGTH);
	snprintf(localVarPath, MAX_BUFFER_LENGTH, "/user/createWithList");

	// Body Param
	// notstring
	cJSON *localVarItemJSON_user = cJSON_CreateObject();
	cJSON *localVarSingleItemJSON_user = cJSON_AddArrayToObject(localVarItemJSON_user, "user");
	if(localVarSingleItemJSON_user == NULL) {
		return 0; // nonprimitive container
	}
	listEntry_t *userBodyListEntry;
	cJSON *localVar_user;
	list_ForEach(userBodyListEntry, user) {
		localVar_user = user_convertToJSON(userBodyListEntry->data);
		if(localVar_user == NULL) {
			return 0;
		}
		cJSON_AddItemToArray(localVarSingleItemJSON_user, localVar_user);
	}

	localVarBodyParameters = cJSON_Print(localVarItemJSON_user);

	apiClient_invoke(apiClient,
	                 localVarPath,
	                 localVarQueryParameters,
	                 localVarHeaderParameters,
	                 localVarFormParameters,
	                 localVarHeaderType,
	                 localVarContentType,
	                 localVarBodyParameters,
	                 "POST");

	// No return type
	apiClient_free(apiClient);
	list_free(localVarQueryParameters);
	list_free(localVarHeaderParameters);
	list_free(localVarFormParameters);
	list_free(localVarHeaderType);
	list_free(localVarContentType);
	free(localVarPath);
	cJSON_Delete(localVarItemJSON_user);
	cJSON_Delete(localVarSingleItemJSON_user);
	cJSON_Delete(localVar_user);
	free(localVarBodyParameters);
}

// Delete user
//
// This can only be done by the logged in user.
//
void *UserAPI_deleteUser(apiClient_t *apiClient, char *username) {
	list_t *localVarQueryParameters = list_create();
	list_t *localVarHeaderParameters = list_create();
	list_t *localVarFormParameters = list_create();
	list_t *localVarHeaderType = list_create();
	list_t *localVarContentType = list_create();
	char *localVarBodyParameters = NULL;

	// create the path
	char *localVarPath = malloc(MAX_BUFFER_LENGTH);
	snprintf(localVarPath, MAX_BUFFER_LENGTH, "/user/{username}");

	// Path Params
	char *localVarToReplace = malloc(sizeof(username) + 2);
	snprintf(localVarToReplace, strlen(username) + 3, "%s%s%s", "{", "username", "}");

	localVarPath = strReplace(localVarPath, localVarToReplace, username);

	apiClient_invoke(apiClient,
	                 localVarPath,
	                 localVarQueryParameters,
	                 localVarHeaderParameters,
	                 localVarFormParameters,
	                 localVarHeaderType,
	                 localVarContentType,
	                 localVarBodyParameters,
	                 "DELETE");

	// No return type
	apiClient_free(apiClient);
	list_free(localVarQueryParameters);
	list_free(localVarHeaderParameters);
	list_free(localVarFormParameters);
	list_free(localVarHeaderType);
	list_free(localVarContentType);
	free(localVarPath);
	free(localVarToReplace);
}

// Get user by user name
//
user_t *UserAPI_getUserByName(apiClient_t *apiClient, char *username) {
	list_t *localVarQueryParameters = list_create();
	list_t *localVarHeaderParameters = list_create();
	list_t *localVarFormParameters = list_create();
	list_t *localVarHeaderType = list_create();
	list_t *localVarContentType = list_create();
	char *localVarBodyParameters = NULL;

	// create the path
	char *localVarPath = malloc(MAX_BUFFER_LENGTH);
	snprintf(localVarPath, MAX_BUFFER_LENGTH, "/user/{username}");

	// Path Params
	char *localVarToReplace = malloc(sizeof(username) + 2);
	snprintf(localVarToReplace, strlen(username) + 3, "%s%s%s", "{", "username", "}");

	localVarPath = strReplace(localVarPath, localVarToReplace, username);

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
	user_t *elementToReturn = user_parseFromJSON(apiClient->dataReceived);
	if(elementToReturn == NULL) {
		return 0;
	}

	// return type
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

// Logs user into the system
//
char *UserAPI_loginUser(apiClient_t *apiClient, char *username, char *password) {
	list_t *localVarQueryParameters = list_create();
	list_t *localVarHeaderParameters = list_create();
	list_t *localVarFormParameters = list_create();
	list_t *localVarHeaderType = list_create();
	list_t *localVarContentType = list_create();
	char *localVarBodyParameters = NULL;

	// create the path
	char *localVarPath = malloc(MAX_BUFFER_LENGTH);
	snprintf(localVarPath, MAX_BUFFER_LENGTH, "/user/login");

	// query parameters
	char *keyQuery_username;
	char *valueQuery_username;
	keyValuePair_t *keyPairQuery_username = 0;
	if(username) {
		// string
		keyQuery_username = strdup("username");
		valueQuery_username = strdup(username);
		keyPairQuery_username = keyValuePair_create(keyQuery_username, valueQuery_username);
		list_addElement(localVarQueryParameters, keyPairQuery_username);
	}

	// query parameters
	char *keyQuery_password;
	char *valueQuery_password;
	keyValuePair_t *keyPairQuery_password = 0;
	if(password) {
		// string
		keyQuery_password = strdup("password");
		valueQuery_password = strdup(password);
		keyPairQuery_password = keyValuePair_create(keyQuery_password, valueQuery_password);
		list_addElement(localVarQueryParameters, keyPairQuery_password);
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

	// primitive reutrn type
	char *elementToReturn = (char *) apiClient->dataReceived;
	apiClient_free(apiClient);
	list_free(localVarQueryParameters);
	list_free(localVarHeaderParameters);
	list_free(localVarFormParameters);
	list_free(localVarHeaderType);
	list_free(localVarContentType);
	free(localVarPath);
	free(keyQuery_username);
	free(valueQuery_username);
	free(keyQuery_password);
	free(valueQuery_password);
	return elementToReturn;
}

// Logs out current logged in user session
//
void *UserAPI_logoutUser(apiClient_t *apiClient) {
	list_t *localVarQueryParameters = list_create();
	list_t *localVarHeaderParameters = list_create();
	list_t *localVarFormParameters = list_create();
	list_t *localVarHeaderType = list_create();
	list_t *localVarContentType = list_create();
	char *localVarBodyParameters = NULL;

	// create the path
	char *localVarPath = malloc(MAX_BUFFER_LENGTH);
	snprintf(localVarPath, MAX_BUFFER_LENGTH, "/user/logout");

	apiClient_invoke(apiClient,
	                 localVarPath,
	                 localVarQueryParameters,
	                 localVarHeaderParameters,
	                 localVarFormParameters,
	                 localVarHeaderType,
	                 localVarContentType,
	                 localVarBodyParameters,
	                 "GET");

	// No return type
	apiClient_free(apiClient);
	list_free(localVarQueryParameters);
	list_free(localVarHeaderParameters);
	list_free(localVarFormParameters);
	list_free(localVarHeaderType);
	list_free(localVarContentType);
	free(localVarPath);
}

// Updated user
//
// This can only be done by the logged in user.
//
void *UserAPI_updateUser(apiClient_t *apiClient, char *username, user_t *user) {
	list_t *localVarQueryParameters = list_create();
	list_t *localVarHeaderParameters = list_create();
	list_t *localVarFormParameters = list_create();
	list_t *localVarHeaderType = list_create();
	list_t *localVarContentType = list_create();
	char *localVarBodyParameters = NULL;

	// create the path
	char *localVarPath = malloc(MAX_BUFFER_LENGTH);
	snprintf(localVarPath, MAX_BUFFER_LENGTH, "/user/{username}");

	// Path Params
	char *localVarToReplace = malloc(sizeof(username) + 2);
	snprintf(localVarToReplace, strlen(username) + 3, "%s%s%s", "{", "username", "}");

	localVarPath = strReplace(localVarPath, localVarToReplace, username);

	// Body Param
	// string
	cJSON *localVarSingleItemJSON_user;
	localVarSingleItemJSON_user = user_convertToJSON(user);
	localVarBodyParameters = cJSON_Print(localVarSingleItemJSON_user);

	apiClient_invoke(apiClient,
	                 localVarPath,
	                 localVarQueryParameters,
	                 localVarHeaderParameters,
	                 localVarFormParameters,
	                 localVarHeaderType,
	                 localVarContentType,
	                 localVarBodyParameters,
	                 "PUT");

	// No return type
	apiClient_free(apiClient);
	list_free(localVarQueryParameters);
	list_free(localVarHeaderParameters);
	list_free(localVarFormParameters);
	list_free(localVarHeaderType);
	list_free(localVarContentType);
	free(localVarPath);
	free(localVarToReplace);
}
