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
void UserAPI_createUser(apiClient_t *apiClient, user_t *user) {
	list_t *localVarQueryParameters = NULL;
	list_t *localVarHeaderParameters = NULL;
	list_t *localVarFormParameters = NULL;
	list_t *localVarHeaderType = NULL;
	list_t *localVarContentType = NULL;
	char *localVarBodyParameters = NULL;

	// create the path
	long sizeOfPath = strlen("/user") + 1;
	char *localVarPath = malloc(sizeOfPath);
	snprintf(localVarPath, sizeOfPath, "/user");


	// Body Param
	cJSON *localVarSingleItemJSON_user;
	if(user != NULL) {
		// string
		localVarSingleItemJSON_user = user_convertToJSON(user);
		localVarBodyParameters =
			cJSON_Print(localVarSingleItemJSON_user);
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

	if(apiClient->response_code == 0) {
		printf("%s\n", "successful operation");
	}
	// No return type
end:    apiClient_free(apiClient);





	free(localVarPath);
	cJSON_Delete(localVarSingleItemJSON_user);
	free(localVarBodyParameters);
}

// Creates list of users with given input array
//
void UserAPI_createUsersWithArrayInput(apiClient_t *apiClient, list_t *user) {
	list_t *localVarQueryParameters = NULL;
	list_t *localVarHeaderParameters = NULL;
	list_t *localVarFormParameters = NULL;
	list_t *localVarHeaderType = NULL;
	list_t *localVarContentType = NULL;
	char *localVarBodyParameters = NULL;

	// create the path
	long sizeOfPath = strlen("/user/createWithArray") + 1;
	char *localVarPath = malloc(sizeOfPath);
	snprintf(localVarPath, sizeOfPath, "/user/createWithArray");


	// Body Param
	// notstring
	cJSON *localVar_user;
	cJSON *localVarItemJSON_user;
	cJSON *localVarSingleItemJSON_user;
	if(user != NULL) {
		localVarItemJSON_user = cJSON_CreateObject();
		localVarSingleItemJSON_user = cJSON_AddArrayToObject(
			localVarItemJSON_user, "user");
		if(localVarSingleItemJSON_user == NULL) {
			// nonprimitive container
			const char *error_ptr = cJSON_GetErrorPtr();
			if(error_ptr != NULL) {
				fprintf(stderr, "Error Before: %s\n",
				        error_ptr);
				goto end;
			}
		}
		listEntry_t *userBodyListEntry;

		list_ForEach(userBodyListEntry, user) {
			localVar_user = user_convertToJSON(
				userBodyListEntry->data);
			if(localVar_user == NULL) {
				const char *error_ptr = cJSON_GetErrorPtr();
				if(error_ptr != NULL) {
					fprintf(stderr, "Error Before: %s\n",
					        error_ptr);
					goto end;
				}
			}
			cJSON_AddItemToArray(localVarSingleItemJSON_user,
			                     localVar_user);
		}

		localVarBodyParameters = cJSON_Print(localVarItemJSON_user);
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

	if(apiClient->response_code == 0) {
		printf("%s\n", "successful operation");
	}
	// No return type
end:    apiClient_free(apiClient);





	free(localVarPath);
	cJSON_Delete(localVarItemJSON_user);
	cJSON_Delete(localVarSingleItemJSON_user);
	cJSON_Delete(localVar_user);
	free(localVarBodyParameters);
}

// Creates list of users with given input array
//
void UserAPI_createUsersWithListInput(apiClient_t *apiClient, list_t *user) {
	list_t *localVarQueryParameters = NULL;
	list_t *localVarHeaderParameters = NULL;
	list_t *localVarFormParameters = NULL;
	list_t *localVarHeaderType = NULL;
	list_t *localVarContentType = NULL;
	char *localVarBodyParameters = NULL;

	// create the path
	long sizeOfPath = strlen("/user/createWithList") + 1;
	char *localVarPath = malloc(sizeOfPath);
	snprintf(localVarPath, sizeOfPath, "/user/createWithList");


	// Body Param
	// notstring
	cJSON *localVar_user;
	cJSON *localVarItemJSON_user;
	cJSON *localVarSingleItemJSON_user;
	if(user != NULL) {
		localVarItemJSON_user = cJSON_CreateObject();
		localVarSingleItemJSON_user = cJSON_AddArrayToObject(
			localVarItemJSON_user, "user");
		if(localVarSingleItemJSON_user == NULL) {
			// nonprimitive container
			const char *error_ptr = cJSON_GetErrorPtr();
			if(error_ptr != NULL) {
				fprintf(stderr, "Error Before: %s\n",
				        error_ptr);
				goto end;
			}
		}
		listEntry_t *userBodyListEntry;

		list_ForEach(userBodyListEntry, user) {
			localVar_user = user_convertToJSON(
				userBodyListEntry->data);
			if(localVar_user == NULL) {
				const char *error_ptr = cJSON_GetErrorPtr();
				if(error_ptr != NULL) {
					fprintf(stderr, "Error Before: %s\n",
					        error_ptr);
					goto end;
				}
			}
			cJSON_AddItemToArray(localVarSingleItemJSON_user,
			                     localVar_user);
		}

		localVarBodyParameters = cJSON_Print(localVarItemJSON_user);
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

	if(apiClient->response_code == 0) {
		printf("%s\n", "successful operation");
	}
	// No return type
end:    apiClient_free(apiClient);





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
void UserAPI_deleteUser(apiClient_t *apiClient, char *username) {
	list_t *localVarQueryParameters = NULL;
	list_t *localVarHeaderParameters = NULL;
	list_t *localVarFormParameters = NULL;
	list_t *localVarHeaderType = NULL;
	list_t *localVarContentType = NULL;
	char *localVarBodyParameters = NULL;

	// create the path
	long sizeOfPath = strlen("/user/{username}") + 1;
	char *localVarPath = malloc(sizeOfPath);
	snprintf(localVarPath, sizeOfPath, "/user/{username}");


	// Path Params
	long sizeOfPathParams_username = strlen(username) + 3 + strlen(
		"{ username }");

	if(username == NULL) {
		goto end;
	}
	char *localVarToReplace_username = malloc(sizeOfPathParams_username);
	sprintf(localVarToReplace_username, "%s%s%s", "{", "username", "}");

	localVarPath = strReplace(localVarPath, localVarToReplace_username,
	                          username);
	apiClient_invoke(apiClient,
	                 localVarPath,
	                 localVarQueryParameters,
	                 localVarHeaderParameters,
	                 localVarFormParameters,
	                 localVarHeaderType,
	                 localVarContentType,
	                 localVarBodyParameters,
	                 "DELETE");

	if(apiClient->response_code == 400) {
		printf("%s\n", "Invalid username supplied");
	}
	if(apiClient->response_code == 404) {
		printf("%s\n", "User not found");
	}
	// No return type
end:    apiClient_free(apiClient);





	free(localVarPath);
	free(localVarToReplace_username);
}

// Get user by user name
//
user_t *UserAPI_getUserByName(apiClient_t *apiClient, char *username) {
	list_t *localVarQueryParameters = NULL;
	list_t *localVarHeaderParameters = NULL;
	list_t *localVarFormParameters = NULL;
	list_t *localVarHeaderType = list_create();
	list_t *localVarContentType = NULL;
	char *localVarBodyParameters = NULL;

	// create the path
	long sizeOfPath = strlen("/user/{username}") + 1;
	char *localVarPath = malloc(sizeOfPath);
	snprintf(localVarPath, sizeOfPath, "/user/{username}");


	// Path Params
	long sizeOfPathParams_username = strlen(username) + 3 + strlen(
		"{ username }");

	if(username == NULL) {
		goto end;
	}
	char *localVarToReplace_username = malloc(sizeOfPathParams_username);
	sprintf(localVarToReplace_username, "%s%s%s", "{", "username", "}");

	localVarPath = strReplace(localVarPath, localVarToReplace_username,
	                          username);
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

	if(apiClient->response_code == 200) {
		printf("%s\n", "successful operation");
	}
	if(apiClient->response_code == 400) {
		printf("%s\n", "Invalid username supplied");
	}
	if(apiClient->response_code == 404) {
		printf("%s\n", "User not found");
	}
	// nonprimitive not container
	user_t *elementToReturn = user_parseFromJSON(apiClient->dataReceived);
	if(elementToReturn == NULL) {
		// return 0;
	}

	// return type
	apiClient_free(apiClient);



	list_free(localVarHeaderType);

	free(localVarPath);
	free(localVarToReplace_username);
	return elementToReturn;
end:
	return NULL;
}

// Logs user into the system
//
char *UserAPI_loginUser(apiClient_t *apiClient, char *username,
                        char *password) {
	list_t *localVarQueryParameters = list_create();
	list_t *localVarHeaderParameters = NULL;
	list_t *localVarFormParameters = NULL;
	list_t *localVarHeaderType = list_create();
	list_t *localVarContentType = NULL;
	char *localVarBodyParameters = NULL;

	// create the path
	long sizeOfPath = strlen("/user/login") + 1;
	char *localVarPath = malloc(sizeOfPath);
	snprintf(localVarPath, sizeOfPath, "/user/login");


	// query parameters
	char *keyQuery_username;
	char *valueQuery_username;
	keyValuePair_t *keyPairQuery_username = 0;
	if(username) {
		// string
		keyQuery_username = strdup("username");
		valueQuery_username = strdup(username);
		keyPairQuery_username = keyValuePair_create(keyQuery_username,
		                                            valueQuery_username);
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
		keyPairQuery_password = keyValuePair_create(keyQuery_password,
		                                            valueQuery_password);
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

	if(apiClient->response_code == 200) {
		printf("%s\n", "successful operation");
	}
	if(apiClient->response_code == 400) {
		printf("%s\n", "Invalid username/password supplied");
	}
	// primitive reutrn type simple
	char *elementToReturn = strdup((char *) apiClient->dataReceived);

	apiClient_free(apiClient);
	list_free(localVarQueryParameters);


	list_free(localVarHeaderType);

	free(localVarPath);
	free(keyQuery_username);
	free(valueQuery_username);
	keyValuePair_free(keyPairQuery_username);
	free(keyQuery_password);
	free(valueQuery_password);
	keyValuePair_free(keyPairQuery_password);
	return elementToReturn;
end:
	return NULL;
}

// Logs out current logged in user session
//
void UserAPI_logoutUser(apiClient_t *apiClient) {
	list_t *localVarQueryParameters = NULL;
	list_t *localVarHeaderParameters = NULL;
	list_t *localVarFormParameters = NULL;
	list_t *localVarHeaderType = NULL;
	list_t *localVarContentType = NULL;
	char *localVarBodyParameters = NULL;

	// create the path
	long sizeOfPath = strlen("/user/logout") + 1;
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

	if(apiClient->response_code == 0) {
		printf("%s\n", "successful operation");
	}
	// No return type
end:    apiClient_free(apiClient);





	free(localVarPath);
}

// Updated user
//
// This can only be done by the logged in user.
//
void UserAPI_updateUser(apiClient_t *apiClient, char *username, user_t *user) {
	list_t *localVarQueryParameters = NULL;
	list_t *localVarHeaderParameters = NULL;
	list_t *localVarFormParameters = NULL;
	list_t *localVarHeaderType = NULL;
	list_t *localVarContentType = NULL;
	char *localVarBodyParameters = NULL;

	// create the path
	long sizeOfPath = strlen("/user/{username}") + 1;
	char *localVarPath = malloc(sizeOfPath);
	snprintf(localVarPath, sizeOfPath, "/user/{username}");


	// Path Params
	long sizeOfPathParams_username = strlen(username) + 3 + strlen(
		"{ username }");

	if(username == NULL) {
		goto end;
	}
	char *localVarToReplace_username = malloc(sizeOfPathParams_username);
	sprintf(localVarToReplace_username, "%s%s%s", "{", "username", "}");

	localVarPath = strReplace(localVarPath, localVarToReplace_username,
	                          username);

	// Body Param
	cJSON *localVarSingleItemJSON_user;
	if(user != NULL) {
		// string
		localVarSingleItemJSON_user = user_convertToJSON(user);
		localVarBodyParameters =
			cJSON_Print(localVarSingleItemJSON_user);
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

	if(apiClient->response_code == 400) {
		printf("%s\n", "Invalid user supplied");
	}
	if(apiClient->response_code == 404) {
		printf("%s\n", "User not found");
	}
	// No return type
end:    apiClient_free(apiClient);





	free(localVarPath);
	free(localVarToReplace_username);
	cJSON_Delete(localVarSingleItemJSON_user);
	free(localVarBodyParameters);
}
