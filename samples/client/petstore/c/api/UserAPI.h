#include <stdlib.h>
#include <stdio.h>
#include "apiClient.h"
#include "cJSON.h"
#include "list.h"
#include "user.h"


// Create user
//
// This can only be done by the logged in user.
//
void UserAPI_createUser(apiClient_t *apiClient, user_t *user);



// Creates list of users with given input array
//
void UserAPI_createUsersWithArrayInput(apiClient_t *apiClient, list_t *user);



// Creates list of users with given input array
//
void UserAPI_createUsersWithListInput(apiClient_t *apiClient, list_t *user);



// Delete user
//
// This can only be done by the logged in user.
//
void UserAPI_deleteUser(apiClient_t *apiClient, char *username);



// Get user by user name
//
user_t *UserAPI_getUserByName(apiClient_t *apiClient, char *username);



// Logs user into the system
//
char *UserAPI_loginUser(apiClient_t *apiClient, char *username, char *password);



// Logs out current logged in user session
//
void UserAPI_logoutUser(apiClient_t *apiClient);



// Updated user
//
// This can only be done by the logged in user.
//
void UserAPI_updateUser(apiClient_t *apiClient, char *username, user_t *user);
