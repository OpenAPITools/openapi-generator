#include <stdio.h>
    #define MAX_BUFFER_LENGTH 9
    #include <stdlib.h>
    #include <string.h>
    #include <assert.h>
    #include "apiClient.h"
    #include "cJSON.h"
    #include "keyValuePair.h"
    #include "user.h"
    #include "UserAPI.h"

    #define USER_ID 1234
    #define USER_NAME "example123"
    #define FIRST_NAME "Example1"
    #define LAST_NAME "Example2Last"
    #define LAST_NAME1 "LastName"
    #define EMAIL "example@example.com"
    #define PASSWORD "thisisexample!123"
    #define PHONE "+123456789"
    #define USER_STATUS 4


int main() {
	printf("Hello world1\n");
	apiClient_t *apiClient = apiClient_create();

	user_t *newuser = user_create(USER_ID, USER_NAME, FIRST_NAME, LAST_NAME, EMAIL,
	                              PASSWORD, PHONE, USER_STATUS);

	UserAPI_createUser(apiClient, newuser);


	printf(
		"------------------------------ Part Ends ----------------------------------\n");

	apiClient_t *apiClient1 = apiClient_create();
	user_t *returnUser = UserAPI_getUserByName(apiClient1, USER_NAME);

	cJSON *JSONNODE = user_convertToJSON(newuser);

	char *dataToPrint = cJSON_Print(JSONNODE);

	printf("User is: \n%s\n", dataToPrint);


	printf(
		"------------------------------ Part Ends ----------------------------------\n");

	apiClient_t *apiClient2 = apiClient_create();

	user_t *newuser1 = user_create(USER_ID, USER_NAME, FIRST_NAME, LAST_NAME1, EMAIL,
	                               PASSWORD, PHONE, USER_STATUS);

	UserAPI_updateUser(apiClient2, "example123", newuser1);
	printf(
		"------------------------------ Part Ends ----------------------------------\n");

	apiClient_t *apiClient3 = apiClient_create();

	char *loginuserreturn = UserAPI_loginUser(apiClient3, "example123", "thisisexample!123");

	printf("Login User: %s\n", loginuserreturn);


	printf(
		"------------------------------ Part Ends ----------------------------------\n");

	apiClient_t *apiClient4 = apiClient_create();

	UserAPI_logoutUser(apiClient4);

	printf(
		"------------------------------ Part Ends ----------------------------------\n");

	apiClient_t *apiClient5 = apiClient_create();

	UserAPI_deleteUser(apiClient5, "example123");

}
