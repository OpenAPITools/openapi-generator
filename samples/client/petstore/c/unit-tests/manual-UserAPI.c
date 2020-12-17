#include <stdio.h>
    #include <stdlib.h>
    #include <string.h>
    #include <assert.h>
    #include "../api/UserAPI.h"

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
// create user test
	apiClient_t *apiClient = apiClient_create();

	char *username = malloc(strlen(USER_NAME) + 1);
	strcpy(username, USER_NAME);
	char *firstname = malloc(strlen(FIRST_NAME) + 1);
	strcpy(firstname, FIRST_NAME);
	char *lastname = malloc(strlen(LAST_NAME) + 1);
	strcpy(lastname, LAST_NAME);
	char *email = malloc(strlen(EMAIL) + 1);
	strcpy(email, EMAIL);
	char *password = malloc(strlen(PASSWORD) + 1);
	strcpy(password, PASSWORD);
	char *phone = malloc(strlen(PHONE) + 1);
	strcpy(phone, PHONE);

	user_t *newuser = user_create(USER_ID,
	                              username,
	                              firstname,
	                              lastname,
	                              email,
	                              password,
	                              phone,
	                              USER_STATUS);

	UserAPI_createUser(apiClient, newuser);
	user_free(newuser);
	apiClient_free(apiClient);

// get user by name test
	apiClient_t *apiClient1 = apiClient_create();
	user_t *returnUser = UserAPI_getUserByName(apiClient1, USER_NAME);

	cJSON *JSONNODE = user_convertToJSON(returnUser);

	char *dataToPrint = cJSON_Print(JSONNODE);

	printf("User is: \n%s\n", dataToPrint);
	user_free(returnUser);
	cJSON_Delete(JSONNODE);
	free(dataToPrint);
	apiClient_free(apiClient1);

// update user test
	{
		apiClient_t *apiClient2 = apiClient_create();
		char *username1 = malloc(strlen(USER_NAME) + 1);
		strcpy(username1, USER_NAME);
		char *firstname = malloc(strlen(FIRST_NAME) + 1);
		strcpy(firstname, FIRST_NAME);
		char *lastname = malloc(strlen(LAST_NAME) + 1);
		strcpy(lastname, LAST_NAME);
		char *email = malloc(strlen(EMAIL) + 1);
		strcpy(email, EMAIL);
		char *password = malloc(strlen(PASSWORD) + 1);
		strcpy(password, PASSWORD);
		char *phone = malloc(strlen(PHONE) + 1);
		strcpy(phone, PHONE);

		user_t *newuser1 = user_create(USER_ID,
		                               username1,
		                               firstname,
		                               lastname,
		                               email,
		                               password,
		                               phone,
		                               USER_STATUS);

		UserAPI_updateUser(apiClient2, username1, newuser1);
		user_free(newuser1);
		apiClient_free(apiClient2);
	}

// login user test
	{
		char *username1 = malloc(strlen(USER_NAME) + 1);
		strcpy(username1, USER_NAME);
		char *password = malloc(strlen(PASSWORD) + 1);
		strcpy(password, PASSWORD);
		apiClient_t *apiClient3 = apiClient_create();

		char *loginuserreturn = UserAPI_loginUser(apiClient3,
		                                          username1,
		                                          password);

		printf("Login User: %s\n", loginuserreturn);
		free(loginuserreturn);
		free(username1);
		free(password);
		apiClient_free(apiClient3);
	}

// logout user test
	apiClient_t *apiClient4 = apiClient_create();

	UserAPI_logoutUser(apiClient4);
	apiClient_free(apiClient4);


// delete user test
	apiClient_t *apiClient5 = apiClient_create();

	UserAPI_deleteUser(apiClient5, "example123");
	apiClient_free(apiClient5);

	apiClient_unsetupGlobalEnv();
}
