#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include "../model/user.h"


#define USER_ID 1234
#define USER_NAME "example123"
#define FIRST_NAME "Example1"
#define LAST_NAME "Example2"
#define EMAIL "example@example.com"
#define PASSWORD "thisisexample!123"
#define PHONE "+123456789"
#define USER_STATUS 4

int main() {

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
	                              USER_STATUS,
	                              NULL,
	                              openapi_petstore_preference__cats);

	cJSON *JSONNODE = user_convertToJSON(newuser);

	char *dataToPrint = cJSON_Print(JSONNODE);

	printf("Created User is: \n%s\n", dataToPrint);

	user_t *parsedUser = user_parseFromJSON(JSONNODE);

	cJSON *fromJSON = user_convertToJSON(parsedUser);

	char *dataToPrintFromJSON = cJSON_Print(fromJSON);

	printf("Parsed User From JSON is: \n%s\n", dataToPrintFromJSON);

	user_free(newuser);
	user_free(parsedUser);
	cJSON_Delete(JSONNODE);
	cJSON_Delete(fromJSON);
}
