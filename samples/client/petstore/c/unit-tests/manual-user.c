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
	user_t *newuser = user_create(USER_ID, USER_NAME, FIRST_NAME, LAST_NAME,
	                              EMAIL,
	                              PASSWORD, PHONE, USER_STATUS);

	cJSON *JSONNODE = user_convertToJSON(newuser);

	char *dataToPrint = cJSON_Print(JSONNODE);

	printf("Created User is: \n%s\n", dataToPrint);

	user_t *pasrsedUser = user_parseFromJSON(JSONNODE);

	cJSON *fromJSON = user_convertToJSON(pasrsedUser);

	char *dataToPrintFromJSON = cJSON_Print(fromJSON);

	printf("Parsed User From JSON is: \n%s\n", dataToPrintFromJSON);

	user_free(newuser);
	user_free(pasrsedUser);
	cJSON_Delete(JSONNODE);
	cJSON_Delete(fromJSON);
}
