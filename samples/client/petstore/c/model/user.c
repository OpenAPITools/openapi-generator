#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include "cJSON.h"
#include "list.h"
#include "keyValuePair.h"
#include "user.h"


user_t *user_create(long id, char *username, char *firstName, char *lastName,
                    char *email, char *password, char *phone, int userStatus) {
	user_t *user = malloc(sizeof(user_t));
	user->id = id;
	user->username = username;
	user->firstName = firstName;
	user->lastName = lastName;
	user->email = email;
	user->password = password;
	user->phone = phone;
	user->userStatus = userStatus;

	return user;
}


void user_free(user_t *user) {
	listEntry_t *listEntry;
	free(user->username);
	free(user->firstName);
	free(user->lastName);
	free(user->email);
	free(user->password);
	free(user->phone);

	free(user);
}

cJSON *user_convertToJSON(user_t *user) {
	cJSON *item = cJSON_CreateObject();
	// user->id
	if(cJSON_AddNumberToObject(item, "id", user->id) == NULL) {
		goto fail; // Numeric
	}

	// user->username
	if(cJSON_AddStringToObject(item, "username", user->username) == NULL) {
		goto fail; // String
	}

	// user->firstName
	if(cJSON_AddStringToObject(item, "firstName",
	                           user->firstName) == NULL)
	{
		goto fail; // String
	}

	// user->lastName
	if(cJSON_AddStringToObject(item, "lastName", user->lastName) == NULL) {
		goto fail; // String
	}

	// user->email
	if(cJSON_AddStringToObject(item, "email", user->email) == NULL) {
		goto fail; // String
	}

	// user->password
	if(cJSON_AddStringToObject(item, "password", user->password) == NULL) {
		goto fail; // String
	}

	// user->phone
	if(cJSON_AddStringToObject(item, "phone", user->phone) == NULL) {
		goto fail; // String
	}

	// user->userStatus
	if(cJSON_AddNumberToObject(item, "userStatus",
	                           user->userStatus) == NULL)
	{
		goto fail; // Numeric
	}

	return item;
fail:
	cJSON_Delete(item);
	return NULL;
}

user_t *user_parseFromJSON(char *jsonString) {
	user_t *user = NULL;
	cJSON *userJSON = cJSON_Parse(jsonString);
	if(userJSON == NULL) {
		const char *error_ptr = cJSON_GetErrorPtr();
		if(error_ptr != NULL) {
			fprintf(stderr, "Error Before: %s\n", error_ptr);
			goto end;
		}
	}

	// user->id
	cJSON *id = cJSON_GetObjectItemCaseSensitive(userJSON, "id");
	if(!cJSON_IsNumber(id)) {
		goto end; // Numeric
	}

	// user->username
	cJSON *username =
		cJSON_GetObjectItemCaseSensitive(userJSON, "username");
	if(!cJSON_IsString(username) ||
	   (username->valuestring == NULL))
	{
		goto end; // String
	}

	// user->firstName
	cJSON *firstName = cJSON_GetObjectItemCaseSensitive(userJSON,
	                                                    "firstName");
	if(!cJSON_IsString(firstName) ||
	   (firstName->valuestring == NULL))
	{
		goto end; // String
	}

	// user->lastName
	cJSON *lastName =
		cJSON_GetObjectItemCaseSensitive(userJSON, "lastName");
	if(!cJSON_IsString(lastName) ||
	   (lastName->valuestring == NULL))
	{
		goto end; // String
	}

	// user->email
	cJSON *email = cJSON_GetObjectItemCaseSensitive(userJSON, "email");
	if(!cJSON_IsString(email) ||
	   (email->valuestring == NULL))
	{
		goto end; // String
	}

	// user->password
	cJSON *password =
		cJSON_GetObjectItemCaseSensitive(userJSON, "password");
	if(!cJSON_IsString(password) ||
	   (password->valuestring == NULL))
	{
		goto end; // String
	}

	// user->phone
	cJSON *phone = cJSON_GetObjectItemCaseSensitive(userJSON, "phone");
	if(!cJSON_IsString(phone) ||
	   (phone->valuestring == NULL))
	{
		goto end; // String
	}

	// user->userStatus
	cJSON *userStatus = cJSON_GetObjectItemCaseSensitive(userJSON,
	                                                     "userStatus");
	if(!cJSON_IsNumber(userStatus)) {
		goto end; // Numeric
	}


	user = user_create(
		id->valuedouble,
		strdup(username->valuestring),
		strdup(firstName->valuestring),
		strdup(lastName->valuestring),
		strdup(email->valuestring),
		strdup(password->valuestring),
		strdup(phone->valuestring),
		userStatus->valuedouble
		);
	cJSON_Delete(userJSON);
	return user;
end:
	cJSON_Delete(userJSON);
	return NULL;
}
