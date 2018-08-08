#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include "cJSON.h"


user_t *user_create(
		long		*id,
		char		*username,
		char		*firstName,
		char		*lastName,
		char		*email,
		char		*password,
		char		*phone,
		int		*userStatus
		) {
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

	free(user->username)

	free(user->firstName)

	free(user->lastName)

	free(user->email)

	free(user->password)

	free(user->phone)


	free(user);
}

cJSON *user_convertToJSON(user_t *user) {
	cJSON *item = cJSON_CreateObject();
	// user->id
	if(cJSON_AddNumberToObject(item, "id", user->id) == NULL) {
    	goto fail;
    }


	// user->username
	if(cJSON_AddStringToObject(item, "username", user->username) == NULL) {
		goto fail;
	}

	// user->firstName
	if(cJSON_AddStringToObject(item, "firstName", user->firstName) == NULL) {
		goto fail;
	}

	// user->lastName
	if(cJSON_AddStringToObject(item, "lastName", user->lastName) == NULL) {
		goto fail;
	}

	// user->email
	if(cJSON_AddStringToObject(item, "email", user->email) == NULL) {
		goto fail;
	}

	// user->password
	if(cJSON_AddStringToObject(item, "password", user->password) == NULL) {
		goto fail;
	}

	// user->phone
	if(cJSON_AddStringToObject(item, "phone", user->phone) == NULL) {
		goto fail;
	}

	// user->userStatus
	if(cJSON_AddNumberToObject(item, "userStatus", user->userStatus) == NULL) {
    	goto fail;
    }


	return item;
fail:
	cJSON_Delete(item);
	return NULL;
}
