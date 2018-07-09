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
	listEntry_t *listEntry;

	free(user->id)

	free(user->username)

	free(user->firstName)

	free(user->lastName)

	free(user->email)

	free(user->password)

	free(user->phone)

	free(user->userStatus)

	free(user);
}
