/*
 * user.h
 *
 * A User who is purchasing from the pet store
 */

#ifndef _user_H_
#define _user_H_

#include <string.h>

typedef struct user_t {
	long id;
	char *username;
	char *firstName;
	char *lastName;
	char *email;
	char *password;
	char *phone;
	int userStatus;
} user_t;

user_t *user_create(
		long id,
		char *username,
		char *firstName,
		char *lastName,
		char *email,
		char *password,
		char *phone,
		int userStatus
		);
		
void user_free(user_t *user);

user_t *user_parseFromJSON(char *jsonString)

cJSON *user_convertToJSON(user_t *user);

#endif /* _user_H_ */
