/*
 * user.h
 *
 * A User who is purchasing from the pet store
 */

#ifndef _user_H_
#define _user_H_

#include <string.h>
#include "../external/cJSON.h"
#include "../include/list.h"
#include "../include/keyValuePair.h"



typedef struct user_t {
    long id; //numeric
    char *username; // string
    char *firstName; // string
    char *lastName; // string
    char *email; // string
    char *password; // string
    char *phone; // string
    int userStatus; //numeric

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

user_t *user_parseFromJSON(cJSON *userJSON);

cJSON *user_convertToJSON(user_t *user);

#endif /* _user_H_ */

