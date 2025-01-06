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
#include "../include/binary.h"

typedef struct user_t user_t;

#include "any_type.h"
#include "preference.h"



typedef struct user_t {
    long id; //numeric
    char *username; // string
    char *first_name; // string
    char *last_name; // string
    char *email; // string
    char *password; // string
    char *phone; // string
    int user_status; //numeric
    list_t* extra; //map
    openapi_petstore_preference__e preference; //referenced enum

    int _library_owned; // Is the library responsible for freeing this object?
} user_t;

__attribute__((deprecated)) user_t *user_create(
    long id,
    char *username,
    char *first_name,
    char *last_name,
    char *email,
    char *password,
    char *phone,
    int user_status,
    list_t* extra,
    openapi_petstore_preference__e preference
);

void user_free(user_t *user);

user_t *user_parseFromJSON(cJSON *userJSON);

cJSON *user_convertToJSON(user_t *user);

#endif /* _user_H_ */

