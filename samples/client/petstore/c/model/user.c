#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include "user.h"



user_t *user_create(
    long id,
    char *username,
    char *firstName,
    char *lastName,
    char *email,
    char *password,
    char *phone,
    int userStatus
    ) {
	user_t *user_local_var = malloc(sizeof(user_t));
    if (!user_local_var) {
        return NULL;
    }
	user_local_var->id = id;
	user_local_var->username = username;
	user_local_var->firstName = firstName;
	user_local_var->lastName = lastName;
	user_local_var->email = email;
	user_local_var->password = password;
	user_local_var->phone = phone;
	user_local_var->userStatus = userStatus;

	return user_local_var;
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
    if(user->id) { 
    if(cJSON_AddNumberToObject(item, "id", user->id) == NULL) {
    goto fail; //Numeric
    }
     } 


	// user->username
    if(user->username) { 
    if(cJSON_AddStringToObject(item, "username", user->username) == NULL) {
    goto fail; //String
    }
     } 


	// user->firstName
    if(user->firstName) { 
    if(cJSON_AddStringToObject(item, "firstName", user->firstName) == NULL) {
    goto fail; //String
    }
     } 


	// user->lastName
    if(user->lastName) { 
    if(cJSON_AddStringToObject(item, "lastName", user->lastName) == NULL) {
    goto fail; //String
    }
     } 


	// user->email
    if(user->email) { 
    if(cJSON_AddStringToObject(item, "email", user->email) == NULL) {
    goto fail; //String
    }
     } 


	// user->password
    if(user->password) { 
    if(cJSON_AddStringToObject(item, "password", user->password) == NULL) {
    goto fail; //String
    }
     } 


	// user->phone
    if(user->phone) { 
    if(cJSON_AddStringToObject(item, "phone", user->phone) == NULL) {
    goto fail; //String
    }
     } 


	// user->userStatus
    if(user->userStatus) { 
    if(cJSON_AddNumberToObject(item, "userStatus", user->userStatus) == NULL) {
    goto fail; //Numeric
    }
     } 

	return item;
fail:
	if (item) {
        cJSON_Delete(item);
    }
	return NULL;
}

user_t *user_parseFromJSON(cJSON *userJSON){

    user_t *user_local_var = NULL;

    // user->id
    cJSON *id = cJSON_GetObjectItemCaseSensitive(userJSON, "id");
    if (id) { 
    if(!cJSON_IsNumber(id))
    {
    goto end; //Numeric
    }
    }

    // user->username
    cJSON *username = cJSON_GetObjectItemCaseSensitive(userJSON, "username");
    if (username) { 
    if(!cJSON_IsString(username))
    {
    goto end; //String
    }
    }

    // user->firstName
    cJSON *firstName = cJSON_GetObjectItemCaseSensitive(userJSON, "firstName");
    if (firstName) { 
    if(!cJSON_IsString(firstName))
    {
    goto end; //String
    }
    }

    // user->lastName
    cJSON *lastName = cJSON_GetObjectItemCaseSensitive(userJSON, "lastName");
    if (lastName) { 
    if(!cJSON_IsString(lastName))
    {
    goto end; //String
    }
    }

    // user->email
    cJSON *email = cJSON_GetObjectItemCaseSensitive(userJSON, "email");
    if (email) { 
    if(!cJSON_IsString(email))
    {
    goto end; //String
    }
    }

    // user->password
    cJSON *password = cJSON_GetObjectItemCaseSensitive(userJSON, "password");
    if (password) { 
    if(!cJSON_IsString(password))
    {
    goto end; //String
    }
    }

    // user->phone
    cJSON *phone = cJSON_GetObjectItemCaseSensitive(userJSON, "phone");
    if (phone) { 
    if(!cJSON_IsString(phone))
    {
    goto end; //String
    }
    }

    // user->userStatus
    cJSON *userStatus = cJSON_GetObjectItemCaseSensitive(userJSON, "userStatus");
    if (userStatus) { 
    if(!cJSON_IsNumber(userStatus))
    {
    goto end; //Numeric
    }
    }


    user_local_var = user_create (
        id ? id->valuedouble : 0,
        username ? strdup(username->valuestring) : NULL,
        firstName ? strdup(firstName->valuestring) : NULL,
        lastName ? strdup(lastName->valuestring) : NULL,
        email ? strdup(email->valuestring) : NULL,
        password ? strdup(password->valuestring) : NULL,
        phone ? strdup(phone->valuestring) : NULL,
        userStatus ? userStatus->valuedouble : 0
        );

    return user_local_var;
end:
    return NULL;

}
