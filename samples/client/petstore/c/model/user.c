#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include "user.h"



static user_t *user_create_internal(
    long id,
    char *username,
    char *first_name,
    char *last_name,
    char *email,
    char *password,
    char *phone,
    int user_status
    ) {
    user_t *user_local_var = malloc(sizeof(user_t));
    if (!user_local_var) {
        return NULL;
    }
    user_local_var->id = id;
    user_local_var->username = username;
    user_local_var->first_name = first_name;
    user_local_var->last_name = last_name;
    user_local_var->email = email;
    user_local_var->password = password;
    user_local_var->phone = phone;
    user_local_var->user_status = user_status;

    user_local_var->_library_owned = 1;
    return user_local_var;
}

__attribute__((deprecated)) user_t *user_create(
    long id,
    char *username,
    char *first_name,
    char *last_name,
    char *email,
    char *password,
    char *phone,
    int user_status
    ) {
    return user_create_internal (
        id,
        username,
        first_name,
        last_name,
        email,
        password,
        phone,
        user_status
        );
}

void user_free(user_t *user) {
    if(NULL == user){
        return ;
    }
    if(user->_library_owned != 1){
        fprintf(stderr, "WARNING: %s() does NOT free objects allocated by the user\n", "user_free");
        return ;
    }
    listEntry_t *listEntry;
    if (user->username) {
        free(user->username);
        user->username = NULL;
    }
    if (user->first_name) {
        free(user->first_name);
        user->first_name = NULL;
    }
    if (user->last_name) {
        free(user->last_name);
        user->last_name = NULL;
    }
    if (user->email) {
        free(user->email);
        user->email = NULL;
    }
    if (user->password) {
        free(user->password);
        user->password = NULL;
    }
    if (user->phone) {
        free(user->phone);
        user->phone = NULL;
    }
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


    // user->first_name
    if(user->first_name) {
    if(cJSON_AddStringToObject(item, "firstName", user->first_name) == NULL) {
    goto fail; //String
    }
    }


    // user->last_name
    if(user->last_name) {
    if(cJSON_AddStringToObject(item, "lastName", user->last_name) == NULL) {
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


    // user->user_status
    if(user->user_status) {
    if(cJSON_AddNumberToObject(item, "userStatus", user->user_status) == NULL) {
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
    if (cJSON_IsNull(id)) {
        id = NULL;
    }
    if (id) { 
    if(!cJSON_IsNumber(id))
    {
    goto end; //Numeric
    }
    }

    // user->username
    cJSON *username = cJSON_GetObjectItemCaseSensitive(userJSON, "username");
    if (cJSON_IsNull(username)) {
        username = NULL;
    }
    if (username) { 
    if(!cJSON_IsString(username) && !cJSON_IsNull(username))
    {
    goto end; //String
    }
    }

    // user->first_name
    cJSON *first_name = cJSON_GetObjectItemCaseSensitive(userJSON, "firstName");
    if (cJSON_IsNull(first_name)) {
        first_name = NULL;
    }
    if (first_name) { 
    if(!cJSON_IsString(first_name) && !cJSON_IsNull(first_name))
    {
    goto end; //String
    }
    }

    // user->last_name
    cJSON *last_name = cJSON_GetObjectItemCaseSensitive(userJSON, "lastName");
    if (cJSON_IsNull(last_name)) {
        last_name = NULL;
    }
    if (last_name) { 
    if(!cJSON_IsString(last_name) && !cJSON_IsNull(last_name))
    {
    goto end; //String
    }
    }

    // user->email
    cJSON *email = cJSON_GetObjectItemCaseSensitive(userJSON, "email");
    if (cJSON_IsNull(email)) {
        email = NULL;
    }
    if (email) { 
    if(!cJSON_IsString(email) && !cJSON_IsNull(email))
    {
    goto end; //String
    }
    }

    // user->password
    cJSON *password = cJSON_GetObjectItemCaseSensitive(userJSON, "password");
    if (cJSON_IsNull(password)) {
        password = NULL;
    }
    if (password) { 
    if(!cJSON_IsString(password) && !cJSON_IsNull(password))
    {
    goto end; //String
    }
    }

    // user->phone
    cJSON *phone = cJSON_GetObjectItemCaseSensitive(userJSON, "phone");
    if (cJSON_IsNull(phone)) {
        phone = NULL;
    }
    if (phone) { 
    if(!cJSON_IsString(phone) && !cJSON_IsNull(phone))
    {
    goto end; //String
    }
    }

    // user->user_status
    cJSON *user_status = cJSON_GetObjectItemCaseSensitive(userJSON, "userStatus");
    if (cJSON_IsNull(user_status)) {
        user_status = NULL;
    }
    if (user_status) { 
    if(!cJSON_IsNumber(user_status))
    {
    goto end; //Numeric
    }
    }


    user_local_var = user_create_internal (
        id ? id->valuedouble : 0,
        username && !cJSON_IsNull(username) ? strdup(username->valuestring) : NULL,
        first_name && !cJSON_IsNull(first_name) ? strdup(first_name->valuestring) : NULL,
        last_name && !cJSON_IsNull(last_name) ? strdup(last_name->valuestring) : NULL,
        email && !cJSON_IsNull(email) ? strdup(email->valuestring) : NULL,
        password && !cJSON_IsNull(password) ? strdup(password->valuestring) : NULL,
        phone && !cJSON_IsNull(phone) ? strdup(phone->valuestring) : NULL,
        user_status ? user_status->valuedouble : 0
        );

    return user_local_var;
end:
    return NULL;

}
