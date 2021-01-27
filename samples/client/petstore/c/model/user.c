#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include "user.h"



user_t *user_create(
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

    return user_local_var;
}


void user_free(user_t *user) {
    if(NULL == user){
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

    // user->first_name
    cJSON *first_name = cJSON_GetObjectItemCaseSensitive(userJSON, "firstName");
    if (first_name) { 
    if(!cJSON_IsString(first_name))
    {
    goto end; //String
    }
    }

    // user->last_name
    cJSON *last_name = cJSON_GetObjectItemCaseSensitive(userJSON, "lastName");
    if (last_name) { 
    if(!cJSON_IsString(last_name))
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

    // user->user_status
    cJSON *user_status = cJSON_GetObjectItemCaseSensitive(userJSON, "userStatus");
    if (user_status) { 
    if(!cJSON_IsNumber(user_status))
    {
    goto end; //Numeric
    }
    }


    user_local_var = user_create (
        id ? id->valuedouble : 0,
        username ? strdup(username->valuestring) : NULL,
        first_name ? strdup(first_name->valuestring) : NULL,
        last_name ? strdup(last_name->valuestring) : NULL,
        email ? strdup(email->valuestring) : NULL,
        password ? strdup(password->valuestring) : NULL,
        phone ? strdup(phone->valuestring) : NULL,
        user_status ? user_status->valuedouble : 0
        );

    return user_local_var;
end:
    return NULL;

}
