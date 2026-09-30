#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include "user.h"



static user_t *user_create_internal(
    long *id,
    char *username,
    char *first_name,
    char *last_name,
    char *email,
    char *password,
    char *phone,
    int *user_status,
    list_t* extra,
    openapi_petstore_preference__e preference
    ) {
    user_t *user_local_var = malloc(sizeof(user_t));
    if (!user_local_var) {
        return NULL;
    }
    memset(user_local_var, 0, sizeof(user_t));
    user_local_var->_library_owned = 1;
    user_local_var->id = id;
    user_local_var->username = username;
    user_local_var->first_name = first_name;
    user_local_var->last_name = last_name;
    user_local_var->email = email;
    user_local_var->password = password;
    user_local_var->phone = phone;
    user_local_var->user_status = user_status;
    user_local_var->extra = extra;
    user_local_var->preference = preference;
    return user_local_var;
}

__attribute__((deprecated)) user_t *user_create(
    long *id,
    char *username,
    char *first_name,
    char *last_name,
    char *email,
    char *password,
    char *phone,
    int *user_status,
    list_t* extra,
    openapi_petstore_preference__e preference
    ) {
    long *id_copy = NULL;
    if (id) {
        id_copy = malloc(sizeof(long));
        if (id_copy) *id_copy = *id;
    }
    int *user_status_copy = NULL;
    if (user_status) {
        user_status_copy = malloc(sizeof(int));
        if (user_status_copy) *user_status_copy = *user_status;
    }
    user_t *result = user_create_internal (
        id_copy,
        username,
        first_name,
        last_name,
        email,
        password,
        phone,
        user_status_copy,
        extra,
        preference
        );
    if (!result) {
        free(id_copy);
        free(user_status_copy);
    }
    return result;
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
    if (user->id) {
        free(user->id);
        user->id = NULL;
    }
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
    if (user->user_status) {
        free(user->user_status);
        user->user_status = NULL;
    }
    if (user->extra) {
        list_ForEach(listEntry, user->extra) {
            keyValuePair_t *localKeyValue = listEntry->data;
            free (localKeyValue->key);
            free (localKeyValue->value);
            keyValuePair_free(localKeyValue);
        }
        list_freeList(user->extra);
        user->extra = NULL;
    }
    free(user);
}

cJSON *user_convertToJSON(user_t *user) {
    cJSON *item = cJSON_CreateObject();

    // user->id
    if(user->id) {
    if(cJSON_AddNumberToObject(item, "id", *user->id) == NULL) {
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
    if(cJSON_AddNumberToObject(item, "userStatus", *user->user_status) == NULL) {
    goto fail; //Numeric
    }
    }


    // user->extra
    if(user->extra) {
    cJSON *extra = cJSON_AddObjectToObject(item, "extra");
    if(extra == NULL) {
        goto fail; //primitive map container
    }
    cJSON *localMapObject = extra;
    listEntry_t *extraListEntry;
    if (user->extra) {
    list_ForEach(extraListEntry, user->extra) {
        keyValuePair_t *localKeyValue = extraListEntry->data;
    }
    }
    }


    // user->preference
    if(user->preference != openapi_petstore_preference__NULL) {
    cJSON *preference_local_JSON = preference_convertToJSON(user->preference);
    if(preference_local_JSON == NULL) {
        goto fail; // custom
    }
    cJSON_AddItemToObject(item, "preference", preference_local_JSON);
    if(item->child == NULL) {
        goto fail;
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

    // define the local variable for user->id
    long *id_local_var = NULL;

    char *username_local_str = NULL;

    char *first_name_local_str = NULL;

    char *last_name_local_str = NULL;

    char *email_local_str = NULL;

    char *password_local_str = NULL;

    char *phone_local_str = NULL;

    // define the local variable for user->user_status
    int *user_status_local_var = NULL;

    // define the local map for user->extra
    list_t *extraList = NULL;

    // define the local variable for user->preference
    openapi_petstore_preference__e preference_local_nonprim = 0;

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
    id_local_var = malloc(sizeof(long));
    if(!id_local_var)
    {
        goto end;
    }
    *id_local_var = id->valuedouble;
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
    user_status_local_var = malloc(sizeof(int));
    if(!user_status_local_var)
    {
        goto end;
    }
    *user_status_local_var = user_status->valuedouble;
    }

    // user->extra
    cJSON *extra = cJSON_GetObjectItemCaseSensitive(userJSON, "extra");
    if (cJSON_IsNull(extra)) {
        extra = NULL;
    }
    if (extra) { 
    cJSON *extra_local_map = NULL;
    if(!cJSON_IsObject(extra) && !cJSON_IsNull(extra))
    {
        goto end;//primitive map container
    }
    if(cJSON_IsObject(extra))
    {
        extraList = list_createList();
        keyValuePair_t *localMapKeyPair;
        cJSON_ArrayForEach(extra_local_map, extra)
        {
            cJSON *localMapObject = extra_local_map;
            list_addElement(extraList , localMapKeyPair);
        }
    }
    }

    // user->preference
    cJSON *preference = cJSON_GetObjectItemCaseSensitive(userJSON, "preference");
    if (cJSON_IsNull(preference)) {
        preference = NULL;
    }
    if (preference) { 
    preference_local_nonprim = preference_parseFromJSON(preference); //custom
    }


    if (username && !cJSON_IsNull(username)) username_local_str = strdup(username->valuestring);
    if (first_name && !cJSON_IsNull(first_name)) first_name_local_str = strdup(first_name->valuestring);
    if (last_name && !cJSON_IsNull(last_name)) last_name_local_str = strdup(last_name->valuestring);
    if (email && !cJSON_IsNull(email)) email_local_str = strdup(email->valuestring);
    if (password && !cJSON_IsNull(password)) password_local_str = strdup(password->valuestring);
    if (phone && !cJSON_IsNull(phone)) phone_local_str = strdup(phone->valuestring);

    user_local_var = user_create_internal (
        id_local_var,
        username_local_str,
        first_name_local_str,
        last_name_local_str,
        email_local_str,
        password_local_str,
        phone_local_str,
        user_status_local_var,
        extra ? extraList : NULL,
        preference ? preference_local_nonprim : 0
        );

    if (!user_local_var) {
        goto end;
    }

    return user_local_var;
end:
    if (id_local_var) {
        free(id_local_var);
        id_local_var = NULL;
    }
    if (username_local_str) {
        free(username_local_str);
        username_local_str = NULL;
    }
    if (first_name_local_str) {
        free(first_name_local_str);
        first_name_local_str = NULL;
    }
    if (last_name_local_str) {
        free(last_name_local_str);
        last_name_local_str = NULL;
    }
    if (email_local_str) {
        free(email_local_str);
        email_local_str = NULL;
    }
    if (password_local_str) {
        free(password_local_str);
        password_local_str = NULL;
    }
    if (phone_local_str) {
        free(phone_local_str);
        phone_local_str = NULL;
    }
    if (user_status_local_var) {
        free(user_status_local_var);
        user_status_local_var = NULL;
    }
    if (extraList) {
        listEntry_t *listEntry = NULL;
        list_ForEach(listEntry, extraList) {
            keyValuePair_t *localKeyValue = listEntry->data;
            free(localKeyValue->key);
            localKeyValue->key = NULL;
            keyValuePair_free(localKeyValue);
            localKeyValue = NULL;
        }
        list_freeList(extraList);
        extraList = NULL;
    }
    if (preference_local_nonprim) {
        preference_local_nonprim = 0;
    }
    return NULL;

}
