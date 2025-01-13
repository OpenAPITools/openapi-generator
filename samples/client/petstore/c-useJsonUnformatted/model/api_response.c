#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include "api_response.h"



static api_response_t *api_response_create_internal(
    int code,
    char *type,
    char *message
    ) {
    api_response_t *api_response_local_var = malloc(sizeof(api_response_t));
    if (!api_response_local_var) {
        return NULL;
    }
    api_response_local_var->code = code;
    api_response_local_var->type = type;
    api_response_local_var->message = message;

    api_response_local_var->_library_owned = 1;
    return api_response_local_var;
}

__attribute__((deprecated)) api_response_t *api_response_create(
    int code,
    char *type,
    char *message
    ) {
    return api_response_create_internal (
        code,
        type,
        message
        );
}

void api_response_free(api_response_t *api_response) {
    if(NULL == api_response){
        return ;
    }
    if(api_response->_library_owned != 1){
        fprintf(stderr, "WARNING: %s() does NOT free objects allocated by the user\n", "api_response_free");
        return ;
    }
    listEntry_t *listEntry;
    if (api_response->type) {
        free(api_response->type);
        api_response->type = NULL;
    }
    if (api_response->message) {
        free(api_response->message);
        api_response->message = NULL;
    }
    free(api_response);
}

cJSON *api_response_convertToJSON(api_response_t *api_response) {
    cJSON *item = cJSON_CreateObject();

    // api_response->code
    if(api_response->code) {
    if(cJSON_AddNumberToObject(item, "code", api_response->code) == NULL) {
    goto fail; //Numeric
    }
    }


    // api_response->type
    if(api_response->type) {
    if(cJSON_AddStringToObject(item, "type", api_response->type) == NULL) {
    goto fail; //String
    }
    }


    // api_response->message
    if(api_response->message) {
    if(cJSON_AddStringToObject(item, "message", api_response->message) == NULL) {
    goto fail; //String
    }
    }

    return item;
fail:
    if (item) {
        cJSON_Delete(item);
    }
    return NULL;
}

api_response_t *api_response_parseFromJSON(cJSON *api_responseJSON){

    api_response_t *api_response_local_var = NULL;

    // api_response->code
    cJSON *code = cJSON_GetObjectItemCaseSensitive(api_responseJSON, "code");
    if (cJSON_IsNull(code)) {
        code = NULL;
    }
    if (code) { 
    if(!cJSON_IsNumber(code))
    {
    goto end; //Numeric
    }
    }

    // api_response->type
    cJSON *type = cJSON_GetObjectItemCaseSensitive(api_responseJSON, "type");
    if (cJSON_IsNull(type)) {
        type = NULL;
    }
    if (type) { 
    if(!cJSON_IsString(type) && !cJSON_IsNull(type))
    {
    goto end; //String
    }
    }

    // api_response->message
    cJSON *message = cJSON_GetObjectItemCaseSensitive(api_responseJSON, "message");
    if (cJSON_IsNull(message)) {
        message = NULL;
    }
    if (message) { 
    if(!cJSON_IsString(message) && !cJSON_IsNull(message))
    {
    goto end; //String
    }
    }


    api_response_local_var = api_response_create_internal (
        code ? code->valuedouble : 0,
        type && !cJSON_IsNull(type) ? strdup(type->valuestring) : NULL,
        message && !cJSON_IsNull(message) ? strdup(message->valuestring) : NULL
        );

    return api_response_local_var;
end:
    return NULL;

}
