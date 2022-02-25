#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include "api_response.h"



api_response_t *api_response_create(
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

    return api_response_local_var;
}


void api_response_free(api_response_t *api_response) {
    if(NULL == api_response){
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
    if (code) { 
    if(!cJSON_IsNumber(code))
    {
    goto end; //Numeric
    }
    }

    // api_response->type
    cJSON *type = cJSON_GetObjectItemCaseSensitive(api_responseJSON, "type");
    if (type) { 
    if(!cJSON_IsString(type))
    {
    goto end; //String
    }
    }

    // api_response->message
    cJSON *message = cJSON_GetObjectItemCaseSensitive(api_responseJSON, "message");
    if (message) { 
    if(!cJSON_IsString(message))
    {
    goto end; //String
    }
    }


    api_response_local_var = api_response_create (
        code ? code->valuedouble : 0,
        type ? strdup(type->valuestring) : NULL,
        message ? strdup(message->valuestring) : NULL
        );

    return api_response_local_var;
end:
    return NULL;

}
