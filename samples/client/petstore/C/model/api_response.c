#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include "cJSON.h"


api_response_t *api_response_create(
		int		*code,
		char		*type,
		char		*message
		) {
	api_response_t *api_response = malloc(sizeof(api_response_t));
	api_response->code = code;
	api_response->type = type;
	api_response->message = message;

	return api_response;
}


void api_response_free(api_response_t *api_response) {

	free(api_response->type)

	free(api_response->message)

	free(api_response);
}

cJSON *api_response_convertToJSON(api_response_t *api_response) {
	cJSON *item = cJSON_CreateObject();
	// api_response->code
	if(cJSON_AddNumberToObject(item, "code", api_response->code) == NULL) {
    	goto fail;
    }


	// api_response->type
	if(cJSON_AddStringToObject(item, "type", api_response->type) == NULL) {
		goto fail;
	}

	// api_response->message
	if(cJSON_AddStringToObject(item, "message", api_response->message) == NULL) {
		goto fail;
	}

	return item;
fail:
	cJSON_Delete(item);
	return NULL;
}
