#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include "cJSON.h"
#include "list.h"
#include "keyValuePair.h"
#include "api_response.h"


api_response_t *api_response_create(int code, char *type, char *message) {
	api_response_t *api_response = malloc(sizeof(api_response_t));
	api_response->code = code;
	api_response->type = type;
	api_response->message = message;

	return api_response;
}


void api_response_free(api_response_t *api_response) {
	listEntry_t *listEntry;
	free(api_response->type);
	free(api_response->message);

	free(api_response);
}

cJSON *api_response_convertToJSON(api_response_t *api_response) {
	cJSON *item = cJSON_CreateObject();
	// api_response->code
	if(cJSON_AddNumberToObject(item, "code", api_response->code) == NULL) {
		goto fail; // Numeric
	}

	// api_response->type
	if(cJSON_AddStringToObject(item, "type", api_response->type) == NULL) {
		goto fail; // String
	}

	// api_response->message
	if(cJSON_AddStringToObject(item, "message",
	                           api_response->message) == NULL)
	{
		goto fail; // String
	}

	return item;
fail:
	cJSON_Delete(item);
	return NULL;
}

api_response_t *api_response_parseFromJSON(char *jsonString) {
	api_response_t *api_response = NULL;
	cJSON *api_responseJSON = cJSON_Parse(jsonString);
	if(api_responseJSON == NULL) {
		const char *error_ptr = cJSON_GetErrorPtr();
		if(error_ptr != NULL) {
			fprintf(stderr, "Error Before: %s\n", error_ptr);
			goto end;
		}
	}

	// api_response->code
	cJSON *code =
		cJSON_GetObjectItemCaseSensitive(api_responseJSON, "code");
	if(!cJSON_IsNumber(code)) {
		goto end; // Numeric
	}

	// api_response->type
	cJSON *type =
		cJSON_GetObjectItemCaseSensitive(api_responseJSON, "type");
	if(!cJSON_IsString(type) ||
	   (type->valuestring == NULL))
	{
		goto end; // String
	}

	// api_response->message
	cJSON *message = cJSON_GetObjectItemCaseSensitive(api_responseJSON,
	                                                  "message");
	if(!cJSON_IsString(message) ||
	   (message->valuestring == NULL))
	{
		goto end; // String
	}


	api_response = api_response_create(
		code->valuedouble,
		strdup(type->valuestring),
		strdup(message->valuestring)
		);
	cJSON_Delete(api_responseJSON);
	return api_response;
end:
	cJSON_Delete(api_responseJSON);
	return NULL;
}
