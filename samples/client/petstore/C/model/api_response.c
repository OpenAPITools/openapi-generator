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
	listEntry_t *listEntry;

	free(api_response->code)

	free(api_response->type)

	free(api_response->message)

	free(api_response);
}
