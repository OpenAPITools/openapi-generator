/*
 * api_response.h
 *
 * Describes the result of uploading an image resource
 */

#ifndef _api_response_H_
#define _api_response_H_

#include <string.h>
#include "../external/cJSON.h"
#include "../include/list.h"
#include "../include/keyValuePair.h"
#include "../include/binary.h"

typedef struct api_response_t api_response_t;




typedef struct api_response_t {
    int code; //numeric
    char *type; // string
    char *message; // string

} api_response_t;

api_response_t *api_response_create(
    int code,
    char *type,
    char *message
);

void api_response_free(api_response_t *api_response);

api_response_t *api_response_parseFromJSON(cJSON *api_responseJSON);

cJSON *api_response_convertToJSON(api_response_t *api_response);

#endif /* _api_response_H_ */

