/*
 * preference.h
 *
 * A user&#39;s preference in pets
 */

#ifndef _preference_H_
#define _preference_H_

#include <string.h>
#include "../external/cJSON.h"
#include "../include/list.h"
#include "../include/keyValuePair.h"
#include "../include/binary.h"

typedef struct preference_t preference_t;


// Enum  for preference

typedef enum { openapi_petstore_preference__NULL = 0, openapi_petstore_preference__cats, openapi_petstore_preference__dogs, openapi_petstore_preference__birds, openapi_petstore_preference__fish, openapi_petstore_preference__snakes, openapi_petstore_preference__other } openapi_petstore_preference__e;

char* preference_preference_ToString(openapi_petstore_preference__e preference);

openapi_petstore_preference__e preference_preference_FromString(char* preference);

cJSON *preference_convertToJSON(openapi_petstore_preference__e preference);

openapi_petstore_preference__e preference_parseFromJSON(cJSON *preferenceJSON);

#endif /* _preference_H_ */

