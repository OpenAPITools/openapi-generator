#include <stdlib.h>
#include <stdio.h>
#include "../include/apiClient.h"
#include "../include/list.h"
#include "../external/cJSON.h"
#include "../include/keyValuePair.h"
#include "../include/binary.h"
#include "../model/object.h"


// Returns private information.
//
// This endpoint requires global security settings.
//
object_t*
DefaultAPI_privateGet(apiClient_t *apiClient);


// Returns public information.
//
// This endpoint does not require authentication.
//
object_t*
DefaultAPI_publicGet(apiClient_t *apiClient);


// Returns a list of users.
//
// Optional extended description in CommonMark or HTML.
//
list_t*
DefaultAPI_usersGet(apiClient_t *apiClient);


