#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include "preference.h"


char* preference_preference_ToString(openapi_petstore_preference__e preference) {
    char *preferenceArray[] =  { "NULL", "cats", "dogs", "birds", "fish", "snakes", "other" };
    return preferenceArray[preference];
}

openapi_petstore_preference__e preference_preference_FromString(char* preference) {
    int stringToReturn = 0;
    char *preferenceArray[] =  { "NULL", "cats", "dogs", "birds", "fish", "snakes", "other" };
    size_t sizeofArray = sizeof(preferenceArray) / sizeof(preferenceArray[0]);
    while(stringToReturn < sizeofArray) {
        if(strcmp(preference, preferenceArray[stringToReturn]) == 0) {
            return stringToReturn;
        }
        stringToReturn++;
    }
    return 0;
}

cJSON *preference_convertToJSON(openapi_petstore_preference__e preference) {
    cJSON *item = cJSON_CreateObject();
    if(cJSON_AddStringToObject(item, "preference", preference_preference_ToString(preference)) == NULL) {
        goto fail;
    }
    return item;
fail:
    cJSON_Delete(item);
    return NULL;
}

openapi_petstore_preference__e preference_parseFromJSON(cJSON *preferenceJSON) {
    if(!cJSON_IsString(preferenceJSON) || (preferenceJSON->valuestring == NULL)) {
        return 0;
    }
    return preference_preference_FromString(preferenceJSON->valuestring);
}
