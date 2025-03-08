#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include "bit.h"


char* bit_bit_ToString(openapi_petstore_bit__e bit) {
    char *bitArray[] =  { "NULL", "0", "1" };
    return bitArray[bit];
}

openapi_petstore_bit__e bit_bit_FromString(char* bit) {
    int stringToReturn = 0;
    char *bitArray[] =  { "NULL", "0", "1" };
    size_t sizeofArray = sizeof(bitArray) / sizeof(bitArray[0]);
    while(stringToReturn < sizeofArray) {
        if(strcmp(bit, bitArray[stringToReturn]) == 0) {
            return stringToReturn;
        }
        stringToReturn++;
    }
    return 0;
}

cJSON *bit_convertToJSON(openapi_petstore_bit__e bit) {
    cJSON *item = cJSON_CreateObject();
    if(cJSON_AddNumberToObject(item, "bit", bit) == NULL) {
        goto fail;
    }
    return item;
fail:
    cJSON_Delete(item);
    return NULL;
}

openapi_petstore_bit__e bit_parseFromJSON(cJSON *bitJSON) {
    if(!cJSON_IsNumber(bitJSON)) {
        return 0;
    }
    return bitJSON->valueint;
}
