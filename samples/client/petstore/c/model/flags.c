#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include "flags.h"



static flags_t *flags_create_internal(
    list_t *bits
    ) {
    flags_t *flags_local_var = malloc(sizeof(flags_t));
    if (!flags_local_var) {
        return NULL;
    }
    memset(flags_local_var, 0, sizeof(flags_t));
    flags_local_var->_library_owned = 1;
    flags_local_var->bits = bits;
    return flags_local_var;
}

__attribute__((deprecated)) flags_t *flags_create(
    list_t *bits
    ) {
    flags_t *result = flags_create_internal (
        bits
        );
    if (!result) {
    }
    return result;
}

void flags_free(flags_t *flags) {
    if(NULL == flags){
        return ;
    }
    if(flags->_library_owned != 1){
        fprintf(stderr, "WARNING: %s() does NOT free objects allocated by the user\n", "flags_free");
        return ;
    }
    listEntry_t *listEntry;
    if (flags->bits) {
        list_ForEach(listEntry, flags->bits) {
            free(listEntry->data);
        }
        list_freeList(flags->bits);
        flags->bits = NULL;
    }
    free(flags);
}

cJSON *flags_convertToJSON(flags_t *flags) {
    cJSON *item = cJSON_CreateObject();

    // flags->bits
    if(flags->bits) {
    cJSON *bits = cJSON_AddArrayToObject(item, "bits");
    if(bits == NULL) {
        goto fail; //primitive container
    }

    listEntry_t *bitsListEntry;
    list_ForEach(bitsListEntry, flags->bits) {
    if(cJSON_AddBoolToObject(bits, "", *(cJSON_bool *)bitsListEntry->data) == NULL)
    {
        goto fail;
    }
    }
    }

    return item;
fail:
    if (item) {
        cJSON_Delete(item);
    }
    return NULL;
}

flags_t *flags_parseFromJSON(cJSON *flagsJSON){

    flags_t *flags_local_var = NULL;

    // define the local list for flags->bits
    list_t *bitsList = NULL;

    // flags->bits
    cJSON *bits = cJSON_GetObjectItemCaseSensitive(flagsJSON, "bits");
    if (cJSON_IsNull(bits)) {
        bits = NULL;
    }
    if (bits) { 
    cJSON *bits_local = NULL;
    if(!cJSON_IsArray(bits)) {
        goto end;//primitive container
    }
    bitsList = list_createList();

    cJSON_ArrayForEach(bits_local, bits)
    {
        if(!cJSON_IsBool(bits_local))
        {
            goto end;
        }
        cJSON_bool *bits_local_value = calloc(1, sizeof(cJSON_bool));
        if(!bits_local_value)
        {
            goto end;
        }
        *bits_local_value = bits_local->valueint;
        list_addElement(bitsList , bits_local_value);
    }
    }



    flags_local_var = flags_create_internal (
        bits ? bitsList : NULL
        );

    if (!flags_local_var) {
        goto end;
    }

    return flags_local_var;
end:
    if (bitsList) {
        list_freeList(bitsList);
        bitsList = NULL;
    }
    return NULL;

}
