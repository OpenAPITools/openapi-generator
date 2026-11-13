/*
 * flags.h
 *
 * boolean array, covers the list of booleans deserializer
 */

#ifndef _flags_H_
#define _flags_H_

#include <string.h>
#include "../external/cJSON.h"
#include "../include/list.h"
#include "../include/keyValuePair.h"
#include "../include/binary.h"

typedef struct flags_t flags_t;




typedef struct flags_t {
    list_t *bits; //primitive container

    int _library_owned; // Is the library responsible for freeing this object?
} flags_t;

__attribute__((deprecated)) flags_t *flags_create(
    list_t *bits
);

void flags_free(flags_t *flags);

flags_t *flags_parseFromJSON(cJSON *flagsJSON);

cJSON *flags_convertToJSON(flags_t *flags);

#endif /* _flags_H_ */

