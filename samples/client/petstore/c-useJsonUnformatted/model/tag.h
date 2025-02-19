/*
 * tag.h
 *
 * A tag for a pet
 */

#ifndef _tag_H_
#define _tag_H_

#include <string.h>
#include "../external/cJSON.h"
#include "../include/list.h"
#include "../include/keyValuePair.h"
#include "../include/binary.h"

typedef struct tag_t tag_t;




typedef struct tag_t {
    long id; //numeric
    char *name; // string

} tag_t;

tag_t *tag_create(
    long id,
    char *name
);

void tag_free(tag_t *tag);

tag_t *tag_parseFromJSON(cJSON *tagJSON);

cJSON *tag_convertToJSON(tag_t *tag);

#endif /* _tag_H_ */

