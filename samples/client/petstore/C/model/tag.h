/*
 * tag.h
 *
 * A tag for a pet
 */

#ifndef _tag_H_
#define _tag_H_

#include <string.h>

typedef struct tag_t {
	long    id; //TODO can be modified for numeric in mustache
	char    name;
	
} tag_t;

tag_t *tag_create(
		long    id,
		char    name
		);
		
void tag_free(tag_t *tag);

tag_t *tag_parseFromJSON(char *jsonString)

cJSON *tag_convertToJSON(tag_t *tag);

#endif /* _tag_H_ */
