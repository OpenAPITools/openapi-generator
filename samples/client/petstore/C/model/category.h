/*
 * category.h
 *
 * A category for a pet
 */

#ifndef _category_H_
#define _category_H_

#include <string.h>

typedef struct category_t {
	long    id; //TODO can be modified for numeric in mustache
	char    name;
	
} category_t;

category_t *category_create(
		long    id,
		char    name
		);
		
void category_free(category_t *category);

category_t *category_parseFromJSON(char *jsonString)

cJSON *category_convertToJSON(category_t *category);

#endif /* _category_H_ */
