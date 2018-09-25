/*
 * pet.h
 *
 * A pet for sale in the pet store
 */

#ifndef _pet_H_
#define _pet_H_

#include <string.h>
#include "cJSON.h"
#include "category.h"
#include "list.h"
#include "tag.h"


typedef struct pet_t {
	long id; // numeric
	category_t *category; // nonprimitive
	char *name; // no enum string
	list_t *photoUrls; // primitive container
	list_t *tags; // nonprimitive container
	char *status; // enum string
} pet_t;

pet_t *pet_create(long		id,
                  category_t	*category,
                  char		*name,
                  list_t	*photoUrls,
                  list_t	*tags,
                  char		*status);

void pet_free(pet_t *pet);

pet_t *pet_parseFromJSON(char *jsonString);

cJSON *pet_convertToJSON(pet_t *pet);



#endif /* _pet_H_ */
