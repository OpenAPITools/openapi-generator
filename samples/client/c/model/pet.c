#include <stdlib.h>
#include "pet.h"

pet_t *pet_create() {
	return malloc(sizeof(pet_t));
}

void pet_free(pet_t *pet) {
	return free(pet);
}