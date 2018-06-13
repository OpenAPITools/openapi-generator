#include <stdlib.h>
#ifdef DEBUG
#include <stdio.h>
#endif // DEBUG
#include "apiClient.h"
#include "cJSON.h"
#include "pet.h"
#include "petApi.h"

#ifdef DEBUG
#include <stdio.h>
#endif // DEBUG

#define EXAMPLE_OPERATION_PARAMETER 4

int main() {
	pet_t *pet = petApi_getPetById(EXAMPLE_OPERATION_PARAMETER);
    #ifdef DEBUG
	char *petJSON = pet_convertToJSON(pet);
	puts(petJSON);
	free(petJSON);
    #endif // DEBUG
	pet_free(pet);
}