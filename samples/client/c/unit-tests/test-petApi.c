#include <stdlib.h>
#include "apiClient.h"
#include "cJSON.h"
#include "pet.h"
#include "petApi.h"

#ifdef DEBUG
#include <stdio.h>
#endif // DEBUG

#define EXAMPLE_OPERATION_PARAMETER 4

int main() {
	pet_free(getPetById(EXAMPLE_OPERATION_PARAMETER));
}