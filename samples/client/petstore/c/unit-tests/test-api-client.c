#include <stdlib.h>
#include "apiClient.h"
#include "cJSON.h"
#include "pet.h"

#ifdef DEBUG
#include <stdio.h>
#endif // DEBUG

#define EXAMPLE_OPERATION_NAME "pet"
#define EXAMPLE_OPERATION_PARAMETER "4"

int main() {
	apiClient_t *apiClient = apiClient_create();
	apiClient_invoke(apiClient,
	                 EXAMPLE_OPERATION_NAME,
	                 EXAMPLE_OPERATION_PARAMETER,
	                 NULL);
	pet_t *pet = pet_parseFromJSON(apiClient->dataReceived);
	if(pet == NULL) {
		return 0;
	} else {
		char *jsonString = pet_convertToJSON(pet);
		#ifdef DEBUG
		puts(jsonString);
		#endif
		free(jsonString);
	}
	apiClient_free(apiClient);
	pet_free(pet);
}