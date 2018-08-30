#include <assert.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include "apiClient.h"
#include "list.h"
#include "keyValuePair.h"

#define EXAMPLE_BASE_PATH "localhost"
#define EXAMPLE_OPERATION_NAME "pets"
#define OPERATION_PARAMETER "5"
#define EXAMPLE_KEY_1 "skin color"
#define EXAMPLE_VALUE_1 "red"
#define EXAMPLE_KEY_2 "legs"
#define EXAMPLE_VALUE_2 "4"

#define OUTPUT_URL_1 "localhost/pets/5?skin+color=red"
#define OUTPUT_URL_2 "localhost/pets/5?skin+color=red&legs=4"

char *assembleTargetUrl(char	*basePath,
                        char	*operationName,
                        char	*operationParameter,
                        list_t	*queryParameters);

int main() {
	char *keyOne = malloc(strlen(EXAMPLE_KEY_1) + 1);
	char *valueOne = malloc(strlen(EXAMPLE_VALUE_1) + 1);

	strcpy(keyOne, EXAMPLE_KEY_1);
	strcpy(valueOne, EXAMPLE_VALUE_1);

	keyValuePair_t *keyValuePairOne = keyValuePair_create(keyOne, valueOne);
	list_t *list = list_create();
	list_addElement(list, keyValuePairOne);

	char *exampleUrlOne = assembleTargetUrl(EXAMPLE_BASE_PATH,
	                                        EXAMPLE_OPERATION_NAME,
	                                        OPERATION_PARAMETER,
	                                        list);

	assert(strcmp(exampleUrlOne, OUTPUT_URL_1) == 0);

	char *keyTwo = malloc(strlen(EXAMPLE_KEY_2) + 1);
	char *valueTwo = malloc(strlen(EXAMPLE_VALUE_2) + 1);

	strcpy(keyTwo, EXAMPLE_KEY_2);
	strcpy(valueTwo, EXAMPLE_VALUE_2);

	keyValuePair_t *keyValuePairTwo = keyValuePair_create(keyTwo, valueTwo);
	list_addElement(list, keyValuePairTwo);

	char *exampleUrlTwo = assembleTargetUrl(EXAMPLE_BASE_PATH,
	                                        EXAMPLE_OPERATION_NAME,
	                                        OPERATION_PARAMETER,
	                                        list);

	assert(strcmp(exampleUrlTwo, OUTPUT_URL_2) == 0);

	free(keyOne);
	free(keyTwo);
	free(valueOne);
	free(valueTwo);
	free(exampleUrlOne);
	free(exampleUrlTwo);
	keyValuePair_free(keyValuePairOne);
	keyValuePair_free(keyValuePairTwo);
	list_free(list);
}