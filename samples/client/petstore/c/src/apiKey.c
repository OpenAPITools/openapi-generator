#include <stdlib.h>
#include "keyValuePair.h"

keyValuePair_t *keyValuePair_create(char *key, char *value) {
	keyValuePair_t *keyValuePair = malloc(sizeof(keyValuePair_t));
	keyValuePair->key = key;
	keyValuePair->value = value;

	return keyValuePair;
}

<<<<<<< HEAD
void apiKey_free(apiKey_t *apiKey) {
	free(apiKey);
}
=======
void keyValuePair_free(keyValuePair_t *keyValuePair) {
	free(keyValuePair);
}
>>>>>>> origin/master
