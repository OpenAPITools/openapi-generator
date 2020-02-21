#include <stdlib.h>
#include <string.h>
#include "../include/keyValuePair.h"

keyValuePair_t *keyValuePair_create(char *key, void *value) {
    keyValuePair_t *keyValuePair =  malloc(sizeof(keyValuePair_t));
    keyValuePair->key = key;
    keyValuePair->value = value;
    return keyValuePair;
}

void keyValuePair_free(keyValuePair_t *keyValuePair) {
    free(keyValuePair);
}
