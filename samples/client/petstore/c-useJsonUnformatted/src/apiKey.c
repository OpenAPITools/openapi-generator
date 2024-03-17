#include <stdlib.h>
#include <string.h>
#include "../include/keyValuePair.h"

keyValuePair_t *keyValuePair_create(char *key, void *value) {
    keyValuePair_t *keyValuePair =  malloc(sizeof(keyValuePair_t));
    keyValuePair->key = key;
    keyValuePair->value = value;
    return keyValuePair;
}

keyValuePair_t* keyValuePair_create_allocate(char* key, double value) {
    double* boolpointer = malloc(sizeof(value));
    memcpy(boolpointer, &value, sizeof(value));
    return keyValuePair_create(key, boolpointer);
}

void keyValuePair_free(keyValuePair_t *keyValuePair) {
    free(keyValuePair);
}
