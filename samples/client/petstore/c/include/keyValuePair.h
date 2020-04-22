#ifndef _keyValuePair_H_
#define _keyValuePair_H_

#include<string.h>

typedef struct keyValuePair_t {
    char* key;
    void* value;
} keyValuePair_t;

keyValuePair_t *keyValuePair_create(char *key, void *value);

void keyValuePair_free(keyValuePair_t *keyValuePair);

#endif /* _keyValuePair_H_ */