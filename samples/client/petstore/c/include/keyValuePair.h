typedef struct keyValuePair_t {
    char* key;
    char* value;
} keyValuePair_t;

keyValuePair_t *keyValuePair_create(char *key, char *value);
void keyValuePair_free(keyValuePair_t *keyValuePair);