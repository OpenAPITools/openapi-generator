typedef struct apiKey_t {
    char* key;
    char* value;
} apiKey_t;

apiKey_t *apiKey_create(char *key, char *value);
void apiKey_free(apiKey_t *apiKey);