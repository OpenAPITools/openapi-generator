#ifndef INCLUDE_API_CLIENT_H
#define INCLUDE_API_CLIENT_H

typedef struct apiClient_t {
    char *basePath;
    void *dataReceived;

} apiClient_t;

apiClient_t* apiClient_create();
void apiClient_free(apiClient_t *apiClient);
void apiClient_invoke(apiClient_t *apiClient, char* operationName, char* parameter);

#endif // INCLUDE_API_CLIENT_H