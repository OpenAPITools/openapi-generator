#ifndef INCLUDE_API_CLIENT_H
#define INCLUDE_API_CLIENT_H

typedef struct apiClient_t {
    char *basePath;
    void *dataReceived;
    // this would only be generated for basic authentication
    #ifdef BASIC_AUTH
    char *username;
    char *password;
    #endif //BASIC_AUTH
    // this would only be generated for OAUTH2 authentication
    #ifdef OAUTH2
    char *accessToken;
    #endif // OAUTH2

} apiClient_t;

apiClient_t* apiClient_create();
void apiClient_free(apiClient_t *apiClient);
void apiClient_invoke(apiClient_t *apiClient, char* operationName, char* operationParameter, char *dataToPost);

#endif // INCLUDE_API_CLIENT_H