#ifndef INCLUDE_API_CLIENT_H
#define INCLUDE_API_CLIENT_H

#ifdef API_KEY
#include "list.h"
#endif // API_KEY

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
    #ifdef API_KEY
    //this would only be generated for apiKey authentication
    list_t *apiKeys;
    #endif // API_KEY
} apiClient_t;

apiClient_t* apiClient_create();
void apiClient_free(apiClient_t *apiClient);
void apiClient_invoke(apiClient_t *apiClient, char* operationName, char* operationParameter, char *bodyParameters);

#endif // INCLUDE_API_CLIENT_H