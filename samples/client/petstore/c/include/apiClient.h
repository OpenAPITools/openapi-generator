#ifndef INCLUDE_API_CLIENT_H
#define INCLUDE_API_CLIENT_H

#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>
#include <stdint.h>
#include "../include/list.h"
#include "../include/keyValuePair.h"
#include "../include/binary.h"

typedef struct sslConfig_t {
    char *clientCertFile;         /* client certificate */
    char *clientKeyFile;          /* client private key */
    char *CACertFile;             /* CA certificate */
    int  insecureSkipTlsVerify ;  /* 0 -- verify server certificate */
                                  /* 1 -- skip ssl verify for server certificate */
} sslConfig_t;

typedef struct apiClient_t {
    char *basePath;
    sslConfig_t *sslConfig;
    void *dataReceived;
    long dataReceivedLen;
    long response_code;
    list_t *apiKeys_api_key;
    char *accessToken;
} apiClient_t;

apiClient_t* apiClient_create();

apiClient_t* apiClient_create_with_base_path(const char *basePath
, sslConfig_t *sslConfig
, list_t *apiKeys_api_key
);

void apiClient_free(apiClient_t *apiClient);

void apiClient_invoke(apiClient_t *apiClient,char* operationParameter, list_t *queryParameters, list_t *headerParameters, list_t *formParameters,list_t *headerType,list_t *contentType, char *bodyParameters, char *requestType);

sslConfig_t *sslConfig_create(const char *clientCertFile, const char *clientKeyFile, const char *CACertFile, int insecureSkipTlsVerify);

void sslConfig_free(sslConfig_t *sslConfig);

char *strReplace(char *orig, char *rep, char *with);

#endif // INCLUDE_API_CLIENT_H
