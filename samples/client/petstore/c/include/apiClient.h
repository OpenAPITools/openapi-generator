#ifndef INCLUDE_API_CLIENT_H
#define INCLUDE_API_CLIENT_H

#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>
#include <stdint.h>
#include "../include/list.h"
#include "../include/keyValuePair.h"

typedef struct apiClient_t {
    char *basePath;
    void *dataReceived;
    long response_code;
    list_t *apiKeys;
    char *accessToken;
} apiClient_t;

typedef struct binary_t
{
    uint8_t* data;
    unsigned int len;
} binary_t;

apiClient_t* apiClient_create();

void apiClient_free(apiClient_t *apiClient);

void apiClient_invoke(apiClient_t *apiClient,char* operationParameter, list_t *queryParameters, list_t *headerParameters, list_t *formParameters,list_t *headerType,list_t *contentType, char *bodyParameters, char *requestType);

char *strReplace(char *orig, char *rep, char *with);

char *base64encode(const void *b64_encode_this, int encode_this_many_bytes);

char *base64decode(const void *b64_decode_this, int decode_this_many_bytes);

#endif // INCLUDE_API_CLIENT_H
