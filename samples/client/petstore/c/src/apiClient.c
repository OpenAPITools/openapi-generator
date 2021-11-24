#include <curl/curl.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include "../include/apiClient.h"

size_t writeDataCallback(void *buffer, size_t size, size_t nmemb, void *userp);

apiClient_t *apiClient_create() {
    apiClient_t *apiClient = malloc(sizeof(apiClient_t));
    apiClient->basePath = strdup("http://petstore.swagger.io/v2");
    apiClient->sslConfig = NULL;
    apiClient->dataReceived = NULL;
    apiClient->dataReceivedLen = 0;
    apiClient->data_callback_func = NULL;
    apiClient->progress_func = NULL;
    apiClient->progress_data = NULL;
    apiClient->response_code = 0;
    apiClient->apiKeys_api_key = NULL;
    apiClient->accessToken = NULL;

    return apiClient;
}

apiClient_t *apiClient_create_with_base_path(const char *basePath
, sslConfig_t *sslConfig
, list_t *apiKeys_api_key
) {
    apiClient_t *apiClient = malloc(sizeof(apiClient_t));
    if(basePath){
        apiClient->basePath = strdup(basePath);
    }else{
        apiClient->basePath = strdup("http://petstore.swagger.io/v2");
    }

    if(sslConfig){
        apiClient->sslConfig = sslConfig;
    }else{
        apiClient->sslConfig = NULL;
    }

    apiClient->dataReceived = NULL;
    apiClient->dataReceivedLen = 0;
    apiClient->data_callback_func = NULL;
    apiClient->progress_func = NULL;
    apiClient->progress_data = NULL;
    apiClient->response_code = 0;
    if(apiKeys_api_key!= NULL) {
        apiClient->apiKeys_api_key = list_create();
        listEntry_t *listEntry = NULL;
        list_ForEach(listEntry, apiKeys_api_key) {
            keyValuePair_t *pair = listEntry->data;
            keyValuePair_t *pairDup = keyValuePair_create(strdup(pair->key), strdup(pair->value));
            list_addElement(apiClient->apiKeys_api_key, pairDup);
        }
    }else{
        apiClient->apiKeys_api_key = NULL;
    }
    apiClient->accessToken = NULL;

    return apiClient;
}

void apiClient_free(apiClient_t *apiClient) {
    if(apiClient->basePath) {
        free(apiClient->basePath);
    }
    apiClient->data_callback_func = NULL;
    apiClient->progress_func = NULL;
    apiClient->progress_data = NULL;
    if(apiClient->apiKeys_api_key) {
        listEntry_t *listEntry = NULL;
        list_ForEach(listEntry, apiClient->apiKeys_api_key) {
            keyValuePair_t *pair = listEntry->data;
            if(pair->key){
                free(pair->key);
            }
            if(pair->value){
                free(pair->value);
            }
            keyValuePair_free(pair);
        }
        list_free(apiClient->apiKeys_api_key);
    }
    if(apiClient->accessToken) {
        free(apiClient->accessToken);
    }
    free(apiClient);
}

sslConfig_t *sslConfig_create(const char *clientCertFile, const char *clientKeyFile, const char *CACertFile, int insecureSkipTlsVerify) {
    sslConfig_t *sslConfig = calloc(1, sizeof(sslConfig_t));
    if ( clientCertFile ) {
        sslConfig->clientCertFile = strdup(clientCertFile);
    }
    if ( clientKeyFile ) {
        sslConfig->clientKeyFile = strdup(clientKeyFile);
    }
    if ( CACertFile ) {
        sslConfig->CACertFile = strdup(CACertFile);
    }
    sslConfig->insecureSkipTlsVerify = insecureSkipTlsVerify;
    return sslConfig;
}

void sslConfig_free(sslConfig_t *sslConfig) {
    if ( sslConfig->clientCertFile ) {
        free(sslConfig->clientCertFile);
    }
    if ( sslConfig->clientKeyFile ) {
        free(sslConfig->clientKeyFile);
    }
    if ( sslConfig->CACertFile ){
        free(sslConfig->CACertFile);
    }
    free(sslConfig);
}

void replaceSpaceWithPlus(char *stringToProcess) {
    for(int i = 0; i < strlen(stringToProcess); i++) {
        if(stringToProcess[i] == ' ') {
            stringToProcess[i] = '+';
        }
    }
}

char *assembleTargetUrl(char    *basePath,
                        char    *operationParameter,
                        list_t    *queryParameters) {
    int neededBufferSizeForQueryParameters = 0;
    listEntry_t *listEntry;

    if(queryParameters != NULL) {
        list_ForEach(listEntry, queryParameters) {
            keyValuePair_t *pair = listEntry->data;
            neededBufferSizeForQueryParameters +=
                strlen(pair->key) + strlen(pair->value);
        }

        neededBufferSizeForQueryParameters +=
            (queryParameters->count * 2); // each keyValuePair is separated by a = and a & except the last, but this makes up for the ? at the beginning
    }

    int operationParameterLength = 0;
    int basePathLength = strlen(basePath);

    if(operationParameter != NULL) {
        operationParameterLength = (1 + strlen(operationParameter));
    }

    char *targetUrl =
        malloc(
            neededBufferSizeForQueryParameters + basePathLength + operationParameterLength +
            1);

    strcpy(targetUrl, basePath);

    if(operationParameter != NULL) {
        strcat(targetUrl, operationParameter);
    }

    if(queryParameters != NULL) {
        strcat(targetUrl, "?");
        list_ForEach(listEntry, queryParameters) {
            keyValuePair_t *pair = listEntry->data;
            replaceSpaceWithPlus(pair->key);
            strcat(targetUrl, pair->key);
            strcat(targetUrl, "=");
            replaceSpaceWithPlus(pair->value);
            strcat(targetUrl, pair->value);
            if(listEntry->nextListEntry != NULL) {
                strcat(targetUrl, "&");
            }
        }
    }

    return targetUrl;
}

char *assembleHeaderField(char *key, char *value) {
    char *header = malloc(strlen(key) + strlen(value) + 3);

    strcpy(header, key),
    strcat(header, ": ");
    strcat(header, value);

    return header;
}

void postData(CURL *handle, char *bodyParameters) {
    curl_easy_setopt(handle, CURLOPT_POSTFIELDS, bodyParameters);
    curl_easy_setopt(handle, CURLOPT_POSTFIELDSIZE_LARGE,
                     strlen(bodyParameters));
}

int lengthOfKeyPair(keyValuePair_t *keyPair) {
    long length = 0;
    if((keyPair->key != NULL) &&
       (keyPair->value != NULL) )
    {
        length = strlen(keyPair->key) + strlen(keyPair->value);
        return length;
    }
    return 0;
}


void apiClient_invoke(apiClient_t    *apiClient,
                      char        *operationParameter,
                      list_t        *queryParameters,
                      list_t        *headerParameters,
                      list_t        *formParameters,
                      list_t        *headerType,
                      list_t        *contentType,
                      char        *bodyParameters,
                      char        *requestType) {
    CURL *handle = curl_easy_init();
    CURLcode res;

    if(handle) {
        listEntry_t *listEntry;
        curl_mime *mime = NULL;
        struct curl_slist *headers = NULL;
        char *buffContent = NULL;
        char *buffHeader = NULL;
        binary_t *fileVar = NULL;
        char *formString = NULL;

        if(headerType != NULL) {
            list_ForEach(listEntry, headerType) {
                if(strstr((char *) listEntry->data,
                          "xml") == NULL)
                {
                    buffHeader = malloc(strlen(
                                    "Accept: ") +
                                        strlen((char *)
                                               listEntry->
                                               data) + 1);
                    sprintf(buffHeader, "%s%s", "Accept: ",
                            (char *) listEntry->data);
                    headers = curl_slist_append(headers,
                                                buffHeader);
                    free(buffHeader);
                }
            }
        }
        if(contentType != NULL) {
            list_ForEach(listEntry, contentType) {
                if(strstr((char *) listEntry->data,
                          "xml") == NULL)
                {
                    buffContent =
                        malloc(strlen(
                                   "Content-Type: ") + strlen(
                                   (char *)
                                   listEntry->data) +
                               1);
                    sprintf(buffContent, "%s%s",
                            "Content-Type: ",
                            (char *) listEntry->data);
                    headers = curl_slist_append(headers,
                                                buffContent);
                    free(buffContent);
                    buffContent = NULL;
                }
            }
        } else {
            headers = curl_slist_append(headers,
                                        "Content-Type: application/json");
        }

        if(requestType != NULL) {
            curl_easy_setopt(handle, CURLOPT_CUSTOMREQUEST,
                             requestType);
        }

        if(formParameters != NULL) {
            if(contentType &&
               findStrInStrList(contentType, "application/x-www-form-urlencoded") != NULL)
            {
                long parameterLength = 0;
                long keyPairLength = 0;
                list_ForEach(listEntry, formParameters) {
                    keyValuePair_t *keyPair =
                        listEntry->data;

                    keyPairLength =
                        lengthOfKeyPair(keyPair) + 1;

                    if(listEntry->nextListEntry != NULL) {
                        parameterLength++;
                    }
                    parameterLength = parameterLength +
                                      keyPairLength;
                }

                formString = malloc(parameterLength + 1);
                memset(formString, 0, parameterLength + 1);

                list_ForEach(listEntry, formParameters) {
                    keyValuePair_t *keyPair =
                        listEntry->data;
                    if((keyPair->key != NULL) &&
                       (keyPair->value != NULL) )
                    {
                        strcat(formString,
                               keyPair->key);
                        strcat(formString, "=");
                        strcat(formString,
                               keyPair->value);
                        if(listEntry->nextListEntry !=
                           NULL)
                        {
                            strcat(formString, "&");
                        }
                    }
                }
                curl_easy_setopt(handle, CURLOPT_POSTFIELDS,
                                 formString);
            }
            if(contentType &&
               findStrInStrList(contentType, "multipart/form-data") != NULL) {
                mime = curl_mime_init(handle);
                list_ForEach(listEntry, formParameters) {
                    keyValuePair_t *keyValuePair =
                        listEntry->data;

                    if((keyValuePair->key != NULL) &&
                       (keyValuePair->value != NULL) )
                    {
                        curl_mimepart *part =
                            curl_mime_addpart(mime);

                        curl_mime_name(part,
                                       keyValuePair->key);


                        if(strcmp(keyValuePair->key,
                                  "file") == 0)
                        {
                            memcpy(&fileVar,
                                   keyValuePair->value,
                                   sizeof(fileVar));
                            curl_mime_data(part,
                                           fileVar->data,
                                           fileVar->len);
                            curl_mime_filename(part,
                                               "image.png");
                        } else {
                            curl_mime_data(part,
                                           keyValuePair->value,
                                           CURL_ZERO_TERMINATED);
                        }
                    }
                }
                curl_easy_setopt(handle, CURLOPT_MIMEPOST,
                                 mime);
            }
        }

        list_ForEach(listEntry, headerParameters) {
            keyValuePair_t *keyValuePair = listEntry->data;
            if((keyValuePair->key != NULL) &&
               (keyValuePair->value != NULL) )
            {
                char *headerValueToWrite = assembleHeaderField(
                    keyValuePair->key, keyValuePair->value);
                curl_slist_append(headers, headerValueToWrite);
                free(headerValueToWrite);
            }
        }

        if ( strstr(apiClient->basePath, "https") != NULL ) {
            if ( apiClient->sslConfig ) {
                if( apiClient->sslConfig->clientCertFile ) {
                    curl_easy_setopt(handle, CURLOPT_SSLCERT, apiClient->sslConfig->clientCertFile);
                }
                if( apiClient->sslConfig->clientKeyFile ) {
                    curl_easy_setopt(handle, CURLOPT_SSLKEY, apiClient->sslConfig->clientKeyFile);
                }
                if( apiClient->sslConfig->CACertFile ) {
                    curl_easy_setopt(handle, CURLOPT_CAINFO, apiClient->sslConfig->CACertFile);
                }
                if ( 1 == apiClient->sslConfig->insecureSkipTlsVerify ) {
                    curl_easy_setopt(handle, CURLOPT_SSL_VERIFYPEER, 0L);
                    curl_easy_setopt(handle, CURLOPT_SSL_VERIFYHOST, 0L);
                } else {
                    curl_easy_setopt(handle, CURLOPT_SSL_VERIFYPEER, 1L);
                    curl_easy_setopt(handle, CURLOPT_SSL_VERIFYHOST, 2L);
                }
            } else {
                curl_easy_setopt(handle, CURLOPT_SSL_VERIFYPEER, 0L);
                curl_easy_setopt(handle, CURLOPT_SSL_VERIFYHOST, 0L);
            }
        }

        if (apiClient->progress_func != NULL) {
            curl_easy_setopt(handle, CURLOPT_XFERINFOFUNCTION, apiClient->progress_func);
            if (apiClient->progress_data != NULL) {
                curl_easy_setopt(handle, CURLOPT_XFERINFODATA, apiClient->progress_data);
            }
            curl_easy_setopt(handle, CURLOPT_NOPROGRESS, 0L);
        }

        // this would only be generated for apiKey authentication
        if (apiClient->apiKeys_api_key != NULL)
        {
        list_ForEach(listEntry, apiClient->apiKeys_api_key) {
        keyValuePair_t *apiKey = listEntry->data;
        if((apiKey->key != NULL) &&
           (apiKey->value != NULL) )
        {
            char *headerValueToWrite = assembleHeaderField(
                apiKey->key, apiKey->value);
            curl_slist_append(headers, headerValueToWrite);
            free(headerValueToWrite);
        }
        }
        }

        char *targetUrl =
            assembleTargetUrl(apiClient->basePath,
                              operationParameter,
                              queryParameters);

        curl_easy_setopt(handle, CURLOPT_URL, targetUrl);
        curl_easy_setopt(handle,
                         CURLOPT_WRITEFUNCTION,
                         writeDataCallback);
        curl_easy_setopt(handle,
                         CURLOPT_WRITEDATA,
                         apiClient);
        curl_easy_setopt(handle, CURLOPT_HTTPHEADER, headers);
        curl_easy_setopt(handle, CURLOPT_VERBOSE, 0); // to get curl debug msg 0: to disable, 1L:to enable

        // this would only be generated for OAuth2 authentication
        if(apiClient->accessToken != NULL) {
            // curl_easy_setopt(handle, CURLOPT_HTTPAUTH, CURLAUTH_BEARER);
            curl_easy_setopt(handle,
                             CURLOPT_XOAUTH2_BEARER,
                             apiClient->accessToken);
        }

        if(bodyParameters != NULL) {
            postData(handle, bodyParameters);
        }

        res = curl_easy_perform(handle);

        curl_slist_free_all(headers);

        free(targetUrl);

        if(res == CURLE_OK) {
            curl_easy_getinfo(handle, CURLINFO_RESPONSE_CODE, &apiClient->response_code);
        } else {
            char *url,*ip,*scheme;
            long port;
            curl_easy_getinfo(handle, CURLINFO_EFFECTIVE_URL, &url);
            curl_easy_getinfo(handle, CURLINFO_PRIMARY_IP, &ip);
            curl_easy_getinfo(handle, CURLINFO_PRIMARY_PORT, &port);
            curl_easy_getinfo(handle, CURLINFO_SCHEME, &scheme);
            fprintf(stderr, "curl_easy_perform() failed\n\nURL: %s\nIP: %s\nPORT: %li\nSCHEME: %s\nStrERROR: %s\n",url,ip,port,scheme,
            curl_easy_strerror(res));
        }

        curl_easy_cleanup(handle);
        if(formParameters != NULL) {
            free(formString);
            curl_mime_free(mime);
        }
    }
}

size_t writeDataCallback(void *buffer, size_t size, size_t nmemb, void *userp) {
    size_t size_this_time = nmemb * size;
    apiClient_t *apiClient = (apiClient_t *)userp;
    apiClient->dataReceived = (char *)realloc( apiClient->dataReceived, apiClient->dataReceivedLen + size_this_time + 1);
    memcpy((char *)apiClient->dataReceived + apiClient->dataReceivedLen, buffer, size_this_time);
    apiClient->dataReceivedLen += size_this_time;
    ((char*)apiClient->dataReceived)[apiClient->dataReceivedLen] = '\0'; // the space size of (apiClient->dataReceived) = dataReceivedLen + 1
    if (apiClient->data_callback_func) {
        apiClient->data_callback_func(&apiClient->dataReceived, &apiClient->dataReceivedLen);
    }
    return size_this_time;
}

char *strReplace(char *orig, char *rep, char *with) {
    char *result; // the return string
    char *ins; // the next insert point
    char *tmp; // varies
    int lenRep; // length of rep (the string to remove)
    int lenWith; // length of with (the string to replace rep with)
    int lenFront; // distance between rep and end of last rep
    int count; // number of replacements

    // sanity checks and initialization
    if(!orig || !rep)
    {
        return NULL;
    }
    lenRep = strlen(rep);
    if(lenRep == 0) {
        return NULL; // empty rep causes infinite loop during count
    }
    if(!with) {
        with = "";
    }
    lenWith = strlen(with);

    // count the number of replacements needed
    ins = orig;
    for(count = 0; tmp = strstr(ins, rep); ++count) {
        ins = tmp + lenRep;
    }

    tmp = result = malloc(strlen(orig) + (lenWith - lenRep) * count + 1);

    if(!result) {
        return NULL;
    }
    char *originalPointer = orig; // copying original pointer to free the memory
    // first time through the loop, all the variable are set correctly
    // from here on,
    // tmp points to the end of the result string
    // ins points to the next occurrence of rep in orig
    // orig points to the remainder of orig after "end of rep"
    while(count--) {
        ins = strstr(orig, rep);
        lenFront = ins - orig;
        tmp = strncpy(tmp, orig, lenFront) + lenFront;
        tmp = strcpy(tmp, with) + lenWith;
        orig += lenFront + lenRep; // move to next "end of rep"
    }
    strcpy(tmp, orig);
    free(originalPointer);
    return result;
}

void apiClient_setupGlobalEnv() {
    curl_global_init(CURL_GLOBAL_ALL);
}

void apiClient_unsetupGlobalEnv() {
    curl_global_cleanup();
}
