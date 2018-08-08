#include <curl/curl.h>
#include <stdlib.h>
#include <string.h>
#include "apiClient.h"
#include "pet.h"
#include "keyValuePair.h"

size_t writeDataCallback(void *buffer, size_t size, size_t nmemb, void *userp);

apiClient_t *apiClient_create() {
	curl_global_init(CURL_GLOBAL_ALL);
	apiClient_t *apiClient = malloc(sizeof(apiClient_t));
	apiClient->basePath = "http://petstore.swagger.io:80/v2/";
	#ifdef BASIC_AUTH
	apiClient->username = NULL;
	apiClient->password = NULL;
	#endif // BASIC_AUTH
	#ifdef OAUTH2
	apiClient->accessToken = NULL;
	#endif // OAUTH2
	return apiClient;
}

void apiClient_free(apiClient_t *apiClient) {
	free(apiClient);
	curl_global_cleanup();
}

void replaceSpaceWithPlus(char *stringToProcess) {
	for(int i = 0; i < strlen(stringToProcess); i++) {
		if(stringToProcess[i] == ' ') {
			stringToProcess[i] = '+';
		}
	}
}

char *assembleTargetUrl(char	*basePath,
                        char	*operationName,
                        char	*operationParameter,
                        list_t	*queryParameters) {
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
	bool slashNeedsToBeAppendedToBasePath = false;

	if(operationParameter != NULL) {
		operationParameterLength = (1 + strlen(operationParameter));
	}
	if(basePath[strlen(basePath) - 1] != '/') {
		slashNeedsToBeAppendedToBasePath = true;
		basePathLength++;
	}

	char *targetUrl =
		malloc(strlen(
			       operationName) + neededBufferSizeForQueryParameters + basePathLength + operationParameterLength + 1
		       );
	strcpy(targetUrl, basePath);
	if(slashNeedsToBeAppendedToBasePath) {
		strcat(targetUrl, "/");
	}
	strcat(targetUrl, operationName);
	if(operationParameter != NULL) {
		strcat(targetUrl, "/");
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


void apiClient_invoke(apiClient_t	*apiClient,
                      char		*operationName,
                      char		*operationParameter,
                      list_t		*queryParameters,
                      list_t		*headerParameters,
                      list_t		*formParameters,
                      char		*bodyParameters,
                      char		*requestType) {
	CURL *handle = curl_easy_init();
	CURLcode res;

	if(handle) {
		listEntry_t *listEntry;
		curl_mime *mime = NULL;
		struct curl_slist *headers = NULL;

		headers =
			curl_slist_append(headers, "accept: application/json");
		headers = curl_slist_append(headers,
		                            "Content-Type: application/json");
		if(requestType != NULL) {
			curl_easy_setopt(handle,
			                 CURLOPT_CUSTOMREQUEST,
			                 requestType);
		}
		if(formParameters != NULL) {
			mime = curl_mime_init(handle);

			list_ForEach(listEntry, formParameters) {
				keyValuePair_t *keyValuePair = listEntry->data;
				if((keyValuePair->key != NULL) &&
				   (keyValuePair->value != NULL) )
				{
					curl_mimepart *part = curl_mime_addpart(
						mime);
					curl_mime_data(part,
					               keyValuePair->key,
					               CURL_ZERO_TERMINATED);
					curl_mime_name(part,
					               keyValuePair->value);
				}
			}

			curl_easy_setopt(handle, CURLOPT_MIMEPOST, mime);
		}

		list_ForEach(listEntry, headerParameters) {
			keyValuePair_t *keyValuePair = listEntry->data;
			if((keyValuePair->key != NULL) &&
			   (keyValuePair->value != NULL) )
			{
				char *headerValueToWrite =
					assembleHeaderField(
						keyValuePair->key,
						keyValuePair->value);
				curl_slist_append(headers, headerValueToWrite);
				free(headerValueToWrite);
			}
		}
		// this would only be generated for apiKey authentication
		#ifdef API_KEY
		list_ForEach(listEntry, apiClient->apiKeys) {
			keyValuePair_t *apiKey = listEntry->data;
			if((apiKey->key != NULL) &&
			   (apiKey->value != NULL) )
			{
				char *headerValueToWrite =
					assembleHeaderField(
						apiKey->key,
						apiKey->value);
				curl_slist_append(headers, headerValueToWrite);
				free(headerValueToWrite);
			}
		}
		#endif // API_KEY

		char *targetUrl =
			assembleTargetUrl(apiClient->basePath,
			                  operationName,
			                  operationParameter,
			                  queryParameters);

		curl_easy_setopt(handle, CURLOPT_URL, targetUrl);
		curl_easy_setopt(handle,
		                 CURLOPT_WRITEFUNCTION,
		                 writeDataCallback);
		curl_easy_setopt(handle,
		                 CURLOPT_WRITEDATA,
		                 &apiClient->dataReceived);
		curl_easy_setopt(handle, CURLOPT_HTTPHEADER, headers);

		// this would only be generated for OAuth2 authentication
		#ifdef OAUTH2
		if(apiClient->accessToken != NULL) {
			// curl_easy_setopt(handle, CURLOPT_HTTPAUTH, CURLAUTH_BEARER);
			curl_easy_setopt(handle,
			                 CURLOPT_XOAUTH2_BEARER,
			                 apiClient->accessToken);
		}
		#endif


		// this would only be generated for basic authentication:
		#ifdef BASIC_AUTH
		char *authenticationToken;

		if((apiClient->username != NULL) &&
		   (apiClient->password != NULL) )
		{
			authenticationToken = malloc(strlen(
							     apiClient->username) +
			                             strlen(
							     apiClient->password) +
			                             2);
			sprintf(authenticationToken,
			        "%s:%s",
			        apiClient->username,
			        apiClient->password);

			curl_easy_setopt(handle,
			                 CURLOPT_HTTPAUTH,
			                 CURLAUTH_BASIC);
			curl_easy_setopt(handle,
			                 CURLOPT_USERPWD,
			                 authenticationToken);
		}

		#endif // BASIC_AUTH

		if(bodyParameters != NULL) {
			postData(handle, bodyParameters);
		}

		res = curl_easy_perform(handle);

		curl_slist_free_all(headers);

		free(targetUrl);

		if(res != CURLE_OK) {
			fprintf(stderr, "curl_easy_perform() failed: %s\n",
			        curl_easy_strerror(res));
		}
		#ifdef BASIC_AUTH
		if((apiClient->username != NULL) &&
		   (apiClient->password != NULL) )
		{
			free(authenticationToken);
		}
		#endif // BASIC_AUTH
		curl_easy_cleanup(handle);
		if(formParameters != NULL) {
			curl_mime_free(mime);
		}
	}
}

size_t writeDataCallback(void *buffer, size_t size, size_t nmemb, void *userp) {
	*(char **) userp = strdup(buffer);

	return size * nmemb;
}