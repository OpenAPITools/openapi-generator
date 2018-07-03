#include <curl/curl.h>
#include <stdlib.h>
#include <string.h>
#include "apiClient.h"
#include "pet.h"

#ifdef API_KEY
#include "apiKey.h"
#endif
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

char *assembleTargetUrl(char	*basePath,
                        char	*operationName,
                        char	*operationParameter) {
	char *targetUrl =
		malloc(strlen(operationName) + strlen(
			       basePath) +
		       ((operationParameter == NULL) ? 1 : (2 + strlen(
								    operationParameter))));
	strcpy(targetUrl, basePath);
	strcat(targetUrl, operationName);
	if(operationParameter != NULL) {
		strcat(targetUrl, "/");
		strcat(targetUrl, operationParameter);
	}

	return targetUrl;
}

char *assembleHeader(char *key, char *value) {
	char *header = malloc(strlen(key) + strlen(value) + 3);

	strcpy(header, key),
	strcat(header, ": ");
	strcat(header, value);

	return header;
}

void postData(CURL *handle, char *dataToPost) {
	curl_easy_setopt(handle, CURLOPT_POSTFIELDS, dataToPost);
	curl_easy_setopt(handle, CURLOPT_POSTFIELDSIZE_LARGE,
	                 strlen(dataToPost));
	curl_easy_setopt(handle, CURLOPT_CUSTOMREQUEST, "POST");
}


void apiClient_invoke(apiClient_t	*apiClient,
                      char		*operationName,
                      char		*operationParameter,
                      char		*dataToPost) {
	CURL *handle = curl_easy_init();
	CURLcode res;

	if(handle) {
		struct curl_slist *headers = NULL;
		headers =
			curl_slist_append(headers, "accept: application/json");
		headers = curl_slist_append(headers,
		                            "Content-Type: application/json");


		// this would only be generated for apiKey authentication
		#ifdef API_KEY
		listEntry_t *listEntry;
		list_ForEach(listEntry, apiClient->apiKeys) {
			apiKey_t *apiKey = listEntry->data;
			if((apiKey->key != NULL) &&
			   (apiKey->value != NULL) )
			{
				char *headerValueToWrite = assembleHeader(
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
			                  operationParameter);

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

		if(dataToPost != NULL) {
			postData(handle, dataToPost);
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
	}
}

size_t writeDataCallback(void *buffer, size_t size, size_t nmemb, void *userp) {
	*(char **) userp = strdup(buffer);

	return size * nmemb;
}