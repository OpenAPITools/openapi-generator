#include <curl/curl.h>
#include <stdlib.h>
#include <string.h>
#include "apiClient.h"
#include "pet.h"

size_t writeDataCallback(void *buffer, size_t size, size_t nmemb, void *userp);

apiClient_t *apiClient_create() {
	curl_global_init(CURL_GLOBAL_ALL);
	apiClient_t *apiClient = malloc(sizeof(apiClient_t));
	apiClient->basePath = "http://petstore.swagger.io:80/v2/";
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

		curl_easy_cleanup(handle);
	}
}

size_t writeDataCallback(void *buffer, size_t size, size_t nmemb, void *userp) {
	*(char **) userp = strdup(buffer);

	return size * nmemb;
}