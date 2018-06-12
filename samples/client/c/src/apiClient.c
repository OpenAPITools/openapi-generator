#include <curl/curl.h>
#include <stdlib.h>
#include <string.h>
#include "apiClient.h"

size_t writeDataCallback(void *buffer, size_t size, size_t nmemb, void *userp);

apiClient_t *apiClient_create() {
	curl_global_init(CURL_GLOBAL_ALL);
	apiClient_t *apiClient = malloc(sizeof(apiClient_t));
	apiClient->basePath = "http://petstore.swagger.io:80/v2/";
	return apiClient;
}

void apiClient_free(apiClient_t *apiClient) {
	free(apiClient->dataReceived);
	free(apiClient);
	curl_global_cleanup();
}

void apiClient_invoke(apiClient_t	*apiClient,
                      char		*operationName,
                      char		*parameter) {
	CURL *handle = curl_easy_init();
	CURLcode res;

	if(handle) {
		char *targetUrl =
			malloc(strlen(operationName) + strlen(
				       apiClient->basePath) + strlen(
				       parameter) + 2);
		strcpy(targetUrl, apiClient->basePath);
		strcat(targetUrl, operationName);
		strcat(targetUrl, "/");
		strcat(targetUrl, parameter);

		curl_easy_setopt(handle, CURLOPT_URL, targetUrl);
		curl_easy_setopt(handle,
		                 CURLOPT_WRITEFUNCTION,
		                 writeDataCallback);
		curl_easy_setopt(handle,
		                 CURLOPT_WRITEDATA,
		                 &apiClient->dataReceived);

		res = curl_easy_perform(handle);

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