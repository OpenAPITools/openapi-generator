#include <curl/curl.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include "apiClient.h"
#include "keyValuePair.h"

size_t writeDataCallback(void *buffer, size_t size, size_t nmemb, void *userp);

apiClient_t *apiClient_create() {
	curl_global_init(CURL_GLOBAL_ALL);
	apiClient_t *apiClient = malloc(sizeof(apiClient_t));
	apiClient->basePath = "http://petstore.swagger.io/v2";
	apiClient->dataReceived = NULL;
	apiClient->response_code = 0;
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
	if(apiClient->dataReceived) {
		free(apiClient->dataReceived);
	}
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

char *assembleTargetUrl(char *basePath, char *operationParameter,
                        list_t *queryParameters) {
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


void apiClient_invoke(apiClient_t *apiClient, char *operationParameter,
                      list_t *queryParameters, list_t *headerParameters,
                      list_t *formParameters, list_t *headerType,
                      list_t *contentType, char *bodyParameters,
                      char *requestType) {
	CURL *handle = curl_easy_init();
	CURLcode res;

	if(handle) {
		listEntry_t *listEntry;
		curl_mime *mime = NULL;
		struct curl_slist *headers = NULL;
		char *buffContent = NULL;
		char *buffHeader = NULL;
		FileStruct *fileVar = NULL;
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
			if(strstr(buffContent,
			          "application/x-www-form-urlencoded") != NULL)
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
			if(strstr(buffContent, "multipart/form-data") != NULL) {
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
							printf(
								"Size of fileVar - %p\n",
								fileVar);
							memcpy(&fileVar,
							       keyValuePair->value,
							       sizeof(fileVar));
							printf(
								"Size of fileVar1 - %p\n",
								fileVar);
							curl_mime_data(part,
							               fileVar->fileData,
							               fileVar->fileSize);
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
		// this would only be generated for apiKey authentication
	#ifdef API_KEY
		list_ForEach(listEntry, apiClient->apiKeys) {
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
	#endif // API_KEY

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
		                 &apiClient->dataReceived);
		curl_easy_setopt(handle, CURLOPT_HTTPHEADER, headers);
		curl_easy_setopt(handle, CURLOPT_VERBOSE, 0); // to get curl debug msg 0: to disable, 1L:to enable
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

		if(contentType != NULL) {
			free(buffContent);
		}

		if(res == CURLE_OK) {
			curl_easy_getinfo(handle, CURLINFO_RESPONSE_CODE,
			                  &apiClient->response_code);
		} else {
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
			free(formString);
			curl_mime_free(mime);
		}
	}
}

size_t writeDataCallback(void *buffer, size_t size, size_t nmemb, void *userp) {
	*(char **) userp = strdup(buffer);

	return size * nmemb;
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
	if(!orig ||
	   !rep)
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
