#include "NetClient.h"

using namespace std;

NetClient::NetClient() {
}

/*
void
NetClient::success(void (*success) (HttpResponse*, void (*cb)(void*, Error*)), void (*cb)(void*, Error*)) {
  this->successFunction = success;
  this->cb = cb;
}
 */

NetClient::~NetClient() {
}

size_t WriteMemoryCallback(void *contents, size_t size, size_t nmemb, void *userp) {
	size_t realsize = size * nmemb;
	MemoryStruct_s *mem = (struct MemoryStruct_s *) userp;

	mem->memory = (char *) realloc(mem->memory, mem->size + realsize + 1);
	if (mem->memory == NULL) {
		/* out of memory! */
		//NET_LOGE("not enough memory (realloc returned NULL)\n");
		printf("not enough memory (realloc returned NULL)\n");
		return 0;
	}

	memcpy(&(mem->memory[mem->size]), contents, realsize);
	mem->size += realsize;
	mem->memory[mem->size] = 0;

	return realsize;
}

static void curl_error_print(CURLcode res, const char *errbuf, int line) {
	//NET_LOGD("start curl_error_print line:%d ", line);
	if (res != CURLE_OK) {
		//NET_LOGD("curl err=%d at line:%d ", res, line);
		printf("curl err=%d at line:%d ", res, line);
		size_t len = strlen(errbuf);
		if (len)
			//NET_LOGD("%s\n", errbuf);
		printf("%s\n", errbuf);
	}
}


class GlobalInit {
public:

	GlobalInit()
	{
		CURLcode curl_res = curl_global_init(CURL_GLOBAL_ALL);
		char errbuf[CURL_ERROR_SIZE] = {0,};
		curl_error_print(curl_res, errbuf, __LINE__);
		if (curl_res != CURLE_OK) {
			printf("cannot global initialize curl");
		}
	}

	~GlobalInit()
	{
		curl_global_cleanup();
	}
};

static GlobalInit globalinit;


int NetClient::easycurl(string host, string path, string method, std::map<string, string> queryParams,
	string mBody, struct curl_slist* headerList, MemoryStruct_s *p_chunk, long *code, char * errormsg) {

	char *url = NULL;
	char *body = (char *) malloc(mBody.size() + 1);
	memcpy(body, mBody.c_str(), mBody.size() + 1);
	string uri = string(host);
	uri.append(path);

	NetHttpMethod httpMethod;
	if (method.compare("GET") == 0)
		httpMethod = NET_HTTP_GET;
	else if (method.compare("PUT") == 0)
		httpMethod = NET_HTTP_PUT;
	else if (method.compare("POST") == 0)
		httpMethod = NET_HTTP_POST;
	else
		httpMethod = NET_HTTP_DELETE;

	CURL *curl_handle;
	CURLcode curl_res;
	errormsg = (char*) malloc(CURL_ERROR_SIZE);
	char errbuf[CURL_ERROR_SIZE] = {0,};
	MemoryStruct_s chunk;
	int ret_val = ArtikCloud_ERROR_NONE;
	//	FILE *file;


	printf("start NET_Utils::easycurl");
	//NET_LOGD("start NET_Utils::easycurl");
	/*	if (put_method) {
			file = fopen("tmp/sami_temp", "wb");
			if (file == NULL) {
				NET_LOGE("fopen() failed, file is NULL");
			}
		}
	 */
	//get_proxy_address();

	chunk.memory = NULL;

	/* init the curl session */
	curl_handle = curl_easy_init();
	if (curl_handle == NULL) {
		printf("Unable to initialize cURL interface");
		//NET_LOGE("Unable to initialize cURL interface");
		ret_val = ArtikCloud_ERROR_UNKNOWN;
		goto RETURN;
	}

	curl_res = curl_easy_setopt(curl_handle, CURLOPT_SSL_VERIFYPEER, 0L);
	curl_error_print(curl_res, errbuf, __LINE__);
	if (curl_res != CURLE_OK) {
		ret_val = ArtikCloud_ERROR_UNKNOWN;
		goto CURL_EASY_CLEANUP;
	}

	if (queryParams.size() > 0) {
	string queryParam = "";
	for (std::map<string, string>::iterator queryIter = queryParams.begin(); queryIter != queryParams.end(); ++queryIter) {
		if (queryIter == queryParams.begin())
			queryParam.append("?");
		else
			queryParam.append("&");
		string pKey = static_cast<string> (queryIter->first);
		string pValue = static_cast<string> (queryIter->second);
		char *encoded = curl_easy_escape(curl_handle, pKey.c_str(), 0);
		queryParam.append(encoded);
		curl_free(encoded);
		queryParam.append("=");
		encoded = curl_easy_escape(curl_handle, pValue.c_str(), 0);
		queryParam.append(encoded);
		curl_free(encoded);
	}
	uri.append(queryParam);
	}
	//AppLog("%ls", uri.c_str());
	printf("%s", uri.c_str());


	curl_res = curl_easy_setopt(curl_handle, CURLOPT_ERRORBUFFER, errbuf);
	curl_error_print(curl_res, errbuf, __LINE__);
	if (curl_res != CURLE_OK) {
		ret_val = ArtikCloud_ERROR_UNKNOWN;
		goto CURL_EASY_CLEANUP;
	}

	/* no progcurl_ress meter please */
	//curl_easy_setopt(curl_handle, CURLOPT_FOLLOWLOCATION, 1L);

	/* send all data to this function  */
	curl_res = curl_easy_setopt(curl_handle, CURLOPT_WRITEFUNCTION, WriteMemoryCallback);
	curl_error_print(curl_res, errbuf, __LINE__);
	if (curl_res != CURLE_OK) {
		ret_val = ArtikCloud_ERROR_UNKNOWN;
		goto CURL_EASY_CLEANUP;
	}

	switch (httpMethod) {
	case NET_HTTP_GET:
		curl_res = curl_easy_setopt(curl_handle, CURLOPT_HTTPGET, true);
		curl_error_print(curl_res, errbuf, __LINE__);
		if (curl_res != CURLE_OK) {
			ret_val = ArtikCloud_ERROR_UNKNOWN;
			goto CURL_EASY_CLEANUP;
		}
		break;
	case NET_HTTP_POST:
		curl_res = curl_easy_setopt(curl_handle, CURLOPT_POST, true);
		curl_error_print(curl_res, errbuf, __LINE__);
		if (curl_res != CURLE_OK) {
			ret_val = ArtikCloud_ERROR_UNKNOWN;
			goto CURL_EASY_CLEANUP;
		}

		curl_res = curl_easy_setopt(curl_handle, CURLOPT_POSTFIELDS, body);
		curl_error_print(curl_res, errbuf, __LINE__);
		if (curl_res != CURLE_OK) {
			ret_val = ArtikCloud_ERROR_UNKNOWN;
			goto CURL_EASY_CLEANUP;
		}
		break;
	case NET_HTTP_DELETE:
		curl_res = curl_easy_setopt(curl_handle, CURLOPT_CUSTOMREQUEST, "DELETE");
		curl_error_print(curl_res, errbuf, __LINE__);
		if (curl_res != CURLE_OK) {
			ret_val = ArtikCloud_ERROR_UNKNOWN;
			goto CURL_EASY_CLEANUP;
		}

		curl_res = curl_easy_setopt(curl_handle, CURLOPT_POSTFIELDS, body);
		curl_error_print(curl_res, errbuf, __LINE__);
		if (curl_res != CURLE_OK) {
			ret_val = ArtikCloud_ERROR_UNKNOWN;
			goto CURL_EASY_CLEANUP;
		}
		break;
	case NET_HTTP_PUT:
		//curl_res = curl_easy_setopt(curl_handle, CURLOPT_PUT, 1);
		//curl_res = curl_easy_setopt(curl_handle, CURLOPT_UPLOAD, 1);
		curl_res = curl_easy_setopt(curl_handle, CURLOPT_CUSTOMREQUEST, "PUT");
		curl_error_print(curl_res, errbuf, __LINE__);
		if (curl_res != CURLE_OK) {
			ret_val = ArtikCloud_ERROR_UNKNOWN;
			goto CURL_EASY_CLEANUP;
		}

		curl_res = curl_easy_setopt(curl_handle, CURLOPT_POSTFIELDS, body);
		curl_error_print(curl_res, errbuf, __LINE__);
		if (curl_res != CURLE_OK) {
			ret_val = ArtikCloud_ERROR_UNKNOWN;
			goto CURL_EASY_CLEANUP;
		}
		break;
	default:
		break;
	}

	/* set URL to get */
	url = (char *) malloc(uri.size() + 1);
	memcpy(url, uri.c_str(), uri.size() + 1);
	printf("\n%s\n", url);
	curl_res = curl_easy_setopt(curl_handle, CURLOPT_URL, url);
	curl_error_print(curl_res, errbuf, __LINE__);
	if (curl_res != CURLE_OK) {
		ret_val = ArtikCloud_ERROR_UNKNOWN;
		goto CURL_EASY_CLEANUP;
	}

	//curl_easy_setopt(curl_handle, CURLOPT_HEADER, 1);
	//	curl_res = curl_easy_setopt(curl_handle, CURLOPT_HEADER, true);
	//	curl_error_print(curl_res, errbuf, __LINE__);

	curl_res = curl_easy_setopt(curl_handle, CURLOPT_HTTPHEADER, headerList);
	curl_error_print(curl_res, errbuf, __LINE__);
	if (curl_res != CURLE_OK) {
		ret_val = ArtikCloud_ERROR_UNKNOWN;
		goto CURL_EASY_CLEANUP;
	}


	/*
	if(body != NULL) {
		if (put_method == false) {
			curl_res = curl_easy_setopt(curl_handle, CURLOPT_POSTFIELDS, body);
			curl_error_print(curl_res, errbuf, 184);
		}

		else {
			curl_res = curl_easy_setopt(curl_handle, CURLOPT_PUT, 1);
			curl_error_print(curl_res, errbuf, 188);
			//curl_easy_setopt(curl_handle, CURLOPT_BINARYTRANSFER, 1);

			if (fwrite(body, sizeof(char), strlen(body), file) != strlen(body)) {
				NET_LOGE("fwrite() failed");
			}
			curl_res = curl_easy_setopt(curl_handle, CURLOPT_INFILE, file);
			curl_error_print(curl_res, errbuf, 195);
			curl_res = curl_easy_setopt(curl_handle, CURLOPT_INFILESIZE, strlen(body));
			curl_error_print(curl_res, errbuf, 197);

			if (g_unlink("tmp/sami_temp") == -1) {
				NET_LOGE("file was not deleted");
			}
		}

	}
	 */

	chunk.memory = (char *) malloc(1); /* will be grown as needed by the realloc above */
	chunk.size = 0; /* no data at this point */

	/* we want the headers to this file handle */
	curl_res = curl_easy_setopt(curl_handle, CURLOPT_WRITEDATA, (void *) &chunk);
	curl_error_print(curl_res, errbuf, __LINE__);
	if (curl_res != CURLE_OK) {
		ret_val = ArtikCloud_ERROR_UNKNOWN;
		goto FREE_CHUNK;
	}

	/* some servers don't like requests that are made without a user-agent
	   field, so we provide one */
	curl_res = curl_easy_setopt(curl_handle, CURLOPT_USERAGENT, "libcurl-agent/1.0");
	curl_error_print(curl_res, errbuf, __LINE__);
	if (curl_res != CURLE_OK) {
		ret_val = ArtikCloud_ERROR_UNKNOWN;
		goto FREE_CHUNK;
	}

	/* set timeout */
	curl_res = curl_easy_setopt(curl_handle, CURLOPT_TIMEOUT, 30L);
	curl_error_print(curl_res, errbuf, __LINE__);
	if (curl_res != CURLE_OK) {
		ret_val = ArtikCloud_ERROR_UNKNOWN;
		goto FREE_CHUNK;
	}

	//curl_res = curl_easy_setopt(curl_handle, CURLOPT_PROXY, "http://10.112.1.184:8080/");
	curl_res = curl_easy_perform(curl_handle);
	curl_error_print(curl_res, errbuf, __LINE__);
	if (curl_res == CURLE_COULDNT_RESOLVE_HOST) {
		ret_val = ArtikCloud_ERROR_UNREACHED_TO_SERVER;
		goto FREE_CHUNK;
	} else if (curl_res != CURLE_OK) {
		ret_val = ArtikCloud_ERROR_UNKNOWN;
		goto FREE_CHUNK;
	}
	curl_easy_getinfo(curl_handle, CURLINFO_RESPONSE_CODE, code);

	p_chunk->memory = chunk.memory;
	p_chunk->size = chunk.size;
	/*
		if (put_method) {
			fclose(file);
			if (g_unlink("tmp/sami_temp") == -1) {
				NET_LOGE("file was not deleted");
			}
		}
	 */

FREE_CHUNK:
	if (curl_res != CURLE_OK) {
		if (chunk.memory) {
			free(chunk.memory);
			chunk.memory = NULL;
		}
		size_t len = strlen(errbuf);
		fprintf(stderr, "\nlibcurl: (%d) ", curl_res);
		if (len) {
			sprintf(errormsg, "%s%s", errbuf, ((errbuf[len - 1] != '\n') ? "\n" : ""));
		} else {
			sprintf(errormsg, "%s\n", curl_easy_strerror(curl_res));
		}
	}

CURL_EASY_CLEANUP:
	/* cleanup curl stuff */
	curl_easy_cleanup(curl_handle);


RETURN:
	printf("end NET_Utils::easycurl");
	free(url);
	free(body);
	//NET_LOGD("end NET_Utils::easycurl");
	return ret_val;
}
