#ifndef _NetClient_H_
#define _NetClient_H_

#include <map>
#include <string>
#include <cstring>
#include <stdbool.h>
#include <stdlib.h>
#include <glib.h>
#include <glib/gstdio.h>
#include <glib-object.h>
#include <json-glib/json-glib.h>
#include <curl/curl.h>

using namespace std;

typedef enum {
	ArtikCloud_ERROR_NONE = 0, /**< Successful */
	ArtikCloud_ERROR_UNKNOWN, /**< Unknown error */
	ArtikCloud_ERROR_NO_RESPONSE_BODY, /**< No response body error */
	ArtikCloud_ERROR_JSON_PARSING_FAIL, /**< Parsing fail of json data */
	ArtikCloud_ERROR_UNREACHED_TO_SERVER, /**< Unreached to artik cloud server */
	ArtikCloud_ERROR_INVALID_PARAMETER, /**< Invalid parameter */
	ArtikCloud_ERROR_PERMISSION_DENIED /**< Permission denied */
} ArtikCloud_error_e;

typedef enum {
	NET_HTTP_GET = 0,
	NET_HTTP_POST,
	NET_HTTP_PUT,
	NET_HTTP_DELETE,
	NET_HTTP_HEAD,
	NET_HTTP_TRACE,
	NET_HTTP_OPTIONS,
	NET_HTTP_CONNECT,
	NET_HTTP_PATCH
} NetHttpMethod;

struct MemoryStruct_s {
	char *memory;
	size_t size;
};

class NetClient {
public:
	NetClient();
	virtual ~NetClient();

	static int easycurl(string host, string path, string method, map<string, string> queryParams,
		string mBody, struct curl_slist* headerList, MemoryStruct_s* p_chunk, long* code, char* errormsg);

};
#endif /* NetClient_H_ */
