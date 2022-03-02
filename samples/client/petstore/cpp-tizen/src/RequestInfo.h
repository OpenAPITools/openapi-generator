#ifndef _REQUESTINFO_H_
#define _REQUESTINFO_H_

#include "NetClient.h"

using namespace std;

namespace Tizen{
namespace ArtikCloud {

class RequestInfo {
public:
	string host;
	string path;
	string method;
	map<string, string> queryParams;
	string mBody;
	struct curl_slist* headerList;
	MemoryStruct_s *p_chunk;
	long *code;
	char *errormsg;
	void *userData;
	void(* handler)();
	bool (*processor)(MemoryStruct_s, long, char*, void*, void(*)());
	GThread *thread;

	RequestInfo(string host, string path, string method, map<string, string> queryParams,
		string mBody, struct curl_slist* headerList, MemoryStruct_s* p_chunk, long* code,
		char* errormsg, void* userData, void(* voidHandler)(),
		bool (*processor)(MemoryStruct_s, long, char*, void*, void(*)()))
	{
		this->host = host;
		this->path = path;
		this->method = method;
		this->queryParams = queryParams;
		this->mBody = mBody;
		this->headerList = headerList;
		this->p_chunk = p_chunk;
		this->code = new long (*code);
		this->errormsg = errormsg;
		this->userData = userData;
		this->handler = reinterpret_cast<void(*)()>(voidHandler);
		this->processor = processor;
	}

	~RequestInfo()
	{
		curl_slist_free_all(headerList);
		if (this->p_chunk) {
			if((this->p_chunk)->memory) {
				free((this->p_chunk)->memory);
			}
			delete (p_chunk);
		}
		delete (this->code);
		if (this->errormsg) {
			free(this->errormsg);
		}

	}

};

}
}
#endif /* REQUESTINFO_H_ */
