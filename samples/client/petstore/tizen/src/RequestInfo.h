/*
 * Copyright (c) 2016 Samsung Electronics Co., Ltd All Rights Reserved
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

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
