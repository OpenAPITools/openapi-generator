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

#include <glib-object.h>
#include <json-glib/json-glib.h>

#include "PetManager.h"
#include "NetClient.h"
#include "Helpers.h"
#include "Error.h"
#include "RequestInfo.h"

using namespace std;
using namespace Tizen::ArtikCloud;


PetManager::PetManager()
{

}

PetManager::~PetManager()
{

}

static gboolean __PetManagerresponseHandler(gpointer data)
{
	RequestInfo *request = static_cast<RequestInfo*>(data);
	g_thread_join(request->thread);

	// invoke the callback function
	bool retval = request->processor(*(request->p_chunk), *(request->code), request->errormsg, request->userData, request->handler);

	delete request;
	return FALSE;
}

static gpointer __PetManagerthreadFunc(gpointer data)
{
	RequestInfo *request = static_cast<RequestInfo*>(data);

	// handle the request
	NetClient::easycurl(request->host, request->path, request->method, request->queryParams,
	request->mBody, request->headerList, request->p_chunk, request->code, request->errormsg);

	request->thread = g_thread_self();
	g_idle_add(__PetManagerresponseHandler, static_cast<gpointer>(request));

	return NULL;
}


bool addPetProcessor(MemoryStruct_s p_chunk, long code, char* errormsg, void* userData,
	void(* voidHandler)())
{
	
	void(* handler)(Error, void* ) = reinterpret_cast<void(*)(Error, void* )> (voidHandler);
	JsonNode* pJson;
	char * data = p_chunk.memory;

	

	if (code >= 200 && code < 300) {
		Error error(code, string("No Error"));


		handler(error, userData);
		return true;



	} else {
		Error error;
		if (errormsg != NULL) {
			error = Error(code, string(errormsg));
		} else if (p_chunk.memory != NULL) {
			error = Error(code, string(p_chunk.memory));
		} else {
			error = Error(code, string("Unkown Error"));
		}
		handler(error, userData);
		return false;
	}
}

bool addPetHelper(char * accessToken,
	Pet body, 
	
	void(* handler)(Error, void* ) , void* userData, bool isAsync)
{

	//TODO: maybe delete headerList after its used to free up space?
	struct curl_slist *headerList = NULL;

	
	string accessHeader = "Authorization: Bearer ";
	accessHeader.append(accessToken);
	headerList = curl_slist_append(headerList, accessHeader.c_str());
	headerList = curl_slist_append(headerList, "Content-Type: application/json");
	headerList = curl_slist_append(headerList, "Content-Type: application/xml");

	map <string, string> queryParams;
	string itemAtq;
	
	string mBody = "";
	JsonNode* node;
	JsonArray* json_array;

	if (isprimitive("Pet")) {
		node = converttoJson(&body, "Pet", "");
	}
	
	char *jsonStr =  body.toJson();
	node = json_from_string(jsonStr, NULL);
	g_free(static_cast<gpointer>(jsonStr));
	

	char *jsonStr1 =  json_to_string(node, false);
	mBody.append(jsonStr1);
	g_free(static_cast<gpointer>(jsonStr1));

	string url("/pet");
	int pos;


	//TODO: free memory of errormsg, memorystruct
	MemoryStruct_s* p_chunk = new MemoryStruct_s();
	long code;
	char* errormsg = NULL;
	string myhttpmethod("POST");

	if(strcmp("PUT", "POST") == 0){
		if(strcmp("", mBody.c_str()) == 0){
			mBody.append("{}");
		}
	}

	if(!isAsync){
		NetClient::easycurl(PetManager::getBasePath(), url, myhttpmethod, queryParams,
			mBody, headerList, p_chunk, &code, errormsg);
		bool retval = addPetProcessor(*p_chunk, code, errormsg, userData,reinterpret_cast<void(*)()>(handler));

		curl_slist_free_all(headerList);
		if (p_chunk) {
			if(p_chunk->memory) {
				free(p_chunk->memory);
			}
			delete (p_chunk);
		}
		if (errormsg) {
			free(errormsg);
		}
		return retval;
	} else{
		GThread *thread = NULL;
		RequestInfo *requestInfo = NULL;

		requestInfo = new(nothrow) RequestInfo (PetManager::getBasePath(), url, myhttpmethod, queryParams,
			mBody, headerList, p_chunk, &code, errormsg, userData, reinterpret_cast<void(*)()>(handler), addPetProcessor);;
		if(requestInfo == NULL)
			return false;

		thread = g_thread_new(NULL, __PetManagerthreadFunc, static_cast<gpointer>(requestInfo));
		return true;
	}
}




bool PetManager::addPetAsync(char * accessToken,
	Pet body, 
	
	void(* handler)(Error, void* ) , void* userData)
{
	return addPetHelper(accessToken,
	body, 
	handler, userData, true);
}

bool PetManager::addPetSync(char * accessToken,
	Pet body, 
	
	void(* handler)(Error, void* ) , void* userData)
{
	return addPetHelper(accessToken,
	body, 
	handler, userData, false);
}

bool deletePetProcessor(MemoryStruct_s p_chunk, long code, char* errormsg, void* userData,
	void(* voidHandler)())
{
	
	void(* handler)(Error, void* ) = reinterpret_cast<void(*)(Error, void* )> (voidHandler);
	JsonNode* pJson;
	char * data = p_chunk.memory;

	

	if (code >= 200 && code < 300) {
		Error error(code, string("No Error"));


		handler(error, userData);
		return true;



	} else {
		Error error;
		if (errormsg != NULL) {
			error = Error(code, string(errormsg));
		} else if (p_chunk.memory != NULL) {
			error = Error(code, string(p_chunk.memory));
		} else {
			error = Error(code, string("Unkown Error"));
		}
		handler(error, userData);
		return false;
	}
}

bool deletePetHelper(char * accessToken,
	long petId, std::string apiKey, 
	
	void(* handler)(Error, void* ) , void* userData, bool isAsync)
{

	//TODO: maybe delete headerList after its used to free up space?
	struct curl_slist *headerList = NULL;

	
	//TODO - should use value of paramname instead. Also check for empty not-required headers
	headerList = curl_slist_append(headerList, "api_key: apiKey");

	string accessHeader = "Authorization: Bearer ";
	accessHeader.append(accessToken);
	headerList = curl_slist_append(headerList, accessHeader.c_str());
	headerList = curl_slist_append(headerList, "Content-Type: application/json");

	map <string, string> queryParams;
	string itemAtq;
	
	string mBody = "";
	JsonNode* node;
	JsonArray* json_array;

	string url("/pet/{petId}");
	int pos;

	string s_petId("{");
	s_petId.append("petId");
	s_petId.append("}");
	pos = url.find(s_petId);
	url.erase(pos, s_petId.length());
	url.insert(pos, stringify(&petId, "long"));

	//TODO: free memory of errormsg, memorystruct
	MemoryStruct_s* p_chunk = new MemoryStruct_s();
	long code;
	char* errormsg = NULL;
	string myhttpmethod("DELETE");

	if(strcmp("PUT", "DELETE") == 0){
		if(strcmp("", mBody.c_str()) == 0){
			mBody.append("{}");
		}
	}

	if(!isAsync){
		NetClient::easycurl(PetManager::getBasePath(), url, myhttpmethod, queryParams,
			mBody, headerList, p_chunk, &code, errormsg);
		bool retval = deletePetProcessor(*p_chunk, code, errormsg, userData,reinterpret_cast<void(*)()>(handler));

		curl_slist_free_all(headerList);
		if (p_chunk) {
			if(p_chunk->memory) {
				free(p_chunk->memory);
			}
			delete (p_chunk);
		}
		if (errormsg) {
			free(errormsg);
		}
		return retval;
	} else{
		GThread *thread = NULL;
		RequestInfo *requestInfo = NULL;

		requestInfo = new(nothrow) RequestInfo (PetManager::getBasePath(), url, myhttpmethod, queryParams,
			mBody, headerList, p_chunk, &code, errormsg, userData, reinterpret_cast<void(*)()>(handler), deletePetProcessor);;
		if(requestInfo == NULL)
			return false;

		thread = g_thread_new(NULL, __PetManagerthreadFunc, static_cast<gpointer>(requestInfo));
		return true;
	}
}




bool PetManager::deletePetAsync(char * accessToken,
	long petId, std::string apiKey, 
	
	void(* handler)(Error, void* ) , void* userData)
{
	return deletePetHelper(accessToken,
	petId, apiKey, 
	handler, userData, true);
}

bool PetManager::deletePetSync(char * accessToken,
	long petId, std::string apiKey, 
	
	void(* handler)(Error, void* ) , void* userData)
{
	return deletePetHelper(accessToken,
	petId, apiKey, 
	handler, userData, false);
}

bool findPetsByStatusProcessor(MemoryStruct_s p_chunk, long code, char* errormsg, void* userData,
	void(* voidHandler)())
{
	void(* handler)(std::list<Pet>, Error, void* )
	= reinterpret_cast<void(*)(std::list<Pet>, Error, void* )> (voidHandler);
	
	JsonNode* pJson;
	char * data = p_chunk.memory;
	printf("%s\n", data);

	std::list<Pet> out;
	

	if (code >= 200 && code < 300) {
		Error error(code, string("No Error"));



		pJson = json_from_string(data, NULL);
		JsonArray * jsonarray = json_node_get_array (pJson);
		guint length = json_array_get_length (jsonarray);
		for(guint i = 0; i < length; i++){
			JsonNode* myJson = json_array_get_element (jsonarray, i);
			char * singlenodestr = json_to_string(myJson, false);
			Pet singlemodel;
			singlemodel.fromJson(singlenodestr);
			out.push_front(singlemodel);
			g_free(static_cast<gpointer>(singlenodestr));
			json_node_free(myJson);
		}
		json_array_unref (jsonarray);
		json_node_free(pJson);


	} else {
		Error error;
		if (errormsg != NULL) {
			error = Error(code, string(errormsg));
		} else if (p_chunk.memory != NULL) {
			error = Error(code, string(p_chunk.memory));
		} else {
			error = Error(code, string("Unkown Error"));
		}
		 handler(out, error, userData);
		return false;
			}
}

bool findPetsByStatusHelper(char * accessToken,
	std::list<std::string> status, 
	void(* handler)(std::list<Pet>, Error, void* )
	, void* userData, bool isAsync)
{

	//TODO: maybe delete headerList after its used to free up space?
	struct curl_slist *headerList = NULL;

	
	string accessHeader = "Authorization: Bearer ";
	accessHeader.append(accessToken);
	headerList = curl_slist_append(headerList, accessHeader.c_str());
	headerList = curl_slist_append(headerList, "Content-Type: application/json");

	map <string, string> queryParams;
	string itemAtq;
	
	for (std::list
	<std::string>::iterator queryIter = status.begin(); queryIter != status.end(); ++queryIter) {
		string itemAt = stringify(&(*queryIter), "std::string");
		queryParams.insert(pair<string, string>("status", itemAt));
	}
	
	string mBody = "";
	JsonNode* node;
	JsonArray* json_array;

	string url("/pet/findByStatus");
	int pos;


	//TODO: free memory of errormsg, memorystruct
	MemoryStruct_s* p_chunk = new MemoryStruct_s();
	long code;
	char* errormsg = NULL;
	string myhttpmethod("GET");

	if(strcmp("PUT", "GET") == 0){
		if(strcmp("", mBody.c_str()) == 0){
			mBody.append("{}");
		}
	}

	if(!isAsync){
		NetClient::easycurl(PetManager::getBasePath(), url, myhttpmethod, queryParams,
			mBody, headerList, p_chunk, &code, errormsg);
		bool retval = findPetsByStatusProcessor(*p_chunk, code, errormsg, userData,reinterpret_cast<void(*)()>(handler));

		curl_slist_free_all(headerList);
		if (p_chunk) {
			if(p_chunk->memory) {
				free(p_chunk->memory);
			}
			delete (p_chunk);
		}
		if (errormsg) {
			free(errormsg);
		}
		return retval;
	} else{
		GThread *thread = NULL;
		RequestInfo *requestInfo = NULL;

		requestInfo = new(nothrow) RequestInfo (PetManager::getBasePath(), url, myhttpmethod, queryParams,
			mBody, headerList, p_chunk, &code, errormsg, userData, reinterpret_cast<void(*)()>(handler), findPetsByStatusProcessor);;
		if(requestInfo == NULL)
			return false;

		thread = g_thread_new(NULL, __PetManagerthreadFunc, static_cast<gpointer>(requestInfo));
		return true;
	}
}




bool PetManager::findPetsByStatusAsync(char * accessToken,
	std::list<std::string> status, 
	void(* handler)(std::list<Pet>, Error, void* )
	, void* userData)
{
	return findPetsByStatusHelper(accessToken,
	status, 
	handler, userData, true);
}

bool PetManager::findPetsByStatusSync(char * accessToken,
	std::list<std::string> status, 
	void(* handler)(std::list<Pet>, Error, void* )
	, void* userData)
{
	return findPetsByStatusHelper(accessToken,
	status, 
	handler, userData, false);
}

bool findPetsByTagsProcessor(MemoryStruct_s p_chunk, long code, char* errormsg, void* userData,
	void(* voidHandler)())
{
	void(* handler)(std::list<Pet>, Error, void* )
	= reinterpret_cast<void(*)(std::list<Pet>, Error, void* )> (voidHandler);
	
	JsonNode* pJson;
	char * data = p_chunk.memory;
	printf("%s\n", data);

	std::list<Pet> out;
	

	if (code >= 200 && code < 300) {
		Error error(code, string("No Error"));



		pJson = json_from_string(data, NULL);
		JsonArray * jsonarray = json_node_get_array (pJson);
		guint length = json_array_get_length (jsonarray);
		for(guint i = 0; i < length; i++){
			JsonNode* myJson = json_array_get_element (jsonarray, i);
			char * singlenodestr = json_to_string(myJson, false);
			Pet singlemodel;
			singlemodel.fromJson(singlenodestr);
			out.push_front(singlemodel);
			g_free(static_cast<gpointer>(singlenodestr));
			json_node_free(myJson);
		}
		json_array_unref (jsonarray);
		json_node_free(pJson);


	} else {
		Error error;
		if (errormsg != NULL) {
			error = Error(code, string(errormsg));
		} else if (p_chunk.memory != NULL) {
			error = Error(code, string(p_chunk.memory));
		} else {
			error = Error(code, string("Unkown Error"));
		}
		 handler(out, error, userData);
		return false;
			}
}

bool findPetsByTagsHelper(char * accessToken,
	std::list<std::string> tags, 
	void(* handler)(std::list<Pet>, Error, void* )
	, void* userData, bool isAsync)
{

	//TODO: maybe delete headerList after its used to free up space?
	struct curl_slist *headerList = NULL;

	
	string accessHeader = "Authorization: Bearer ";
	accessHeader.append(accessToken);
	headerList = curl_slist_append(headerList, accessHeader.c_str());
	headerList = curl_slist_append(headerList, "Content-Type: application/json");

	map <string, string> queryParams;
	string itemAtq;
	
	for (std::list
	<std::string>::iterator queryIter = tags.begin(); queryIter != tags.end(); ++queryIter) {
		string itemAt = stringify(&(*queryIter), "std::string");
		queryParams.insert(pair<string, string>("tags", itemAt));
	}
	
	string mBody = "";
	JsonNode* node;
	JsonArray* json_array;

	string url("/pet/findByTags");
	int pos;


	//TODO: free memory of errormsg, memorystruct
	MemoryStruct_s* p_chunk = new MemoryStruct_s();
	long code;
	char* errormsg = NULL;
	string myhttpmethod("GET");

	if(strcmp("PUT", "GET") == 0){
		if(strcmp("", mBody.c_str()) == 0){
			mBody.append("{}");
		}
	}

	if(!isAsync){
		NetClient::easycurl(PetManager::getBasePath(), url, myhttpmethod, queryParams,
			mBody, headerList, p_chunk, &code, errormsg);
		bool retval = findPetsByTagsProcessor(*p_chunk, code, errormsg, userData,reinterpret_cast<void(*)()>(handler));

		curl_slist_free_all(headerList);
		if (p_chunk) {
			if(p_chunk->memory) {
				free(p_chunk->memory);
			}
			delete (p_chunk);
		}
		if (errormsg) {
			free(errormsg);
		}
		return retval;
	} else{
		GThread *thread = NULL;
		RequestInfo *requestInfo = NULL;

		requestInfo = new(nothrow) RequestInfo (PetManager::getBasePath(), url, myhttpmethod, queryParams,
			mBody, headerList, p_chunk, &code, errormsg, userData, reinterpret_cast<void(*)()>(handler), findPetsByTagsProcessor);;
		if(requestInfo == NULL)
			return false;

		thread = g_thread_new(NULL, __PetManagerthreadFunc, static_cast<gpointer>(requestInfo));
		return true;
	}
}




bool PetManager::findPetsByTagsAsync(char * accessToken,
	std::list<std::string> tags, 
	void(* handler)(std::list<Pet>, Error, void* )
	, void* userData)
{
	return findPetsByTagsHelper(accessToken,
	tags, 
	handler, userData, true);
}

bool PetManager::findPetsByTagsSync(char * accessToken,
	std::list<std::string> tags, 
	void(* handler)(std::list<Pet>, Error, void* )
	, void* userData)
{
	return findPetsByTagsHelper(accessToken,
	tags, 
	handler, userData, false);
}

bool getPetByIdProcessor(MemoryStruct_s p_chunk, long code, char* errormsg, void* userData,
	void(* voidHandler)())
{
	void(* handler)(Pet, Error, void* )
	= reinterpret_cast<void(*)(Pet, Error, void* )> (voidHandler);
	
	JsonNode* pJson;
	char * data = p_chunk.memory;
	printf("%s\n", data);

	
	Pet out;

	if (code >= 200 && code < 300) {
		Error error(code, string("No Error"));




		if (isprimitive("Pet")) {
			pJson = json_from_string(data, NULL);
			jsonToValue(&out, pJson, "Pet", "Pet");
			json_node_free(pJson);
		} else {
			
			out.fromJson(data);
			char *jsonStr =  out.toJson();
			printf("\n%s\n", jsonStr);
			g_free(static_cast<gpointer>(jsonStr));
			
		}
		handler(out, error, userData);
		return true;
		//TODO: handle case where json parsing has an error

	} else {
		Error error;
		if (errormsg != NULL) {
			error = Error(code, string(errormsg));
		} else if (p_chunk.memory != NULL) {
			error = Error(code, string(p_chunk.memory));
		} else {
			error = Error(code, string("Unkown Error"));
		}
		 handler(out, error, userData);
		return false;
			}
}

bool getPetByIdHelper(char * accessToken,
	long petId, 
	void(* handler)(Pet, Error, void* )
	, void* userData, bool isAsync)
{

	//TODO: maybe delete headerList after its used to free up space?
	struct curl_slist *headerList = NULL;

	
	string accessHeader = "Authorization: Bearer ";
	accessHeader.append(accessToken);
	headerList = curl_slist_append(headerList, accessHeader.c_str());
	headerList = curl_slist_append(headerList, "Content-Type: application/json");

	map <string, string> queryParams;
	string itemAtq;
	
	string mBody = "";
	JsonNode* node;
	JsonArray* json_array;

	string url("/pet/{petId}");
	int pos;

	string s_petId("{");
	s_petId.append("petId");
	s_petId.append("}");
	pos = url.find(s_petId);
	url.erase(pos, s_petId.length());
	url.insert(pos, stringify(&petId, "long"));

	//TODO: free memory of errormsg, memorystruct
	MemoryStruct_s* p_chunk = new MemoryStruct_s();
	long code;
	char* errormsg = NULL;
	string myhttpmethod("GET");

	if(strcmp("PUT", "GET") == 0){
		if(strcmp("", mBody.c_str()) == 0){
			mBody.append("{}");
		}
	}

	if(!isAsync){
		NetClient::easycurl(PetManager::getBasePath(), url, myhttpmethod, queryParams,
			mBody, headerList, p_chunk, &code, errormsg);
		bool retval = getPetByIdProcessor(*p_chunk, code, errormsg, userData,reinterpret_cast<void(*)()>(handler));

		curl_slist_free_all(headerList);
		if (p_chunk) {
			if(p_chunk->memory) {
				free(p_chunk->memory);
			}
			delete (p_chunk);
		}
		if (errormsg) {
			free(errormsg);
		}
		return retval;
	} else{
		GThread *thread = NULL;
		RequestInfo *requestInfo = NULL;

		requestInfo = new(nothrow) RequestInfo (PetManager::getBasePath(), url, myhttpmethod, queryParams,
			mBody, headerList, p_chunk, &code, errormsg, userData, reinterpret_cast<void(*)()>(handler), getPetByIdProcessor);;
		if(requestInfo == NULL)
			return false;

		thread = g_thread_new(NULL, __PetManagerthreadFunc, static_cast<gpointer>(requestInfo));
		return true;
	}
}




bool PetManager::getPetByIdAsync(char * accessToken,
	long petId, 
	void(* handler)(Pet, Error, void* )
	, void* userData)
{
	return getPetByIdHelper(accessToken,
	petId, 
	handler, userData, true);
}

bool PetManager::getPetByIdSync(char * accessToken,
	long petId, 
	void(* handler)(Pet, Error, void* )
	, void* userData)
{
	return getPetByIdHelper(accessToken,
	petId, 
	handler, userData, false);
}

bool updatePetProcessor(MemoryStruct_s p_chunk, long code, char* errormsg, void* userData,
	void(* voidHandler)())
{
	
	void(* handler)(Error, void* ) = reinterpret_cast<void(*)(Error, void* )> (voidHandler);
	JsonNode* pJson;
	char * data = p_chunk.memory;

	

	if (code >= 200 && code < 300) {
		Error error(code, string("No Error"));


		handler(error, userData);
		return true;



	} else {
		Error error;
		if (errormsg != NULL) {
			error = Error(code, string(errormsg));
		} else if (p_chunk.memory != NULL) {
			error = Error(code, string(p_chunk.memory));
		} else {
			error = Error(code, string("Unkown Error"));
		}
		handler(error, userData);
		return false;
	}
}

bool updatePetHelper(char * accessToken,
	Pet body, 
	
	void(* handler)(Error, void* ) , void* userData, bool isAsync)
{

	//TODO: maybe delete headerList after its used to free up space?
	struct curl_slist *headerList = NULL;

	
	string accessHeader = "Authorization: Bearer ";
	accessHeader.append(accessToken);
	headerList = curl_slist_append(headerList, accessHeader.c_str());
	headerList = curl_slist_append(headerList, "Content-Type: application/json");
	headerList = curl_slist_append(headerList, "Content-Type: application/xml");

	map <string, string> queryParams;
	string itemAtq;
	
	string mBody = "";
	JsonNode* node;
	JsonArray* json_array;

	if (isprimitive("Pet")) {
		node = converttoJson(&body, "Pet", "");
	}
	
	char *jsonStr =  body.toJson();
	node = json_from_string(jsonStr, NULL);
	g_free(static_cast<gpointer>(jsonStr));
	

	char *jsonStr1 =  json_to_string(node, false);
	mBody.append(jsonStr1);
	g_free(static_cast<gpointer>(jsonStr1));

	string url("/pet");
	int pos;


	//TODO: free memory of errormsg, memorystruct
	MemoryStruct_s* p_chunk = new MemoryStruct_s();
	long code;
	char* errormsg = NULL;
	string myhttpmethod("PUT");

	if(strcmp("PUT", "PUT") == 0){
		if(strcmp("", mBody.c_str()) == 0){
			mBody.append("{}");
		}
	}

	if(!isAsync){
		NetClient::easycurl(PetManager::getBasePath(), url, myhttpmethod, queryParams,
			mBody, headerList, p_chunk, &code, errormsg);
		bool retval = updatePetProcessor(*p_chunk, code, errormsg, userData,reinterpret_cast<void(*)()>(handler));

		curl_slist_free_all(headerList);
		if (p_chunk) {
			if(p_chunk->memory) {
				free(p_chunk->memory);
			}
			delete (p_chunk);
		}
		if (errormsg) {
			free(errormsg);
		}
		return retval;
	} else{
		GThread *thread = NULL;
		RequestInfo *requestInfo = NULL;

		requestInfo = new(nothrow) RequestInfo (PetManager::getBasePath(), url, myhttpmethod, queryParams,
			mBody, headerList, p_chunk, &code, errormsg, userData, reinterpret_cast<void(*)()>(handler), updatePetProcessor);;
		if(requestInfo == NULL)
			return false;

		thread = g_thread_new(NULL, __PetManagerthreadFunc, static_cast<gpointer>(requestInfo));
		return true;
	}
}




bool PetManager::updatePetAsync(char * accessToken,
	Pet body, 
	
	void(* handler)(Error, void* ) , void* userData)
{
	return updatePetHelper(accessToken,
	body, 
	handler, userData, true);
}

bool PetManager::updatePetSync(char * accessToken,
	Pet body, 
	
	void(* handler)(Error, void* ) , void* userData)
{
	return updatePetHelper(accessToken,
	body, 
	handler, userData, false);
}

bool updatePetWithFormProcessor(MemoryStruct_s p_chunk, long code, char* errormsg, void* userData,
	void(* voidHandler)())
{
	
	void(* handler)(Error, void* ) = reinterpret_cast<void(*)(Error, void* )> (voidHandler);
	JsonNode* pJson;
	char * data = p_chunk.memory;

	

	if (code >= 200 && code < 300) {
		Error error(code, string("No Error"));


		handler(error, userData);
		return true;



	} else {
		Error error;
		if (errormsg != NULL) {
			error = Error(code, string(errormsg));
		} else if (p_chunk.memory != NULL) {
			error = Error(code, string(p_chunk.memory));
		} else {
			error = Error(code, string("Unkown Error"));
		}
		handler(error, userData);
		return false;
	}
}

bool updatePetWithFormHelper(char * accessToken,
	long petId, std::string name, std::string status, 
	
	void(* handler)(Error, void* ) , void* userData, bool isAsync)
{

	//TODO: maybe delete headerList after its used to free up space?
	struct curl_slist *headerList = NULL;

	
	string accessHeader = "Authorization: Bearer ";
	accessHeader.append(accessToken);
	headerList = curl_slist_append(headerList, accessHeader.c_str());
	headerList = curl_slist_append(headerList, "Content-Type: application/x-www-form-urlencoded");

	map <string, string> queryParams;
	string itemAtq;
	
	string mBody = "";
	JsonNode* node;
	JsonArray* json_array;

	string url("/pet/{petId}");
	int pos;

	string s_petId("{");
	s_petId.append("petId");
	s_petId.append("}");
	pos = url.find(s_petId);
	url.erase(pos, s_petId.length());
	url.insert(pos, stringify(&petId, "long"));

	//TODO: free memory of errormsg, memorystruct
	MemoryStruct_s* p_chunk = new MemoryStruct_s();
	long code;
	char* errormsg = NULL;
	string myhttpmethod("POST");

	if(strcmp("PUT", "POST") == 0){
		if(strcmp("", mBody.c_str()) == 0){
			mBody.append("{}");
		}
	}

	if(!isAsync){
		NetClient::easycurl(PetManager::getBasePath(), url, myhttpmethod, queryParams,
			mBody, headerList, p_chunk, &code, errormsg);
		bool retval = updatePetWithFormProcessor(*p_chunk, code, errormsg, userData,reinterpret_cast<void(*)()>(handler));

		curl_slist_free_all(headerList);
		if (p_chunk) {
			if(p_chunk->memory) {
				free(p_chunk->memory);
			}
			delete (p_chunk);
		}
		if (errormsg) {
			free(errormsg);
		}
		return retval;
	} else{
		GThread *thread = NULL;
		RequestInfo *requestInfo = NULL;

		requestInfo = new(nothrow) RequestInfo (PetManager::getBasePath(), url, myhttpmethod, queryParams,
			mBody, headerList, p_chunk, &code, errormsg, userData, reinterpret_cast<void(*)()>(handler), updatePetWithFormProcessor);;
		if(requestInfo == NULL)
			return false;

		thread = g_thread_new(NULL, __PetManagerthreadFunc, static_cast<gpointer>(requestInfo));
		return true;
	}
}




bool PetManager::updatePetWithFormAsync(char * accessToken,
	long petId, std::string name, std::string status, 
	
	void(* handler)(Error, void* ) , void* userData)
{
	return updatePetWithFormHelper(accessToken,
	petId, name, status, 
	handler, userData, true);
}

bool PetManager::updatePetWithFormSync(char * accessToken,
	long petId, std::string name, std::string status, 
	
	void(* handler)(Error, void* ) , void* userData)
{
	return updatePetWithFormHelper(accessToken,
	petId, name, status, 
	handler, userData, false);
}

bool uploadFileProcessor(MemoryStruct_s p_chunk, long code, char* errormsg, void* userData,
	void(* voidHandler)())
{
	void(* handler)(ApiResponse, Error, void* )
	= reinterpret_cast<void(*)(ApiResponse, Error, void* )> (voidHandler);
	
	JsonNode* pJson;
	char * data = p_chunk.memory;
	printf("%s\n", data);

	
	ApiResponse out;

	if (code >= 200 && code < 300) {
		Error error(code, string("No Error"));




		if (isprimitive("ApiResponse")) {
			pJson = json_from_string(data, NULL);
			jsonToValue(&out, pJson, "ApiResponse", "ApiResponse");
			json_node_free(pJson);
		} else {
			
			out.fromJson(data);
			char *jsonStr =  out.toJson();
			printf("\n%s\n", jsonStr);
			g_free(static_cast<gpointer>(jsonStr));
			
		}
		handler(out, error, userData);
		return true;
		//TODO: handle case where json parsing has an error

	} else {
		Error error;
		if (errormsg != NULL) {
			error = Error(code, string(errormsg));
		} else if (p_chunk.memory != NULL) {
			error = Error(code, string(p_chunk.memory));
		} else {
			error = Error(code, string("Unkown Error"));
		}
		 handler(out, error, userData);
		return false;
			}
}

bool uploadFileHelper(char * accessToken,
	long petId, std::string additionalMetadata, std::string file, 
	void(* handler)(ApiResponse, Error, void* )
	, void* userData, bool isAsync)
{

	//TODO: maybe delete headerList after its used to free up space?
	struct curl_slist *headerList = NULL;

	
	string accessHeader = "Authorization: Bearer ";
	accessHeader.append(accessToken);
	headerList = curl_slist_append(headerList, accessHeader.c_str());
	headerList = curl_slist_append(headerList, "Content-Type: multipart/form-data");

	map <string, string> queryParams;
	string itemAtq;
	
	string mBody = "";
	JsonNode* node;
	JsonArray* json_array;

	string url("/pet/{petId}/uploadImage");
	int pos;

	string s_petId("{");
	s_petId.append("petId");
	s_petId.append("}");
	pos = url.find(s_petId);
	url.erase(pos, s_petId.length());
	url.insert(pos, stringify(&petId, "long"));

	//TODO: free memory of errormsg, memorystruct
	MemoryStruct_s* p_chunk = new MemoryStruct_s();
	long code;
	char* errormsg = NULL;
	string myhttpmethod("POST");

	if(strcmp("PUT", "POST") == 0){
		if(strcmp("", mBody.c_str()) == 0){
			mBody.append("{}");
		}
	}

	if(!isAsync){
		NetClient::easycurl(PetManager::getBasePath(), url, myhttpmethod, queryParams,
			mBody, headerList, p_chunk, &code, errormsg);
		bool retval = uploadFileProcessor(*p_chunk, code, errormsg, userData,reinterpret_cast<void(*)()>(handler));

		curl_slist_free_all(headerList);
		if (p_chunk) {
			if(p_chunk->memory) {
				free(p_chunk->memory);
			}
			delete (p_chunk);
		}
		if (errormsg) {
			free(errormsg);
		}
		return retval;
	} else{
		GThread *thread = NULL;
		RequestInfo *requestInfo = NULL;

		requestInfo = new(nothrow) RequestInfo (PetManager::getBasePath(), url, myhttpmethod, queryParams,
			mBody, headerList, p_chunk, &code, errormsg, userData, reinterpret_cast<void(*)()>(handler), uploadFileProcessor);;
		if(requestInfo == NULL)
			return false;

		thread = g_thread_new(NULL, __PetManagerthreadFunc, static_cast<gpointer>(requestInfo));
		return true;
	}
}




bool PetManager::uploadFileAsync(char * accessToken,
	long petId, std::string additionalMetadata, std::string file, 
	void(* handler)(ApiResponse, Error, void* )
	, void* userData)
{
	return uploadFileHelper(accessToken,
	petId, additionalMetadata, file, 
	handler, userData, true);
}

bool PetManager::uploadFileSync(char * accessToken,
	long petId, std::string additionalMetadata, std::string file, 
	void(* handler)(ApiResponse, Error, void* )
	, void* userData)
{
	return uploadFileHelper(accessToken,
	petId, additionalMetadata, file, 
	handler, userData, false);
}

