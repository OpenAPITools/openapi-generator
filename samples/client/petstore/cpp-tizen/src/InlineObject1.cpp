#include <map>
#include <cstdlib>
#include <glib-object.h>
#include <json-glib/json-glib.h>
#include "Helpers.h"


#include "Inline_object_1.h"

using namespace std;
using namespace Tizen::ArtikCloud;

Inline_object_1::Inline_object_1()
{
	//__init();
}

Inline_object_1::~Inline_object_1()
{
	//__cleanup();
}

void
Inline_object_1::__init()
{
	//additionalMetadata = std::string();
	//file = std::string();
}

void
Inline_object_1::__cleanup()
{
	//if(additionalMetadata != NULL) {
	//
	//delete additionalMetadata;
	//additionalMetadata = NULL;
	//}
	//if(file != NULL) {
	//
	//delete file;
	//file = NULL;
	//}
	//
}

void
Inline_object_1::fromJson(char* jsonStr)
{
	JsonObject *pJsonObject = json_node_get_object(json_from_string(jsonStr,NULL));
	JsonNode *node;
	const gchar *additionalMetadataKey = "additionalMetadata";
	node = json_object_get_member(pJsonObject, additionalMetadataKey);
	if (node !=NULL) {
	

		if (isprimitive("std::string")) {
			jsonToValue(&additionalMetadata, node, "std::string", "");
		} else {
			
		}
	}
	const gchar *fileKey = "file";
	node = json_object_get_member(pJsonObject, fileKey);
	if (node !=NULL) {
	

		if (isprimitive("std::string")) {
			jsonToValue(&file, node, "std::string", "");
		} else {
			
			std::string* obj = static_cast<std::string*> (&file);
			obj->fromJson(json_to_string(node, false));
			
		}
	}
}

Inline_object_1::Inline_object_1(char* json)
{
	this->fromJson(json);
}

char*
Inline_object_1::toJson()
{
	JsonObject *pJsonObject = json_object_new();
	JsonNode *node;
	if (isprimitive("std::string")) {
		std::string obj = getAdditionalMetadata();
		node = converttoJson(&obj, "std::string", "");
	}
	else {
		
	}
	const gchar *additionalMetadataKey = "additionalMetadata";
	json_object_set_member(pJsonObject, additionalMetadataKey, node);
	if (isprimitive("std::string")) {
		std::string obj = getFile();
		node = converttoJson(&obj, "std::string", "");
	}
	else {
		
		std::string obj = static_cast<std::string> (getFile());
		GError *mygerror;
		mygerror = NULL;
		node = json_from_string(obj.toJson(), &mygerror);
		
	}
	const gchar *fileKey = "file";
	json_object_set_member(pJsonObject, fileKey, node);
	node = json_node_alloc();
	json_node_init(node, JSON_NODE_OBJECT);
	json_node_take_object(node, pJsonObject);
	char * ret = json_to_string(node, false);
	json_node_free(node);
	return ret;
}

std::string
Inline_object_1::getAdditionalMetadata()
{
	return additionalMetadata;
}

void
Inline_object_1::setAdditionalMetadata(std::string  additionalMetadata)
{
	this->additionalMetadata = additionalMetadata;
}

std::string
Inline_object_1::getFile()
{
	return file;
}

void
Inline_object_1::setFile(std::string  file)
{
	this->file = file;
}


