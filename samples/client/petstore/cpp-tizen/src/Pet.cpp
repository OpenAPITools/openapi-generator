#include <map>
#include <cstdlib>
#include <glib-object.h>
#include <json-glib/json-glib.h>
#include "Helpers.h"


#include "Pet.h"

using namespace std;
using namespace Tizen::ArtikCloud;

Pet::Pet()
{
	//__init();
}

Pet::~Pet()
{
	//__cleanup();
}

void
Pet::__init()
{
	//id = long(0);
	//category = new Category();
	//name = std::string();
	//new std::list()std::list> photoUrls;
	//new std::list()std::list> tags;
	//status = std::string();
}

void
Pet::__cleanup()
{
	//if(id != NULL) {
	//
	//delete id;
	//id = NULL;
	//}
	//if(category != NULL) {
	//
	//delete category;
	//category = NULL;
	//}
	//if(name != NULL) {
	//
	//delete name;
	//name = NULL;
	//}
	//if(photoUrls != NULL) {
	//photoUrls.RemoveAll(true);
	//delete photoUrls;
	//photoUrls = NULL;
	//}
	//if(tags != NULL) {
	//tags.RemoveAll(true);
	//delete tags;
	//tags = NULL;
	//}
	//if(status != NULL) {
	//
	//delete status;
	//status = NULL;
	//}
	//
}

void
Pet::fromJson(char* jsonStr)
{
	JsonObject *pJsonObject = json_node_get_object(json_from_string(jsonStr,NULL));
	JsonNode *node;
	const gchar *idKey = "id";
	node = json_object_get_member(pJsonObject, idKey);
	if (node !=NULL) {
	

		if (isprimitive("long long")) {
			jsonToValue(&id, node, "long long", "");
		} else {
			
		}
	}
	const gchar *categoryKey = "category";
	node = json_object_get_member(pJsonObject, categoryKey);
	if (node !=NULL) {
	

		if (isprimitive("Category")) {
			jsonToValue(&category, node, "Category", "Category");
		} else {
			
			Category* obj = static_cast<Category*> (&category);
			obj->fromJson(json_to_string(node, false));
			
		}
	}
	const gchar *nameKey = "name";
	node = json_object_get_member(pJsonObject, nameKey);
	if (node !=NULL) {
	

		if (isprimitive("std::string")) {
			jsonToValue(&name, node, "std::string", "");
		} else {
			
		}
	}
	const gchar *photoUrlsKey = "photoUrls";
	node = json_object_get_member(pJsonObject, photoUrlsKey);
	if (node !=NULL) {
	
		{
			JsonArray* arr = json_node_get_array(node);
			JsonNode*  temp_json;
			list<std::string> new_list;
			std::string inst;
			for (guint i=0;i<json_array_get_length(arr);i++) {
				temp_json = json_array_get_element(arr,i);
				if (isprimitive("std::string")) {
					jsonToValue(&inst, temp_json, "std::string", "");
				} else {
					
				}
				new_list.push_back(inst);
			}
			photoUrls = new_list;
		}
		
	}
	const gchar *tagsKey = "tags";
	node = json_object_get_member(pJsonObject, tagsKey);
	if (node !=NULL) {
	
		{
			JsonArray* arr = json_node_get_array(node);
			JsonNode*  temp_json;
			list<Tag> new_list;
			Tag inst;
			for (guint i=0;i<json_array_get_length(arr);i++) {
				temp_json = json_array_get_element(arr,i);
				if (isprimitive("Tag")) {
					jsonToValue(&inst, temp_json, "Tag", "");
				} else {
					
					inst.fromJson(json_to_string(temp_json, false));
					
				}
				new_list.push_back(inst);
			}
			tags = new_list;
		}
		
	}
	const gchar *statusKey = "status";
	node = json_object_get_member(pJsonObject, statusKey);
	if (node !=NULL) {
	

		if (isprimitive("std::string")) {
			jsonToValue(&status, node, "std::string", "");
		} else {
			
		}
	}
}

Pet::Pet(char* json)
{
	this->fromJson(json);
}

char*
Pet::toJson()
{
	JsonObject *pJsonObject = json_object_new();
	JsonNode *node;
	if (isprimitive("long long")) {
		long long obj = getId();
		node = converttoJson(&obj, "long long", "");
	}
	else {
		
	}
	const gchar *idKey = "id";
	json_object_set_member(pJsonObject, idKey, node);
	if (isprimitive("Category")) {
		Category obj = getCategory();
		node = converttoJson(&obj, "Category", "");
	}
	else {
		
		Category obj = static_cast<Category> (getCategory());
		GError *mygerror;
		mygerror = NULL;
		node = json_from_string(obj.toJson(), &mygerror);
		
	}
	const gchar *categoryKey = "category";
	json_object_set_member(pJsonObject, categoryKey, node);
	if (isprimitive("std::string")) {
		std::string obj = getName();
		node = converttoJson(&obj, "std::string", "");
	}
	else {
		
	}
	const gchar *nameKey = "name";
	json_object_set_member(pJsonObject, nameKey, node);
	if (isprimitive("std::string")) {
		list<std::string> new_list = static_cast<list <std::string> > (getPhotoUrls());
		node = converttoJson(&new_list, "std::string", "array");
	} else {
		node = json_node_alloc();
		list<std::string> new_list = static_cast<list <std::string> > (getPhotoUrls());
		JsonArray* json_array = json_array_new();
		GError *mygerror;
		
	}


	
	const gchar *photoUrlsKey = "photoUrls";
	json_object_set_member(pJsonObject, photoUrlsKey, node);
	if (isprimitive("Tag")) {
		list<Tag> new_list = static_cast<list <Tag> > (getTags());
		node = converttoJson(&new_list, "Tag", "array");
	} else {
		node = json_node_alloc();
		list<Tag> new_list = static_cast<list <Tag> > (getTags());
		JsonArray* json_array = json_array_new();
		GError *mygerror;
		
		for (list<Tag>::iterator it = new_list.begin(); it != new_list.end(); it++) {
			mygerror = NULL;
			Tag obj = *it;
			JsonNode *node_temp = json_from_string(obj.toJson(), &mygerror);
			json_array_add_element(json_array, node_temp);
			g_clear_error(&mygerror);
		}
		json_node_init_array(node, json_array);
		json_array_unref(json_array);
		
	}


	
	const gchar *tagsKey = "tags";
	json_object_set_member(pJsonObject, tagsKey, node);
	if (isprimitive("std::string")) {
		std::string obj = getStatus();
		node = converttoJson(&obj, "std::string", "");
	}
	else {
		
	}
	const gchar *statusKey = "status";
	json_object_set_member(pJsonObject, statusKey, node);
	node = json_node_alloc();
	json_node_init(node, JSON_NODE_OBJECT);
	json_node_take_object(node, pJsonObject);
	char * ret = json_to_string(node, false);
	json_node_free(node);
	return ret;
}

long long
Pet::getId()
{
	return id;
}

void
Pet::setId(long long  id)
{
	this->id = id;
}

Category
Pet::getCategory()
{
	return category;
}

void
Pet::setCategory(Category  category)
{
	this->category = category;
}

std::string
Pet::getName()
{
	return name;
}

void
Pet::setName(std::string  name)
{
	this->name = name;
}

std::list<std::string>
Pet::getPhotoUrls()
{
	return photoUrls;
}

void
Pet::setPhotoUrls(std::list <std::string> photoUrls)
{
	this->photoUrls = photoUrls;
}

std::list<Tag>
Pet::getTags()
{
	return tags;
}

void
Pet::setTags(std::list <Tag> tags)
{
	this->tags = tags;
}

std::string
Pet::getStatus()
{
	return status;
}

void
Pet::setStatus(std::string  status)
{
	this->status = status;
}


