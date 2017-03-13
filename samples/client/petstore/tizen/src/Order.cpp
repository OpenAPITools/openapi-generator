#include <map>
#include <cstdlib>
#include <glib-object.h>
#include <json-glib/json-glib.h>
#include "Helpers.h"


#include "Order.h"

using namespace std;
using namespace Tizen::ArtikCloud;

Order::Order()
{
	//__init();
}

Order::~Order()
{
	//__cleanup();
}

void
Order::__init()
{
	//
	//
	//id = long(0);
	//
	//
	//petId = long(0);
	//
	//
	//quantity = int(0);
	//
	//
	//shipDate = null;
	//
	//
	//status = std::string();
	//
	//
	//complete = bool(false);
	//
}

void
Order::__cleanup()
{
	//if(id != NULL) {
	//
	//delete id;
	//id = NULL;
	//}
	//if(petId != NULL) {
	//
	//delete petId;
	//petId = NULL;
	//}
	//if(quantity != NULL) {
	//
	//delete quantity;
	//quantity = NULL;
	//}
	//if(shipDate != NULL) {
	//
	//delete shipDate;
	//shipDate = NULL;
	//}
	//if(status != NULL) {
	//
	//delete status;
	//status = NULL;
	//}
	//if(complete != NULL) {
	//
	//delete complete;
	//complete = NULL;
	//}
	//
}

void
Order::fromJson(char* jsonStr)
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
	const gchar *petIdKey = "petId";
	node = json_object_get_member(pJsonObject, petIdKey);
	if (node !=NULL) {
	

		if (isprimitive("long long")) {
			jsonToValue(&petId, node, "long long", "");
		} else {
			
		}
	}
	const gchar *quantityKey = "quantity";
	node = json_object_get_member(pJsonObject, quantityKey);
	if (node !=NULL) {
	

		if (isprimitive("int")) {
			jsonToValue(&quantity, node, "int", "");
		} else {
			
		}
	}
	const gchar *shipDateKey = "shipDate";
	node = json_object_get_member(pJsonObject, shipDateKey);
	if (node !=NULL) {
	

		if (isprimitive("std::string")) {
			jsonToValue(&shipDate, node, "std::string", "");
		} else {
			
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
	const gchar *completeKey = "complete";
	node = json_object_get_member(pJsonObject, completeKey);
	if (node !=NULL) {
	

		if (isprimitive("bool")) {
			jsonToValue(&complete, node, "bool", "");
		} else {
			
		}
	}
}

Order::Order(char* json)
{
	this->fromJson(json);
}

char*
Order::toJson()
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
	if (isprimitive("long long")) {
		long long obj = getPetId();
		node = converttoJson(&obj, "long long", "");
	}
	else {
		
	}
	const gchar *petIdKey = "petId";
	json_object_set_member(pJsonObject, petIdKey, node);
	if (isprimitive("int")) {
		int obj = getQuantity();
		node = converttoJson(&obj, "int", "");
	}
	else {
		
	}
	const gchar *quantityKey = "quantity";
	json_object_set_member(pJsonObject, quantityKey, node);
	if (isprimitive("std::string")) {
		std::string obj = getShipDate();
		node = converttoJson(&obj, "std::string", "");
	}
	else {
		
	}
	const gchar *shipDateKey = "shipDate";
	json_object_set_member(pJsonObject, shipDateKey, node);
	if (isprimitive("std::string")) {
		std::string obj = getStatus();
		node = converttoJson(&obj, "std::string", "");
	}
	else {
		
	}
	const gchar *statusKey = "status";
	json_object_set_member(pJsonObject, statusKey, node);
	if (isprimitive("bool")) {
		bool obj = getComplete();
		node = converttoJson(&obj, "bool", "");
	}
	else {
		
	}
	const gchar *completeKey = "complete";
	json_object_set_member(pJsonObject, completeKey, node);
	node = json_node_alloc();
	json_node_init(node, JSON_NODE_OBJECT);
	json_node_take_object(node, pJsonObject);
	char * ret = json_to_string(node, false);
	json_node_free(node);
	return ret;
}

long long
Order::getId()
{
	return id;
}

void
Order::setId(long long  id)
{
	this->id = id;
}

long long
Order::getPetId()
{
	return petId;
}

void
Order::setPetId(long long  petId)
{
	this->petId = petId;
}

int
Order::getQuantity()
{
	return quantity;
}

void
Order::setQuantity(int  quantity)
{
	this->quantity = quantity;
}

std::string
Order::getShipDate()
{
	return shipDate;
}

void
Order::setShipDate(std::string  shipDate)
{
	this->shipDate = shipDate;
}

std::string
Order::getStatus()
{
	return status;
}

void
Order::setStatus(std::string  status)
{
	this->status = status;
}

bool
Order::getComplete()
{
	return complete;
}

void
Order::setComplete(bool  complete)
{
	this->complete = complete;
}


