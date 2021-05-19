

#include "Order.h"

using namespace Tiny;

Order::Order()
{
	id = long(0);
	petId = long(0);
	quantity = int(0);
	shipDate = std::string();
	status = std::string();
	complete = bool(false);
}

Order::Order(std::string jsonString)
{
	this->fromJson(jsonString);
}

Order::~Order()
{

}

void
Order::fromJson(std::string jsonObj)
{   
    bourne::json object = bourne::json::parse(jsonObj);

    const char *idKey = "id";
    
    if(object.has_key(idKey)) 
    {
        bourne::json value = object[idKey];


        
        jsonToValue(&id, value, "long");


    }

    const char *petIdKey = "petId";
    
    if(object.has_key(petIdKey)) 
    {
        bourne::json value = object[petIdKey];


        
        jsonToValue(&petId, value, "long");


    }

    const char *quantityKey = "quantity";
    
    if(object.has_key(quantityKey)) 
    {
        bourne::json value = object[quantityKey];


        
        jsonToValue(&quantity, value, "int");


    }

    const char *shipDateKey = "shipDate";
    
    if(object.has_key(shipDateKey)) 
    {
        bourne::json value = object[shipDateKey];


        
        jsonToValue(&shipDate, value, "std::string");


    }

    const char *statusKey = "status";
    
    if(object.has_key(statusKey)) 
    {
        bourne::json value = object[statusKey];


        
        jsonToValue(&status, value, "std::string");


    }

    const char *completeKey = "complete";
    
    if(object.has_key(completeKey)) 
    {
        bourne::json value = object[completeKey];


        
        jsonToValue(&complete, value, "bool");


    }


}

bourne::json
Order::toJson()
{
    bourne::json object = bourne::json::object();

    



    object["id"] = getId();


    



    object["petId"] = getPetId();


    



    object["quantity"] = getQuantity();


    



    object["shipDate"] = getShipDate();


    



    object["status"] = getStatus();


    



    object["complete"] = isComplete();



    return object;

}

long
Order::getId()
{
	return id;
}

void
Order::setId(long  id)
{
	this->id = id;
}

long
Order::getPetId()
{
	return petId;
}

void
Order::setPetId(long  petId)
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
Order::isComplete()
{
	return complete;
}

void
Order::setComplete(bool  complete)
{
	this->complete = complete;
}



