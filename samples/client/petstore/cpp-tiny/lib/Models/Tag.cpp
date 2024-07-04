

#include "Tag.h"

using namespace Tiny;

Tag::Tag()
{
	id = long(0);
	name = std::string();
}

Tag::Tag(std::string jsonString)
{
	this->fromJson(jsonString);
}

Tag::~Tag()
{

}

void
Tag::fromJson(std::string jsonObj)
{
    bourne::json object = bourne::json::parse(jsonObj);

    const char *idKey = "id";

    if(object.has_key(idKey))
    {
        bourne::json value = object[idKey];



        jsonToValue(&id, value, "long");


    }

    const char *nameKey = "name";

    if(object.has_key(nameKey))
    {
        bourne::json value = object[nameKey];



        jsonToValue(&name, value, "std::string");


    }


}

bourne::json
Tag::toJson()
{
    bourne::json object = bourne::json::object();





    object["id"] = getId();






    object["name"] = getName();



    return object;

}

long
Tag::getId()
{
	return id;
}

void
Tag::setId(long  id)
{
	this->id = id;
}

std::string
Tag::getName()
{
	return name;
}

void
Tag::setName(std::string  name)
{
	this->name = name;
}



