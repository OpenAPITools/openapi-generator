

#include "ApiResponse.h"

using namespace Tiny;

ApiResponse::ApiResponse()
{
	code = int(0);
	type = std::string();
	message = std::string();
}

ApiResponse::ApiResponse(std::string jsonString)
{
	this->fromJson(jsonString);
}

ApiResponse::~ApiResponse()
{

}

void
ApiResponse::fromJson(std::string jsonObj)
{
    bourne::json object = bourne::json::parse(jsonObj);

    const char *codeKey = "code";

    if(object.has_key(codeKey))
    {
        bourne::json value = object[codeKey];



        jsonToValue(&code, value, "int");


    }

    const char *typeKey = "type";

    if(object.has_key(typeKey))
    {
        bourne::json value = object[typeKey];



        jsonToValue(&type, value, "std::string");


    }

    const char *messageKey = "message";

    if(object.has_key(messageKey))
    {
        bourne::json value = object[messageKey];



        jsonToValue(&message, value, "std::string");


    }


}

bourne::json
ApiResponse::toJson()
{
    bourne::json object = bourne::json::object();





    object["code"] = getCode();






    object["type"] = getType();






    object["message"] = getMessage();



    return object;

}

int
ApiResponse::getCode()
{
	return code;
}

void
ApiResponse::setCode(int  code)
{
	this->code = code;
}

std::string
ApiResponse::getType()
{
	return type;
}

void
ApiResponse::setType(std::string  type)
{
	this->type = type;
}

std::string
ApiResponse::getMessage()
{
	return message;
}

void
ApiResponse::setMessage(std::string  message)
{
	this->message = message;
}



