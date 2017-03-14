#include <map>
#include <cstdlib>
#include <glib-object.h>
#include <json-glib/json-glib.h>
#include "Helpers.h"


#include "User.h"

using namespace std;
using namespace Tizen::ArtikCloud;

User::User()
{
	//__init();
}

User::~User()
{
	//__cleanup();
}

void
User::__init()
{
	//
	//
	//id = long(0);
	//
	//
	//username = std::string();
	//
	//
	//firstName = std::string();
	//
	//
	//lastName = std::string();
	//
	//
	//email = std::string();
	//
	//
	//password = std::string();
	//
	//
	//phone = std::string();
	//
	//
	//userStatus = int(0);
	//
}

void
User::__cleanup()
{
	//if(id != NULL) {
	//
	//delete id;
	//id = NULL;
	//}
	//if(username != NULL) {
	//
	//delete username;
	//username = NULL;
	//}
	//if(firstName != NULL) {
	//
	//delete firstName;
	//firstName = NULL;
	//}
	//if(lastName != NULL) {
	//
	//delete lastName;
	//lastName = NULL;
	//}
	//if(email != NULL) {
	//
	//delete email;
	//email = NULL;
	//}
	//if(password != NULL) {
	//
	//delete password;
	//password = NULL;
	//}
	//if(phone != NULL) {
	//
	//delete phone;
	//phone = NULL;
	//}
	//if(userStatus != NULL) {
	//
	//delete userStatus;
	//userStatus = NULL;
	//}
	//
}

void
User::fromJson(char* jsonStr)
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
	const gchar *usernameKey = "username";
	node = json_object_get_member(pJsonObject, usernameKey);
	if (node !=NULL) {
	

		if (isprimitive("std::string")) {
			jsonToValue(&username, node, "std::string", "");
		} else {
			
		}
	}
	const gchar *firstNameKey = "firstName";
	node = json_object_get_member(pJsonObject, firstNameKey);
	if (node !=NULL) {
	

		if (isprimitive("std::string")) {
			jsonToValue(&firstName, node, "std::string", "");
		} else {
			
		}
	}
	const gchar *lastNameKey = "lastName";
	node = json_object_get_member(pJsonObject, lastNameKey);
	if (node !=NULL) {
	

		if (isprimitive("std::string")) {
			jsonToValue(&lastName, node, "std::string", "");
		} else {
			
		}
	}
	const gchar *emailKey = "email";
	node = json_object_get_member(pJsonObject, emailKey);
	if (node !=NULL) {
	

		if (isprimitive("std::string")) {
			jsonToValue(&email, node, "std::string", "");
		} else {
			
		}
	}
	const gchar *passwordKey = "password";
	node = json_object_get_member(pJsonObject, passwordKey);
	if (node !=NULL) {
	

		if (isprimitive("std::string")) {
			jsonToValue(&password, node, "std::string", "");
		} else {
			
		}
	}
	const gchar *phoneKey = "phone";
	node = json_object_get_member(pJsonObject, phoneKey);
	if (node !=NULL) {
	

		if (isprimitive("std::string")) {
			jsonToValue(&phone, node, "std::string", "");
		} else {
			
		}
	}
	const gchar *userStatusKey = "userStatus";
	node = json_object_get_member(pJsonObject, userStatusKey);
	if (node !=NULL) {
	

		if (isprimitive("int")) {
			jsonToValue(&userStatus, node, "int", "");
		} else {
			
		}
	}
}

User::User(char* json)
{
	this->fromJson(json);
}

char*
User::toJson()
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
	if (isprimitive("std::string")) {
		std::string obj = getUsername();
		node = converttoJson(&obj, "std::string", "");
	}
	else {
		
	}
	const gchar *usernameKey = "username";
	json_object_set_member(pJsonObject, usernameKey, node);
	if (isprimitive("std::string")) {
		std::string obj = getFirstName();
		node = converttoJson(&obj, "std::string", "");
	}
	else {
		
	}
	const gchar *firstNameKey = "firstName";
	json_object_set_member(pJsonObject, firstNameKey, node);
	if (isprimitive("std::string")) {
		std::string obj = getLastName();
		node = converttoJson(&obj, "std::string", "");
	}
	else {
		
	}
	const gchar *lastNameKey = "lastName";
	json_object_set_member(pJsonObject, lastNameKey, node);
	if (isprimitive("std::string")) {
		std::string obj = getEmail();
		node = converttoJson(&obj, "std::string", "");
	}
	else {
		
	}
	const gchar *emailKey = "email";
	json_object_set_member(pJsonObject, emailKey, node);
	if (isprimitive("std::string")) {
		std::string obj = getPassword();
		node = converttoJson(&obj, "std::string", "");
	}
	else {
		
	}
	const gchar *passwordKey = "password";
	json_object_set_member(pJsonObject, passwordKey, node);
	if (isprimitive("std::string")) {
		std::string obj = getPhone();
		node = converttoJson(&obj, "std::string", "");
	}
	else {
		
	}
	const gchar *phoneKey = "phone";
	json_object_set_member(pJsonObject, phoneKey, node);
	if (isprimitive("int")) {
		int obj = getUserStatus();
		node = converttoJson(&obj, "int", "");
	}
	else {
		
	}
	const gchar *userStatusKey = "userStatus";
	json_object_set_member(pJsonObject, userStatusKey, node);
	node = json_node_alloc();
	json_node_init(node, JSON_NODE_OBJECT);
	json_node_take_object(node, pJsonObject);
	char * ret = json_to_string(node, false);
	json_node_free(node);
	return ret;
}

long long
User::getId()
{
	return id;
}

void
User::setId(long long  id)
{
	this->id = id;
}

std::string
User::getUsername()
{
	return username;
}

void
User::setUsername(std::string  username)
{
	this->username = username;
}

std::string
User::getFirstName()
{
	return firstName;
}

void
User::setFirstName(std::string  firstName)
{
	this->firstName = firstName;
}

std::string
User::getLastName()
{
	return lastName;
}

void
User::setLastName(std::string  lastName)
{
	this->lastName = lastName;
}

std::string
User::getEmail()
{
	return email;
}

void
User::setEmail(std::string  email)
{
	this->email = email;
}

std::string
User::getPassword()
{
	return password;
}

void
User::setPassword(std::string  password)
{
	this->password = password;
}

std::string
User::getPhone()
{
	return phone;
}

void
User::setPhone(std::string  phone)
{
	this->phone = phone;
}

int
User::getUserStatus()
{
	return userStatus;
}

void
User::setUserStatus(int  userStatus)
{
	this->userStatus = userStatus;
}


