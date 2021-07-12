

#include "User.h"

using namespace Tiny;

User::User()
{
	id = long(0);
	username = std::string();
	firstName = std::string();
	lastName = std::string();
	email = std::string();
	password = std::string();
	phone = std::string();
	userStatus = int(0);
}

User::User(std::string jsonString)
{
	this->fromJson(jsonString);
}

User::~User()
{

}

void
User::fromJson(std::string jsonObj)
{   
    bourne::json object = bourne::json::parse(jsonObj);

    const char *idKey = "id";
    
    if(object.has_key(idKey)) 
    {
        bourne::json value = object[idKey];


        
        jsonToValue(&id, value, "long");


    }

    const char *usernameKey = "username";
    
    if(object.has_key(usernameKey)) 
    {
        bourne::json value = object[usernameKey];


        
        jsonToValue(&username, value, "std::string");


    }

    const char *firstNameKey = "firstName";
    
    if(object.has_key(firstNameKey)) 
    {
        bourne::json value = object[firstNameKey];


        
        jsonToValue(&firstName, value, "std::string");


    }

    const char *lastNameKey = "lastName";
    
    if(object.has_key(lastNameKey)) 
    {
        bourne::json value = object[lastNameKey];


        
        jsonToValue(&lastName, value, "std::string");


    }

    const char *emailKey = "email";
    
    if(object.has_key(emailKey)) 
    {
        bourne::json value = object[emailKey];


        
        jsonToValue(&email, value, "std::string");


    }

    const char *passwordKey = "password";
    
    if(object.has_key(passwordKey)) 
    {
        bourne::json value = object[passwordKey];


        
        jsonToValue(&password, value, "std::string");


    }

    const char *phoneKey = "phone";
    
    if(object.has_key(phoneKey)) 
    {
        bourne::json value = object[phoneKey];


        
        jsonToValue(&phone, value, "std::string");


    }

    const char *userStatusKey = "userStatus";
    
    if(object.has_key(userStatusKey)) 
    {
        bourne::json value = object[userStatusKey];


        
        jsonToValue(&userStatus, value, "int");


    }


}

bourne::json
User::toJson()
{
    bourne::json object = bourne::json::object();

    



    object["id"] = getId();


    



    object["username"] = getUsername();


    



    object["firstName"] = getFirstName();


    



    object["lastName"] = getLastName();


    



    object["email"] = getEmail();


    



    object["password"] = getPassword();


    



    object["phone"] = getPhone();


    



    object["userStatus"] = getUserStatus();



    return object;

}

long
User::getId()
{
	return id;
}

void
User::setId(long  id)
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



