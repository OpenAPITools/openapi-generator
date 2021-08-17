
#include "User.h"

using namespace Tiny;

#include <string>
#include <list>
#include <unity.h>
#include "bourne/json.hpp"



void test_User_id_is_assigned_from_json()
{






    bourne::json input = 
    {
        "id", 1
    };

    User obj(input.dump());

    TEST_ASSERT_EQUAL_INT(1, obj.getId());


}


void test_User_username_is_assigned_from_json()
{


    bourne::json input = 
    {
        "username", "hello"
    };

    User obj(input.dump());

    TEST_ASSERT_EQUAL_STRING("hello", obj.getUsername().c_str());






}


void test_User_firstName_is_assigned_from_json()
{


    bourne::json input = 
    {
        "firstName", "hello"
    };

    User obj(input.dump());

    TEST_ASSERT_EQUAL_STRING("hello", obj.getFirstName().c_str());






}


void test_User_lastName_is_assigned_from_json()
{


    bourne::json input = 
    {
        "lastName", "hello"
    };

    User obj(input.dump());

    TEST_ASSERT_EQUAL_STRING("hello", obj.getLastName().c_str());






}


void test_User_email_is_assigned_from_json()
{


    bourne::json input = 
    {
        "email", "hello"
    };

    User obj(input.dump());

    TEST_ASSERT_EQUAL_STRING("hello", obj.getEmail().c_str());






}


void test_User_password_is_assigned_from_json()
{


    bourne::json input = 
    {
        "password", "hello"
    };

    User obj(input.dump());

    TEST_ASSERT_EQUAL_STRING("hello", obj.getPassword().c_str());






}


void test_User_phone_is_assigned_from_json()
{


    bourne::json input = 
    {
        "phone", "hello"
    };

    User obj(input.dump());

    TEST_ASSERT_EQUAL_STRING("hello", obj.getPhone().c_str());






}


void test_User_userStatus_is_assigned_from_json()
{
    bourne::json input = 
    {
        "userStatus", 1
    };

    User obj(input.dump());

    TEST_ASSERT_EQUAL_INT(1, obj.getUserStatus());








}



void test_User_id_is_converted_to_json()
{



    bourne::json input = 
    {
        "id", 1
    };

    User obj(input.dump());

    bourne::json output = bourne::json::object();

    output = obj.toJson();

    TEST_ASSERT(input["id"] == output["id"]);

}


void test_User_username_is_converted_to_json()
{

    bourne::json input = 
    {
        "username", "hello"
    };

    User obj(input.dump());

    bourne::json output = bourne::json::object();

    output = obj.toJson();

    TEST_ASSERT(input["username"] == output["username"]);



}


void test_User_firstName_is_converted_to_json()
{

    bourne::json input = 
    {
        "firstName", "hello"
    };

    User obj(input.dump());

    bourne::json output = bourne::json::object();

    output = obj.toJson();

    TEST_ASSERT(input["firstName"] == output["firstName"]);



}


void test_User_lastName_is_converted_to_json()
{

    bourne::json input = 
    {
        "lastName", "hello"
    };

    User obj(input.dump());

    bourne::json output = bourne::json::object();

    output = obj.toJson();

    TEST_ASSERT(input["lastName"] == output["lastName"]);



}


void test_User_email_is_converted_to_json()
{

    bourne::json input = 
    {
        "email", "hello"
    };

    User obj(input.dump());

    bourne::json output = bourne::json::object();

    output = obj.toJson();

    TEST_ASSERT(input["email"] == output["email"]);



}


void test_User_password_is_converted_to_json()
{

    bourne::json input = 
    {
        "password", "hello"
    };

    User obj(input.dump());

    bourne::json output = bourne::json::object();

    output = obj.toJson();

    TEST_ASSERT(input["password"] == output["password"]);



}


void test_User_phone_is_converted_to_json()
{

    bourne::json input = 
    {
        "phone", "hello"
    };

    User obj(input.dump());

    bourne::json output = bourne::json::object();

    output = obj.toJson();

    TEST_ASSERT(input["phone"] == output["phone"]);



}


void test_User_userStatus_is_converted_to_json()
{
    bourne::json input = 
    {
        "userStatus", 1
    };

    User obj(input.dump());

    bourne::json output = bourne::json::object();

    output = obj.toJson();

    TEST_ASSERT(input["userStatus"] == output["userStatus"]);




}


