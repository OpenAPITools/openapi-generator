
#include "Order.h"

using namespace Tiny;

#include <string>
#include <list>
#include <unity.h>
#include "bourne/json.hpp"



void test_Order_id_is_assigned_from_json()
{






    bourne::json input = 
    {
        "id", 1
    };

    Order obj(input.dump());

    TEST_ASSERT_EQUAL_INT(1, obj.getId());


}


void test_Order_petId_is_assigned_from_json()
{






    bourne::json input = 
    {
        "petId", 1
    };

    Order obj(input.dump());

    TEST_ASSERT_EQUAL_INT(1, obj.getPetId());


}


void test_Order_quantity_is_assigned_from_json()
{
    bourne::json input = 
    {
        "quantity", 1
    };

    Order obj(input.dump());

    TEST_ASSERT_EQUAL_INT(1, obj.getQuantity());








}


void test_Order_shipDate_is_assigned_from_json()
{








}


void test_Order_status_is_assigned_from_json()
{


    bourne::json input = 
    {
        "status", "hello"
    };

    Order obj(input.dump());

    TEST_ASSERT_EQUAL_STRING("hello", obj.getStatus().c_str());






}


void test_Order_complete_is_assigned_from_json()
{




    bourne::json input = 
    {
        "complete", true
    };

    Order obj(input.dump());

    TEST_ASSERT(true == obj.isComplete());




}



void test_Order_id_is_converted_to_json()
{



    bourne::json input = 
    {
        "id", 1
    };

    Order obj(input.dump());

    bourne::json output = bourne::json::object();

    output = obj.toJson();

    TEST_ASSERT(input["id"] == output["id"]);

}


void test_Order_petId_is_converted_to_json()
{



    bourne::json input = 
    {
        "petId", 1
    };

    Order obj(input.dump());

    bourne::json output = bourne::json::object();

    output = obj.toJson();

    TEST_ASSERT(input["petId"] == output["petId"]);

}


void test_Order_quantity_is_converted_to_json()
{
    bourne::json input = 
    {
        "quantity", 1
    };

    Order obj(input.dump());

    bourne::json output = bourne::json::object();

    output = obj.toJson();

    TEST_ASSERT(input["quantity"] == output["quantity"]);




}


void test_Order_shipDate_is_converted_to_json()
{




}


void test_Order_status_is_converted_to_json()
{

    bourne::json input = 
    {
        "status", "hello"
    };

    Order obj(input.dump());

    bourne::json output = bourne::json::object();

    output = obj.toJson();

    TEST_ASSERT(input["status"] == output["status"]);



}


void test_Order_complete_is_converted_to_json()
{


    bourne::json input = 
    {
        "complete", true
    };

    Order obj(input.dump());

    bourne::json output = bourne::json::object();

    output = obj.toJson();

    TEST_ASSERT(input["complete"] == output["complete"]);


}


