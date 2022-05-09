#define BOOST_TEST_INCLUDED
#include <boost/test/unit_test.hpp>
#include <boost/test/data/test_case.hpp>
#include <boost/property_tree/ptree.hpp>
#include <boost/property_tree/json_parser.hpp>
#include <sstream>

#include "model/Order.h"

using namespace org::openapitools::server::model;


BOOST_AUTO_TEST_SUITE(OrderModelTest)

BOOST_AUTO_TEST_CASE(toJsonString)
{
  Order order;
  order.setId(2L);
  order.setPetId(3L);
  order.setQuantity(6);
  order.setShipDate("123");
  order.setStatus("placed");
  order.setComplete(false);

  const auto json = order.toJsonString(true);

  Order newOrder;
  newOrder.fromJsonString(json);

  BOOST_TEST(newOrder.getId() == 2L);
  BOOST_TEST(newOrder.getPetId() == 3L);
  BOOST_TEST(newOrder.getQuantity() == 6);
  BOOST_TEST(newOrder.getShipDate() == "123");
  BOOST_TEST(newOrder.getStatus() == "placed");
  BOOST_TEST(newOrder.isComplete() == false);
}

BOOST_AUTO_TEST_CASE(fromJsonString)
{
  const std::string json = R"JSON(
{
    "id": "22",
    "petId": "32",
    "quantity": "1",
    "shipDate": "999999",
    "status": "placed",
    "complete": "true"
}
)JSON";

  Order order;
  order.fromJsonString(json);

  BOOST_TEST(order.getId() == 22L);
  BOOST_TEST(order.getPetId() == 32L);
  BOOST_TEST(order.getQuantity() == 1);
  BOOST_TEST(order.getShipDate() == "999999");
  BOOST_TEST(order.getStatus() == "placed");
  BOOST_TEST(order.isComplete() == true);
}

BOOST_AUTO_TEST_CASE(fromJsonArrayString)
{
  const std::string json = R"JSON(
[{
    "id": "1",
    "petId": "1",
    "quantity": "1",
    "shipDate": "11111",
    "status": "placed",
    "complete": "true"
},{
    "id": "2",
    "petId": "2",
    "quantity": "2",
    "shipDate": "222222",
    "status": "approved",
    "complete": "false"
},{
    "id": "3",
    "petId": "3",
    "quantity": "3",
    "shipDate": "33333",
    "status": "delivered"
}]
)JSON";

  const auto orderVec = createOrderVectorFromJsonString(json);


  BOOST_TEST(orderVec.size() == 3);
  BOOST_TEST(orderVec[0].getId() == 1);
  BOOST_TEST(orderVec[0].getPetId() == 1);
  BOOST_TEST(orderVec[0].getQuantity() == 1);
  BOOST_TEST(orderVec[0].getShipDate() == "11111");
  BOOST_TEST(orderVec[0].getStatus() == "placed");
  BOOST_TEST(orderVec[0].isComplete() == true);
  BOOST_TEST(orderVec[1].getId() == 2);
  BOOST_TEST(orderVec[1].getPetId() == 2);
  BOOST_TEST(orderVec[1].getQuantity() == 2);
  BOOST_TEST(orderVec[1].getShipDate() == "222222");
  BOOST_TEST(orderVec[1].getStatus() == "approved");
  BOOST_TEST(orderVec[1].isComplete() == false);
  BOOST_TEST(orderVec[2].getId() == 3);
  BOOST_TEST(orderVec[2].getPetId() == 3);
  BOOST_TEST(orderVec[2].getQuantity() == 3);
  BOOST_TEST(orderVec[2].getShipDate() == "33333");
  BOOST_TEST(orderVec[2].getStatus() == "delivered");
  BOOST_TEST(orderVec[2].isComplete() == false);
}

BOOST_AUTO_TEST_CASE(toAndFromPropertyTree)
{
  Order order;
  order.setId(1L);
  order.setPetId(2L);
  order.setQuantity(4);
  order.setShipDate("111123");
  order.setStatus("approved");
  order.setComplete(false);

  const auto pt = order.toPropertyTree();

  const auto newOrder = Order(pt);

  BOOST_TEST(newOrder.getId() == 1L);
  BOOST_TEST(newOrder.getPetId() == 2L);
  BOOST_TEST(newOrder.getQuantity() == 4);
  BOOST_TEST(newOrder.getShipDate() == "111123");
  BOOST_TEST(newOrder.getStatus() == "approved");
  BOOST_TEST(newOrder.isComplete() == false);
}


BOOST_DATA_TEST_CASE(
    validStatusValues,
    boost::unit_test::data::make({"placed","approved","delivered"}),
    status)
{
    Order order;
    order.setStatus(status);

    BOOST_TEST(order.getStatus() == status);

}

BOOST_DATA_TEST_CASE(invalidStatusValues,
    boost::unit_test::data::make({"",
                                  "notallowed",
                                  "not available"}),
    invalid_status)
{
    bool exceptionCaught = false;

    Order order;

    try {
        order.setStatus(invalid_status);
    }
    catch(const std::runtime_error& excp) {
        exceptionCaught = true;
        const auto expectedErrorMessage = std::string("Value ") +
                                  invalid_status +
                                  " not allowed";
        BOOST_TEST(excp.what() == expectedErrorMessage);
    }
    BOOST_TEST(exceptionCaught);
}

BOOST_AUTO_TEST_SUITE_END()
