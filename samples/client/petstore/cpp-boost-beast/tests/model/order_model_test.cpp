#define BOOST_TEST_INCLUDED
#include <list>
#include <boost/property_tree/ptree.hpp>
#include <boost/test/data/test_case.hpp>
#include <boost/test/unit_test.hpp>
#include <sstream>

#include "../ApprovalTests.hpp"

#include "model/Order.h"

using namespace ApprovalTests;
using namespace org::openapitools::client::model;

BOOST_AUTO_TEST_SUITE(OrderModelTest)

BOOST_AUTO_TEST_CASE(toJsonString) {
  Order order;
  order.setId(2L);
  order.setPetId(3L);
  order.setQuantity(6);
  order.setShipDate("123");
  order.setStatus("placed");
  order.setComplete(false);

  Approvals::verify(order.toJsonString(true));
}

BOOST_AUTO_TEST_CASE(fromJsonString) {
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

BOOST_AUTO_TEST_CASE(test_createJsonStringFromOrderVector) {
  auto order0 = std::make_shared<Order>();
  auto order1 = std::make_shared<Order>();
  auto order2 = std::make_shared<Order>();

  order0->setId(1);
  order0->setPetId(1);
  order0->setQuantity(1);
  order0->setShipDate("11111");
  order0->setStatus("placed");
  order0->setComplete(true);
  order1->setId(2);
  order1->setPetId(2);
  order1->setQuantity(2);
  order1->setShipDate("222222");
  order1->setStatus("approved");
  order1->setComplete(false);
  order2->setId(3);
  order2->setPetId(3);
  order2->setQuantity(3);
  order2->setShipDate("33333");
  order2->setStatus("delivered");
  order2->setComplete(false);

  auto vec = std::vector<std::shared_ptr<Order>>();
  vec.emplace_back(order0);
  vec.emplace_back(order1);
  vec.emplace_back(order2);

  const auto json = createJsonStringFromModelVector(vec);

  Approvals::verify(json);
}

BOOST_AUTO_TEST_CASE(toAndFromPropertyTree) {
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

BOOST_DATA_TEST_CASE(validStatusValues,
                     boost::unit_test::data::make({"placed", "approved",
                                                   "delivered"}),
                     status) {
  Order order;
  order.setStatus(status);

  BOOST_TEST(order.getStatus() == status);
}

BOOST_DATA_TEST_CASE(invalidStatusValues,
                     boost::unit_test::data::make({"", "notallowed",
                                                   "not available"}),
                     invalid_status) {
  bool exceptionCaught = false;

  Order order;

  try {
    order.setStatus(invalid_status);
  } catch (const std::runtime_error &excp) {
    exceptionCaught = true;
    const auto expectedErrorMessage =
        std::string("Value ") + invalid_status + " not allowed";
    BOOST_TEST(excp.what() == expectedErrorMessage);
  }
  BOOST_TEST(exceptionCaught);
}

BOOST_AUTO_TEST_SUITE_END()
