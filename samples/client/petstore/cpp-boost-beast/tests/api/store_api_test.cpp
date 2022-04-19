#define BOOST_TEST_INCLUDED
#include <list>
#include "../ApprovalTests.hpp"
#include <boost/property_tree/ptree.hpp>
#include <boost/test/data/test_case.hpp>
#include <boost/test/unit_test.hpp>

#include "api/HttpClientImpl.h"
#include "api/StoreApi.h"

#include "testing_helper.h"

#include <vector>


using namespace ApprovalTests;
using namespace org::openapitools::client::api;

namespace {
struct fixture {
    explicit fixture()
            : client(std::make_shared<HttpClientImpl>("localhost", "8080")),
              api(client) {}

    ~fixture() = default;

    std::shared_ptr<HttpClient> client;
    StoreApi api;
};

}


BOOST_FIXTURE_TEST_SUITE(StoreApiTest, fixture)


BOOST_AUTO_TEST_CASE(deleteOrder) {

    // Nothing to assert. Should not throw any exception
    api.deleteOrder("order_id_ok");
}

BOOST_DATA_TEST_CASE(deleteOrder_fails, boost::unit_test::data::make(
    std::vector<std::tuple<std::string, int, std::string>>{
            {"order_id_invalid", 400, "Invalid ID supplied"},
            {"order_id_not_found", 404, "Order not found"}}), order_id, expected_http_status, expected_error_message) {

    REQUIRE_THROW(api.deleteOrder(order_id), StoreApiException, [&](const auto& e) {
        BOOST_REQUIRE_EQUAL(e.getStatus(), boost::beast::http::status{expected_http_status});
        BOOST_REQUIRE_EQUAL(e.what(), expected_error_message);
    });
}

BOOST_AUTO_TEST_CASE(getOrderById) {

    const auto resp = api.getOrderById(0);

    BOOST_REQUIRE_EQUAL(resp->getId(), 0);
    BOOST_REQUIRE_EQUAL(resp->getStatus(), "placed");
    BOOST_REQUIRE_EQUAL(resp->getQuantity(), 0);
}

BOOST_AUTO_TEST_CASE(getInventory) {

    const auto resp = api.getInventory();

    BOOST_REQUIRE_EQUAL(resp.size(), 2);

    BOOST_REQUIRE_EQUAL(resp.at("cats"), 3);
    BOOST_REQUIRE_EQUAL(resp.at("dogs"), 2);
}

BOOST_AUTO_TEST_CASE(placeOrder) {

    std::shared_ptr<Order> order = std::make_shared<Order>();

    const auto resp = api.placeOrder(order);

    BOOST_REQUIRE_EQUAL(resp->getId(), 0);
    BOOST_REQUIRE_EQUAL(resp->getStatus(), "placed");
    BOOST_REQUIRE_EQUAL(resp->getQuantity(), 0);
}

BOOST_AUTO_TEST_SUITE_END()
