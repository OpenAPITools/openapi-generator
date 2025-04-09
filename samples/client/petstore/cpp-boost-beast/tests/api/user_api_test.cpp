#define BOOST_TEST_INCLUDED
#include <list>
#include "../ApprovalTests.hpp"
#include <boost/property_tree/ptree.hpp>
#include <boost/test/data/test_case.hpp>
#include <boost/test/unit_test.hpp>

#include "api/HttpClientImpl.h"
#include "api/UserApi.h"

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
    UserApi api;
};

}


BOOST_FIXTURE_TEST_SUITE(UserApiTest, fixture)


BOOST_AUTO_TEST_CASE(createUser) {
    auto user = std::make_shared<User>();
    user->setId(0);

    // Nothing to assert. Should not throw any exception
    api.createUser(user);
}

BOOST_AUTO_TEST_CASE(createUsersWithArrayInput) {
    auto user = std::make_shared<User>();
    user->setId(0);

    std::vector<std::shared_ptr<User>> userVec;
    userVec.emplace_back(user);

    // Nothing to assert. Should not throw any exception
    api.createUsersWithArrayInput(userVec);
}

BOOST_AUTO_TEST_CASE(createUsersWithListInput) {
    auto user = std::make_shared<User>();
    user->setId(0);

    std::vector<std::shared_ptr<User>> userVec;
    userVec.emplace_back(user);

    // Nothing to assert. Should not throw any exception
    api.createUsersWithListInput(userVec);
}

BOOST_AUTO_TEST_CASE(deleteUser) {

    // Nothing to assert. Should not throw any exception
    api.deleteUser("me");
}

BOOST_DATA_TEST_CASE(deleteUser_fails, boost::unit_test::data::make(
    std::vector<std::tuple<std::string, int, std::string>>{
            {"user_name_invalid", 400, "Invalid username supplied"},
            {"user_name_not_found", 404, "User not found"}}), order_id, expected_http_status, expected_error_message) {

    REQUIRE_THROW(api.deleteUser(order_id), UserApiException, [&](const auto& e) {
        BOOST_REQUIRE_EQUAL(e.getStatus(), boost::beast::http::status{expected_http_status});
        BOOST_REQUIRE_EQUAL(e.what(), expected_error_message);
    });
}

BOOST_AUTO_TEST_CASE(getUserByName) {
    const auto resp = api.getUserByName("myName");
    BOOST_REQUIRE_EQUAL(resp->getId(), 0);
}

BOOST_AUTO_TEST_CASE(updateUser) {

    auto user = std::make_shared<User>();

    // Nothing to assert. Should not throw any exception
    api.updateUser("me", user);
}

BOOST_AUTO_TEST_CASE(loginUser) {

    const auto resp = api.loginUser("me", "pa55w0rd");
    BOOST_REQUIRE_EQUAL(resp, "token");

}

BOOST_AUTO_TEST_CASE(logoutUser) {

    // Nothing to assert. Should not throw any exception
    api.logoutUser();
}

BOOST_AUTO_TEST_SUITE_END()
