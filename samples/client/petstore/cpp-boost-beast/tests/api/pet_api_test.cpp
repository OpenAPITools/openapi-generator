#define BOOST_TEST_INCLUDED
#include <list>
#include "../ApprovalTests.hpp"
#include <boost/property_tree/json_parser.hpp>
#include <boost/property_tree/ptree.hpp>
#include <boost/test/data/test_case.hpp>
#include <boost/test/unit_test.hpp>
#include <initializer_list>
#include <sstream>

#include "api/HttpClientImpl.h"
#include "api/PetApi.h"
#include "model/Pet.h"


using namespace ApprovalTests;
using namespace org::openapitools::client::api;


#define REQUIRE_THROW(fn, ex, lambda) do { \
        bool exceptionThrown = false;      \
        try { fn; }                        \
        catch(const ex& e) {               \
            exceptionThrown = true;        \
            lambda(e);                     \
         }                                 \
         BOOST_REQUIRE(exceptionThrown);   \
    } while (0)


BOOST_AUTO_TEST_SUITE(PetApiTest)


BOOST_AUTO_TEST_CASE(getPetById_success) {
    std::shared_ptr<HttpClient> client = std::make_shared<HttpClientImpl>("localhost", "8080");
    PetApi api(client);
    const auto resp = api.getPetById(0);

    Approvals::verify(resp->toJsonString(true));
}

BOOST_AUTO_TEST_CASE(getPetById_invalid_id) {
    std::shared_ptr<HttpClient> client = std::make_shared<HttpClientImpl>("localhost", "8080");
    PetApi api(client);

    REQUIRE_THROW(api.getPetById(1), PetApiException, [](const auto& e) {
        BOOST_REQUIRE_EQUAL(e.getStatus(), boost::beast::http::status{400});
        BOOST_REQUIRE_EQUAL(e.what(), "Invalid ID supplied");
    });
}

BOOST_AUTO_TEST_CASE(getPetById_not_found) {
    std::shared_ptr<HttpClient> client = std::make_shared<HttpClientImpl>("localhost", "8080");
    PetApi api(client);
    REQUIRE_THROW(api.getPetById(2), PetApiException, [](const auto& e) {
        BOOST_REQUIRE_EQUAL(e.getStatus(), boost::beast::http::status{404});
        BOOST_REQUIRE_EQUAL(e.what(), "Pet not found");
    });
}

BOOST_AUTO_TEST_SUITE_END()
