#define BOOST_TEST_INCLUDED
#include <list>
#include "../ApprovalTests.hpp"
#include <boost/property_tree/ptree.hpp>
#include <boost/test/data/test_case.hpp>
#include <boost/test/unit_test.hpp>
#include <initializer_list>

#include "api/HttpClientImpl.h"
#include "api/PetApi.h"
#include "model/Pet.h"
#include "testing_helper.h"

#include <vector>
#include <tuple>

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


struct fixture {
    fixture() = default;
    ~fixture() = default;

    std::shared_ptr<HttpClient> client = std::make_shared<HttpClientImpl>("localhost", "8080");
};


BOOST_FIXTURE_TEST_SUITE(PetApiTest, fixture)


BOOST_AUTO_TEST_CASE(getPetById_success) {
    PetApi api(client);
    const auto resp = api.getPetById(0);

    Approvals::verify(resp->toJsonString(true));
}

BOOST_AUTO_TEST_CASE(getPetById_invalid_id) {
    PetApi api(client);

    REQUIRE_THROW(api.getPetById(1), PetApiException, [](const auto& e) {
        BOOST_REQUIRE_EQUAL(e.getStatus(), boost::beast::http::status{400});
        BOOST_REQUIRE_EQUAL(e.what(), "Invalid ID supplied");
    });
}

BOOST_AUTO_TEST_CASE(getPetById_not_found) {
    PetApi api(client);
    REQUIRE_THROW(api.getPetById(2), PetApiException, [](const auto& e) {
        BOOST_REQUIRE_EQUAL(e.getStatus(), boost::beast::http::status{404});
        BOOST_REQUIRE_EQUAL(e.what(), "Pet not found");
    });
}

BOOST_AUTO_TEST_CASE(addPet) {
    PetApi api(client);

    const auto petJson = R"JSON(
    {
        "id": "0",
        "name": "doggie",
        "status": "available"
    })JSON";

    auto pet = std::make_shared<Pet>();
    pet->fromJsonString(petJson);


    const auto responsePet = api.addPet(pet);
    BOOST_REQUIRE_EQUAL(0, responsePet->getId());
}

BOOST_AUTO_TEST_CASE(updatePet_success) {
    PetApi api(client);

    const auto petJson = R"JSON(
    {
        "id": "1",
        "name": "cat",
        "status": "available"
    })JSON";

    auto pet = std::make_shared<Pet>();
    pet->fromJsonString(petJson);

    const auto responsePet = api.updatePet(pet);
    Approvals::verify(responsePet->toJsonString(true));
}

BOOST_DATA_TEST_CASE(updatePet_fails, boost::unit_test::data::make(
        std::vector<std::tuple<int, int, std::string>>{
            {400, 400, "Invalid ID supplied"},
            {404, 404, "Pet not found"},
            {405, 405, "Validation exception"}}), pet_id, expected_http_status, expected_error_message) {

    PetApi api(client);

    auto pet = std::make_shared<Pet>();
    pet->setId(pet_id);
    pet->setName("doggie");
    pet->setStatus("available");
    const auto photoUrls = std::vector<std::string>();
    pet->setPhotoUrls(photoUrls);

    REQUIRE_THROW(api.updatePet(pet), PetApiException, [&](const auto& e) {
        BOOST_REQUIRE_EQUAL(e.getStatus(), boost::beast::http::status{expected_http_status});
        BOOST_REQUIRE_EQUAL(e.what(), expected_error_message);
    });

}

BOOST_AUTO_TEST_CASE(updatePetWithForm) {
    PetApi api(client);

    // Nothing to assert. Should not throw any exception
    api.updatePetWithForm(42, "my_pet", "sold");
}

BOOST_AUTO_TEST_CASE(findPetsByStatus) {
    PetApi api(client);

    const std::vector<std::string> states{{"available"}};
    const auto response = api.findPetsByStatus(states);
    const auto json = createJsonStringFromModelVector(response);
    Approvals::verify(json);
}

BOOST_AUTO_TEST_CASE(findPetsByStatus_list) {
    PetApi api(client);

    const std::vector<std::string> states{{"available"}, {"sold"}};
    const auto response = api.findPetsByStatus(states);
    const auto json = createJsonStringFromModelVector(response);
    Approvals::verify(json);
}

BOOST_AUTO_TEST_CASE(deletePet) {
    PetApi api(client);

    api.deletePet(50, "myApiKey");
}

BOOST_AUTO_TEST_CASE(findPetsByTags) {
    PetApi api(client);

    const std::vector<std::string> tags{"tag_a", "tag_b"};
    const auto resp = api.findPetsByTags(tags);
    const auto json = createJsonStringFromModelVector(resp);
    Approvals::verify(json);
}

BOOST_AUTO_TEST_CASE(uploadFile) {
    PetApi api(client);
    const auto resp = api.uploadFile(1, "some metadata", "a file");

    const auto json = resp->toJsonString(true);
    Approvals::verify(json);
}

BOOST_AUTO_TEST_SUITE_END()



struct throwing_tests_fixture {
    throwing_tests_fixture() = default;
    ~throwing_tests_fixture() = default;

    std::shared_ptr<ThrowingClient> client = std::make_shared<ThrowingClient>();
};


BOOST_FIXTURE_TEST_SUITE(PetApiExceptionsTest, throwing_tests_fixture)

BOOST_AUTO_TEST_CASE(getPetById_std_exception) {
        client->setExceptionType(ExceptionType::STD_EXCEPTION);
        auto baseClient = std::static_pointer_cast<HttpClient>(client);
        PetApi api(baseClient);

        BOOST_REQUIRE_THROW(api.getPetById(0), std::exception);
}

    BOOST_AUTO_TEST_CASE(getPetById_int_exception) {
        client->setExceptionType(ExceptionType::INT);
        auto baseClient = std::static_pointer_cast<HttpClient>(client);
        PetApi api(baseClient);

        BOOST_REQUIRE_THROW(api.getPetById(0), int);
    }


BOOST_AUTO_TEST_SUITE_END()
