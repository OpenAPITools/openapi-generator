#ifndef OAI_PETAPI_TESTS_H
#define OAI_PETAPI_TESTS_H

#include <memory>
#include "client/ApiClient.h"
#include "client/ApiConfiguration.h"
#include "client/api/PetApi.h"
#include "client/model/Pet.h"


using namespace std;
using namespace org::openapitools::client::api;

class OAIPetApiTests
{
public:
    explicit OAIPetApiTests(utility::string_t host = utility::conversions::to_string_t("http://petstore.swagger.io"), utility::string_t basePath = utility::conversions::to_string_t("/v2"));

    virtual ~OAIPetApiTests();
public:
    void runTests();
private:
    void testAddPet();
    void testFindPetsByStatus();
    void testGetPetById();

    void getPetByIdCallback(std::shared_ptr<Pet> pet);

    std::shared_ptr<ApiConfiguration> apiconfiguration;
    std::shared_ptr<ApiClient>        apiclient;
    std::shared_ptr<PetApi>           api;
};

#endif // OAI_PETAPI_TESTS_H
