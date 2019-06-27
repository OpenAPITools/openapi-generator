#include "PetApiTests.h"
#include <iostream>
#include <chrono>
#include <thread>
#include <functional>

OAIPetApiTests::OAIPetApiTests(std::string host, std::string basePath){
    apiconfiguration = std::make_shared<ApiConfiguration>();
    apiconfiguration->setBaseUrl(host + basePath);
    apiconfiguration->setUserAgent(U("OpenAPI Client"));
    apiclient = std::make_shared<ApiClient>(apiconfiguration);
    api = std::make_shared<PetApi>(apiclient);
}

OAIPetApiTests::~OAIPetApiTests() {

}

void OAIPetApiTests::runTests(){
    testAddPet();
    testFindPetsByStatus();
    testGetPetById();
}

void OAIPetApiTests::testAddPet(){
    auto req = std::make_shared<Pet>();
    req->setId(12345);
    req->setName("cpprest-pet");
    req->setStatus(U("123"));

    std::function<void()> responseCallback = []()
    {
        std::cout << "added pet successfully" << std::endl;
    };

    auto reqTask = api->addPet(req).then(responseCallback);
    try{
        reqTask.wait();
    }
    catch(const ApiException& ex){
        std::cout << ex.what() << std::endl << std::flush;
        std::string err(ex.what());
    }
    catch(const std::exception &ex){
        std::cout << ex.what() << std::endl << std::flush;
        std::string err(ex.what());
    }
}

void OAIPetApiTests::testFindPetsByStatus(){
    auto req = std::vector<utility::string_t>();
    req.push_back(U("123"));
    auto reqTask = api->findPetsByStatus(req)
            .then([=](std::vector<std::shared_ptr<Pet>> pets)
            {
                std::cout << "found pet successfully" << std::endl;
            });
    try{
        reqTask.wait();
    }
    catch(const ApiException& ex){
        std::cout << ex.what() << std::endl << std::flush;
        std::string err(ex.what());
    }
    catch(const std::exception &ex){
        std::cout << ex.what() << std::endl << std::flush;
        std::string err(ex.what());
    }
}

void OAIPetApiTests::testGetPetById(){
    int req = 12345;
    auto responseCallback = std::bind(&OAIPetApiTests::getPetByIdCallback, this, std::placeholders::_1);

    auto reqTask = api->getPetById(req).then(responseCallback);

    try{
        reqTask.wait();
    }
    catch(const ApiException& ex){
        std::cout << ex.what() << std::endl << std::flush;
        std::string err(ex.what());
    }
    catch(const std::exception &ex){
        std::cout << ex.what() << std::endl << std::flush;
        std::string err(ex.what());
    }
}

void OAIPetApiTests::getPetByIdCallback(std::shared_ptr<Pet> pet){
    std::cout << "found pet by id successfully" << std::endl;
}
