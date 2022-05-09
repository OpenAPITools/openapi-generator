#include "api/PetApi.h"
#include "api/StoreApi.h"
#include "api/UserApi.h"

using namespace org::openapitools::server::api;

std::shared_ptr<Pet> createPetForTesting_1() {
  auto pet = std::make_shared<Pet>();
  pet->setName("HelloPet");
  pet->setId(23);
  pet->setStatus("available");
  return pet;
}

std::shared_ptr<Pet> createPetForTesting_2() {
  auto pet = std::make_shared<Pet>();
  pet->setName("HelloPet2");
  pet->setId(44);
  pet->setStatus("sold");
  return pet;
}

class MyPetApiPetResource : public PetApiPetResource {
public:
  std::pair<int, std::shared_ptr<Pet>>
  handler_POST(const std::shared_ptr<Pet> &Pet) override {
    return std::make_pair(200, Pet);
  }

  std::pair<int, std::shared_ptr<Pet>>
  handler_PUT(const std::shared_ptr<Pet> &Pet) override {
    return std::make_pair(200, Pet);
  }
};

class MyPetApiPetPetIdResource : public PetApiPetPetIdResource {
public:
  int handler_DELETE(const int64_t &petId,
                     const std::string &api_key) override {
    return 200;
  }

  std::pair<int, std::shared_ptr<Pet>>
  handler_GET(const int64_t &petId) override {
    std::shared_ptr<Pet> pet = createPetForTesting_1();

    return std::make_pair(200, pet);
  }
};

class MyStoreApiStoreOrderOrderIdResource : public StoreApiStoreOrderOrderIdResource {
public:
  int handler_DELETE(const std::string &orderId) override {
    return 200;
  }

  std::pair<int, std::shared_ptr<Order>>
  handler_GET(const int64_t &orderId) override {
    auto order = std::make_shared<Order>();
    order->setStatus("placed");
    order->setId(orderId);
    order->setComplete(false);
    order->setPetId(4444L);
    order->setQuantity(3);
    order->setShipDate("2011-09-21T17:32:28Z");

    return std::make_pair(200, order);
  }
};

class MyStoreApiStoreInventoryResource : public StoreApiStoreInventoryResource {
public:
  std::pair<int, std::map<std::string, int32_t>> handler_GET() override {
    std::map<std::string, int32_t> result {{"MyPet", 123}, {"OtherPet", 99}};
    return std::make_pair(200, result);
  }
};

class MyStoreApiStoreOrderResource: public StoreApiStoreOrderResource {
public:
  std::pair<int, std::shared_ptr<Order>>
  handler_POST(const std::shared_ptr<Order> &order) override {
    order->setStatus("placed");
    order->setShipDate("2011-01-12T23:24:02Z");
    return {200, order};
  }
};

class MyUserApiUserResource : public UserApiUserResource {
public:
  int handler_POST(const std::shared_ptr<User> &User) override {
    return 200;
  }
};


class MyUserApiUserCreateWithArrayResource : public UserApiUserCreateWithArrayResource {
public:
  int handler_POST(const std::vector<std::shared_ptr<User>> &User) override {
    return 200;
  }
};

class MyUserApiUserCreateWithListResource : public UserApiUserCreateWithListResource {
public:
  int handler_POST(const std::vector<std::shared_ptr<User>> &User) override {
    return 200;
  }
};

class MyUserApiUserLoginResource : public UserApiUserLoginResource {
public:
  std::pair<int, std::string>
  handler_GET(const std::string &username,
              const std::string &password) override {
    return {200, username};
  }
};

class MyUserApiUserLogoutResource : public UserApiUserLogoutResource {
public:
  int handler_GET() override {
    return 200;
  }
};

int main() {
  const auto service = std::make_shared<restbed::Service>();

  auto petApi = PetApi(service);
  petApi.setPetApiPetPetIdResource(std::make_shared<MyPetApiPetPetIdResource>());
  petApi.setPetApiPetResource(std::make_shared<MyPetApiPetResource>());

  auto storeApi = StoreApi(service);
  storeApi.setStoreApiStoreOrderOrderIdResource(std::make_shared<MyStoreApiStoreOrderOrderIdResource>());
  storeApi.setStoreApiStoreInventoryResource(std::make_shared<MyStoreApiStoreInventoryResource>());
  storeApi.setStoreApiStoreOrderResource(std::make_shared<MyStoreApiStoreOrderResource>());

  auto userApi = UserApi(service);
  userApi.setUserApiUserResource(std::make_shared<MyUserApiUserResource>());
  userApi.setUserApiUserCreateWithArrayResource(std::make_shared<MyUserApiUserCreateWithArrayResource>());
  userApi.setUserApiUserCreateWithListResource(std::make_shared<MyUserApiUserCreateWithListResource>());
  userApi.setUserApiUserLoginResource(std::make_shared<MyUserApiUserLoginResource>());
  userApi.setUserApiUserLogoutResource(std::make_shared<MyUserApiUserLogoutResource>());

  const auto settings = std::make_shared<restbed::Settings>();
  settings->set_port(1235);

  service->start(settings);

}