#include "api/PetApi.h"
#include "api/StoreApi.h"
#include "api/UserApi.h"

using namespace org::openapitools::server::api;
using namespace org::openapitools::server::api::PetApiResources;
using namespace org::openapitools::server::api::StoreApiResources;
using namespace org::openapitools::server::api::UserApiResources;

Pet createPetForTesting_1() {
  auto pet = Pet();
  pet.setName("HelloPet");
  pet.setId(23);
  pet.setStatus("available");
  return pet;
}

std::shared_ptr<Pet> createPetForTesting_2() {
  auto pet = std::make_shared<Pet>();
  pet->setName("HelloPet2");
  pet->setId(44);
  pet->setStatus("sold");
  return pet;
}

class MyPetApiPetResource : public PetResource {
public:
    int
    handler_POST(Pet &pet) override {
    return 200;
  }

  int
  handler_PUT(Pet &pet) override {
    return 200;
  }
};

class MyPetApiPetPetIdResource : public PetPetIdResource {
public:
  int handler_DELETE(int64_t &petId,
                     std::string &api_key) override {
    return 200;
  }

  std::pair<int, Pet>
  handler_GET(int64_t &petId) override {
    Pet pet = createPetForTesting_1();

    return std::make_pair(200, pet);
  }
};

class MyStoreApiStoreOrderOrderIdResource : public StoreOrderOrder_idResource {
public:
  int handler_DELETE(std::string &orderId) override {
    return 200;
  }

  std::pair<int, Order>
  handler_GET(int64_t &orderId) override {
    auto order = Order();
    order.setStatus("placed");
    order.setId(orderId);
    order.setComplete(false);
    order.setPetId(4444L);
    order.setQuantity(3);
    order.setShipDate("2011-09-21T17:32:28Z");

    return std::make_pair(200, order);
  }
};

class MyStoreApiStoreInventoryResource : public StoreInventoryResource {
public:
  std::pair<int, std::map<std::string, int32_t>> handler_GET() override {
    std::map<std::string, int32_t> result {{"MyPet", 123}, {"OtherPet", 99}};
    return std::make_pair(200, result);
  }
};

class MyStoreApiStoreOrderResource: public StoreOrderResource {
public:
  std::pair<int, Order>
  handler_POST(Order &order) override {
    order.setStatus("placed");
    order.setShipDate("2011-01-12T23:24:02Z");
    return {200, order};
  }
};

class MyUserApiUserResource : public UserResource {
public:
  int handler_POST(User &User) override {
    return 200;
  }
};


class MyUserApiUserCreateWithArrayResource : public UserCreateWithArrayResource {
public:
  int handler_POST(std::vector<User> &User) override {
    return 200;
  }
};

class MyUserApiUserCreateWithListResource : public UserCreateWithListResource {
public:
  int handler_POST(std::vector<User> &User) override {
    return 200;
  }
};

class MyUserApiUserLoginResource : public UserLoginResource {
public:
  std::pair<int, std::string>
  handler_GET(std::string &username,
              std::string &password) override {
    return {200, username};
  }
};

class MyUserApiUserLogoutResource : public UserLogoutResource {
public:
  int handler_GET() override {
    return 200;
  }
};

int main() {
  const auto service = std::make_shared<restbed::Service>();

  auto petApi = PetApi(service);
  petApi.setResource(std::make_shared<MyPetApiPetPetIdResource>());
  petApi.setResource(std::make_shared<MyPetApiPetResource>());

  auto storeApi = StoreApi(service);
  storeApi.setResource(std::make_shared<MyStoreApiStoreOrderOrderIdResource>());
  storeApi.setResource(std::make_shared<MyStoreApiStoreInventoryResource>());
  storeApi.setResource(std::make_shared<MyStoreApiStoreOrderResource>());

  auto userApi = UserApi(service);
  userApi.setResource(std::make_shared<MyUserApiUserResource>());
  userApi.setResource(std::make_shared<MyUserApiUserCreateWithArrayResource>());
  userApi.setResource(std::make_shared<MyUserApiUserCreateWithListResource>());
  userApi.setResource(std::make_shared<MyUserApiUserLoginResource>());
  userApi.setResource(std::make_shared<MyUserApiUserLogoutResource>());

  const auto settings = std::make_shared<restbed::Settings>();
  settings->set_port(1235);

  service->start(settings);

}