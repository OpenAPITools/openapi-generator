#include "api/PetApi.h"
#include "api/StoreApi.h"
#include "api/UserApi.h"

#include <stdexcept>

#include<signal.h>
void sig_handler(int signum){
  printf("\nSIGINT received!\n");
  exit(0);
}

using namespace org::openapitools::server::api;
using namespace org::openapitools::server::api::PetApiResources;
using namespace org::openapitools::server::api::StoreApiResources;
using namespace org::openapitools::server::api::UserApiResources;

namespace {
const auto RETURN_STATUS = std::string("ReturnsStatus");

bool isReturnStatusString(const std::string &errorType) {
  return errorType.rfind(RETURN_STATUS, 0) == 0;
}

int extractReturnStatus(const std::string &errorType) {
  assert(isReturnStatusString(errorType));

  auto returnValString = errorType;
  returnValString.erase(0, RETURN_STATUS.length());

  return std::stoi(returnValString);
}


template <class RETURN_T, class API_EXCEPTION_T>
std::pair<int, RETURN_T>
raiseErrorForTesting(const RETURN_T &modelObj,
                     const std::string &errorType) {
  if ("ThrowsApiException" == errorType) {
    throw API_EXCEPTION_T(500, "ApiException raised");
  } else if ("ThrowsStdExceptionDerivedException" == errorType) {
    throw std::logic_error("std::logic_error raised");
  } else if ("ThrowsInt" == errorType) {
    throw int(1);
  }  else if (isReturnStatusString(errorType)) {
    auto retStatus = extractReturnStatus(errorType);
    return {retStatus, modelObj};
  }

  return std::make_pair(500, modelObj);
}

std::string intToErrorRaisingString(const int64_t &id) {
  std::string errorType;
  switch(id) {
  case 9100:  errorType = "ThrowsApiException";
  break;
  case 9200:  errorType = "ThrowsStdExceptionDerivedException";
  break;
  case 9300:  errorType = "ThrowsInt";
  break;
  default: errorType = RETURN_STATUS + std::to_string(id);
  }
  return errorType;
}

} // namespace

class MyPetApiPetResource : public PetResource {
public:
  int handler_POST(Pet &pet) override {
    const std::string &name = pet.getName();
    int status;
    Pet pet_;
    std::tie(status, pet_) = raiseErrorForTesting<Pet, PetApiException>(pet, name);
    return status;
  }

    int handler_PUT(Pet &pet) override {
    const std::string &name = pet.getName();
    int status;
    Pet pet_;
    std::tie(status, pet_) =  raiseErrorForTesting<Pet, PetApiException>(pet, name);
    return status;
  }
};

class MyPetApiPetPetIdResource : public PetPetIdResource {
public:
  int handler_DELETE(int64_t &petId,
                     std::string &api_key) override {
    int status;
    Pet pet;
    std::tie(status, pet) = raiseErrorForTesting<Pet, PetApiException>(Pet(), api_key);
    return status;
  }

  std::pair<int, Pet>
  handler_GET(int64_t &id) override {
    std::string errorType = intToErrorRaisingString(id);

    auto pet = Pet();
    pet.setName("MyPuppy");
    pet.setStatus("available");

    return raiseErrorForTesting<Pet, PetApiException>(pet, errorType);
  }
};

class MyStoreApiStoreOrderOrderIdResource : public StoreOrderOrder_idResource {
public:
  int handler_DELETE(std::string &orderId) override {
    int status;
    Pet pet;
    std::tie(status, pet) = raiseErrorForTesting<Pet, PetApiException>(Pet(), orderId);
    return status;
  }

  std::pair<int, Order>
  handler_GET(int64_t &orderId) override {
    std::string errorType = intToErrorRaisingString(orderId);
    const auto order = Order();
    return raiseErrorForTesting<Order, StoreApiException>(order, errorType);
  }
};

class MyStoreApiStoreInventoryResource : public StoreInventoryResource {
public:
  std::pair<int, std::map<std::string, int32_t>> handler_GET() override {
    std::map<std::string, int32_t> ret;
    return {300,  ret};
  }
};

class MyStoreApiStoreOrderResource : public StoreOrderResource {
public:
  std::pair<int, Order>
  handler_POST(Order &order) override {
    std::string errorType = intToErrorRaisingString(order.getId());
    return raiseErrorForTesting<Order, StoreApiException>(order, errorType);
  }
};

class MyUserApiUserCreateWithArrayResource : public UserCreateWithArrayResource {
public:
  int handler_POST(std::vector<User> &user) override {
    const auto errorType = user[0].getFirstName();
    int status;
    User user_;
    std::tie(status, user_) = raiseErrorForTesting<User, UserApiException>(User(), errorType);
    return status;
  }
};

class MyUserApiUserCreateWithListResource : public UserCreateWithListResource {
public:
  int handler_POST(std::vector<User> &user) override {
    const auto errorType = user[0].getFirstName();
    int status;
    User user_;
    std::tie(status, user_) = raiseErrorForTesting<User, UserApiException>(User(), errorType);
    return status;
  }
};

class MyUserApiUserLoginResource : public UserLoginResource {
public:
  std::pair<int, std::string>
  handler_GET(std::string &username,
              std::string &password) override {
    int status;
    User user_;
    std::tie(status, user_) = raiseErrorForTesting<User, UserApiException>(User(), username);
    return {status, username};
  }
};

class MyUserApiUserLogoutResource : public UserLogoutResource {
public:
  int handler_GET() override {
    throw int(5);
  }
};

int main() {
  signal(SIGINT,sig_handler);

  const auto service = std::make_shared<restbed::Service>();

  auto petApi = PetApi(service);
  petApi.setResource(std::make_shared<MyPetApiPetResource>());
  petApi.setResource(std::make_shared<MyPetApiPetPetIdResource>());

  auto storeApi = StoreApi(service);
  storeApi.setResource(std::make_shared<MyStoreApiStoreOrderOrderIdResource>());
  storeApi.setResource(std::make_shared<MyStoreApiStoreInventoryResource>());
  storeApi.setResource(std::make_shared<MyStoreApiStoreOrderResource>());

  auto userApi = UserApi(service);
  userApi.getUserResource()->handler_POST_func = [](auto& user) {
            const auto errorType = user.getFirstName();
            int status;
            User user_;
            std::tie(status, user_) = raiseErrorForTesting<User, UserApiException>(user, errorType);
            return status;
        };
  userApi.setResource(std::make_shared<MyUserApiUserCreateWithArrayResource>());
  userApi.setResource(std::make_shared<MyUserApiUserCreateWithListResource>());
  userApi.setResource(std::make_shared<MyUserApiUserLoginResource>());
  userApi.setResource(std::make_shared<MyUserApiUserLogoutResource>());


  const auto settings = std::make_shared<restbed::Settings>();
  settings->set_port(1236);

  service->start(settings);

}
