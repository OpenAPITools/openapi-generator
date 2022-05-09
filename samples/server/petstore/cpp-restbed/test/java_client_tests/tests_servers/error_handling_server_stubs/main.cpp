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
std::pair<int, std::shared_ptr<RETURN_T>>
raiseErrorForTesting(const std::shared_ptr<RETURN_T> &modelObj,
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

class MyPetApiPetResource : public PetApiPetResource {
public:
  std::pair<int, std::shared_ptr<Pet>>
  handler_POST(const std::shared_ptr<Pet> &pet) override {
    const std::string &name = pet->getName();

    return raiseErrorForTesting<Pet, PetApiException>(pet, name);
  }

  std::pair<int, std::shared_ptr<Pet>>
  handler_PUT(const std::shared_ptr<Pet> &pet) override {
    const std::string &name = pet->getName();

    return raiseErrorForTesting<Pet, PetApiException>(pet, name);
  }
};

class MyPetApiPetPetIdResource : public PetApiPetPetIdResource {
public:
  int handler_DELETE(const int64_t &petId,
                     const std::string &api_key) override {
    int status;
    std::shared_ptr<Pet> pet;
    std::tie(status, pet) = raiseErrorForTesting<Pet, PetApiException>(std::make_shared<Pet>(), api_key);
    return status;
  }

  std::pair<int, std::shared_ptr<Pet>>
  handler_GET(const int64_t &id) override {
    std::string errorType = intToErrorRaisingString(id);

    auto pet = std::make_shared<Pet>();
    pet->setName("MyPuppy");
    pet->setStatus("available");

    return raiseErrorForTesting<Pet, PetApiException>(pet, errorType);
  }
};

class MyStoreApiStoreOrderOrderIdResource : public StoreApiStoreOrderOrderIdResource {
public:
  int handler_DELETE(const std::string &orderId) override {
    int status;
    std::shared_ptr<Pet> pet;
    std::tie(status, pet) = raiseErrorForTesting<Pet, PetApiException>(std::make_shared<Pet>(), orderId);
    return status;
  }

  std::pair<int, std::shared_ptr<Order>>
  handler_GET(const int64_t &orderId) override {
    std::string errorType = intToErrorRaisingString(orderId);
    const auto order = std::make_shared<Order>();
    return raiseErrorForTesting<Order, StoreApiException>(order, errorType);
  }
};

class MyStoreApiStoreInventoryResource : public StoreApiStoreInventoryResource {
public:
  std::pair<int, std::map<std::string, int32_t>> handler_GET() override {
    std::map<std::string, int32_t> ret;
    return {300,  ret};
  }
};

class MyStoreApiStoreOrderResource : public StoreApiStoreOrderResource {
public:
  std::pair<int, std::shared_ptr<Order>>
  handler_POST(const std::shared_ptr<Order> &order) override {
    std::string errorType = intToErrorRaisingString(order->getId());
    return raiseErrorForTesting<Order, StoreApiException>(order, errorType);
  }
};

class MyUserApiUserResource : public UserApiUserResource {
public:
  int handler_POST(const std::shared_ptr<User> &user) override {
    const auto errorType = user->getFirstName();
    int status;
    std::shared_ptr<User> user_;
    std::tie(status, user_) = raiseErrorForTesting<User, UserApiException>(user, errorType);
    return status;
  }
};

class MyUserApiUserCreateWithArrayResource : public UserApiUserCreateWithArrayResource {
public:
  int handler_POST(const std::vector<std::shared_ptr<User>> &user) override {
    const auto errorType = user[0]->getFirstName();
    int status;
    std::shared_ptr<User> user_;
    std::tie(status, user_) = raiseErrorForTesting<User, UserApiException>(std::make_shared<User>(), errorType);
    return status;
  }
};

class MyUserApiUserCreateWithListResource : public UserApiUserCreateWithListResource {
public:
  int handler_POST(const std::vector<std::shared_ptr<User>> &user) override {
    const auto errorType = user[0]->getFirstName();
    int status;
    std::shared_ptr<User> user_;
    std::tie(status, user_) = raiseErrorForTesting<User, UserApiException>(std::make_shared<User>(), errorType);
    return status;
  }
};

class MyUserApiUserLoginResource : public UserApiUserLoginResource {
public:
  std::pair<int, std::string>
  handler_GET(const std::string &username,
              const std::string &password) override {
    int status;
    std::shared_ptr<User> user_;
    std::tie(status, user_) = raiseErrorForTesting<User, UserApiException>(std::make_shared<User>(), username);
    return {status, username};
  }
};

class MyUserApiUserLogoutResource : public UserApiUserLogoutResource {
public:
  int handler_GET() override {
    throw int(5);
  }
};

int main() {
  signal(SIGINT,sig_handler);

  const auto service = std::make_shared<restbed::Service>();

  auto petApi = PetApi(service);
  petApi.setPetApiPetResource(std::make_shared<MyPetApiPetResource>());
  petApi.setPetApiPetPetIdResource(std::make_shared<MyPetApiPetPetIdResource>());

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
  settings->set_port(1236);

  service->start(settings);

}
