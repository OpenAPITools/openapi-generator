# cpp-httplib-server - C++ Server

## Overview

This server was generated using the [OpenAPI Generator](https://openapi-generator.tech) project.
It uses the [cpp-httplib](https://github.com/yhirose/cpp-httplib) library to implement a lightweight HTTP server
with JSON request/response handling via [nlohmann/json](https://github.com/nlohmann/json).

## Requirements

- C++17 compatible compiler
- [cpp-httplib](https://github.com/yhirose/cpp-httplib) library
- [nlohmann/json](https://github.com/nlohmann/json) library
- CMake (3.14 or higher)

## Project Structure

```
├── CMakeLists.txt          # Project build configuration
├── README.md               # This file
├── model/                  # Generated model classes
└── api/                    # Generated API handler classes
```

## Building the Project

```bash
mkdir build
cd build
cmake ..
make
```

## Working with Models

### Model Classes

#### models::ApiResponse

```cpp
// Create a model
auto model = models::ApiResponse();
model.setCode(/* value */);  // Set code
model.setType(/* value */);  // Set type
model.setMessage(/* value */);  // Set message

// Serialize to JSON
nlohmann::json json = models::ApiResponse::toJson(model);
std::string jsonString = json.dump();

// Deserialize from JSON
auto parsedModel = models::ApiResponse::fromJson(nlohmann::json::parse(jsonString));
```
#### models::Category

```cpp
// Create a model
auto model = models::Category();
model.setId(/* value */);  // Set id
model.setName(/* value */);  // Set name

// Serialize to JSON
nlohmann::json json = models::Category::toJson(model);
std::string jsonString = json.dump();

// Deserialize from JSON
auto parsedModel = models::Category::fromJson(nlohmann::json::parse(jsonString));
```
#### models::Order

```cpp
// Create a model
auto model = models::Order();
model.setId(/* value */);  // Set id
model.setPetId(/* value */);  // Set petId
model.setQuantity(/* value */);  // Set quantity
model.setShipDate(/* value */);  // Set shipDate
model.setStatus(/* value */);  // Set status
model.setComplete(/* value */);  // Set complete

// Serialize to JSON
nlohmann::json json = models::Order::toJson(model);
std::string jsonString = json.dump();

// Deserialize from JSON
auto parsedModel = models::Order::fromJson(nlohmann::json::parse(jsonString));
```
#### models::Pet

```cpp
// Create a model
auto model = models::Pet();
model.setId(/* value */);  // Set id
model.setCategory(/* value */);  // Set category
model.setName(/* value */);  // Set name
model.setPhotoUrls(/* value */);  // Set photoUrls
model.setTags(/* value */);  // Set tags
model.setStatus(/* value */);  // Set status

// Serialize to JSON
nlohmann::json json = models::Pet::toJson(model);
std::string jsonString = json.dump();

// Deserialize from JSON
auto parsedModel = models::Pet::fromJson(nlohmann::json::parse(jsonString));
```
#### models::Tag

```cpp
// Create a model
auto model = models::Tag();
model.setId(/* value */);  // Set id
model.setName(/* value */);  // Set name

// Serialize to JSON
nlohmann::json json = models::Tag::toJson(model);
std::string jsonString = json.dump();

// Deserialize from JSON
auto parsedModel = models::Tag::fromJson(nlohmann::json::parse(jsonString));
```
#### models::User

```cpp
// Create a model
auto model = models::User();
model.setId(/* value */);  // Set id
model.setUsername(/* value */);  // Set username
model.setFirstName(/* value */);  // Set firstName
model.setLastName(/* value */);  // Set lastName
model.setEmail(/* value */);  // Set email
model.setPassword(/* value */);  // Set password
model.setPhone(/* value */);  // Set phone
model.setUserStatus(/* value */);  // Set userStatus

// Serialize to JSON
nlohmann::json json = models::User::toJson(model);
std::string jsonString = json.dump();

// Deserialize from JSON
auto parsedModel = models::User::fromJson(nlohmann::json::parse(jsonString));
```

## Implementing API Handlers

### API Classes

#### Pet

To implement this API, create a class that inherits from the generated base class:

```cpp
class PetImpl : public api::Pet {
public:
    PetResponse handlePostForPet(const models::Pet& request) override {
        // Implement your logic here
        // Access request data with request.getters

        // For successful response:
        return models::Pet();

        // For error responses:
    }

    Pet{petId}Response handleDeleteForPet{petId}() override {
        // Implement your logic here
        // Access request parameters with req.get_param_value("param_name")

        // For successful response:
        return ();

        // For error responses:
    }

    PetFindByStatusResponse handleGetForPetFindByStatus() override {
        // Implement your logic here
        // Access request parameters with req.get_param_value("param_name")

        // For successful response:
        return models::Pet();

        // For error responses:
    }

    PetFindByTagsResponse handleGetForPetFindByTags() override {
        // Implement your logic here
        // Access request parameters with req.get_param_value("param_name")

        // For successful response:
        return models::Pet();

        // For error responses:
    }

    Pet{petId}Response handleGetForPet{petId}() override {
        // Implement your logic here
        // Access request parameters with req.get_param_value("param_name")

        // For successful response:
        return models::Pet();

        // For error responses:
    }

    PetResponse handlePutForPet(const models::Pet& request) override {
        // Implement your logic here
        // Access request data with request.getters

        // For successful response:
        return models::Pet();

        // For error responses:
    }

    Pet{petId}Response handlePostForPet{petId}() override {
        // Implement your logic here
        // Access request parameters with req.get_param_value("param_name")

        // For successful response:
        return ();

        // For error responses:
    }

    Pet{petId}UploadImageResponse handlePostForPet{petId}UploadImage() override {
        // Implement your logic here
        // Access request parameters with req.get_param_value("param_name")

        // For successful response:
        return models::ApiResponse();

        // For error responses:
    }

};
```
#### Store

To implement this API, create a class that inherits from the generated base class:

```cpp
class StoreImpl : public api::Store {
public:
    StoreOrder{orderId}Response handleDeleteForStoreOrder{orderId}() override {
        // Implement your logic here
        // Access request parameters with req.get_param_value("param_name")

        // For successful response:
        return ();

        // For error responses:
    }

    StoreInventoryResponse handleGetForStoreInventory() override {
        // Implement your logic here
        // Access request parameters with req.get_param_value("param_name")

        // For successful response:
        return int();

        // For error responses:
    }

    StoreOrder{orderId}Response handleGetForStoreOrder{orderId}() override {
        // Implement your logic here
        // Access request parameters with req.get_param_value("param_name")

        // For successful response:
        return models::Order();

        // For error responses:
    }

    StoreOrderResponse handlePostForStoreOrder(const models::Order& request) override {
        // Implement your logic here
        // Access request data with request.getters

        // For successful response:
        return models::Order();

        // For error responses:
    }

};
```
#### User

To implement this API, create a class that inherits from the generated base class:

```cpp
class UserImpl : public api::User {
public:
    UserResponse handlePostForUser(const models::User& request) override {
        // Implement your logic here
        // Access request data with request.getters

        // For successful response:
        return ();

        // For error responses:
    }

    UserCreateWithArrayResponse handlePostForUserCreateWithArray(const models::User& request) override {
        // Implement your logic here
        // Access request data with request.getters

        // For successful response:
        return ();

        // For error responses:
    }

    UserCreateWithListResponse handlePostForUserCreateWithList(const models::User& request) override {
        // Implement your logic here
        // Access request data with request.getters

        // For successful response:
        return ();

        // For error responses:
    }

    User{username}Response handleDeleteForUser{username}() override {
        // Implement your logic here
        // Access request parameters with req.get_param_value("param_name")

        // For successful response:
        return ();

        // For error responses:
    }

    User{username}Response handleGetForUser{username}() override {
        // Implement your logic here
        // Access request parameters with req.get_param_value("param_name")

        // For successful response:
        return models::User();

        // For error responses:
    }

    UserLoginResponse handleGetForUserLogin() override {
        // Implement your logic here
        // Access request parameters with req.get_param_value("param_name")

        // For successful response:
        return std::string();

        // For error responses:
    }

    UserLogoutResponse handleGetForUserLogout() override {
        // Implement your logic here
        // Access request parameters with req.get_param_value("param_name")

        // For successful response:
        return ();

        // For error responses:
    }

    User{username}Response handlePutForUser{username}(const models::User& request) override {
        // Implement your logic here
        // Access request data with request.getters

        // For successful response:
        return ();

        // For error responses:
    }

};
```

## Running the Server

```cpp
#include <httplib.h>
#include "api/YourApiImpl.h"

int main() {
    // Create server
    auto svr = std::make_unique<httplib::Server>();

    // Create API implementation
    auto <your_api>Impl = std::make_shared<YourApiImpl>();

    // Register routes
    <your_api>Impl->RegisterRoutes(std::move(svr));

    // Start server on port 8080
    svr->listen("localhost", 8080);

    return 0;
}
```

## Error Handling

Each API endpoint returns a variant type that can hold either a success response or one of several error responses.
The server automatically handles this variant and returns the appropriate HTTP status code.

## Working with Optional Fields

Optional parameters and model fields are represented using `std::optional`:

```cpp
if (model.getOptionalField()) {
    // Field is present
    auto value = *model.getOptionalField();
} else {
    // Field is not present
}
```

## Additional Resources

- [cpp-httplib Documentation](https://github.com/yhirose/cpp-httplib)
- [nlohmann/json Documentation](https://github.com/nlohmann/json)
- [OpenAPI Generator Documentation](https://openapi-generator.tech/docs/generators/)
```cpp
if (model.getOptionalField()) {
    // Field is present
    auto value = *model.getOptionalField();
} else {
    // Field is not present
}
```

## Additional Resources

- [cpp-httplib Documentation](https://github.com/yhirose/cpp-httplib)
- [nlohmann/json Documentation](https://github.com/nlohmann/json)
- [OpenAPI Generator Documentation](https://openapi-generator.tech/docs/generators/)
