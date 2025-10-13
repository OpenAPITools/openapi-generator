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
├── models/                  # Generated model classes
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
#### models::PetError

```cpp
// Create a model
auto model = models::PetError();
model.setCode(/* value */);  // Set code
model.setType(/* value */);  // Set type
model.setMessage(/* value */);  // Set message

// Serialize to JSON
nlohmann::json json = models::PetError::toJson(model);
std::string jsonString = json.dump();

// Deserialize from JSON
auto parsedModel = models::PetError::fromJson(nlohmann::json::parse(jsonString));
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
    // Handler for this endpoint
    PetResponse handlePostForPet(const PetRequestParams& params) override {
        // Access parameters from the params struct:
        // - Query parameters: params.m_<paramName>
        // - Header parameters: params.m_<paramName>
        // - Request body (if present): params.m_request
        // Example:
        //   int id = params.m_id; // query/header param
        //   std::string token = params.m_token; // header param
        //   if (params.m_request) { /* use request body */ }

        // Implement your logic here

    // For successful response:
    // Use the corresponding status code constant for the success type:
    //   return models::Pet(); // Will set status code 
    return models::Pet();

    // For error responses:
    // Use the corresponding status code constant for each error type:
    }
    // Handler for this endpoint
    void handleDeleteForPet{petId}(const Pet{petId}RequestParams& params) override {
        // Implement your logic here
    }
    // Handler for this endpoint
    PetFindByStatusResponse handleGetForPetFindByStatus(const PetFindByStatusRequestParams& params) override {
        // Access parameters from the params struct:
        // - Query parameters: params.m_<paramName>
        // - Header parameters: params.m_<paramName>
        // - Request body (if present): params.m_request
        // Example:
        //   int id = params.m_id; // query/header param
        //   std::string token = params.m_token; // header param
        //   if (params.m_request) { /* use request body */ }

        // Implement your logic here

    // For successful response:
    // Use the corresponding status code constant for the success type:
    //   return models::Pet(); // Will set status code 
    return models::Pet();

    // For error responses:
    // Use the corresponding status code constant for each error type:
    }
    // Handler for this endpoint
    PetFindByTagsResponse handleGetForPetFindByTags(const PetFindByTagsRequestParams& params) override {
        // Access parameters from the params struct:
        // - Query parameters: params.m_<paramName>
        // - Header parameters: params.m_<paramName>
        // - Request body (if present): params.m_request
        // Example:
        //   int id = params.m_id; // query/header param
        //   std::string token = params.m_token; // header param
        //   if (params.m_request) { /* use request body */ }

        // Implement your logic here

    // For successful response:
    // Use the corresponding status code constant for the success type:
    //   return models::Pet(); // Will set status code 
    return models::Pet();

    // For error responses:
    // Use the corresponding status code constant for each error type:
    // return models::PetError(); // Will set status code 
    }
    // Handler for this endpoint
    Pet{petId}Response handleGetForPet{petId}() override {
        // Access parameters from the params struct:
        // - Query parameters: params.m_<paramName>
        // - Header parameters: params.m_<paramName>
        // - Request body (if present): params.m_request
        // Example:
        //   int id = params.m_id; // query/header param
        //   std::string token = params.m_token; // header param
        //   if (params.m_request) { /* use request body */ }

        // Implement your logic here

    // For successful response:
    // Use the corresponding status code constant for the success type:
    //   return models::Pet(); // Will set status code 
    return models::Pet();

    // For error responses:
    // Use the corresponding status code constant for each error type:
    }
    // Handler for this endpoint
    PetResponse handlePutForPet(const PetRequestParams& params) override {
        // Access parameters from the params struct:
        // - Query parameters: params.m_<paramName>
        // - Header parameters: params.m_<paramName>
        // - Request body (if present): params.m_request
        // Example:
        //   int id = params.m_id; // query/header param
        //   std::string token = params.m_token; // header param
        //   if (params.m_request) { /* use request body */ }

        // Implement your logic here

    // For successful response:
    // Use the corresponding status code constant for the success type:
    //   return models::Pet(); // Will set status code 
    return models::Pet();

    // For error responses:
    // Use the corresponding status code constant for each error type:
    }
    // Handler for this endpoint
    void handlePostForPet{petId}() override {
        // Implement your logic here
    }
    // Handler for this endpoint
    Pet{petId}UploadImageResponse handlePostForPet{petId}UploadImage() override {
        // Access parameters from the params struct:
        // - Query parameters: params.m_<paramName>
        // - Header parameters: params.m_<paramName>
        // - Request body (if present): params.m_request
        // Example:
        //   int id = params.m_id; // query/header param
        //   std::string token = params.m_token; // header param
        //   if (params.m_request) { /* use request body */ }

        // Implement your logic here

    // For successful response:
    // Use the corresponding status code constant for the success type:
    //   return models::ApiResponse(); // Will set status code 
    return models::ApiResponse();

    // For error responses:
    // Use the corresponding status code constant for each error type:
    }
};
```
#### Store

To implement this API, create a class that inherits from the generated base class:

```cpp
class StoreImpl : public api::Store {
public:
    // Handler for this endpoint
    void handleDeleteForStoreOrder{orderId}() override {
        // Implement your logic here
    }
    // Handler for this endpoint
    StoreInventoryResponse handleGetForStoreInventory() override {
        // Access parameters from the params struct:
        // - Query parameters: params.m_<paramName>
        // - Header parameters: params.m_<paramName>
        // - Request body (if present): params.m_request
        // Example:
        //   int id = params.m_id; // query/header param
        //   std::string token = params.m_token; // header param
        //   if (params.m_request) { /* use request body */ }

        // Implement your logic here

    // For successful response:
    // Use the corresponding status code constant for the success type:
    //   return int(); // Will set status code 
    return int();

    // For error responses:
    // Use the corresponding status code constant for each error type:
    }
    // Handler for this endpoint
    StoreOrder{orderId}Response handleGetForStoreOrder{orderId}() override {
        // Access parameters from the params struct:
        // - Query parameters: params.m_<paramName>
        // - Header parameters: params.m_<paramName>
        // - Request body (if present): params.m_request
        // Example:
        //   int id = params.m_id; // query/header param
        //   std::string token = params.m_token; // header param
        //   if (params.m_request) { /* use request body */ }

        // Implement your logic here

    // For successful response:
    // Use the corresponding status code constant for the success type:
    //   return models::Order(); // Will set status code 
    return models::Order();

    // For error responses:
    // Use the corresponding status code constant for each error type:
    }
    // Handler for this endpoint
    StoreOrderResponse handlePostForStoreOrder(const StoreOrderRequestParams& params) override {
        // Access parameters from the params struct:
        // - Query parameters: params.m_<paramName>
        // - Header parameters: params.m_<paramName>
        // - Request body (if present): params.m_request
        // Example:
        //   int id = params.m_id; // query/header param
        //   std::string token = params.m_token; // header param
        //   if (params.m_request) { /* use request body */ }

        // Implement your logic here

    // For successful response:
    // Use the corresponding status code constant for the success type:
    //   return models::Order(); // Will set status code 
    return models::Order();

    // For error responses:
    // Use the corresponding status code constant for each error type:
    }
};
```
#### User

To implement this API, create a class that inherits from the generated base class:

```cpp
class UserImpl : public api::User {
public:
    // Handler for this endpoint
    void handlePostForUser(const UserRequestParams& params) override {
        // Implement your logic here
    }
    // Handler for this endpoint
    void handlePostForUserCreateWithArray(const UserCreateWithArrayRequestParams& params) override {
        // Implement your logic here
    }
    // Handler for this endpoint
    void handlePostForUserCreateWithList(const UserCreateWithListRequestParams& params) override {
        // Implement your logic here
    }
    // Handler for this endpoint
    void handleDeleteForUser{username}() override {
        // Implement your logic here
    }
    // Handler for this endpoint
    User{username}Response handleGetForUser{username}() override {
        // Access parameters from the params struct:
        // - Query parameters: params.m_<paramName>
        // - Header parameters: params.m_<paramName>
        // - Request body (if present): params.m_request
        // Example:
        //   int id = params.m_id; // query/header param
        //   std::string token = params.m_token; // header param
        //   if (params.m_request) { /* use request body */ }

        // Implement your logic here

    // For successful response:
    // Use the corresponding status code constant for the success type:
    //   return models::User(); // Will set status code 
    return models::User();

    // For error responses:
    // Use the corresponding status code constant for each error type:
    }
    // Handler for this endpoint
    UserLoginResponse handleGetForUserLogin(const UserLoginRequestParams& params) override {
        // Access parameters from the params struct:
        // - Query parameters: params.m_<paramName>
        // - Header parameters: params.m_<paramName>
        // - Request body (if present): params.m_request
        // Example:
        //   int id = params.m_id; // query/header param
        //   std::string token = params.m_token; // header param
        //   if (params.m_request) { /* use request body */ }

        // Implement your logic here

    // For successful response:
    // Use the corresponding status code constant for the success type:
    //   return std::string(); // Will set status code 
    return std::string();

    // For error responses:
    // Use the corresponding status code constant for each error type:
    }
    // Handler for this endpoint
    void handleGetForUserLogout() override {
        // Implement your logic here
    }
    // Handler for this endpoint
    void handlePutForUser{username}(const User{username}RequestParams& params) override {
        // Implement your logic here
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


Each API endpoint returns a `std::variant` type that can hold either a success response or one of several error responses.
The server automatically handles this variant and returns the appropriate HTTP status code and JSON body using the generated helper functions.

For each response type (success or error), a corresponding status code constant (e.g., `MYRESPONSE_200`, `MYERROR_400`) is generated and used by the server when returning that type. You do not need to set the status code manually; just return the appropriate type from your handler.

Error handling for invalid parameters or JSON is performed automatically in the generated route registration code. You can customize error responses by returning the appropriate error type from your handler.

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
