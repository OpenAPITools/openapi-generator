# cpp-httplib-server-feature-test - C++ Server

## Overview

This server was generated using the [OpenAPI Generator](https://openapi-generator.tech) project.
It uses the [cpp-httplib](https://github.com/yhirose/cpp-httplib) library to implement a lightweight HTTP server
with JSON request/response handling via [nlohmann/json](https://github.com/nlohmann/json).

## Requirements

- C++17 compatible compiler
- CMake (3.14 or higher)
- OpenSSL (for HTTPS support)
- ZLIB (for compression support)

**Note:** The following libraries are automatically downloaded via CMake FetchContent:
- [cpp-httplib](https://github.com/yhirose/cpp-httplib) v0.15.3
- [nlohmann/json](https://github.com/nlohmann/json) v3.11.3

### Platform-Specific Installation

**Linux (Ubuntu/Debian):**
```bash
sudo apt-get update
sudo apt-get install -y libssl-dev zlib1g-dev cmake build-essential
```

**macOS:**
```bash
brew install openssl zlib cmake
```

**Windows:**
```powershell
# Using vcpkg
vcpkg install openssl:x64-windows zlib:x64-windows

# Then configure CMake with vcpkg toolchain:
cmake -B build -DCMAKE_TOOLCHAIN_FILE=[vcpkg_root]/scripts/buildsystems/vcpkg.cmake
```

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

#### models::Address

```cpp
// Create a model
auto model = models::Address();
model.setStreet(/* value */);  // Set street
model.setCity(/* value */);  // Set city
model.setZipCode(/* value */);  // Set zipCode
model.setCountry(/* value */);  // Set country

// Serialize to JSON
nlohmann::json json = models::Address::toJson(model);
std::string jsonString = json.dump();

// Deserialize from JSON
auto parsedModel = models::Address::fromJson(nlohmann::json::parse(jsonString));
```
#### models::Animal

```cpp
// Create a model
auto model = models::Animal();
model.setName(/* value */);  // Set name
model.setType(/* value */);  // Set type

// Serialize to JSON
nlohmann::json json = models::Animal::toJson(model);
std::string jsonString = json.dump();

// Deserialize from JSON
auto parsedModel = models::Animal::fromJson(nlohmann::json::parse(jsonString));
```
#### models::ArrayTypes

```cpp
// Create a model
auto model = models::ArrayTypes();
model.setStringArray(/* value */);  // Set stringArray
model.setIntArray(/* value */);  // Set intArray
model.setObjectArray(/* value */);  // Set objectArray
model.setEnumArray(/* value */);  // Set enumArray
model.setNestedArray(/* value */);  // Set nestedArray

// Serialize to JSON
nlohmann::json json = models::ArrayTypes::toJson(model);
std::string jsonString = json.dump();

// Deserialize from JSON
auto parsedModel = models::ArrayTypes::fromJson(nlohmann::json::parse(jsonString));
```
#### models::BankAccount

```cpp
// Create a model
auto model = models::BankAccount();
model.setPaymentType(/* value */);  // Set paymentType
model.setAccountNumber(/* value */);  // Set accountNumber
model.setBankName(/* value */);  // Set bankName

// Serialize to JSON
nlohmann::json json = models::BankAccount::toJson(model);
std::string jsonString = json.dump();

// Deserialize from JSON
auto parsedModel = models::BankAccount::fromJson(nlohmann::json::parse(jsonString));
```
#### models::Company

```cpp
// Create a model
auto model = models::Company();
model.setName(/* value */);  // Set name
model.setHeadquarters(/* value */);  // Set headquarters
model.setDepartments(/* value */);  // Set departments
model.setMetadata(/* value */);  // Set metadata

// Serialize to JSON
nlohmann::json json = models::Company::toJson(model);
std::string jsonString = json.dump();

// Deserialize from JSON
auto parsedModel = models::Company::fromJson(nlohmann::json::parse(jsonString));
```
#### models::CreatedResponse

```cpp
// Create a model
auto model = models::CreatedResponse();
model.setStatus(/* value */);  // Set status
model.setId(/* value */);  // Set id
model.setLocation(/* value */);  // Set location

// Serialize to JSON
nlohmann::json json = models::CreatedResponse::toJson(model);
std::string jsonString = json.dump();

// Deserialize from JSON
auto parsedModel = models::CreatedResponse::fromJson(nlohmann::json::parse(jsonString));
```
#### models::CreditCard

```cpp
// Create a model
auto model = models::CreditCard();
model.setPaymentType(/* value */);  // Set paymentType
model.setCardNumber(/* value */);  // Set cardNumber
model.setCardType(/* value */);  // Set cardType

// Serialize to JSON
nlohmann::json json = models::CreditCard::toJson(model);
std::string jsonString = json.dump();

// Deserialize from JSON
auto parsedModel = models::CreditCard::fromJson(nlohmann::json::parse(jsonString));
```
#### models::Department

```cpp
// Create a model
auto model = models::Department();
model.setName(/* value */);  // Set name
model.setManager(/* value */);  // Set manager
model.setEmployees(/* value */);  // Set employees

// Serialize to JSON
nlohmann::json json = models::Department::toJson(model);
std::string jsonString = json.dump();

// Deserialize from JSON
auto parsedModel = models::Department::fromJson(nlohmann::json::parse(jsonString));
```
#### models::Dog

```cpp
// Create a model
auto model = models::Dog();
model.setBreed(/* value */);  // Set breed
model.setBarkVolume(/* value */);  // Set barkVolume

// Serialize to JSON
nlohmann::json json = models::Dog::toJson(model);
std::string jsonString = json.dump();

// Deserialize from JSON
auto parsedModel = models::Dog::fromJson(nlohmann::json::parse(jsonString));
```
#### models::Employee

```cpp
// Create a model
auto model = models::Employee();
model.setId(/* value */);  // Set id
model.setName(/* value */);  // Set name
model.setEmail(/* value */);  // Set email
model.setAddress(/* value */);  // Set address
model.setSkills(/* value */);  // Set skills

// Serialize to JSON
nlohmann::json json = models::Employee::toJson(model);
std::string jsonString = json.dump();

// Deserialize from JSON
auto parsedModel = models::Employee::fromJson(nlohmann::json::parse(jsonString));
```
#### models::EnumTypes

```cpp
// Create a model
auto model = models::EnumTypes();
model.setStringEnum(/* value */);  // Set stringEnum
model.setNumericEnum(/* value */);  // Set numericEnum
model.setStatusCode(/* value */);  // Set statusCode

// Serialize to JSON
nlohmann::json json = models::EnumTypes::toJson(model);
std::string jsonString = json.dump();

// Deserialize from JSON
auto parsedModel = models::EnumTypes::fromJson(nlohmann::json::parse(jsonString));
```
#### models::ErrorResponse

```cpp
// Create a model
auto model = models::ErrorResponse();
model.setMessage(/* value */);  // Set message
model.setCode(/* value */);  // Set code
model.setDetails(/* value */);  // Set details

// Serialize to JSON
nlohmann::json json = models::ErrorResponse::toJson(model);
std::string jsonString = json.dump();

// Deserialize from JSON
auto parsedModel = models::ErrorResponse::fromJson(nlohmann::json::parse(jsonString));
```
#### models::NotFoundResponse

```cpp
// Create a model
auto model = models::NotFoundResponse();
model.setError(/* value */);  // Set error
model.setResource(/* value */);  // Set resource

// Serialize to JSON
nlohmann::json json = models::NotFoundResponse::toJson(model);
std::string jsonString = json.dump();

// Deserialize from JSON
auto parsedModel = models::NotFoundResponse::fromJson(nlohmann::json::parse(jsonString));
```
#### models::NullableOptionalTypes

```cpp
// Create a model
auto model = models::NullableOptionalTypes();
model.setRequiredField(/* value */);  // Set requiredField
model.setOptionalField(/* value */);  // Set optionalField
model.setNullableField(/* value */);  // Set nullableField
model.setOptionalNullableField(/* value */);  // Set optionalNullableField
model.setFieldWithDefault(/* value */);  // Set fieldWithDefault

// Serialize to JSON
nlohmann::json json = models::NullableOptionalTypes::toJson(model);
std::string jsonString = json.dump();

// Deserialize from JSON
auto parsedModel = models::NullableOptionalTypes::fromJson(nlohmann::json::parse(jsonString));
```
#### models::PaymentMethod

```cpp
// Create a model
auto model = models::PaymentMethod();
model.setPaymentType(/* value */);  // Set paymentType
model.setCardNumber(/* value */);  // Set cardNumber
model.setCardType(/* value */);  // Set cardType
model.setAccountNumber(/* value */);  // Set accountNumber
model.setBankName(/* value */);  // Set bankName

// Serialize to JSON
nlohmann::json json = models::PaymentMethod::toJson(model);
std::string jsonString = json.dump();

// Deserialize from JSON
auto parsedModel = models::PaymentMethod::fromJson(nlohmann::json::parse(jsonString));
```
#### models::PrimitiveTypes

```cpp
// Create a model
auto model = models::PrimitiveTypes();
model.setStringField(/* value */);  // Set stringField
model.setIntField(/* value */);  // Set intField
model.setLongField(/* value */);  // Set longField
model.setFloatField(/* value */);  // Set floatField
model.setDoubleField(/* value */);  // Set doubleField
model.setBoolField(/* value */);  // Set boolField
model.setByteField(/* value */);  // Set byteField
model.setBinaryField(/* value */);  // Set binaryField
model.setDateField(/* value */);  // Set dateField
model.setDateTimeField(/* value */);  // Set dateTimeField
model.setPasswordField(/* value */);  // Set passwordField

// Serialize to JSON
nlohmann::json json = models::PrimitiveTypes::toJson(model);
std::string jsonString = json.dump();

// Deserialize from JSON
auto parsedModel = models::PrimitiveTypes::fromJson(nlohmann::json::parse(jsonString));
```
#### models::SimpleObject

```cpp
// Create a model
auto model = models::SimpleObject();
model.setId(/* value */);  // Set id
model.setName(/* value */);  // Set name
model.setDescription(/* value */);  // Set description

// Serialize to JSON
nlohmann::json json = models::SimpleObject::toJson(model);
std::string jsonString = json.dump();

// Deserialize from JSON
auto parsedModel = models::SimpleObject::fromJson(nlohmann::json::parse(jsonString));
```
#### models::StringOrNumber

```cpp
// Create a model
auto model = models::StringOrNumber();

// Serialize to JSON
nlohmann::json json = models::StringOrNumber::toJson(model);
std::string jsonString = json.dump();

// Deserialize from JSON
auto parsedModel = models::StringOrNumber::fromJson(nlohmann::json::parse(jsonString));
```
#### models::SuccessResponse

```cpp
// Create a model
auto model = models::SuccessResponse();
model.setStatus(/* value */);  // Set status
model.setData(/* value */);  // Set data

// Serialize to JSON
nlohmann::json json = models::SuccessResponse::toJson(model);
std::string jsonString = json.dump();

// Deserialize from JSON
auto parsedModel = models::SuccessResponse::fromJson(nlohmann::json::parse(jsonString));
```
#### models::TestAllParameterTypes200Response

```cpp
// Create a model
auto model = models::TestAllParameterTypes200Response();
model.setMessage(/* value */);  // Set message
model.setResourceId(/* value */);  // Set resourceId
model.setFilter(/* value */);  // Set filter
model.setCorrelationId(/* value */);  // Set correlationId

// Serialize to JSON
nlohmann::json json = models::TestAllParameterTypes200Response::toJson(model);
std::string jsonString = json.dump();

// Deserialize from JSON
auto parsedModel = models::TestAllParameterTypes200Response::fromJson(nlohmann::json::parse(jsonString));
```
#### models::TestApiKeySecurity200Response

```cpp
// Create a model
auto model = models::TestApiKeySecurity200Response();
model.setMessage(/* value */);  // Set message

// Serialize to JSON
nlohmann::json json = models::TestApiKeySecurity200Response::toJson(model);
std::string jsonString = json.dump();

// Deserialize from JSON
auto parsedModel = models::TestApiKeySecurity200Response::fromJson(nlohmann::json::parse(jsonString));
```
#### models::TestBasicSecurity200Response

```cpp
// Create a model
auto model = models::TestBasicSecurity200Response();
model.setMessage(/* value */);  // Set message

// Serialize to JSON
nlohmann::json json = models::TestBasicSecurity200Response::toJson(model);
std::string jsonString = json.dump();

// Deserialize from JSON
auto parsedModel = models::TestBasicSecurity200Response::fromJson(nlohmann::json::parse(jsonString));
```
#### models::TestBearerSecurity200Response

```cpp
// Create a model
auto model = models::TestBearerSecurity200Response();
model.setMessage(/* value */);  // Set message

// Serialize to JSON
nlohmann::json json = models::TestBearerSecurity200Response::toJson(model);
std::string jsonString = json.dump();

// Deserialize from JSON
auto parsedModel = models::TestBearerSecurity200Response::fromJson(nlohmann::json::parse(jsonString));
```
#### models::TestCookieParameters200Response

```cpp
// Create a model
auto model = models::TestCookieParameters200Response();
model.setMessage(/* value */);  // Set message

// Serialize to JSON
nlohmann::json json = models::TestCookieParameters200Response::toJson(model);
std::string jsonString = json.dump();

// Deserialize from JSON
auto parsedModel = models::TestCookieParameters200Response::fromJson(nlohmann::json::parse(jsonString));
```
#### models::TestHeaderParameters200Response

```cpp
// Create a model
auto model = models::TestHeaderParameters200Response();
model.setMessage(/* value */);  // Set message

// Serialize to JSON
nlohmann::json json = models::TestHeaderParameters200Response::toJson(model);
std::string jsonString = json.dump();

// Deserialize from JSON
auto parsedModel = models::TestHeaderParameters200Response::fromJson(nlohmann::json::parse(jsonString));
```
#### models::TestHeaderParameters401Response

```cpp
// Create a model
auto model = models::TestHeaderParameters401Response();
model.setMessage(/* value */);  // Set message

// Serialize to JSON
nlohmann::json json = models::TestHeaderParameters401Response::toJson(model);
std::string jsonString = json.dump();

// Deserialize from JSON
auto parsedModel = models::TestHeaderParameters401Response::fromJson(nlohmann::json::parse(jsonString));
```
#### models::TestOAuth2Security200Response

```cpp
// Create a model
auto model = models::TestOAuth2Security200Response();
model.setMessage(/* value */);  // Set message

// Serialize to JSON
nlohmann::json json = models::TestOAuth2Security200Response::toJson(model);
std::string jsonString = json.dump();

// Deserialize from JSON
auto parsedModel = models::TestOAuth2Security200Response::fromJson(nlohmann::json::parse(jsonString));
```
#### models::TestQueryParameters200Response

```cpp
// Create a model
auto model = models::TestQueryParameters200Response();
model.setMessage(/* value */);  // Set message

// Serialize to JSON
nlohmann::json json = models::TestQueryParameters200Response::toJson(model);
std::string jsonString = json.dump();

// Deserialize from JSON
auto parsedModel = models::TestQueryParameters200Response::fromJson(nlohmann::json::parse(jsonString));
```
#### models::TestQueryParametersDeepObjectParameter

```cpp
// Create a model
auto model = models::TestQueryParametersDeepObjectParameter();
model.setName(/* value */);  // Set name
model.setAge(/* value */);  // Set age

// Serialize to JSON
nlohmann::json json = models::TestQueryParametersDeepObjectParameter::toJson(model);
std::string jsonString = json.dump();

// Deserialize from JSON
auto parsedModel = models::TestQueryParametersDeepObjectParameter::fromJson(nlohmann::json::parse(jsonString));
```

## Implementing API Handlers

### API Classes

Each API is generated as an abstract base class with pure virtual methods that you must implement.

#### Composition

Create a class that inherits from the generated base class:

```cpp
#include "api/CompositionApi.h"

class CompositionImpl : public api::Composition {
public:
    CompositionallofPostResponse handlePostForCompositionallof(const CompositionallofPostRequest& params) override {
        // Access request parameters:
        // Body: params.m_request (std::optional<models::Dog>)

        // Implement your business logic here

        // Return success response (HTTP HTTP_RESPONSE_CODE_DOG):
        models::Dog successResponse;
        // ... populate response ...
        return successResponse;
    }

    CompositionanyofPostResponse handlePostForCompositionanyof(const CompositionanyofPostRequest& params) override {
        // Access request parameters:
        // Body: params.m_request (std::optional<models::StringOrNumber>)

        // Implement your business logic here

        // Return success response (HTTP HTTP_RESPONSE_CODE_STRING_OR_NUMBER):
        models::StringOrNumber successResponse;
        // ... populate response ...
        return successResponse;
    }

    CompositiononeofPostResponse handlePostForCompositiononeof(const CompositiononeofPostRequest& params) override {
        // Access request parameters:
        // Body: params.m_request (std::optional<models::PaymentMethod>)

        // Implement your business logic here

        // Return success response (HTTP HTTP_RESPONSE_CODE_PAYMENT_METHOD):
        models::PaymentMethod successResponse;
        // ... populate response ...
        return successResponse;
    }

};
```
#### Datatypes

Create a class that inherits from the generated base class:

```cpp
#include "api/DatatypesApi.h"

class DatatypesImpl : public api::Datatypes {
public:
    DatatypesarraysPostResponse handlePostForDatatypesarrays(const DatatypesarraysPostRequest& params) override {
        // Access request parameters:
        // Body: params.m_request (std::optional<models::ArrayTypes>)

        // Implement your business logic here

        // Return success response (HTTP HTTP_RESPONSE_CODE_ARRAY_TYPES):
        models::ArrayTypes successResponse;
        // ... populate response ...
        return successResponse;
    }

    DatatypesenumsPostResponse handlePostForDatatypesenums(const DatatypesenumsPostRequest& params) override {
        // Access request parameters:
        // Body: params.m_request (std::optional<models::EnumTypes>)

        // Implement your business logic here

        // Return success response (HTTP HTTP_RESPONSE_CODE_ENUM_TYPES):
        models::EnumTypes successResponse;
        // ... populate response ...
        return successResponse;
    }

    ResponsesmultipleGetResponse handleGetForResponsesmultiple(const ResponsesmultipleGetRequest& params) override {
        // Access request parameters:
        // Query: params.m_scenario

        // Implement your business logic here

        // Return success response (HTTP HTTP_RESPONSE_CODE_SUCCESS_RESPONSE):
        models::SuccessResponse successResponse;
        // ... populate response ...
        return successResponse;
        // Return success response (HTTP HTTP_RESPONSE_CODE_CREATED_RESPONSE):
        models::CreatedResponse successResponse;
        // ... populate response ...
        return successResponse;

        // Or return error response (HTTP HTTP_RESPONSE_CODE_NOT_FOUND_RESPONSE):
        // models::NotFoundResponse errorResponse;
        // return errorResponse;

        // Or return error response (HTTP HTTP_RESPONSE_CODE_ERROR_RESPONSE):
        // models::ErrorResponse errorResponse;
        // return errorResponse;
    }

    NestedobjectsPostResponse handlePostForNestedobjects(const NestedobjectsPostRequest& params) override {
        // Access request parameters:
        // Body: params.m_request (std::optional<models::Company>)

        // Implement your business logic here

        // Return success response (HTTP HTTP_RESPONSE_CODE_COMPANY):
        models::Company successResponse;
        // ... populate response ...
        return successResponse;
    }

    void handleDeleteForResponsesnocontent(const ResponsesnocontentDeleteRequest& params) override {
        // Access request parameters from params struct
        // Implement your logic here
    }

    NullableoptionalPostResponse handlePostForNullableoptional(const NullableoptionalPostRequest& params) override {
        // Access request parameters:
        // Body: params.m_request (std::optional<models::NullableOptionalTypes>)

        // Implement your business logic here

        // Return success response (HTTP HTTP_RESPONSE_CODE_NULLABLE_OPTIONAL_TYPES):
        models::NullableOptionalTypes successResponse;
        // ... populate response ...
        return successResponse;
    }

    DatatypesprimitivesPostResponse handlePostForDatatypesprimitives(const DatatypesprimitivesPostRequest& params) override {
        // Access request parameters:
        // Body: params.m_request (std::optional<models::PrimitiveTypes>)

        // Implement your business logic here

        // Return success response (HTTP HTTP_RESPONSE_CODE_PRIMITIVE_TYPES):
        models::PrimitiveTypes successResponse;
        // ... populate response ...
        return successResponse;

        // Or return error response (HTTP HTTP_RESPONSE_CODE_ERROR_RESPONSE):
        // models::ErrorResponse errorResponse;
        // return errorResponse;
    }

};
```
#### Parameters

Create a class that inherits from the generated base class:

```cpp
#include "api/ParametersApi.h"

class ParametersImpl : public api::Parameters {
public:
    ParameterscombinedresourceIdPostResponse handlePostForParameterscombinedresourceId(const ParameterscombinedresourceIdPostRequest& params) override {
        // Access request parameters:
        // Path: params.m_resourceId
        // Query: params.m_filter
        // Query: params.m_limit (optional)
        // Header: params.m_xCorrelationId
        // Header: params.m_xClientVersion (optional)
        // Body: params.m_request (std::optional<models::SimpleObject>)

        // Implement your business logic here

        // Return success response (HTTP HTTP_RESPONSE_CODE_TEST_ALL_PARAMETER_TYPES200_RESPONSE):
        models::TestAllParameterTypes200Response successResponse;
        // ... populate response ...
        return successResponse;

        // Or return error response (HTTP HTTP_RESPONSE_CODE_ERROR_RESPONSE):
        // models::ErrorResponse errorResponse;
        // return errorResponse;
    }

    ParameterscookiesGetResponse handleGetForParameterscookies(const ParameterscookiesGetRequest& params) override {
        // Access request parameters:

        // Implement your business logic here

        // Return success response (HTTP HTTP_RESPONSE_CODE_TEST_COOKIE_PARAMETERS200_RESPONSE):
        models::TestCookieParameters200Response successResponse;
        // ... populate response ...
        return successResponse;
    }

    ParametersheadersGetResponse handleGetForParametersheaders(const ParametersheadersGetRequest& params) override {
        // Access request parameters:
        // Header: params.m_xApiVersion
        // Header: params.m_xRequestId (optional)
        // Header: params.m_xRateLimit (optional)
        // Header: params.m_xTags (optional)

        // Implement your business logic here

        // Return success response (HTTP HTTP_RESPONSE_CODE_TEST_HEADER_PARAMETERS200_RESPONSE):
        models::TestHeaderParameters200Response successResponse;
        // ... populate response ...
        return successResponse;

        // Or return error response (HTTP HTTP_RESPONSE_CODE_TEST_HEADER_PARAMETERS401_RESPONSE):
        // models::TestHeaderParameters401Response errorResponse;
        // return errorResponse;
    }

    ParametersquerypathIdGetResponse handleGetForParametersquerypathId(const ParametersquerypathIdGetRequest& params) override {
        // Access request parameters:
        // Path: params.m_pathId
        // Query: params.m_stringParam
        // Query: params.m_intParam (optional)
        // Query: params.m_boolParam (optional)
        // Query: params.m_arrayParam (optional)
        // Query: params.m_spaceDelimited (optional)
        // Query: params.m_pipeDelimited (optional)
        // Query: params.m_deepObject (optional)

        // Implement your business logic here

        // Return success response (HTTP HTTP_RESPONSE_CODE_TEST_QUERY_PARAMETERS200_RESPONSE):
        models::TestQueryParameters200Response successResponse;
        // ... populate response ...
        return successResponse;
    }

};
```
#### Security

Create a class that inherits from the generated base class:

```cpp
#include "api/SecurityApi.h"

class SecurityImpl : public api::Security {
public:
    SecurityapikeyGetResponse handleGetForSecurityapikey() override {

        // Implement your business logic here

        // Return success response (HTTP HTTP_RESPONSE_CODE_TEST_API_KEY_SECURITY200_RESPONSE):
        models::TestApiKeySecurity200Response successResponse;
        // ... populate response ...
        return successResponse;

        // Or return error response (HTTP HTTP_RESPONSE_CODE_ERROR_RESPONSE):
        // models::ErrorResponse errorResponse;
        // return errorResponse;
    }

    SecuritybasicGetResponse handleGetForSecuritybasic() override {

        // Implement your business logic here

        // Return success response (HTTP HTTP_RESPONSE_CODE_TEST_BASIC_SECURITY200_RESPONSE):
        models::TestBasicSecurity200Response successResponse;
        // ... populate response ...
        return successResponse;

        // Or return error response (HTTP HTTP_RESPONSE_CODE_ERROR_RESPONSE):
        // models::ErrorResponse errorResponse;
        // return errorResponse;
    }

    SecuritybearerGetResponse handleGetForSecuritybearer() override {

        // Implement your business logic here

        // Return success response (HTTP HTTP_RESPONSE_CODE_TEST_BEARER_SECURITY200_RESPONSE):
        models::TestBearerSecurity200Response successResponse;
        // ... populate response ...
        return successResponse;

        // Or return error response (HTTP HTTP_RESPONSE_CODE_ERROR_RESPONSE):
        // models::ErrorResponse errorResponse;
        // return errorResponse;
    }

    Securityoauth2getResponse handleGetForSecurityoauth2() override {

        // Implement your business logic here

        // Return success response (HTTP HTTP_RESPONSE_CODE_TEST_O_AUTH2_SECURITY200_RESPONSE):
        models::TestOAuth2Security200Response successResponse;
        // ... populate response ...
        return successResponse;

        // Or return error response (HTTP HTTP_RESPONSE_CODE_ERROR_RESPONSE):
        // models::ErrorResponse errorResponse;
        // return errorResponse;
    }

};
```

## Running the Server

Here's a complete example of setting up and running the server:

```cpp
#include <httplib.h>
#include <memory>

#include "api/CompositionApi.h"

#include "api/DatatypesApi.h"

#include "api/ParametersApi.h"

#include "api/SecurityApi.h"

#include "api/AuthenticationManager.h"

int main() {
    httplib::Server server;

    // Create authentication manager (required for this API)
    auto authMgr = std::make_shared<MyAuthManager>();

    // Create API implementations

    CompositionImpl composition;

    DatatypesImpl datatypes;

    ParametersImpl parameters;

    SecurityImpl security;


    // Register routes

    composition.registerRoutes(server, authMgr);

    datatypes.registerRoutes(server, authMgr);

    parameters.registerRoutes(server, authMgr);

    security.registerRoutes(server, authMgr);


    // Start server
    std::cout << "Server starting on http://localhost:8080" << std::endl;
    server.listen("localhost", 8080);

    return 0;
}
```

### With Authentication

When authentication is required, you must:
1. Implement the `AuthenticationManager` interface (see Authentication section below)
2. Pass the authentication manager to `registerRoutes()`


## Authentication

This API requires authentication. Implement the `AuthenticationManager` interface to provide your authentication logic:

```cpp
#include "api/AuthenticationManager.h"

class MyAuthManager : public api::AuthenticationManager {
public:
    bool validateApiKey(const std::string& key) override {
        // Validate API key from header, query, or cookie
        // Example: check against database or cache
        return checkApiKeyInDatabase(key);
    }

    bool validateBearerToken(const std::string& token) override {
        // Validate JWT or other bearer tokens
        // Example: verify signature and expiration
        return jwt::verify(token, secret_key);
    }

    bool validateBasicAuth(const std::string& username, const std::string& password) override {
        // Validate username/password credentials
        // Example: check against user database with hashed passwords
        auto user = findUser(username);
        return user && bcrypt::verify(password, user->passwordHash);
    }

    bool validateOAuth2(const std::string& token, const std::vector<std::string>& scopes) override {
        // Validate OAuth2 token and check required scopes
        // Example: introspect token and verify scopes
        auto introspection = oauthProvider.introspect(token);
        return introspection.active && hasAllScopes(introspection.scopes, scopes);
    }
};
```

### Authentication Flow

1. The server automatically extracts credentials from requests (headers, query params, cookies)
2. Before calling your handler, it validates credentials using your `AuthenticationManager`
3. If validation fails, the server returns HTTP 401 Unauthorized automatically
4. If validation succeeds, your handler is called

### Security Schemes

The generated code supports:
- **API Key**: Header, query parameter, or cookie-based authentication
- **Bearer Token**: Authorization header with "Bearer" scheme (e.g., JWT)
- **Basic Auth**: HTTP Basic authentication (username:password)
- **OAuth2**: OAuth 2.0 token-based authentication with scope validation


## Error Handling

### Response Variants

Each API endpoint that returns data uses `std::variant` to represent multiple possible response types (success and errors):

```cpp
// Example: endpoint returns success (User) or errors (NotFound, ServerError)
using GetUserResponse = std::variant<User, NotFound, ServerError>;

GetUserResponse handleGetUser(const GetUserRequest& params) override {
    if (userExists(params.m_userId)) {
        User user = fetchUser(params.m_userId);
        return user;  // Automatically sets HTTP 200
    } else {
        NotFound error;
        error.setMessage("User not found");
        return error;  // Automatically sets HTTP 404
    }
}
```

The server automatically:
- Detects which type is returned from the variant
- Sets the appropriate HTTP status code
- Serializes the response to JSON

### HTTP Status Codes

Status codes are automatically set based on the response type you return. Each model type is associated with a specific HTTP status code defined in your OpenAPI specification.

**Optimized Status Code Constants:**
The generator only creates HTTP status code constants (e.g., `HTTP_RESPONSE_CODE_200`, `HTTP_RESPONSE_CODE_404`) for codes actually used by your API operations. This reduces code bloat and compilation time compared to generating all possible HTTP status codes.

### Parameter Validation

The generated code automatically validates:
- **Required parameters**: Returns HTTP 400 if missing
- **Type conversion**: Returns HTTP 400 if parameter cannot be converted to expected type
- **JSON parsing**: Returns HTTP 400 if request body is invalid JSON

Custom validation logic should be implemented in your handler methods.

### Working with Optional Parameters

Optional parameters and model fields use `std::optional`:

```cpp
void handleRequest(const RequestParams& params) override {
    // Check if optional query parameter is present
    if (params.m_optionalParam) {
        auto value = *params.m_optionalParam;  // Dereference to get value
        // Use value...
    }

    // Check if optional request body is present
    if (params.m_request) {
        auto body = *params.m_request;  // Dereference to get body
        // Use body...
    }
}
```

## Advanced Features

### Parameter Serialization Styles

The generator supports various parameter serialization styles as defined in OpenAPI:

- **simple**: Comma-separated values (default for path/header)
- **form**: Ampersand-separated values (default for query)
- **spaceDelimited**: Space-separated values
- **pipeDelimited**: Pipe-separated values
- **deepObject**: Nested object notation for query parameters

These are automatically handled during parameter parsing.

### Enum Handling

All generated enums automatically include an `UNSPECIFIED` value as the first enum entry for safe initialization:

```cpp
enum class Status {
    UNSPECIFIED = 0,  // Added automatically for safety
    PENDING,
    APPROVED,
    REJECTED
};

// Safe default initialization
Status status;  // Defaults to UNSPECIFIED (0)

// Explicit initialization
Status activeStatus = Status::APPROVED;

// Enum serialization/deserialization
// UNSPECIFIED is not a valid API value and indicates uninitialized state
```

**Why UNSPECIFIED?**
- Provides a safe default value for uninitialized enums
- Prevents undefined behavior from using uninitialized enum values
- Makes it clear when an enum hasn't been set vs. having a valid API value
- Does not appear in OpenAPI spec - internal C++ implementation detail

### Union Types (anyOf/oneOf)

When your OpenAPI spec uses `anyOf` or `oneOf`, the generated code uses `std::variant`:

```cpp
// OpenAPI: { "anyOf": [{"type": "string"}, {"type": "number"}] }
using MyUnionType = std::variant<std::string, double>;

// In your model:
MyUnionType value;

// Use std::visit to handle different types:
std::visit([](const auto& v) {
    using T = std::decay_t<decltype(v)>;
    if constexpr (std::is_same_v<T, std::string>) {
        std::cout << "String: " << v << std::endl;
    } else if constexpr (std::is_same_v<T, double>) {
        std::cout << "Number: " << v << std::endl;
    }
}, value);
```

## Additional Resources

- [cpp-httplib Documentation](https://github.com/yhirose/cpp-httplib)
- [nlohmann/json Documentation](https://github.com/nlohmann/json)
- [OpenAPI Generator Documentation](https://openapi-generator.tech/docs/generators/cpp-httplib-server)
- [OpenAPI Specification](https://swagger.io/specification/)

- [cpp-httplib Documentation](https://github.com/yhirose/cpp-httplib)
- [nlohmann/json Documentation](https://github.com/nlohmann/json)
- [OpenAPI Generator Documentation](https://openapi-generator.tech/docs/generators/)
