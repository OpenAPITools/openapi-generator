# org.openapitools.client - Kotlin client library for OpenAPI Petstore

Design decisions for the Android Kotlin library

A kotlin client for Android using the currently recommended http client, Volley. See https://developer.android.com/training/volley

- Currently sends GsonRequests
- Currently only supports Gson as a serializer
- Default the source location to src/main/java as per standard Android builds



- Reuse model class and other jvm common infrastructure
- Api calls use co-routines, and execute them using volley callbacks to avoid tying up a thread.
- Facilitate dependency injection, with default implementations available.
- Generate a requestFactory that can be overridden
- Allow the passing of the RequestFactory per tag (api client) or per operation (an extra parameter is created on operations with non-global security), with per operation auth overriding global security.
- DI scoping of the Request Factory and pre-generated auth header factories allow for thread safe and secure setting of credentials.
- Lazy header factories allow for refreshing tokens etc
- Factoring of header factories to the Request Factory allow ambient provision of credentials. Code gen library is credential storage agnostic.
- Header factories allow the merging of generated headers from open api spec with dynamically added headers

- Injection of http url stack to allow custom http stacks. Default implementation is best practice singleton
- Data classes.

- Optional generation of room database models, and transform methods to these from open api models
- Room and api models can be extended with additional extension properties.

- Additional supporting files ??

Future improvements
- Option to generate image requests on certain conditionals e.g content-type gif etc
- Support for kotlin serialization.
- Form support ?
- Support for file inputs


Hilt Dependency injection example

Call Site example
- Including tryInvokeApi wrapper.

## Requires

* Kotlin 1.4.30
* Gradle 6.8.3

## Build

First, create the gradle wrapper script:

```
gradle wrapper
```

Then, run:

```
./gradlew check assemble
```

This runs all tests and packages the library.

## Features/Implementation Notes

* Supports JSON inputs/outputs, File inputs, and Form inputs.
* Supports collection formats for query parameters: csv, tsv, ssv, pipes.
* Some Kotlin and Java types are fully qualified to avoid conflicts with types defined in OpenAPI definitions.
* Implementation of ApiClient is intended to reduce method counts, specifically to benefit Android targets.

<a name="documentation-for-api-endpoints"></a>
## Documentation for API Endpoints

All URIs are relative to *http://petstore.swagger.io/v2*

Class | Method | HTTP request | Description
------------ | ------------- | ------------- | -------------
*PetApi* | [**addPet**](docs/PetApi.md#addpet) | **POST** /pet | Add a new pet to the store
*PetApi* | [**deletePet**](docs/PetApi.md#deletepet) | **DELETE** /pet/{petId} | Deletes a pet
*PetApi* | [**findPetsByStatus**](docs/PetApi.md#findpetsbystatus) | **GET** /pet/findByStatus | Finds Pets by status
*PetApi* | [**findPetsByTags**](docs/PetApi.md#findpetsbytags) | **GET** /pet/findByTags | Finds Pets by tags
*PetApi* | [**getPetById**](docs/PetApi.md#getpetbyid) | **GET** /pet/{petId} | Find pet by ID
*PetApi* | [**updatePet**](docs/PetApi.md#updatepet) | **PUT** /pet | Update an existing pet
*PetApi* | [**updatePetWithForm**](docs/PetApi.md#updatepetwithform) | **POST** /pet/{petId} | Updates a pet in the store with form data
*PetApi* | [**uploadFile**](docs/PetApi.md#uploadfile) | **POST** /pet/{petId}/uploadImage | uploads an image
*StoreApi* | [**deleteOrder**](docs/StoreApi.md#deleteorder) | **DELETE** /store/order/{orderId} | Delete purchase order by ID
*StoreApi* | [**getInventory**](docs/StoreApi.md#getinventory) | **GET** /store/inventory | Returns pet inventories by status
*StoreApi* | [**getOrderById**](docs/StoreApi.md#getorderbyid) | **GET** /store/order/{orderId} | Find purchase order by ID
*StoreApi* | [**placeOrder**](docs/StoreApi.md#placeorder) | **POST** /store/order | Place an order for a pet
*UserApi* | [**createUser**](docs/UserApi.md#createuser) | **POST** /user | Create user
*UserApi* | [**createUsersWithArrayInput**](docs/UserApi.md#createuserswitharrayinput) | **POST** /user/createWithArray | Creates list of users with given input array
*UserApi* | [**createUsersWithListInput**](docs/UserApi.md#createuserswithlistinput) | **POST** /user/createWithList | Creates list of users with given input array
*UserApi* | [**deleteUser**](docs/UserApi.md#deleteuser) | **DELETE** /user/{username} | Delete user
*UserApi* | [**getUserByName**](docs/UserApi.md#getuserbyname) | **GET** /user/{username} | Get user by user name
*UserApi* | [**loginUser**](docs/UserApi.md#loginuser) | **GET** /user/login | Logs user into the system
*UserApi* | [**logoutUser**](docs/UserApi.md#logoutuser) | **GET** /user/logout | Logs out current logged in user session
*UserApi* | [**updateUser**](docs/UserApi.md#updateuser) | **PUT** /user/{username} | Updated user


<a name="documentation-for-models"></a>
## Documentation for Models

 - [org.openapitools.client.models.ApiResponse](docs/ApiResponse.md)
 - [org.openapitools.client.models.Category](docs/Category.md)
 - [org.openapitools.client.models.Order](docs/Order.md)
 - [org.openapitools.client.models.Pet](docs/Pet.md)
 - [org.openapitools.client.models.Tag](docs/Tag.md)
 - [org.openapitools.client.models.User](docs/User.md)


<a name="documentation-for-authorization"></a>
## Documentation for Authorization

<a name="api_key"></a>
### api_key

- **Type**: API key
- **API key parameter name**: api_key
- **Location**: HTTP header

<a name="petstore_auth"></a>
### petstore_auth

- **Type**: OAuth
- **Flow**: implicit
- **Authorization URL**: http://petstore.swagger.io/api/oauth/dialog
- **Scopes**: 
  - write:pets: modify pets in your account
  - read:pets: read your pets

