# org.openapitools.server - Kotlin Server library for OpenAPI Petstore

## Requires

* Kotlin 1.4.31
* Gradle 6.8.2

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

<a id="documentation-for-api-endpoints"></a>
## Documentation for API Endpoints

All URIs are relative to *http://petstore.swagger.io/v2*

Class | Method | HTTP request | Description
------------ | ------------- | ------------- | -------------
*PetApi* | [**addPet**](docs/PetApi.md#addpet) | **POST**  | Add a new pet to the store
*PetApi* | [**deletePet**](docs/PetApi.md#deletepet) | **DELETE** /{petId} | Deletes a pet
*PetApi* | [**findPetsByStatus**](docs/PetApi.md#findpetsbystatus) | **GET** /findByStatus | Finds Pets by status
*PetApi* | [**findPetsByTags**](docs/PetApi.md#findpetsbytags) | **GET** /findByTags | Finds Pets by tags
*PetApi* | [**getPetById**](docs/PetApi.md#getpetbyid) | **GET** /{petId} | Find pet by ID
*PetApi* | [**updatePet**](docs/PetApi.md#updatepet) | **PUT**  | Update an existing pet
*PetApi* | [**updatePetWithForm**](docs/PetApi.md#updatepetwithform) | **POST** /{petId} | Updates a pet in the store with form data
*PetApi* | [**uploadFile**](docs/PetApi.md#uploadfile) | **POST** /{petId}/uploadImage | uploads an image
*StoreApi* | [**deleteOrder**](docs/StoreApi.md#deleteorder) | **DELETE** /order/{orderId} | Delete purchase order by ID
*StoreApi* | [**getInventory**](docs/StoreApi.md#getinventory) | **GET** /inventory | Returns pet inventories by status
*StoreApi* | [**getOrderById**](docs/StoreApi.md#getorderbyid) | **GET** /order/{orderId} | Find purchase order by ID
*StoreApi* | [**placeOrder**](docs/StoreApi.md#placeorder) | **POST** /order | Place an order for a pet
*UserApi* | [**createUser**](docs/UserApi.md#createuser) | **POST**  | Create user
*UserApi* | [**createUsersWithArrayInput**](docs/UserApi.md#createuserswitharrayinput) | **POST** /createWithArray | Creates list of users with given input array
*UserApi* | [**createUsersWithListInput**](docs/UserApi.md#createuserswithlistinput) | **POST** /createWithList | Creates list of users with given input array
*UserApi* | [**deleteUser**](docs/UserApi.md#deleteuser) | **DELETE** /{username} | Delete user
*UserApi* | [**getUserByName**](docs/UserApi.md#getuserbyname) | **GET** /{username} | Get user by user name
*UserApi* | [**loginUser**](docs/UserApi.md#loginuser) | **GET** /login | Logs user into the system
*UserApi* | [**logoutUser**](docs/UserApi.md#logoutuser) | **GET** /logout | Logs out current logged in user session
*UserApi* | [**updateUser**](docs/UserApi.md#updateuser) | **PUT** /{username} | Updated user


<a id="documentation-for-models"></a>
## Documentation for Models

 - [org.openapitools.server.models.Category](docs/Category.md)
 - [org.openapitools.server.models.ModelApiResponse](docs/ModelApiResponse.md)
 - [org.openapitools.server.models.Order](docs/Order.md)
 - [org.openapitools.server.models.Pet](docs/Pet.md)
 - [org.openapitools.server.models.Tag](docs/Tag.md)
 - [org.openapitools.server.models.User](docs/User.md)


<a id="documentation-for-authorization"></a>
## Documentation for Authorization


Authentication schemes defined for the API:
<a id="petstore_auth"></a>
### petstore_auth

- **Type**: OAuth
- **Flow**: implicit
- **Authorization URL**: http://petstore.swagger.io/api/oauth/dialog
- **Scopes**: 
  - write:pets: modify pets in your account
  - read:pets: read your pets

<a id="api_key"></a>
### api_key

- **Type**: API key
- **API key parameter name**: api_key
- **Location**: HTTP header

