# org.openapitools - Kotlin Server library for OpenAPI Petstore

## Requires

* Kotlin 1.3.10
* Maven 3.3

## Build

```
mvn clean package
```

This runs all tests and packages the library.

## Features/Implementation Notes

* Supports JSON inputs/outputs and Form inputs.
* Supports collection formats for query parameters: csv, tsv, ssv, pipes.
* Some Kotlin and Java types are fully qualified to avoid conflicts with types defined in OpenAPI definitions.

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

         - [org.openapitools.server.api.model.Category](docs/Category.md)
         - [org.openapitools.server.api.model.ModelApiResponse](docs/ModelApiResponse.md)
         - [org.openapitools.server.api.model.Order](docs/Order.md)
         - [org.openapitools.server.api.model.Pet](docs/Pet.md)
         - [org.openapitools.server.api.model.Tag](docs/Tag.md)
         - [org.openapitools.server.api.model.User](docs/User.md)
        

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

