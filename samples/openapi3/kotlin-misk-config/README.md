# Documentation for OpenAPI Petstore

    <a id="documentation-for-api-endpoints"></a>
    ## Documentation for API Endpoints

    All URIs are relative to *http://petstore.swagger.io/v2*

    Class | Method | HTTP request | Description
    ------------ | ------------- | ------------- | -------------
    *PetApi* | [**addPet**](Apis/docs/PetApi.md#addpet) | **Post** /pet | Add a new pet to the store
    *PetApi* | [**deletePet**](Apis/docs/PetApi.md#deletepet) | **Delete** /pet/{petId} | Deletes a pet
    *PetApi* | [**findPetsByStatus**](Apis/docs/PetApi.md#findpetsbystatus) | **Get** /pet/findByStatus | Finds Pets by status
    *PetApi* | [**findPetsByTags**](Apis/docs/PetApi.md#findpetsbytags) | **Get** /pet/findByTags | Finds Pets by tags
    *PetApi* | [**getPetById**](Apis/docs/PetApi.md#getpetbyid) | **Get** /pet/{petId} | Find pet by ID
    *PetApi* | [**updatePet**](Apis/docs/PetApi.md#updatepet) | **Put** /pet | Update an existing pet
    *PetApi* | [**updatePetWithForm**](Apis/docs/PetApi.md#updatepetwithform) | **Post** /pet/{petId} | Updates a pet in the store with form data
    *PetApi* | [**uploadFile**](Apis/docs/PetApi.md#uploadfile) | **Post** /pet/{petId}/uploadImage | uploads an image
    *StoreApi* | [**deleteOrder**](Apis/docs/StoreApi.md#deleteorder) | **Delete** /store/order/{orderId} | Delete purchase order by ID
    *StoreApi* | [**getInventory**](Apis/docs/StoreApi.md#getinventory) | **Get** /store/inventory | Returns pet inventories by status
    *StoreApi* | [**getOrderById**](Apis/docs/StoreApi.md#getorderbyid) | **Get** /store/order/{orderId} | Find purchase order by ID
    *StoreApi* | [**placeOrder**](Apis/docs/StoreApi.md#placeorder) | **Post** /store/order | Place an order for a pet
    *UserApi* | [**createUser**](Apis/docs/UserApi.md#createuser) | **Post** /user | Create user
    *UserApi* | [**createUsersWithArrayInput**](Apis/docs/UserApi.md#createuserswitharrayinput) | **Post** /user/createWithArray | Creates list of users with given input array
    *UserApi* | [**createUsersWithListInput**](Apis/docs/UserApi.md#createuserswithlistinput) | **Post** /user/createWithList | Creates list of users with given input array
    *UserApi* | [**deleteUser**](Apis/docs/UserApi.md#deleteuser) | **Delete** /user/{username} | Delete user
    *UserApi* | [**getUserByName**](Apis/docs/UserApi.md#getuserbyname) | **Get** /user/{username} | Get user by user name
    *UserApi* | [**loginUser**](Apis/docs/UserApi.md#loginuser) | **Get** /user/login | Logs user into the system
    *UserApi* | [**logoutUser**](Apis/docs/UserApi.md#logoutuser) | **Get** /user/logout | Logs out current logged in user session
    *UserApi* | [**updateUser**](Apis/docs/UserApi.md#updateuser) | **Put** /user/{username} | Updated user
    

    <a id="documentation-for-models"></a>
    ## Documentation for Models

         - [org.openapitools.server.api.model.Category](Models/docs/Category.md)
         - [org.openapitools.server.api.model.ModelApiResponse](Models/docs/ModelApiResponse.md)
         - [org.openapitools.server.api.model.Order](Models/docs/Order.md)
         - [org.openapitools.server.api.model.Pet](Models/docs/Pet.md)
         - [org.openapitools.server.api.model.Tag](Models/docs/Tag.md)
         - [org.openapitools.server.api.model.User](Models/docs/User.md)
        

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
                    
