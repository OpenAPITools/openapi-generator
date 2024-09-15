# 

All URIs are relative to *http://petstore.swagger.io/v2*

Method | HTTP request | Description
------------- | ------------- | -------------



# Documentation for OpenAPI Petstore

    <a id="documentation-for-api-endpoints"></a>
    ## Documentation for API Endpoints

    All URIs are relative to *http://petstore.swagger.io/v2*

    Class | Method | HTTP request | Description
    ------------ | ------------- | ------------- | -------------
    *PetApi* | [**AddPet**](Apis/PetApi.md#addpet) | **Post** /pet | Add a new pet to the store
    *PetApi* | [**DeletePet**](Apis/PetApi.md#deletepet) | **Delete** /pet/{petId} | Deletes a pet
    *PetApi* | [**FindPetsByStatus**](Apis/PetApi.md#findpetsbystatus) | **Get** /pet/findByStatus | Finds Pets by status
    *PetApi* | [**FindPetsByTags**](Apis/PetApi.md#findpetsbytags) | **Get** /pet/findByTags | Finds Pets by tags
    *PetApi* | [**GetPetById**](Apis/PetApi.md#getpetbyid) | **Get** /pet/{petId} | Find pet by ID
    *PetApi* | [**UpdatePet**](Apis/PetApi.md#updatepet) | **Put** /pet | Update an existing pet
    *PetApi* | [**UpdatePetWithForm**](Apis/PetApi.md#updatepetwithform) | **Post** /pet/{petId} | Updates a pet in the store with form data
    *PetApi* | [**UploadFile**](Apis/PetApi.md#uploadfile) | **Post** /pet/{petId}/uploadImage | uploads an image
    *StoreApi* | [**DeleteOrder**](Apis/StoreApi.md#deleteorder) | **Delete** /store/order/{orderId} | Delete purchase order by ID
    *StoreApi* | [**GetInventory**](Apis/StoreApi.md#getinventory) | **Get** /store/inventory | Returns pet inventories by status
    *StoreApi* | [**GetOrderById**](Apis/StoreApi.md#getorderbyid) | **Get** /store/order/{orderId} | Find purchase order by ID
    *StoreApi* | [**PlaceOrder**](Apis/StoreApi.md#placeorder) | **Post** /store/order | Place an order for a pet
    *UserApi* | [**CreateUser**](Apis/UserApi.md#createuser) | **Post** /user | Create user
    *UserApi* | [**CreateUsersWithArrayInput**](Apis/UserApi.md#createuserswitharrayinput) | **Post** /user/createWithArray | Creates list of users with given input array
    *UserApi* | [**CreateUsersWithListInput**](Apis/UserApi.md#createuserswithlistinput) | **Post** /user/createWithList | Creates list of users with given input array
    *UserApi* | [**DeleteUser**](Apis/UserApi.md#deleteuser) | **Delete** /user/{username} | Delete user
    *UserApi* | [**GetUserByName**](Apis/UserApi.md#getuserbyname) | **Get** /user/{username} | Get user by user name
    *UserApi* | [**LoginUser**](Apis/UserApi.md#loginuser) | **Get** /user/login | Logs user into the system
    *UserApi* | [**LogoutUser**](Apis/UserApi.md#logoutuser) | **Get** /user/logout | Logs out current logged in user session
    *UserApi* | [**UpdateUser**](Apis/UserApi.md#updateuser) | **Put** /user/{username} | Updated user
    

    <a id="documentation-for-models"></a>
    ## Documentation for Models

         - [Models.ApiResponse](Models/ApiResponse.md)
         - [Models.Category](Models/Category.md)
         - [Models.Order](Models/Order.md)
         - [Models.Pet](Models/Pet.md)
         - [Models.Tag](Models/Tag.md)
         - [Models.User](Models/User.md)
        

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
                    
