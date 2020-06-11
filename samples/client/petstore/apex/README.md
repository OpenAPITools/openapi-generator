# OpenAPI Petstore API Client


This is a sample server Petstore server. For this sample, you can use the api key `special-key` to test the authorization filters.

## Requirements

- [Salesforce DX](https://www.salesforce.com/products/platform/products/salesforce-dx/)

If everything is set correctly:

- Running `sfdx version` in a command prompt should output something like:

  ```bash
  sfdx-cli/5.7.5-05549de (darwin-amd64) go1.7.5 sfdxstable
  ```

## Installation

1. Copy the output into your Salesforce DX folder - or alternatively deploy the output directly into the workspace.
2. Deploy the code via Salesforce DX to your Scratch Org

   ```bash
      sfdx force:source:push
   ```

3. If the API needs authentication update the Named Credential in Setup.
4. Run your Apex tests using

   ```bash
       sfdx sfdx force:apex:test:run
   ```

5. Retrieve the job id from the console and check the test results.

  ```bash
  sfdx force:apex:test:report -i theJobId
  ```

## Getting Started

Please follow the [installation](#installation) instruction and execute the following Apex code:

```java
OASPetApi api = new OASPetApi();
OASClient client = api.getClient();


Map<String, Object> params = new Map<String, Object>{
    'oaSPet' => ''
};

try {
    // cross your fingers
    OASPet result = api.addPet(params);
    System.debug(result);
} catch (OAS.ApiException e) {
    // ...handle your exceptions
}
```

## Documentation for API Endpoints

All URIs are relative to *http://petstore.swagger.io/v2*

Class | Method | HTTP request | Description
------------ | ------------- | ------------- | -------------
*OASPetApi* | [**addPet**](OASPetApi.md#addPet) | **POST** /pet | Add a new pet to the store
*OASPetApi* | [**deletePet**](OASPetApi.md#deletePet) | **DELETE** /pet/{petId} | Deletes a pet
*OASPetApi* | [**findPetsByStatus**](OASPetApi.md#findPetsByStatus) | **GET** /pet/findByStatus | Finds Pets by status
*OASPetApi* | [**findPetsByTags**](OASPetApi.md#findPetsByTags) | **GET** /pet/findByTags | Finds Pets by tags
*OASPetApi* | [**getPetById**](OASPetApi.md#getPetById) | **GET** /pet/{petId} | Find pet by ID
*OASPetApi* | [**updatePet**](OASPetApi.md#updatePet) | **PUT** /pet | Update an existing pet
*OASPetApi* | [**updatePetWithForm**](OASPetApi.md#updatePetWithForm) | **POST** /pet/{petId} | Updates a pet in the store with form data
*OASPetApi* | [**uploadFile**](OASPetApi.md#uploadFile) | **POST** /pet/{petId}/uploadImage | uploads an image
*OASStoreApi* | [**deleteOrder**](OASStoreApi.md#deleteOrder) | **DELETE** /store/order/{orderId} | Delete purchase order by ID
*OASStoreApi* | [**getInventory**](OASStoreApi.md#getInventory) | **GET** /store/inventory | Returns pet inventories by status
*OASStoreApi* | [**getOrderById**](OASStoreApi.md#getOrderById) | **GET** /store/order/{orderId} | Find purchase order by ID
*OASStoreApi* | [**placeOrder**](OASStoreApi.md#placeOrder) | **POST** /store/order | Place an order for a pet
*OASUserApi* | [**createUser**](OASUserApi.md#createUser) | **POST** /user | Create user
*OASUserApi* | [**createUsersWithArrayInput**](OASUserApi.md#createUsersWithArrayInput) | **POST** /user/createWithArray | Creates list of users with given input array
*OASUserApi* | [**createUsersWithListInput**](OASUserApi.md#createUsersWithListInput) | **POST** /user/createWithList | Creates list of users with given input array
*OASUserApi* | [**deleteUser**](OASUserApi.md#deleteUser) | **DELETE** /user/{username} | Delete user
*OASUserApi* | [**getUserByName**](OASUserApi.md#getUserByName) | **GET** /user/{username} | Get user by user name
*OASUserApi* | [**loginUser**](OASUserApi.md#loginUser) | **GET** /user/login | Logs user into the system
*OASUserApi* | [**logoutUser**](OASUserApi.md#logoutUser) | **GET** /user/logout | Logs out current logged in user session
*OASUserApi* | [**updateUser**](OASUserApi.md#updateUser) | **PUT** /user/{username} | Updated user


## Documentation for Models

 - [OASApiResponse](OASApiResponse.md)
 - [OASCategory](OASCategory.md)
 - [OASOrder](OASOrder.md)
 - [OASPet](OASPet.md)
 - [OASTag](OASTag.md)
 - [OASUser](OASUser.md)


## Documentation for Authorization

Authentication schemes defined for the API:
### api_key


- **Type**: API key
- **API key parameter name**: api_key
- **Location**: HTTP header

### petstore_auth

- **Type**: OAuth
- **Flow**: implicit
- **Authorization URL**: http://petstore.swagger.io/api/oauth/dialog
- **Scopes**: 
  - write:pets: modify pets in your account
  - read:pets: read your pets


## Author



