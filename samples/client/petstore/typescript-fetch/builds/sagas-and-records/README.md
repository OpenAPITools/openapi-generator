# @openapitools/typescript-fetch-petstore@1.0.0

A TypeScript SDK client for the petstore.swagger.io API.

## Usage

First, install the SDK from npm.

```bash
npm install @openapitools/typescript-fetch-petstore --save
```

Next, try it out.


```ts
import {
  Configuration,
  BehaviorApi,
} from '@openapitools/typescript-fetch-petstore';
import type { GetBehaviorPermissionsRequest } from '@openapitools/typescript-fetch-petstore';

async function example() {
  console.log("🚀 Testing @openapitools/typescript-fetch-petstore SDK...");
  const api = new BehaviorApi();

  const body = {
    // number
    behaviorId: 789,
  } satisfies GetBehaviorPermissionsRequest;

  try {
    const data = await api.getBehaviorPermissions(body);
    console.log(data);
  } catch (error) {
    console.error(error);
  }
}

// Run the test
example().catch(console.error);
```


## Documentation

### API Endpoints

All URIs are relative to *http://petstore.swagger.io/v2*

| Class | Method | HTTP request | Description
| ----- | ------ | ------------ | -------------
*BehaviorApi* | [**getBehaviorPermissions**](docs/BehaviorApi.md#getbehaviorpermissions) | **GET** /fake_behavior/{behavior-id}/permissions | Get permissions for the behavior
*BehaviorApi* | [**getBehaviorType**](docs/BehaviorApi.md#getbehaviortype) | **GET** /fake_behavior/{behavior-id}/type | Get the type of behavior
*PetApi* | [**addPet**](docs/PetApi.md#addpet) | **POST** /pet | Add a new pet to the store
*PetApi* | [**deletePet**](docs/PetApi.md#deletepet) | **DELETE** /pet/{petId} | Deletes a pet
*PetApi* | [**findPetsByIds**](docs/PetApi.md#findpetsbyids) | **GET** /pet/findByIds | Finds Pets by ids
*PetApi* | [**findPetsByStatus**](docs/PetApi.md#findpetsbystatus) | **GET** /pet/findByStatus | Finds Pets by status
*PetApi* | [**findPetsByTags**](docs/PetApi.md#findpetsbytags) | **GET** /pet/findByTags | Finds Pets by tags
*PetApi* | [**findPetsByUserIds**](docs/PetApi.md#findpetsbyuserids) | **GET** /pet/findByUserIds | Finds Pets by user ids
*PetApi* | [**getPetById**](docs/PetApi.md#getpetbyid) | **GET** /pet/{petId} | Find pet by ID
*PetApi* | [**getPetRegions**](docs/PetApi.md#getpetregions) | **GET** /pet/{petId}/regions | Gets regions for a single pet.
*PetApi* | [**updatePet**](docs/PetApi.md#updatepet) | **PUT** /pet | Update an existing pet
*PetApi* | [**updatePetRegions**](docs/PetApi.md#updatepetregions) | **PUT** /pet/{petId}/regions | Updates the pet regions.
*PetApi* | [**updatePetWithForm**](docs/PetApi.md#updatepetwithform) | **POST** /pet/{petId} | Updates a pet in the store with form data
*PetApi* | [**uploadFile**](docs/PetApi.md#uploadfile) | **POST** /pet/{petId}/uploadImage | uploads an image
*PetPartApi* | [**getFakePetPartType**](docs/PetPartApi.md#getfakepetparttype) | **GET** /fake_petParts/{fake_petPart-id}/part-type | Returns single pet part type for the petPart id.
*PetPartApi* | [**getMatchingParts**](docs/PetPartApi.md#getmatchingparts) | **GET** /fake_petParts/{fake_petPart-id}/matching-parts | Get the matching parts for the given pet part.
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


### Models

- [BehaviorType](docs/BehaviorType.md)
- [Category](docs/Category.md)
- [DefaultMetaOnlyResponse](docs/DefaultMetaOnlyResponse.md)
- [DeploymentRequestStatus](docs/DeploymentRequestStatus.md)
- [ErrorCode](docs/ErrorCode.md)
- [FindPetsByStatusResponse](docs/FindPetsByStatusResponse.md)
- [FindPetsByUserResponse](docs/FindPetsByUserResponse.md)
- [GetBehaviorPermissionsResponse](docs/GetBehaviorPermissionsResponse.md)
- [GetBehaviorTypeResponse](docs/GetBehaviorTypeResponse.md)
- [GetMatchingPartsResponse](docs/GetMatchingPartsResponse.md)
- [GetPetPartTypeResponse](docs/GetPetPartTypeResponse.md)
- [ItemId](docs/ItemId.md)
- [MatchingParts](docs/MatchingParts.md)
- [ModelApiResponse](docs/ModelApiResponse.md)
- [ModelError](docs/ModelError.md)
- [Order](docs/Order.md)
- [Part](docs/Part.md)
- [Pet](docs/Pet.md)
- [PetPartType](docs/PetPartType.md)
- [PetRegionsResponse](docs/PetRegionsResponse.md)
- [ResponseMeta](docs/ResponseMeta.md)
- [Tag](docs/Tag.md)
- [User](docs/User.md)
- [WarningCode](docs/WarningCode.md)

### Authorization


Authentication schemes defined for the API:
<a id="petstore_auth-implicit"></a>
#### petstore_auth implicit


- **Type**: OAuth
- **Flow**: implicit
- **Authorization URL**: http://petstore.swagger.io/api/oauth/dialog
- **Scopes**: 
  - `write:pets`: modify pets in your account
  - `read:pets`: read your pets
<a id="api_key"></a>
#### api_key


- **Type**: API key
- **API key parameter name**: `api_key`
- **Location**: HTTP header

## About

This TypeScript SDK client supports the [Fetch API](https://fetch.spec.whatwg.org/)
and is automatically generated by the
[OpenAPI Generator](https://openapi-generator.tech) project:

- API version: `1.0.0`
- Package version: `1.0.0`
- Generator version: `7.17.0-SNAPSHOT`
- Build package: `org.openapitools.codegen.languages.TypeScriptFetchClientCodegen`

The generated npm module supports the following:

- Environments
  * Node.js
  * Webpack
  * Browserify
- Language levels
  * ES5 - you must have a Promises/A+ library installed
  * ES6
- Module systems
  * CommonJS
  * ES6 module system


## Development

### Building

To build the TypeScript source code, you need to have Node.js and npm installed.
After cloning the repository, navigate to the project directory and run:

```bash
npm install
npm run build
```

### Publishing

Once you've built the package, you can publish it to npm:

```bash
npm publish
```

## License

[Apache-2.0](https://www.apache.org/licenses/LICENSE-2.0.html)
