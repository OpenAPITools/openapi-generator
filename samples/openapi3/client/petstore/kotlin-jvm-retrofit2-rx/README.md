# org.openapitools.client - Kotlin client library for OpenAPI Petstore

## Requires

* Kotlin 1.3.61
* Gradle 4.9

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

All URIs are relative to *http://petstore.swagger.io:80/v2*

Class | Method | HTTP request | Description
------------ | ------------- | ------------- | -------------
*AnotherFakeApi* | [**call123testSpecialTags**](docs/AnotherFakeApi.md#call123testspecialtags) | **PATCH** another-fake/dummy | To test special tags
*DefaultApi* | [**fooGet**](docs/DefaultApi.md#fooget) | **GET** foo | 
*FakeApi* | [**fakeHealthGet**](docs/FakeApi.md#fakehealthget) | **GET** fake/health | Health check endpoint
*FakeApi* | [**fakeHttpSignatureTest**](docs/FakeApi.md#fakehttpsignaturetest) | **GET** fake/http-signature-test | test http signature authentication
*FakeApi* | [**fakeOuterBooleanSerialize**](docs/FakeApi.md#fakeouterbooleanserialize) | **POST** fake/outer/boolean | 
*FakeApi* | [**fakeOuterCompositeSerialize**](docs/FakeApi.md#fakeoutercompositeserialize) | **POST** fake/outer/composite | 
*FakeApi* | [**fakeOuterNumberSerialize**](docs/FakeApi.md#fakeouternumberserialize) | **POST** fake/outer/number | 
*FakeApi* | [**fakeOuterStringSerialize**](docs/FakeApi.md#fakeouterstringserialize) | **POST** fake/outer/string | 
*FakeApi* | [**testBodyWithFileSchema**](docs/FakeApi.md#testbodywithfileschema) | **PUT** fake/body-with-file-schema | 
*FakeApi* | [**testBodyWithQueryParams**](docs/FakeApi.md#testbodywithqueryparams) | **PUT** fake/body-with-query-params | 
*FakeApi* | [**testClientModel**](docs/FakeApi.md#testclientmodel) | **PATCH** fake | To test \"client\" model
*FakeApi* | [**testEndpointParameters**](docs/FakeApi.md#testendpointparameters) | **POST** fake | Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 
*FakeApi* | [**testEnumParameters**](docs/FakeApi.md#testenumparameters) | **GET** fake | To test enum parameters
*FakeApi* | [**testGroupParameters**](docs/FakeApi.md#testgroupparameters) | **DELETE** fake | Fake endpoint to test group parameters (optional)
*FakeApi* | [**testInlineAdditionalProperties**](docs/FakeApi.md#testinlineadditionalproperties) | **POST** fake/inline-additionalProperties | test inline additionalProperties
*FakeApi* | [**testJsonFormData**](docs/FakeApi.md#testjsonformdata) | **GET** fake/jsonFormData | test json serialization of form data
*FakeApi* | [**testQueryParameterCollectionFormat**](docs/FakeApi.md#testqueryparametercollectionformat) | **PUT** fake/test-query-parameters | 
*FakeClassnameTags123Api* | [**testClassname**](docs/FakeClassnameTags123Api.md#testclassname) | **PATCH** fake_classname_test | To test class name in snake case
*PetApi* | [**addPet**](docs/PetApi.md#addpet) | **POST** pet | Add a new pet to the store
*PetApi* | [**deletePet**](docs/PetApi.md#deletepet) | **DELETE** pet/{petId} | Deletes a pet
*PetApi* | [**findPetsByStatus**](docs/PetApi.md#findpetsbystatus) | **GET** pet/findByStatus | Finds Pets by status
*PetApi* | [**findPetsByTags**](docs/PetApi.md#findpetsbytags) | **GET** pet/findByTags | Finds Pets by tags
*PetApi* | [**getPetById**](docs/PetApi.md#getpetbyid) | **GET** pet/{petId} | Find pet by ID
*PetApi* | [**updatePet**](docs/PetApi.md#updatepet) | **PUT** pet | Update an existing pet
*PetApi* | [**updatePetWithForm**](docs/PetApi.md#updatepetwithform) | **POST** pet/{petId} | Updates a pet in the store with form data
*PetApi* | [**uploadFile**](docs/PetApi.md#uploadfile) | **POST** pet/{petId}/uploadImage | uploads an image
*PetApi* | [**uploadFileWithRequiredFile**](docs/PetApi.md#uploadfilewithrequiredfile) | **POST** fake/{petId}/uploadImageWithRequiredFile | uploads an image (required)
*StoreApi* | [**deleteOrder**](docs/StoreApi.md#deleteorder) | **DELETE** store/order/{order_id} | Delete purchase order by ID
*StoreApi* | [**getInventory**](docs/StoreApi.md#getinventory) | **GET** store/inventory | Returns pet inventories by status
*StoreApi* | [**getOrderById**](docs/StoreApi.md#getorderbyid) | **GET** store/order/{order_id} | Find purchase order by ID
*StoreApi* | [**placeOrder**](docs/StoreApi.md#placeorder) | **POST** store/order | Place an order for a pet
*UserApi* | [**createUser**](docs/UserApi.md#createuser) | **POST** user | Create user
*UserApi* | [**createUsersWithArrayInput**](docs/UserApi.md#createuserswitharrayinput) | **POST** user/createWithArray | Creates list of users with given input array
*UserApi* | [**createUsersWithListInput**](docs/UserApi.md#createuserswithlistinput) | **POST** user/createWithList | Creates list of users with given input array
*UserApi* | [**deleteUser**](docs/UserApi.md#deleteuser) | **DELETE** user/{username} | Delete user
*UserApi* | [**getUserByName**](docs/UserApi.md#getuserbyname) | **GET** user/{username} | Get user by user name
*UserApi* | [**loginUser**](docs/UserApi.md#loginuser) | **GET** user/login | Logs user into the system
*UserApi* | [**logoutUser**](docs/UserApi.md#logoutuser) | **GET** user/logout | Logs out current logged in user session
*UserApi* | [**updateUser**](docs/UserApi.md#updateuser) | **PUT** user/{username} | Updated user


<a name="documentation-for-models"></a>
## Documentation for Models

 - [org.openapitools.client.models.AdditionalPropertiesClass](docs/AdditionalPropertiesClass.md)
 - [org.openapitools.client.models.Animal](docs/Animal.md)
 - [org.openapitools.client.models.ApiResponse](docs/ApiResponse.md)
 - [org.openapitools.client.models.ArrayOfArrayOfNumberOnly](docs/ArrayOfArrayOfNumberOnly.md)
 - [org.openapitools.client.models.ArrayOfNumberOnly](docs/ArrayOfNumberOnly.md)
 - [org.openapitools.client.models.ArrayTest](docs/ArrayTest.md)
 - [org.openapitools.client.models.Capitalization](docs/Capitalization.md)
 - [org.openapitools.client.models.Cat](docs/Cat.md)
 - [org.openapitools.client.models.CatAllOf](docs/CatAllOf.md)
 - [org.openapitools.client.models.Category](docs/Category.md)
 - [org.openapitools.client.models.ClassModel](docs/ClassModel.md)
 - [org.openapitools.client.models.Client](docs/Client.md)
 - [org.openapitools.client.models.Dog](docs/Dog.md)
 - [org.openapitools.client.models.DogAllOf](docs/DogAllOf.md)
 - [org.openapitools.client.models.EnumArrays](docs/EnumArrays.md)
 - [org.openapitools.client.models.EnumClass](docs/EnumClass.md)
 - [org.openapitools.client.models.EnumTest](docs/EnumTest.md)
 - [org.openapitools.client.models.FileSchemaTestClass](docs/FileSchemaTestClass.md)
 - [org.openapitools.client.models.Foo](docs/Foo.md)
 - [org.openapitools.client.models.FormatTest](docs/FormatTest.md)
 - [org.openapitools.client.models.HasOnlyReadOnly](docs/HasOnlyReadOnly.md)
 - [org.openapitools.client.models.HealthCheckResult](docs/HealthCheckResult.md)
 - [org.openapitools.client.models.InlineResponseDefault](docs/InlineResponseDefault.md)
 - [org.openapitools.client.models.List](docs/List.md)
 - [org.openapitools.client.models.MapTest](docs/MapTest.md)
 - [org.openapitools.client.models.MixedPropertiesAndAdditionalPropertiesClass](docs/MixedPropertiesAndAdditionalPropertiesClass.md)
 - [org.openapitools.client.models.Model200Response](docs/Model200Response.md)
 - [org.openapitools.client.models.Name](docs/Name.md)
 - [org.openapitools.client.models.NullableClass](docs/NullableClass.md)
 - [org.openapitools.client.models.NumberOnly](docs/NumberOnly.md)
 - [org.openapitools.client.models.Order](docs/Order.md)
 - [org.openapitools.client.models.OuterComposite](docs/OuterComposite.md)
 - [org.openapitools.client.models.OuterEnum](docs/OuterEnum.md)
 - [org.openapitools.client.models.OuterEnumDefaultValue](docs/OuterEnumDefaultValue.md)
 - [org.openapitools.client.models.OuterEnumInteger](docs/OuterEnumInteger.md)
 - [org.openapitools.client.models.OuterEnumIntegerDefaultValue](docs/OuterEnumIntegerDefaultValue.md)
 - [org.openapitools.client.models.Pet](docs/Pet.md)
 - [org.openapitools.client.models.ReadOnlyFirst](docs/ReadOnlyFirst.md)
 - [org.openapitools.client.models.Return](docs/Return.md)
 - [org.openapitools.client.models.SpecialModelname](docs/SpecialModelname.md)
 - [org.openapitools.client.models.Tag](docs/Tag.md)
 - [org.openapitools.client.models.User](docs/User.md)


<a name="documentation-for-authorization"></a>
## Documentation for Authorization

<a name="api_key"></a>
### api_key

- **Type**: API key
- **API key parameter name**: api_key
- **Location**: HTTP header

<a name="api_key_query"></a>
### api_key_query

- **Type**: API key
- **API key parameter name**: api_key_query
- **Location**: URL query string

<a name="bearer_test"></a>
### bearer_test

- **Type**: HTTP basic authentication

<a name="http_basic_test"></a>
### http_basic_test

- **Type**: HTTP basic authentication

<a name="http_signature_test"></a>
### http_signature_test

- **Type**: HTTP basic authentication

<a name="petstore_auth"></a>
### petstore_auth

- **Type**: OAuth
- **Flow**: implicit
- **Authorization URL**: http://petstore.swagger.io/api/oauth/dialog
- **Scopes**: 
  - write:pets: modify pets in your account
  - read:pets: read your pets

