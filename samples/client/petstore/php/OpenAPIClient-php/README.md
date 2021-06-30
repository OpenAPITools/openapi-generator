# OpenAPIClient-php

This spec is mainly for testing Petstore server and contains fake endpoints, models. Please do not use this for any other purpose. Special characters: \" \\


## Installation & Usage

### Requirements

PHP 7.2 and later.

### Composer

To install the bindings via [Composer](https://getcomposer.org/), add the following to `composer.json`:

```json
{
  "repositories": [
    {
      "type": "vcs",
      "url": "https://github.com/GIT_USER_ID/GIT_REPO_ID.git"
    }
  ],
  "require": {
    "GIT_USER_ID/GIT_REPO_ID": "*@dev"
  }
}
```

Then run `composer install`

### Manual Installation

Download the files and include `autoload.php`:

```php
<?php
require_once('/path/to/OpenAPIClient-php/vendor/autoload.php');
```

## Getting Started

Please follow the [installation procedure](#installation--usage) and then run the following:

```php
<?php
require_once(__DIR__ . '/vendor/autoload.php');




$apiInstance = new OpenAPI\Client\Api\AnotherFakeApi(
    // If you want use custom http client, pass your client which implements `GuzzleHttp\ClientInterface`.
    // This is optional, `GuzzleHttp\Client` will be used as default.
    new GuzzleHttp\Client()
);
$client = new \OpenAPI\Client\Model\Client(); // \OpenAPI\Client\Model\Client | client model

try {
    $result = $apiInstance->call123TestSpecialTags($client);
    print_r($result);
} catch (Exception $e) {
    echo 'Exception when calling AnotherFakeApi->call123TestSpecialTags: ', $e->getMessage(), PHP_EOL;
}

```

## API Endpoints

All URIs are relative to *http://petstore.swagger.io:80/v2*

Class | Method | HTTP request | Description
------------ | ------------- | ------------- | -------------
*AnotherFakeApi* | [**call123TestSpecialTags**](docs/Api/AnotherFakeApi.md#call123testspecialtags) | **PATCH** /another-fake/dummy | To test special tags
*DefaultApi* | [**fooGet**](docs/Api/DefaultApi.md#fooget) | **GET** /foo | 
*FakeApi* | [**fakeHealthGet**](docs/Api/FakeApi.md#fakehealthget) | **GET** /fake/health | Health check endpoint
*FakeApi* | [**fakeHttpSignatureTest**](docs/Api/FakeApi.md#fakehttpsignaturetest) | **GET** /fake/http-signature-test | test http signature authentication
*FakeApi* | [**fakeOuterBooleanSerialize**](docs/Api/FakeApi.md#fakeouterbooleanserialize) | **POST** /fake/outer/boolean | 
*FakeApi* | [**fakeOuterCompositeSerialize**](docs/Api/FakeApi.md#fakeoutercompositeserialize) | **POST** /fake/outer/composite | 
*FakeApi* | [**fakeOuterNumberSerialize**](docs/Api/FakeApi.md#fakeouternumberserialize) | **POST** /fake/outer/number | 
*FakeApi* | [**fakeOuterStringSerialize**](docs/Api/FakeApi.md#fakeouterstringserialize) | **POST** /fake/outer/string | 
*FakeApi* | [**fakePropertyEnumIntegerSerialize**](docs/Api/FakeApi.md#fakepropertyenumintegerserialize) | **POST** /fake/property/enum-int | 
*FakeApi* | [**testBodyWithBinary**](docs/Api/FakeApi.md#testbodywithbinary) | **PUT** /fake/body-with-binary | 
*FakeApi* | [**testBodyWithFileSchema**](docs/Api/FakeApi.md#testbodywithfileschema) | **PUT** /fake/body-with-file-schema | 
*FakeApi* | [**testBodyWithQueryParams**](docs/Api/FakeApi.md#testbodywithqueryparams) | **PUT** /fake/body-with-query-params | 
*FakeApi* | [**testClientModel**](docs/Api/FakeApi.md#testclientmodel) | **PATCH** /fake | To test \&quot;client\&quot; model
*FakeApi* | [**testEndpointParameters**](docs/Api/FakeApi.md#testendpointparameters) | **POST** /fake | Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트
*FakeApi* | [**testEnumParameters**](docs/Api/FakeApi.md#testenumparameters) | **GET** /fake | To test enum parameters
*FakeApi* | [**testGroupParameters**](docs/Api/FakeApi.md#testgroupparameters) | **DELETE** /fake | Fake endpoint to test group parameters (optional)
*FakeApi* | [**testInlineAdditionalProperties**](docs/Api/FakeApi.md#testinlineadditionalproperties) | **POST** /fake/inline-additionalProperties | test inline additionalProperties
*FakeApi* | [**testJsonFormData**](docs/Api/FakeApi.md#testjsonformdata) | **GET** /fake/jsonFormData | test json serialization of form data
*FakeApi* | [**testQueryParameterCollectionFormat**](docs/Api/FakeApi.md#testqueryparametercollectionformat) | **PUT** /fake/test-query-paramters | 
*FakeClassnameTags123Api* | [**testClassname**](docs/Api/FakeClassnameTags123Api.md#testclassname) | **PATCH** /fake_classname_test | To test class name in snake case
*PetApi* | [**addPet**](docs/Api/PetApi.md#addpet) | **POST** /pet | Add a new pet to the store
*PetApi* | [**deletePet**](docs/Api/PetApi.md#deletepet) | **DELETE** /pet/{petId} | Deletes a pet
*PetApi* | [**findPetsByStatus**](docs/Api/PetApi.md#findpetsbystatus) | **GET** /pet/findByStatus | Finds Pets by status
*PetApi* | [**findPetsByTags**](docs/Api/PetApi.md#findpetsbytags) | **GET** /pet/findByTags | Finds Pets by tags
*PetApi* | [**getPetById**](docs/Api/PetApi.md#getpetbyid) | **GET** /pet/{petId} | Find pet by ID
*PetApi* | [**updatePet**](docs/Api/PetApi.md#updatepet) | **PUT** /pet | Update an existing pet
*PetApi* | [**updatePetWithForm**](docs/Api/PetApi.md#updatepetwithform) | **POST** /pet/{petId} | Updates a pet in the store with form data
*PetApi* | [**uploadFile**](docs/Api/PetApi.md#uploadfile) | **POST** /pet/{petId}/uploadImage | uploads an image
*PetApi* | [**uploadFileWithRequiredFile**](docs/Api/PetApi.md#uploadfilewithrequiredfile) | **POST** /fake/{petId}/uploadImageWithRequiredFile | uploads an image (required)
*StoreApi* | [**deleteOrder**](docs/Api/StoreApi.md#deleteorder) | **DELETE** /store/order/{order_id} | Delete purchase order by ID
*StoreApi* | [**getInventory**](docs/Api/StoreApi.md#getinventory) | **GET** /store/inventory | Returns pet inventories by status
*StoreApi* | [**getOrderById**](docs/Api/StoreApi.md#getorderbyid) | **GET** /store/order/{order_id} | Find purchase order by ID
*StoreApi* | [**placeOrder**](docs/Api/StoreApi.md#placeorder) | **POST** /store/order | Place an order for a pet
*UserApi* | [**createUser**](docs/Api/UserApi.md#createuser) | **POST** /user | Create user
*UserApi* | [**createUsersWithArrayInput**](docs/Api/UserApi.md#createuserswitharrayinput) | **POST** /user/createWithArray | Creates list of users with given input array
*UserApi* | [**createUsersWithListInput**](docs/Api/UserApi.md#createuserswithlistinput) | **POST** /user/createWithList | Creates list of users with given input array
*UserApi* | [**deleteUser**](docs/Api/UserApi.md#deleteuser) | **DELETE** /user/{username} | Delete user
*UserApi* | [**getUserByName**](docs/Api/UserApi.md#getuserbyname) | **GET** /user/{username} | Get user by user name
*UserApi* | [**loginUser**](docs/Api/UserApi.md#loginuser) | **GET** /user/login | Logs user into the system
*UserApi* | [**logoutUser**](docs/Api/UserApi.md#logoutuser) | **GET** /user/logout | Logs out current logged in user session
*UserApi* | [**updateUser**](docs/Api/UserApi.md#updateuser) | **PUT** /user/{username} | Updated user

## Models

- [AdditionalPropertiesClass](docs/Model/AdditionalPropertiesClass.md)
- [Animal](docs/Model/Animal.md)
- [ApiResponse](docs/Model/ApiResponse.md)
- [ArrayOfArrayOfNumberOnly](docs/Model/ArrayOfArrayOfNumberOnly.md)
- [ArrayOfNumberOnly](docs/Model/ArrayOfNumberOnly.md)
- [ArrayTest](docs/Model/ArrayTest.md)
- [Capitalization](docs/Model/Capitalization.md)
- [Cat](docs/Model/Cat.md)
- [CatAllOf](docs/Model/CatAllOf.md)
- [Category](docs/Model/Category.md)
- [ClassModel](docs/Model/ClassModel.md)
- [Client](docs/Model/Client.md)
- [Dog](docs/Model/Dog.md)
- [DogAllOf](docs/Model/DogAllOf.md)
- [EnumArrays](docs/Model/EnumArrays.md)
- [EnumClass](docs/Model/EnumClass.md)
- [EnumTest](docs/Model/EnumTest.md)
- [File](docs/Model/File.md)
- [FileSchemaTestClass](docs/Model/FileSchemaTestClass.md)
- [Foo](docs/Model/Foo.md)
- [FormatTest](docs/Model/FormatTest.md)
- [HasOnlyReadOnly](docs/Model/HasOnlyReadOnly.md)
- [HealthCheckResult](docs/Model/HealthCheckResult.md)
- [InlineResponseDefault](docs/Model/InlineResponseDefault.md)
- [MapTest](docs/Model/MapTest.md)
- [MixedPropertiesAndAdditionalPropertiesClass](docs/Model/MixedPropertiesAndAdditionalPropertiesClass.md)
- [Model200Response](docs/Model/Model200Response.md)
- [ModelList](docs/Model/ModelList.md)
- [ModelReturn](docs/Model/ModelReturn.md)
- [Name](docs/Model/Name.md)
- [NullableClass](docs/Model/NullableClass.md)
- [NumberOnly](docs/Model/NumberOnly.md)
- [Order](docs/Model/Order.md)
- [OuterComposite](docs/Model/OuterComposite.md)
- [OuterEnum](docs/Model/OuterEnum.md)
- [OuterEnumDefaultValue](docs/Model/OuterEnumDefaultValue.md)
- [OuterEnumInteger](docs/Model/OuterEnumInteger.md)
- [OuterEnumIntegerDefaultValue](docs/Model/OuterEnumIntegerDefaultValue.md)
- [OuterObjectWithEnumProperty](docs/Model/OuterObjectWithEnumProperty.md)
- [Pet](docs/Model/Pet.md)
- [ReadOnlyFirst](docs/Model/ReadOnlyFirst.md)
- [SpecialModelName](docs/Model/SpecialModelName.md)
- [Tag](docs/Model/Tag.md)
- [User](docs/Model/User.md)

## Authorization

### api_key

- **Type**: API key
- **API key parameter name**: api_key
- **Location**: HTTP header



### api_key_query

- **Type**: API key
- **API key parameter name**: api_key_query
- **Location**: URL query string



### bearer_test

- **Type**: Bearer authentication (JWT)


### http_basic_test

- **Type**: HTTP basic authentication


### http_signature_test


### petstore_auth

- **Type**: `OAuth`
- **Flow**: `implicit`
- **Authorization URL**: `http://petstore.swagger.io/api/oauth/dialog`
- **Scopes**: 
    - **write:pets**: modify pets in your account
    - **read:pets**: read your pets

## Tests

To run the tests, use:

```bash
composer install
vendor/bin/phpunit
```

## Author



## About this package

This PHP package is automatically generated by the [OpenAPI Generator](https://openapi-generator.tech) project:

- API version: `1.0.0`
- Build package: `org.openapitools.codegen.languages.PhpClientCodegen`
