# php-base - PHP Slim Server library for OpenAPI Petstore

[Slim Framework Documentation](https://www.slimframework.com/docs/)

## Requirements

* Web server with URL rewriting
* PHP 7.0 or newer

This package contains `.htaccess` for Apache configuration.
If you use another server(Nginx, HHVM, IIS, lighttpd) check out [Web Servers](https://www.slimframework.com/docs/v3/start/web-servers.html) doc.

## Installation via [Composer](https://getcomposer.org/)

Navigate into your project's root directory and execute the bash command shown below.
This command downloads the Slim Framework and its third-party dependencies into your project's `vendor/` directory.
```bash
$ composer install
```

## Start devserver

Run the following command in terminal to start localhost web server, assuming `./php-slim-server/` is public-accessible directory with `index.php` file:
```bash
$ php -S localhost:8888 -t php-slim-server
```
> **Warning** This web server was designed to aid application development.
> It may also be useful for testing purposes or for application demonstrations that are run in controlled environments.
> It is not intended to be a full-featured web server. It should not be used on a public network.

## Run tests

This package uses PHPUnit 6 or 7(depends from your PHP version) for unit testing and PHP Codesniffer to check source code against user defined coding standard(`phpcsStandard` generator config option).
[Test folder](test) contains templates which you can fill with real test assertions.
How to write tests read at [PHPUnit Manual - Chapter 2. Writing Tests for PHPUnit](https://phpunit.de/manual/6.5/en/writing-tests-for-phpunit.html).
How to configure PHP CodeSniffer read at [PHP CodeSniffer Documentation](https://github.com/squizlabs/PHP_CodeSniffer/wiki).
There is [phplint](https://github.com/overtrue/phplint) tool to check php syntax automatically.

Command | Tool | Target
---- | ---- | ----
`$ composer test` | PHPUnit | All tests
`$ composer run test-apis` | PHPUnit | Apis tests
`$ composer run test-models` | PHPUnit | Models tests
`$ composer run phpcs` | PHP CodeSniffer | All files
`$ composer run phplint` | phplint | All files

## Show errors

Change line in `./index.php`:
```diff
--- $router = new SlimRouter();
+++ $router = new SlimRouter(['settings' => ['displayErrorDetails' => true]]);
```

## API Endpoints

All URIs are relative to *http://petstore.swagger.io:80/v2*

> Important! Do not modify abstract API controllers directly! Instead extend them by implementation classes like:

```php
// src/Api/PetApi.php

namespace OpenAPIServer\Api;

use OpenAPIServer\Api\AbstractPetApi;

class PetApi extends AbstractPetApi
{

    public function addPet($request, $response, $args)
    {
        // your implementation of addPet method here
    }
}
```

Place all your implementation classes in `./src` folder accordingly.
For instance, when abstract class located at `./lib/Api/AbstractPetApi.php` you need to create implementation class at `./src/Api/PetApi.php`.

Class | Method | HTTP request | Description
------------ | ------------- | ------------- | -------------
*AbstractAnotherFakeApi* | **call123TestSpecialTags** | **PATCH** /another-fake/dummy | To test special tags
*AbstractFakeApi* | **fakeOuterBooleanSerialize** | **POST** /fake/outer/boolean | 
*AbstractFakeApi* | **fakeOuterCompositeSerialize** | **POST** /fake/outer/composite | 
*AbstractFakeApi* | **fakeOuterNumberSerialize** | **POST** /fake/outer/number | 
*AbstractFakeApi* | **fakeOuterStringSerialize** | **POST** /fake/outer/string | 
*AbstractFakeApi* | **testBodyWithFileSchema** | **PUT** /fake/body-with-file-schema | 
*AbstractFakeApi* | **testBodyWithQueryParams** | **PUT** /fake/body-with-query-params | 
*AbstractFakeApi* | **testClientModel** | **PATCH** /fake | To test \"client\" model
*AbstractFakeApi* | **testEndpointParameters** | **POST** /fake | Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트
*AbstractFakeApi* | **testEnumParameters** | **GET** /fake | To test enum parameters
*AbstractFakeApi* | **testGroupParameters** | **DELETE** /fake | Fake endpoint to test group parameters (optional)
*AbstractFakeApi* | **testInlineAdditionalProperties** | **POST** /fake/inline-additionalProperties | test inline additionalProperties
*AbstractFakeApi* | **testJsonFormData** | **GET** /fake/jsonFormData | test json serialization of form data
*AbstractFakeClassnameTags123Api* | **testClassname** | **PATCH** /fake_classname_test | To test class name in snake case
*AbstractPetApi* | **addPet** | **POST** /pet | Add a new pet to the store
*AbstractPetApi* | **findPetsByStatus** | **GET** /pet/findByStatus | Finds Pets by status
*AbstractPetApi* | **findPetsByTags** | **GET** /pet/findByTags | Finds Pets by tags
*AbstractPetApi* | **updatePet** | **PUT** /pet | Update an existing pet
*AbstractPetApi* | **deletePet** | **DELETE** /pet/{petId} | Deletes a pet
*AbstractPetApi* | **getPetById** | **GET** /pet/{petId} | Find pet by ID
*AbstractPetApi* | **updatePetWithForm** | **POST** /pet/{petId} | Updates a pet in the store with form data
*AbstractPetApi* | **uploadFile** | **POST** /pet/{petId}/uploadImage | uploads an image
*AbstractPetApi* | **uploadFileWithRequiredFile** | **POST** /fake/{petId}/uploadImageWithRequiredFile | uploads an image (required)
*AbstractStoreApi* | **getInventory** | **GET** /store/inventory | Returns pet inventories by status
*AbstractStoreApi* | **placeOrder** | **POST** /store/order | Place an order for a pet
*AbstractStoreApi* | **deleteOrder** | **DELETE** /store/order/{order_id} | Delete purchase order by ID
*AbstractStoreApi* | **getOrderById** | **GET** /store/order/{order_id} | Find purchase order by ID
*AbstractUserApi* | **createUser** | **POST** /user | Create user
*AbstractUserApi* | **createUsersWithArrayInput** | **POST** /user/createWithArray | Creates list of users with given input array
*AbstractUserApi* | **createUsersWithListInput** | **POST** /user/createWithList | Creates list of users with given input array
*AbstractUserApi* | **loginUser** | **GET** /user/login | Logs user into the system
*AbstractUserApi* | **logoutUser** | **GET** /user/logout | Logs out current logged in user session
*AbstractUserApi* | **deleteUser** | **DELETE** /user/{username} | Delete user
*AbstractUserApi* | **getUserByName** | **GET** /user/{username} | Get user by user name
*AbstractUserApi* | **updateUser** | **PUT** /user/{username} | Updated user


## Models

* OpenAPIServer\Model\AdditionalPropertiesClass
* OpenAPIServer\Model\Animal
* OpenAPIServer\Model\ApiResponse
* OpenAPIServer\Model\ArrayOfArrayOfNumberOnly
* OpenAPIServer\Model\ArrayOfNumberOnly
* OpenAPIServer\Model\ArrayTest
* OpenAPIServer\Model\Capitalization
* OpenAPIServer\Model\Cat
* OpenAPIServer\Model\Category
* OpenAPIServer\Model\ClassModel
* OpenAPIServer\Model\Client
* OpenAPIServer\Model\Dog
* OpenAPIServer\Model\EnumArrays
* OpenAPIServer\Model\EnumClass
* OpenAPIServer\Model\EnumTest
* OpenAPIServer\Model\File
* OpenAPIServer\Model\FileSchemaTestClass
* OpenAPIServer\Model\FormatTest
* OpenAPIServer\Model\HasOnlyReadOnly
* OpenAPIServer\Model\MapTest
* OpenAPIServer\Model\MixedPropertiesAndAdditionalPropertiesClass
* OpenAPIServer\Model\Model200Response
* OpenAPIServer\Model\ModelList
* OpenAPIServer\Model\ModelReturn
* OpenAPIServer\Model\Name
* OpenAPIServer\Model\NumberOnly
* OpenAPIServer\Model\Order
* OpenAPIServer\Model\OuterComposite
* OpenAPIServer\Model\OuterEnum
* OpenAPIServer\Model\Pet
* OpenAPIServer\Model\ReadOnlyFirst
* OpenAPIServer\Model\SpecialModelName
* OpenAPIServer\Model\Tag
* OpenAPIServer\Model\User


## Authentication

> Important! To make Basic Authentication work you need to implement `authenticator` function in [SlimRouter](lib/SlimRouter.php) class.
> Documentation [tuupola/slim-basic-auth](https://github.com/tuupola/slim-basic-auth#readme)
