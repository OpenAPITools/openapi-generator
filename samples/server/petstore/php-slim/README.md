# php-base - PHP Slim Server library for OpenAPI Petstore

[Slim Framework Documentation](https://www.slimframework.com/docs/)

## Requirements

* Web server with URL rewriting
* PHP 5.5.9 or newer

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

This package uses PHPUnit 4.8 for unit testing and PHP Codesniffer to check source code against user defined coding standard(`phpcsStandard` generator config option).
[Test folder](test) contains templates which you can fill with real test assertions.
How to write tests read at [PHPUnit Manual - Chapter 2. Writing Tests for PHPUnit](https://phpunit.de/manual/4.8/en/writing-tests-for-phpunit.html).
How to configure PHP CodeSniffer read at [PHP CodeSniffer Documentation](https://github.com/squizlabs/PHP_CodeSniffer/wiki).
There is [phplint](https://github.com/overtrue/phplint) tool to check php syntax automatically.

Command | Tool | Target
---- | ---- | ----
`$ composer test` | PHPUnit | All tests
`$ composer run test-apis` | PHPUnit | Apis tests
`$ composer run test-models` | PHPUnit | Models tests
`$ composer run phpcs` | PHP CodeSniffer | All files
`$ composer run phplint` | phplint | All files


## API Endpoints

All URIs are relative to *http://petstore.swagger.io:80/v2*

Class | Method | HTTP request | Description
------------ | ------------- | ------------- | -------------
*AnotherFakeApi* | **call123TestSpecialTags** | **PATCH** /another-fake/dummy | To test special tags
*FakeApi* | **fakeOuterBooleanSerialize** | **POST** /fake/outer/boolean | 
*FakeApi* | **fakeOuterCompositeSerialize** | **POST** /fake/outer/composite | 
*FakeApi* | **fakeOuterNumberSerialize** | **POST** /fake/outer/number | 
*FakeApi* | **fakeOuterStringSerialize** | **POST** /fake/outer/string | 
*FakeApi* | **testBodyWithFileSchema** | **PUT** /fake/body-with-file-schema | 
*FakeApi* | **testBodyWithQueryParams** | **PUT** /fake/body-with-query-params | 
*FakeApi* | **testClientModel** | **PATCH** /fake | To test \"client\" model
*FakeApi* | **testEndpointParameters** | **POST** /fake | Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트
*FakeApi* | **testEnumParameters** | **GET** /fake | To test enum parameters
*FakeApi* | **testGroupParameters** | **DELETE** /fake | Fake endpoint to test group parameters (optional)
*FakeApi* | **testInlineAdditionalProperties** | **POST** /fake/inline-additionalProperties | test inline additionalProperties
*FakeApi* | **testJsonFormData** | **GET** /fake/jsonFormData | test json serialization of form data
*FakeClassnameTags123Api* | **testClassname** | **PATCH** /fake_classname_test | To test class name in snake case
*PetApi* | **addPet** | **POST** /pet | Add a new pet to the store
*PetApi* | **findPetsByStatus** | **GET** /pet/findByStatus | Finds Pets by status
*PetApi* | **findPetsByTags** | **GET** /pet/findByTags | Finds Pets by tags
*PetApi* | **updatePet** | **PUT** /pet | Update an existing pet
*PetApi* | **deletePet** | **DELETE** /pet/{petId} | Deletes a pet
*PetApi* | **getPetById** | **GET** /pet/{petId} | Find pet by ID
*PetApi* | **updatePetWithForm** | **POST** /pet/{petId} | Updates a pet in the store with form data
*PetApi* | **uploadFile** | **POST** /pet/{petId}/uploadImage | uploads an image
*PetApi* | **uploadFileWithRequiredFile** | **POST** /fake/{petId}/uploadImageWithRequiredFile | uploads an image (required)
*StoreApi* | **getInventory** | **GET** /store/inventory | Returns pet inventories by status
*StoreApi* | **placeOrder** | **POST** /store/order | Place an order for a pet
*StoreApi* | **deleteOrder** | **DELETE** /store/order/{order_id} | Delete purchase order by ID
*StoreApi* | **getOrderById** | **GET** /store/order/{order_id} | Find purchase order by ID
*UserApi* | **createUser** | **POST** /user | Create user
*UserApi* | **createUsersWithArrayInput** | **POST** /user/createWithArray | Creates list of users with given input array
*UserApi* | **createUsersWithListInput** | **POST** /user/createWithList | Creates list of users with given input array
*UserApi* | **loginUser** | **GET** /user/login | Logs user into the system
*UserApi* | **logoutUser** | **GET** /user/logout | Logs out current logged in user session
*UserApi* | **deleteUser** | **DELETE** /user/{username} | Delete user
*UserApi* | **getUserByName** | **GET** /user/{username} | Get user by user name
*UserApi* | **updateUser** | **PUT** /user/{username} | Updated user


## Models

* OpenAPIServer\Model\AdditionalPropertiesClass
* OpenAPIServer\Model\Animal
* OpenAPIServer\Model\AnimalFarm
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
* OpenAPIServer\Model\StringBooleanMap
* OpenAPIServer\Model\Tag
* OpenAPIServer\Model\User


## Authentication

> Important! To make Basic Authentication work you need to implement `authenticator` function in [SlimRouter](lib/SlimRouter.php) class.
> Documentation [tuupola/slim-basic-auth](https://github.com/tuupola/slim-basic-auth#readme)
