# php-base - PHP Slim Server library for OpenAPI Petstore

* [OpenAPI Generator](https://openapi-generator.tech)
* [Slim Framework Documentation](https://www.slimframework.com/docs/)

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

## Tests

### PHPUnit

This package uses PHPUnit 6 or 7(depends from your PHP version) for unit testing.
[Test folder](test) contains templates which you can fill with real test assertions.
How to write tests read at [PHPUnit Manual - Chapter 2. Writing Tests for PHPUnit](https://phpunit.de/manual/6.5/en/writing-tests-for-phpunit.html).

#### Run

Command | Target
---- | ----
`$ composer test` | All tests
`$ composer test-apis` | Apis tests
`$ composer test-models` | Models tests

#### Config

Package contains fully functional config `./phpunit.xml.dist` file. Create `./phpunit.xml` in root folder to override it.

Quote from [3. The Command-Line Test Runner — PHPUnit 7.4 Manual](https://phpunit.readthedocs.io/en/7.4/textui.html#command-line-options):

> If phpunit.xml or phpunit.xml.dist (in that order) exist in the current working directory and --configuration is not used, the configuration will be automatically read from that file.

### PHP CodeSniffer

[PHP CodeSniffer Documentation](https://github.com/squizlabs/PHP_CodeSniffer/wiki). This tool helps to follow coding style and avoid common PHP coding mistakes.

#### Run

```bash
$ composer phpcs
```

#### Config

Package contains fully functional config `./phpcs.xml.dist` file. It checks source code against PSR-1 and PSR-2 coding standards.
Create `./phpcs.xml` in root folder to override it. More info at [Using a Default Configuration File](https://github.com/squizlabs/PHP_CodeSniffer/wiki/Advanced-Usage#using-a-default-configuration-file)

### PHPLint

[PHPLint Documentation](https://github.com/overtrue/phplint). Checks PHP syntax only.

#### Run

```bash
$ composer phplint
```

## Show errors

Switch on option in `./index.php`:
```diff
    /**
     * When true, additional information about exceptions are displayed by the default
     * error handler.
     * Default: false
     */
--- // 'displayErrorDetails' => false,
+++ 'displayErrorDetails' => true,
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
*AbstractFakeApi* | **createXmlItem** | **POST** /fake/create_xml_item | creates an XmlItem
*AbstractFakeApi* | **fakeOuterBooleanSerialize** | **POST** /fake/outer/boolean | 
*AbstractFakeApi* | **fakeOuterCompositeSerialize** | **POST** /fake/outer/composite | 
*AbstractFakeApi* | **fakeOuterNumberSerialize** | **POST** /fake/outer/number | 
*AbstractFakeApi* | **fakeOuterStringSerialize** | **POST** /fake/outer/string | 
*AbstractFakeApi* | **testBodyWithFileSchema** | **PUT** /fake/body-with-file-schema | 
*AbstractFakeApi* | **testBodyWithQueryParams** | **PUT** /fake/body-with-query-params | 
*AbstractFakeApi* | **testClientModel** | **PATCH** /fake | To test \"client\" model
*AbstractFakeApi* | **testEndpointParameters** | **POST** /fake | Fake endpoint for testing various parameters  假端點  偽のエンドポイント  가짜 엔드 포인트
*AbstractFakeApi* | **testEnumParameters** | **GET** /fake | To test enum parameters
*AbstractFakeApi* | **testGroupParameters** | **DELETE** /fake | Fake endpoint to test group parameters (optional)
*AbstractFakeApi* | **testInlineAdditionalProperties** | **POST** /fake/inline-additionalProperties | test inline additionalProperties
*AbstractFakeApi* | **testJsonFormData** | **GET** /fake/jsonFormData | test json serialization of form data
*AbstractFakeApi* | **testQueryParameterCollectionFormat** | **PUT** /fake/test-query-paramters | 
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

* OpenAPIServer\Model\AdditionalPropertiesAnyType
* OpenAPIServer\Model\AdditionalPropertiesArray
* OpenAPIServer\Model\AdditionalPropertiesBoolean
* OpenAPIServer\Model\AdditionalPropertiesClass
* OpenAPIServer\Model\AdditionalPropertiesInteger
* OpenAPIServer\Model\AdditionalPropertiesNumber
* OpenAPIServer\Model\AdditionalPropertiesObject
* OpenAPIServer\Model\AdditionalPropertiesString
* OpenAPIServer\Model\Animal
* OpenAPIServer\Model\ApiResponse
* OpenAPIServer\Model\ArrayOfArrayOfNumberOnly
* OpenAPIServer\Model\ArrayOfNumberOnly
* OpenAPIServer\Model\ArrayTest
* OpenAPIServer\Model\BigCat
* OpenAPIServer\Model\BigCatAllOf
* OpenAPIServer\Model\Capitalization
* OpenAPIServer\Model\Cat
* OpenAPIServer\Model\CatAllOf
* OpenAPIServer\Model\Category
* OpenAPIServer\Model\ClassModel
* OpenAPIServer\Model\Client
* OpenAPIServer\Model\Dog
* OpenAPIServer\Model\DogAllOf
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
* OpenAPIServer\Model\TypeHolderDefault
* OpenAPIServer\Model\TypeHolderExample
* OpenAPIServer\Model\User
* OpenAPIServer\Model\XmlItem


## Authentication

### Security schema `api_key`
> Important! To make ApiKey authentication work you need to extend [\OpenAPIServer\Auth\AbstractAuthenticator](./lib/Auth/AbstractAuthenticator.php) class by [\OpenAPIServer\Auth\ApiKeyAuthenticator](./src/Auth/ApiKeyAuthenticator.php) class.

### Security schema `api_key_query`
> Important! To make ApiKey authentication work you need to extend [\OpenAPIServer\Auth\AbstractAuthenticator](./lib/Auth/AbstractAuthenticator.php) class by [\OpenAPIServer\Auth\ApiKeyAuthenticator](./src/Auth/ApiKeyAuthenticator.php) class.

### Security schema `http_basic_test`
> Important! To make Basic authentication work you need to extend [\OpenAPIServer\Auth\AbstractAuthenticator](./lib/Auth/AbstractAuthenticator.php) class by [\OpenAPIServer\Auth\BasicAuthenticator](./src/Auth/BasicAuthenticator.php) class.

### Security schema `petstore_auth`
> Important! To make OAuth authentication work you need to extend [\OpenAPIServer\Auth\AbstractAuthenticator](./lib/Auth/AbstractAuthenticator.php) class by [\OpenAPIServer\Auth\OAuthAuthenticator](./src/Auth/OAuthAuthenticator.php) class.

Scope list:
* `write:pets` - modify pets in your account
* `read:pets` - read your pets

### Advanced middleware configuration
Ref to used Slim Token Middleware [dyorg/slim-token-authentication](https://github.com/dyorg/slim-token-authentication/tree/1.x#readme)
