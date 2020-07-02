# php-base - PHP Slim 4 Server library for OpenAPI Petstore

* [OpenAPI Generator](https://openapi-generator.tech)
* [Slim 4 Documentation](https://www.slimframework.com/docs/v4/)

This server has been generated with [Slim PSR-7](https://github.com/slimphp/Slim-Psr7) implementation.

## Requirements

* Web server with URL rewriting
* PHP 7.2 or newer

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

This package uses PHPUnit 8 or 9(depends from your PHP version) for unit testing.
[Test folder](test) contains templates which you can fill with real test assertions.
How to write tests read at [2. Writing Tests for PHPUnit - PHPUnit 8.5 Manual](https://phpunit.readthedocs.io/en/8.5/writing-tests-for-phpunit.html).

#### Run

Command | Target
---- | ----
`$ composer test` | All tests
`$ composer test-apis` | Apis tests
`$ composer test-models` | Models tests

#### Config

Package contains fully functional config `./phpunit.xml.dist` file. Create `./phpunit.xml` in root folder to override it.

Quote from [3. The Command-Line Test Runner — PHPUnit 8.5 Manual](https://phpunit.readthedocs.io/en/8.5/textui.html#command-line-options):

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
 * Add Error Handling Middleware
 *
 * @param bool $displayErrorDetails -> Should be set to false in production
 * @param bool $logErrors -> Parameter is passed to the default ErrorHandler
 * @param bool $logErrorDetails -> Display error details in error log
 * which can be replaced by a callable of your choice.

 * Note: This middleware should be added last. It will not handle any exceptions/errors
 * for middleware added after it.
 */
--- $app->addErrorMiddleware(false, true, true);
+++ $app->addErrorMiddleware(true, true, true);
```

## Mock Server
For a quick start uncomment [mocker middleware config](index.php#L62-L89).

Used packages:
* [Openapi Data Mocker](https://github.com/ybelenko/openapi-data-mocker) - first implementation of OAS3 fake data generator.
* [Openapi Data Mocker Server Middleware](https://github.com/ybelenko/openapi-data-mocker-server-middleware) - PSR-15 HTTP server middleware.
* [Openapi Data Mocker Interfaces](https://github.com/ybelenko/openapi-data-mocker-interfaces) - package with mocking interfaces.

## API Endpoints

All URIs are relative to *http://petstore.swagger.io/v2*

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
*AbstractPetApi* | **addPet** | **POST** /pet | Add a new pet to the store
*AbstractPetApi* | **findPetsByStatus** | **GET** /pet/findByStatus | Finds Pets by status
*AbstractPetApi* | **findPetsByTags** | **GET** /pet/findByTags | Finds Pets by tags
*AbstractPetApi* | **updatePet** | **PUT** /pet | Update an existing pet
*AbstractPetApi* | **deletePet** | **DELETE** /pet/{petId} | Deletes a pet
*AbstractPetApi* | **getPetById** | **GET** /pet/{petId} | Find pet by ID
*AbstractPetApi* | **updatePetWithForm** | **POST** /pet/{petId} | Updates a pet in the store with form data
*AbstractPetApi* | **uploadFile** | **POST** /pet/{petId}/uploadImage | uploads an image
*AbstractStoreApi* | **getInventory** | **GET** /store/inventory | Returns pet inventories by status
*AbstractStoreApi* | **placeOrder** | **POST** /store/order | Place an order for a pet
*AbstractStoreApi* | **deleteOrder** | **DELETE** /store/order/{orderId} | Delete purchase order by ID
*AbstractStoreApi* | **getOrderById** | **GET** /store/order/{orderId} | Find purchase order by ID
*AbstractUserApi* | **createUser** | **POST** /user | Create user
*AbstractUserApi* | **createUsersWithArrayInput** | **POST** /user/createWithArray | Creates list of users with given input array
*AbstractUserApi* | **createUsersWithListInput** | **POST** /user/createWithList | Creates list of users with given input array
*AbstractUserApi* | **loginUser** | **GET** /user/login | Logs user into the system
*AbstractUserApi* | **logoutUser** | **GET** /user/logout | Logs out current logged in user session
*AbstractUserApi* | **deleteUser** | **DELETE** /user/{username} | Delete user
*AbstractUserApi* | **getUserByName** | **GET** /user/{username} | Get user by user name
*AbstractUserApi* | **updateUser** | **PUT** /user/{username} | Updated user


## Models

* OpenAPIServer\Model\ApiResponse
* OpenAPIServer\Model\Category
* OpenAPIServer\Model\InlineObject
* OpenAPIServer\Model\InlineObject1
* OpenAPIServer\Model\Order
* OpenAPIServer\Model\Pet
* OpenAPIServer\Model\Tag
* OpenAPIServer\Model\User


## Authentication

### Security schema `api_key`
> Important! To make ApiKey authentication work you need to extend [\OpenAPIServer\Auth\AbstractAuthenticator](./lib/Auth/AbstractAuthenticator.php) class by [\OpenAPIServer\Auth\ApiKeyAuthenticator](./src/Auth/ApiKeyAuthenticator.php) class.

### Security schema `petstore_auth`
> Important! To make OAuth authentication work you need to extend [\OpenAPIServer\Auth\AbstractAuthenticator](./lib/Auth/AbstractAuthenticator.php) class by [\OpenAPIServer\Auth\OAuthAuthenticator](./src/Auth/OAuthAuthenticator.php) class.

Scope list:
* `write:pets` - modify pets in your account
* `read:pets` - read your pets

### Advanced middleware configuration
Ref to used Slim Token Middleware [dyorg/slim-token-authentication](https://github.com/dyorg/slim-token-authentication/tree/1.x#readme)
