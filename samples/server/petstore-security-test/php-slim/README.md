# php-base - PHP Slim Server library for OpenAPI Petstore *_/ &#39; \&quot; &#x3D;end -- \\r\\n \\n \\r

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

All URIs are relative to *http://petstore.swagger.io *_/ ' \" =end -- \\r\\n \\n \\r/v2 *_/ ' \" =end -- \\r\\n \\n \\r*

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
*AbstractFakeApi* | **testCodeInjectEndRnNR** | **PUT** /fake | To test code injection *_/ ' \" =end -- \\r\\n \\n \\r


## Models

* OpenAPIServer\Model\ModelReturn


## Authentication

