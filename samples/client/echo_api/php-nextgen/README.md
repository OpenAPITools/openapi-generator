# OpenAPIClient-php

Echo Server API


## Installation & Usage

### Requirements

PHP 8.1 and later.

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



// Configure HTTP basic authorization: http_auth
$config = OpenAPI\Client\Configuration::getDefaultConfiguration()
              ->setUsername('YOUR_USERNAME')
              ->setPassword('YOUR_PASSWORD');


$apiInstance = new OpenAPI\Client\Api\AuthApi(
    // If you want use custom http client, pass your client which implements `GuzzleHttp\ClientInterface`.
    // This is optional, `GuzzleHttp\Client` will be used as default.
    new GuzzleHttp\Client(),
    $config
);

try {
    $result = $apiInstance->testAuthHttpBasic();
    print_r($result);
} catch (Exception $e) {
    echo 'Exception when calling AuthApi->testAuthHttpBasic: ', $e->getMessage(), PHP_EOL;
}

```

## API Endpoints

All URIs are relative to *http://localhost:3000*

Class | Method | HTTP request | Description
------------ | ------------- | ------------- | -------------
*AuthApi* | [**testAuthHttpBasic**](docs/Api/AuthApi.md#testauthhttpbasic) | **POST** /auth/http/basic | To test HTTP basic authentication
*BodyApi* | [**testBinaryGif**](docs/Api/BodyApi.md#testbinarygif) | **POST** /binary/gif | Test binary (gif) response body
*BodyApi* | [**testBodyApplicationOctetstreamBinary**](docs/Api/BodyApi.md#testbodyapplicationoctetstreambinary) | **POST** /body/application/octetstream/binary | Test body parameter(s)
*BodyApi* | [**testBodyMultipartFormdataArrayOfBinary**](docs/Api/BodyApi.md#testbodymultipartformdataarrayofbinary) | **POST** /body/application/octetstream/array_of_binary | Test array of binary in multipart mime
*BodyApi* | [**testEchoBodyFreeFormObjectResponseString**](docs/Api/BodyApi.md#testechobodyfreeformobjectresponsestring) | **POST** /echo/body/FreeFormObject/response_string | Test free form object
*BodyApi* | [**testEchoBodyPet**](docs/Api/BodyApi.md#testechobodypet) | **POST** /echo/body/Pet | Test body parameter(s)
*BodyApi* | [**testEchoBodyPetResponseString**](docs/Api/BodyApi.md#testechobodypetresponsestring) | **POST** /echo/body/Pet/response_string | Test empty response body
*BodyApi* | [**testEchoBodyTagResponseString**](docs/Api/BodyApi.md#testechobodytagresponsestring) | **POST** /echo/body/Tag/response_string | Test empty json (request body)
*FormApi* | [**testFormIntegerBooleanString**](docs/Api/FormApi.md#testformintegerbooleanstring) | **POST** /form/integer/boolean/string | Test form parameter(s)
*FormApi* | [**testFormOneof**](docs/Api/FormApi.md#testformoneof) | **POST** /form/oneof | Test form parameter(s) for oneOf schema
*HeaderApi* | [**testHeaderIntegerBooleanString**](docs/Api/HeaderApi.md#testheaderintegerbooleanstring) | **GET** /header/integer/boolean/string | Test header parameter(s)
*PathApi* | [**testsPathStringPathStringIntegerPathInteger**](docs/Api/PathApi.md#testspathstringpathstringintegerpathinteger) | **GET** /path/string/{path_string}/integer/{path_integer} | Test path parameter(s)
*QueryApi* | [**testEnumRefString**](docs/Api/QueryApi.md#testenumrefstring) | **GET** /query/enum_ref_string | Test query parameter(s)
*QueryApi* | [**testQueryDatetimeDateString**](docs/Api/QueryApi.md#testquerydatetimedatestring) | **GET** /query/datetime/date/string | Test query parameter(s)
*QueryApi* | [**testQueryIntegerBooleanString**](docs/Api/QueryApi.md#testqueryintegerbooleanstring) | **GET** /query/integer/boolean/string | Test query parameter(s)
*QueryApi* | [**testQueryStyleDeepObjectExplodeTrueObject**](docs/Api/QueryApi.md#testquerystyledeepobjectexplodetrueobject) | **GET** /query/style_deepObject/explode_true/object | Test query parameter(s)
*QueryApi* | [**testQueryStyleDeepObjectExplodeTrueObjectAllOf**](docs/Api/QueryApi.md#testquerystyledeepobjectexplodetrueobjectallof) | **GET** /query/style_deepObject/explode_true/object/allOf | Test query parameter(s)
*QueryApi* | [**testQueryStyleFormExplodeTrueArrayString**](docs/Api/QueryApi.md#testquerystyleformexplodetruearraystring) | **GET** /query/style_form/explode_true/array_string | Test query parameter(s)
*QueryApi* | [**testQueryStyleFormExplodeTrueObject**](docs/Api/QueryApi.md#testquerystyleformexplodetrueobject) | **GET** /query/style_form/explode_true/object | Test query parameter(s)
*QueryApi* | [**testQueryStyleFormExplodeTrueObjectAllOf**](docs/Api/QueryApi.md#testquerystyleformexplodetrueobjectallof) | **GET** /query/style_form/explode_true/object/allOf | Test query parameter(s)

## Models

- [Bird](docs/Model/Bird.md)
- [Category](docs/Model/Category.md)
- [DataQuery](docs/Model/DataQuery.md)
- [DefaultValue](docs/Model/DefaultValue.md)
- [NumberPropertiesOnly](docs/Model/NumberPropertiesOnly.md)
- [Pet](docs/Model/Pet.md)
- [Query](docs/Model/Query.md)
- [StringEnumRef](docs/Model/StringEnumRef.md)
- [Tag](docs/Model/Tag.md)
- [TestQueryStyleDeepObjectExplodeTrueObjectAllOfQueryObjectParameter](docs/Model/TestQueryStyleDeepObjectExplodeTrueObjectAllOfQueryObjectParameter.md)
- [TestQueryStyleFormExplodeTrueArrayStringQueryObjectParameter](docs/Model/TestQueryStyleFormExplodeTrueArrayStringQueryObjectParameter.md)

## Authorization

### http_auth

- **Type**: HTTP basic authentication

## Tests

To run the tests, use:

```bash
composer install
vendor/bin/phpunit
```

## Author

team@openapitools.org

## About this package

This PHP package is automatically generated by the [OpenAPI Generator](https://openapi-generator.tech) project:

- API version: `0.1.0`
- Build package: `org.openapitools.codegen.languages.PhpNextgenClientCodegen`
