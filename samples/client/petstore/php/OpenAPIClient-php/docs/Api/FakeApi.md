# OpenAPI\Client\FakeApi

All URIs are relative to http://petstore.swagger.io:80/v2.

Method | HTTP request | Description
------------- | ------------- | -------------
[**fakeHealthGet()**](FakeApi.md#fakeHealthGet) | **GET** /fake/health | Health check endpoint
[**fakeHttpSignatureTest()**](FakeApi.md#fakeHttpSignatureTest) | **GET** /fake/http-signature-test | test http signature authentication
[**fakeOuterBooleanSerialize()**](FakeApi.md#fakeOuterBooleanSerialize) | **POST** /fake/outer/boolean | 
[**fakeOuterCompositeSerialize()**](FakeApi.md#fakeOuterCompositeSerialize) | **POST** /fake/outer/composite | 
[**fakeOuterNumberSerialize()**](FakeApi.md#fakeOuterNumberSerialize) | **POST** /fake/outer/number | 
[**fakeOuterStringSerialize()**](FakeApi.md#fakeOuterStringSerialize) | **POST** /fake/outer/string | 
[**fakePropertyEnumIntegerSerialize()**](FakeApi.md#fakePropertyEnumIntegerSerialize) | **POST** /fake/property/enum-int | 
[**testBodyWithBinary()**](FakeApi.md#testBodyWithBinary) | **PUT** /fake/body-with-binary | 
[**testBodyWithFileSchema()**](FakeApi.md#testBodyWithFileSchema) | **PUT** /fake/body-with-file-schema | 
[**testBodyWithQueryParams()**](FakeApi.md#testBodyWithQueryParams) | **PUT** /fake/body-with-query-params | 
[**testClientModel()**](FakeApi.md#testClientModel) | **PATCH** /fake | To test \&quot;client\&quot; model
[**testEndpointParameters()**](FakeApi.md#testEndpointParameters) | **POST** /fake | Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트
[**testEnumParameters()**](FakeApi.md#testEnumParameters) | **GET** /fake | To test enum parameters
[**testGroupParameters()**](FakeApi.md#testGroupParameters) | **DELETE** /fake | Fake endpoint to test group parameters (optional)
[**testInlineAdditionalProperties()**](FakeApi.md#testInlineAdditionalProperties) | **POST** /fake/inline-additionalProperties | test inline additionalProperties
[**testJsonFormData()**](FakeApi.md#testJsonFormData) | **GET** /fake/jsonFormData | test json serialization of form data
[**testQueryParameterCollectionFormat()**](FakeApi.md#testQueryParameterCollectionFormat) | **PUT** /fake/test-query-parameters | 


## `fakeHealthGet()`

```php
fakeHealthGet(): \OpenAPI\Client\Model\HealthCheckResult
```

Health check endpoint

### Example

```php
<?php
require_once(__DIR__ . '/vendor/autoload.php');



$apiInstance = new OpenAPI\Client\Api\FakeApi(
    // If you want use custom http client, pass your client which implements `GuzzleHttp\ClientInterface`.
    // This is optional, `GuzzleHttp\Client` will be used as default.
    new GuzzleHttp\Client()
);

try {
    $result = $apiInstance->fakeHealthGet();
    print_r($result);
} catch (Exception $e) {
    echo 'Exception when calling FakeApi->fakeHealthGet: ', $e->getMessage(), PHP_EOL;
}
```

### Parameters

This endpoint does not need any parameter.

### Return type

[**\OpenAPI\Client\Model\HealthCheckResult**](../Model/HealthCheckResult.md)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: `application/json`

[[Back to top]](#) [[Back to API list]](../../README.md#endpoints)
[[Back to Model list]](../../README.md#models)
[[Back to README]](../../README.md)

## `fakeHttpSignatureTest()`

```php
fakeHttpSignatureTest($pet, $query_1, $header_1)
```

test http signature authentication

### Example

```php
<?php
require_once(__DIR__ . '/vendor/autoload.php');




$apiInstance = new OpenAPI\Client\Api\FakeApi(
    // If you want use custom http client, pass your client which implements `GuzzleHttp\ClientInterface`.
    // This is optional, `GuzzleHttp\Client` will be used as default.
    new GuzzleHttp\Client(),
    $config
);
$pet = new \OpenAPI\Client\Model\Pet(); // \OpenAPI\Client\Model\Pet | Pet object that needs to be added to the store
$query_1 = 'query_1_example'; // string | query parameter
$header_1 = 'header_1_example'; // string | header parameter

try {
    $apiInstance->fakeHttpSignatureTest($pet, $query_1, $header_1);
} catch (Exception $e) {
    echo 'Exception when calling FakeApi->fakeHttpSignatureTest: ', $e->getMessage(), PHP_EOL;
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **pet** | [**\OpenAPI\Client\Model\Pet**](../Model/Pet.md)| Pet object that needs to be added to the store |
 **query_1** | **string**| query parameter | [optional]
 **header_1** | **string**| header parameter | [optional]

### Return type

void (empty response body)

### Authorization

[http_signature_test](../../README.md#http_signature_test)

### HTTP request headers

- **Content-Type**: `application/json`, `application/xml`
- **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../../README.md#endpoints)
[[Back to Model list]](../../README.md#models)
[[Back to README]](../../README.md)

## `fakeOuterBooleanSerialize()`

```php
fakeOuterBooleanSerialize($body): bool
```



Test serialization of outer boolean types

### Example

```php
<?php
require_once(__DIR__ . '/vendor/autoload.php');



$apiInstance = new OpenAPI\Client\Api\FakeApi(
    // If you want use custom http client, pass your client which implements `GuzzleHttp\ClientInterface`.
    // This is optional, `GuzzleHttp\Client` will be used as default.
    new GuzzleHttp\Client()
);
$body = True; // bool | Input boolean as post body

try {
    $result = $apiInstance->fakeOuterBooleanSerialize($body);
    print_r($result);
} catch (Exception $e) {
    echo 'Exception when calling FakeApi->fakeOuterBooleanSerialize: ', $e->getMessage(), PHP_EOL;
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | **bool**| Input boolean as post body | [optional]

### Return type

**bool**

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: `application/json`
- **Accept**: `*/*`

[[Back to top]](#) [[Back to API list]](../../README.md#endpoints)
[[Back to Model list]](../../README.md#models)
[[Back to README]](../../README.md)

## `fakeOuterCompositeSerialize()`

```php
fakeOuterCompositeSerialize($outer_composite): \OpenAPI\Client\Model\OuterComposite
```



Test serialization of object with outer number type

### Example

```php
<?php
require_once(__DIR__ . '/vendor/autoload.php');



$apiInstance = new OpenAPI\Client\Api\FakeApi(
    // If you want use custom http client, pass your client which implements `GuzzleHttp\ClientInterface`.
    // This is optional, `GuzzleHttp\Client` will be used as default.
    new GuzzleHttp\Client()
);
$outer_composite = new \OpenAPI\Client\Model\OuterComposite(); // \OpenAPI\Client\Model\OuterComposite | Input composite as post body

try {
    $result = $apiInstance->fakeOuterCompositeSerialize($outer_composite);
    print_r($result);
} catch (Exception $e) {
    echo 'Exception when calling FakeApi->fakeOuterCompositeSerialize: ', $e->getMessage(), PHP_EOL;
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **outer_composite** | [**\OpenAPI\Client\Model\OuterComposite**](../Model/OuterComposite.md)| Input composite as post body | [optional]

### Return type

[**\OpenAPI\Client\Model\OuterComposite**](../Model/OuterComposite.md)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: `application/json`
- **Accept**: `*/*`

[[Back to top]](#) [[Back to API list]](../../README.md#endpoints)
[[Back to Model list]](../../README.md#models)
[[Back to README]](../../README.md)

## `fakeOuterNumberSerialize()`

```php
fakeOuterNumberSerialize($body): float
```



Test serialization of outer number types

### Example

```php
<?php
require_once(__DIR__ . '/vendor/autoload.php');



$apiInstance = new OpenAPI\Client\Api\FakeApi(
    // If you want use custom http client, pass your client which implements `GuzzleHttp\ClientInterface`.
    // This is optional, `GuzzleHttp\Client` will be used as default.
    new GuzzleHttp\Client()
);
$body = 3.4; // float | Input number as post body

try {
    $result = $apiInstance->fakeOuterNumberSerialize($body);
    print_r($result);
} catch (Exception $e) {
    echo 'Exception when calling FakeApi->fakeOuterNumberSerialize: ', $e->getMessage(), PHP_EOL;
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | **float**| Input number as post body | [optional]

### Return type

**float**

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: `application/json`
- **Accept**: `*/*`

[[Back to top]](#) [[Back to API list]](../../README.md#endpoints)
[[Back to Model list]](../../README.md#models)
[[Back to README]](../../README.md)

## `fakeOuterStringSerialize()`

```php
fakeOuterStringSerialize($body): string
```



Test serialization of outer string types

### Example

```php
<?php
require_once(__DIR__ . '/vendor/autoload.php');



$apiInstance = new OpenAPI\Client\Api\FakeApi(
    // If you want use custom http client, pass your client which implements `GuzzleHttp\ClientInterface`.
    // This is optional, `GuzzleHttp\Client` will be used as default.
    new GuzzleHttp\Client()
);
$body = 'body_example'; // string | Input string as post body

try {
    $result = $apiInstance->fakeOuterStringSerialize($body);
    print_r($result);
} catch (Exception $e) {
    echo 'Exception when calling FakeApi->fakeOuterStringSerialize: ', $e->getMessage(), PHP_EOL;
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | **string**| Input string as post body | [optional]

### Return type

**string**

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: `application/json`
- **Accept**: `*/*`

[[Back to top]](#) [[Back to API list]](../../README.md#endpoints)
[[Back to Model list]](../../README.md#models)
[[Back to README]](../../README.md)

## `fakePropertyEnumIntegerSerialize()`

```php
fakePropertyEnumIntegerSerialize($outer_object_with_enum_property): \OpenAPI\Client\Model\OuterObjectWithEnumProperty
```



Test serialization of enum (int) properties with examples

### Example

```php
<?php
require_once(__DIR__ . '/vendor/autoload.php');



$apiInstance = new OpenAPI\Client\Api\FakeApi(
    // If you want use custom http client, pass your client which implements `GuzzleHttp\ClientInterface`.
    // This is optional, `GuzzleHttp\Client` will be used as default.
    new GuzzleHttp\Client()
);
$outer_object_with_enum_property = new \OpenAPI\Client\Model\OuterObjectWithEnumProperty(); // \OpenAPI\Client\Model\OuterObjectWithEnumProperty | Input enum (int) as post body

try {
    $result = $apiInstance->fakePropertyEnumIntegerSerialize($outer_object_with_enum_property);
    print_r($result);
} catch (Exception $e) {
    echo 'Exception when calling FakeApi->fakePropertyEnumIntegerSerialize: ', $e->getMessage(), PHP_EOL;
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **outer_object_with_enum_property** | [**\OpenAPI\Client\Model\OuterObjectWithEnumProperty**](../Model/OuterObjectWithEnumProperty.md)| Input enum (int) as post body |

### Return type

[**\OpenAPI\Client\Model\OuterObjectWithEnumProperty**](../Model/OuterObjectWithEnumProperty.md)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: `application/json`
- **Accept**: `*/*`

[[Back to top]](#) [[Back to API list]](../../README.md#endpoints)
[[Back to Model list]](../../README.md#models)
[[Back to README]](../../README.md)

## `testBodyWithBinary()`

```php
testBodyWithBinary($body)
```



For this test, the body has to be a binary file.

### Example

```php
<?php
require_once(__DIR__ . '/vendor/autoload.php');



$apiInstance = new OpenAPI\Client\Api\FakeApi(
    // If you want use custom http client, pass your client which implements `GuzzleHttp\ClientInterface`.
    // This is optional, `GuzzleHttp\Client` will be used as default.
    new GuzzleHttp\Client()
);
$body = "/path/to/file.txt"; // \SplFileObject | image to upload

try {
    $apiInstance->testBodyWithBinary($body);
} catch (Exception $e) {
    echo 'Exception when calling FakeApi->testBodyWithBinary: ', $e->getMessage(), PHP_EOL;
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | **\SplFileObject****\SplFileObject**| image to upload |

### Return type

void (empty response body)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: `image/png`
- **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../../README.md#endpoints)
[[Back to Model list]](../../README.md#models)
[[Back to README]](../../README.md)

## `testBodyWithFileSchema()`

```php
testBodyWithFileSchema($file_schema_test_class)
```



For this test, the body for this request must reference a schema named `File`.

### Example

```php
<?php
require_once(__DIR__ . '/vendor/autoload.php');



$apiInstance = new OpenAPI\Client\Api\FakeApi(
    // If you want use custom http client, pass your client which implements `GuzzleHttp\ClientInterface`.
    // This is optional, `GuzzleHttp\Client` will be used as default.
    new GuzzleHttp\Client()
);
$file_schema_test_class = new \OpenAPI\Client\Model\FileSchemaTestClass(); // \OpenAPI\Client\Model\FileSchemaTestClass

try {
    $apiInstance->testBodyWithFileSchema($file_schema_test_class);
} catch (Exception $e) {
    echo 'Exception when calling FakeApi->testBodyWithFileSchema: ', $e->getMessage(), PHP_EOL;
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **file_schema_test_class** | [**\OpenAPI\Client\Model\FileSchemaTestClass**](../Model/FileSchemaTestClass.md)|  |

### Return type

void (empty response body)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: `application/json`
- **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../../README.md#endpoints)
[[Back to Model list]](../../README.md#models)
[[Back to README]](../../README.md)

## `testBodyWithQueryParams()`

```php
testBodyWithQueryParams($query, $user)
```



### Example

```php
<?php
require_once(__DIR__ . '/vendor/autoload.php');



$apiInstance = new OpenAPI\Client\Api\FakeApi(
    // If you want use custom http client, pass your client which implements `GuzzleHttp\ClientInterface`.
    // This is optional, `GuzzleHttp\Client` will be used as default.
    new GuzzleHttp\Client()
);
$query = 'query_example'; // string
$user = new \OpenAPI\Client\Model\User(); // \OpenAPI\Client\Model\User

try {
    $apiInstance->testBodyWithQueryParams($query, $user);
} catch (Exception $e) {
    echo 'Exception when calling FakeApi->testBodyWithQueryParams: ', $e->getMessage(), PHP_EOL;
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **query** | **string**|  |
 **user** | [**\OpenAPI\Client\Model\User**](../Model/User.md)|  |

### Return type

void (empty response body)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: `application/json`
- **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../../README.md#endpoints)
[[Back to Model list]](../../README.md#models)
[[Back to README]](../../README.md)

## `testClientModel()`

```php
testClientModel($client): \OpenAPI\Client\Model\Client
```

To test \"client\" model

To test \"client\" model

### Example

```php
<?php
require_once(__DIR__ . '/vendor/autoload.php');



$apiInstance = new OpenAPI\Client\Api\FakeApi(
    // If you want use custom http client, pass your client which implements `GuzzleHttp\ClientInterface`.
    // This is optional, `GuzzleHttp\Client` will be used as default.
    new GuzzleHttp\Client()
);
$client = new \OpenAPI\Client\Model\Client(); // \OpenAPI\Client\Model\Client | client model

try {
    $result = $apiInstance->testClientModel($client);
    print_r($result);
} catch (Exception $e) {
    echo 'Exception when calling FakeApi->testClientModel: ', $e->getMessage(), PHP_EOL;
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **client** | [**\OpenAPI\Client\Model\Client**](../Model/Client.md)| client model |

### Return type

[**\OpenAPI\Client\Model\Client**](../Model/Client.md)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: `application/json`
- **Accept**: `application/json`

[[Back to top]](#) [[Back to API list]](../../README.md#endpoints)
[[Back to Model list]](../../README.md#models)
[[Back to README]](../../README.md)

## `testEndpointParameters()`

```php
testEndpointParameters($number, $double, $pattern_without_delimiter, $byte, $integer, $int32, $int64, $float, $string, $binary, $date, $date_time, $password, $callback)
```

Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트

Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트

### Example

```php
<?php
require_once(__DIR__ . '/vendor/autoload.php');


// Configure HTTP basic authorization: http_basic_test
$config = OpenAPI\Client\Configuration::getDefaultConfiguration()
              ->setUsername('YOUR_USERNAME')
              ->setPassword('YOUR_PASSWORD');


$apiInstance = new OpenAPI\Client\Api\FakeApi(
    // If you want use custom http client, pass your client which implements `GuzzleHttp\ClientInterface`.
    // This is optional, `GuzzleHttp\Client` will be used as default.
    new GuzzleHttp\Client(),
    $config
);
$number = 3.4; // float | None
$double = 3.4; // double | None
$pattern_without_delimiter = 'pattern_without_delimiter_example'; // string | None
$byte = 'byte_example'; // string | None
$integer = 56; // int | None
$int32 = 56; // int | None
$int64 = 56; // int | None
$float = 3.4; // float | None
$string = 'string_example'; // string | None
$binary = "/path/to/file.txt"; // \SplFileObject | None
$date = new \DateTime("2013-10-20T19:20:30+01:00"); // \DateTime | None
$date_time = new \DateTime("2013-10-20T19:20:30+01:00"); // \DateTime | None
$password = 'password_example'; // string | None
$callback = 'callback_example'; // string | None

try {
    $apiInstance->testEndpointParameters($number, $double, $pattern_without_delimiter, $byte, $integer, $int32, $int64, $float, $string, $binary, $date, $date_time, $password, $callback);
} catch (Exception $e) {
    echo 'Exception when calling FakeApi->testEndpointParameters: ', $e->getMessage(), PHP_EOL;
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **number** | **float**| None |
 **double** | **double**| None |
 **pattern_without_delimiter** | **string**| None |
 **byte** | **string**| None |
 **integer** | **int**| None | [optional]
 **int32** | **int**| None | [optional]
 **int64** | **int**| None | [optional]
 **float** | **float**| None | [optional]
 **string** | **string**| None | [optional]
 **binary** | **\SplFileObject****\SplFileObject**| None | [optional]
 **date** | **\DateTime**| None | [optional]
 **date_time** | **\DateTime**| None | [optional]
 **password** | **string**| None | [optional]
 **callback** | **string**| None | [optional]

### Return type

void (empty response body)

### Authorization

[http_basic_test](../../README.md#http_basic_test)

### HTTP request headers

- **Content-Type**: `application/x-www-form-urlencoded`
- **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../../README.md#endpoints)
[[Back to Model list]](../../README.md#models)
[[Back to README]](../../README.md)

## `testEnumParameters()`

```php
testEnumParameters($enum_header_string_array, $enum_header_string, $enum_query_string_array, $enum_query_string, $enum_query_integer, $enum_query_double, $enum_form_string_array, $enum_form_string)
```

To test enum parameters

To test enum parameters

### Example

```php
<?php
require_once(__DIR__ . '/vendor/autoload.php');



$apiInstance = new OpenAPI\Client\Api\FakeApi(
    // If you want use custom http client, pass your client which implements `GuzzleHttp\ClientInterface`.
    // This is optional, `GuzzleHttp\Client` will be used as default.
    new GuzzleHttp\Client()
);
$enum_header_string_array = array('enum_header_string_array_example'); // string[] | Header parameter enum test (string array)
$enum_header_string = '-efg'; // string | Header parameter enum test (string)
$enum_query_string_array = array('enum_query_string_array_example'); // string[] | Query parameter enum test (string array)
$enum_query_string = '-efg'; // string | Query parameter enum test (string)
$enum_query_integer = 56; // int | Query parameter enum test (double)
$enum_query_double = 3.4; // double | Query parameter enum test (double)
$enum_form_string_array = array('$'); // string[] | Form parameter enum test (string array)
$enum_form_string = '-efg'; // string | Form parameter enum test (string)

try {
    $apiInstance->testEnumParameters($enum_header_string_array, $enum_header_string, $enum_query_string_array, $enum_query_string, $enum_query_integer, $enum_query_double, $enum_form_string_array, $enum_form_string);
} catch (Exception $e) {
    echo 'Exception when calling FakeApi->testEnumParameters: ', $e->getMessage(), PHP_EOL;
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **enum_header_string_array** | [**string[]**](../Model/string.md)| Header parameter enum test (string array) | [optional]
 **enum_header_string** | **string**| Header parameter enum test (string) | [optional] [default to &#39;-efg&#39;]
 **enum_query_string_array** | [**string[]**](../Model/string.md)| Query parameter enum test (string array) | [optional]
 **enum_query_string** | **string**| Query parameter enum test (string) | [optional] [default to &#39;-efg&#39;]
 **enum_query_integer** | **int**| Query parameter enum test (double) | [optional]
 **enum_query_double** | **double**| Query parameter enum test (double) | [optional]
 **enum_form_string_array** | [**string[]**](../Model/string.md)| Form parameter enum test (string array) | [optional] [default to &#39;$&#39;]
 **enum_form_string** | **string**| Form parameter enum test (string) | [optional] [default to &#39;-efg&#39;]

### Return type

void (empty response body)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: `application/x-www-form-urlencoded`
- **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../../README.md#endpoints)
[[Back to Model list]](../../README.md#models)
[[Back to README]](../../README.md)

## `testGroupParameters()`

```php
testGroupParameters($required_string_group, $required_boolean_group, $required_int64_group, $string_group, $boolean_group, $int64_group)
```

Fake endpoint to test group parameters (optional)

Fake endpoint to test group parameters (optional)

### Example

```php
<?php
require_once(__DIR__ . '/vendor/autoload.php');


// Configure Bearer (JWT) authorization: bearer_test
$config = OpenAPI\Client\Configuration::getDefaultConfiguration()->setAccessToken('YOUR_ACCESS_TOKEN');


$apiInstance = new OpenAPI\Client\Api\FakeApi(
    // If you want use custom http client, pass your client which implements `GuzzleHttp\ClientInterface`.
    // This is optional, `GuzzleHttp\Client` will be used as default.
    new GuzzleHttp\Client(),
    $config
);
$associate_array['required_string_group'] = 56; // int | Required String in group parameters
$associate_array['required_boolean_group'] = True; // bool | Required Boolean in group parameters
$associate_array['required_int64_group'] = 56; // int | Required Integer in group parameters
$associate_array['string_group'] = 56; // int | String in group parameters
$associate_array['boolean_group'] = True; // bool | Boolean in group parameters
$associate_array['int64_group'] = 56; // int | Integer in group parameters

try {
    $apiInstance->testGroupParameters($associate_array);
} catch (Exception $e) {
    echo 'Exception when calling FakeApi->testGroupParameters: ', $e->getMessage(), PHP_EOL;
}
```

### Parameters

Note: the input parameter is an associative array with the keys listed as the parameter name below.

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **required_string_group** | **int**| Required String in group parameters |
 **required_boolean_group** | **bool**| Required Boolean in group parameters |
 **required_int64_group** | **int**| Required Integer in group parameters |
 **string_group** | **int**| String in group parameters | [optional]
 **boolean_group** | **bool**| Boolean in group parameters | [optional]
 **int64_group** | **int**| Integer in group parameters | [optional]

### Return type

void (empty response body)

### Authorization

[bearer_test](../../README.md#bearer_test)

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../../README.md#endpoints)
[[Back to Model list]](../../README.md#models)
[[Back to README]](../../README.md)

## `testInlineAdditionalProperties()`

```php
testInlineAdditionalProperties($request_body)
```

test inline additionalProperties



### Example

```php
<?php
require_once(__DIR__ . '/vendor/autoload.php');



$apiInstance = new OpenAPI\Client\Api\FakeApi(
    // If you want use custom http client, pass your client which implements `GuzzleHttp\ClientInterface`.
    // This is optional, `GuzzleHttp\Client` will be used as default.
    new GuzzleHttp\Client()
);
$request_body = array('key' => 'request_body_example'); // array<string,string> | request body

try {
    $apiInstance->testInlineAdditionalProperties($request_body);
} catch (Exception $e) {
    echo 'Exception when calling FakeApi->testInlineAdditionalProperties: ', $e->getMessage(), PHP_EOL;
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **request_body** | [**array<string,string>**](../Model/string.md)| request body |

### Return type

void (empty response body)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: `application/json`
- **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../../README.md#endpoints)
[[Back to Model list]](../../README.md#models)
[[Back to README]](../../README.md)

## `testJsonFormData()`

```php
testJsonFormData($param, $param2)
```

test json serialization of form data



### Example

```php
<?php
require_once(__DIR__ . '/vendor/autoload.php');



$apiInstance = new OpenAPI\Client\Api\FakeApi(
    // If you want use custom http client, pass your client which implements `GuzzleHttp\ClientInterface`.
    // This is optional, `GuzzleHttp\Client` will be used as default.
    new GuzzleHttp\Client()
);
$param = 'param_example'; // string | field1
$param2 = 'param2_example'; // string | field2

try {
    $apiInstance->testJsonFormData($param, $param2);
} catch (Exception $e) {
    echo 'Exception when calling FakeApi->testJsonFormData: ', $e->getMessage(), PHP_EOL;
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **param** | **string**| field1 |
 **param2** | **string**| field2 |

### Return type

void (empty response body)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: `application/x-www-form-urlencoded`
- **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../../README.md#endpoints)
[[Back to Model list]](../../README.md#models)
[[Back to README]](../../README.md)

## `testQueryParameterCollectionFormat()`

```php
testQueryParameterCollectionFormat($pipe, $ioutil, $http, $url, $context, $allow_empty, $language)
```



To test the collection format in query parameters

### Example

```php
<?php
require_once(__DIR__ . '/vendor/autoload.php');



$apiInstance = new OpenAPI\Client\Api\FakeApi(
    // If you want use custom http client, pass your client which implements `GuzzleHttp\ClientInterface`.
    // This is optional, `GuzzleHttp\Client` will be used as default.
    new GuzzleHttp\Client()
);
$pipe = array('pipe_example'); // string[]
$ioutil = array('ioutil_example'); // string[]
$http = array('http_example'); // string[]
$url = array('url_example'); // string[]
$context = array('context_example'); // string[]
$allow_empty = 'allow_empty_example'; // string
$language = array('key' => 'language_example'); // array<string,string>

try {
    $apiInstance->testQueryParameterCollectionFormat($pipe, $ioutil, $http, $url, $context, $allow_empty, $language);
} catch (Exception $e) {
    echo 'Exception when calling FakeApi->testQueryParameterCollectionFormat: ', $e->getMessage(), PHP_EOL;
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **pipe** | [**string[]**](../Model/string.md)|  |
 **ioutil** | [**string[]**](../Model/string.md)|  |
 **http** | [**string[]**](../Model/string.md)|  |
 **url** | [**string[]**](../Model/string.md)|  |
 **context** | [**string[]**](../Model/string.md)|  |
 **allow_empty** | **string**|  |
 **language** | [**array<string,string>**](../Model/string.md)|  | [optional]

### Return type

void (empty response body)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../../README.md#endpoints)
[[Back to Model list]](../../README.md#models)
[[Back to README]](../../README.md)
