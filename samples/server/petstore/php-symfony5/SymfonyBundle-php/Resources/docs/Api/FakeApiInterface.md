# OpenAPI\Server\Api\FakeApiInterface

All URIs are relative to *http://petstore.swagger.io:80/v2*

Method | HTTP request | Description
------------- | ------------- | -------------
[**fakeHealthGet**](FakeApiInterface.md#fakeHealthGet) | **GET** /fake/health | Health check endpoint
[**fakeHttpSignatureTest**](FakeApiInterface.md#fakeHttpSignatureTest) | **GET** /fake/http-signature-test | test http signature authentication
[**fakeOuterBooleanSerialize**](FakeApiInterface.md#fakeOuterBooleanSerialize) | **POST** /fake/outer/boolean | 
[**fakeOuterCompositeSerialize**](FakeApiInterface.md#fakeOuterCompositeSerialize) | **POST** /fake/outer/composite | 
[**fakeOuterNumberSerialize**](FakeApiInterface.md#fakeOuterNumberSerialize) | **POST** /fake/outer/number | 
[**fakeOuterStringSerialize**](FakeApiInterface.md#fakeOuterStringSerialize) | **POST** /fake/outer/string | 
[**testBodyWithFileSchema**](FakeApiInterface.md#testBodyWithFileSchema) | **PUT** /fake/body-with-file-schema | 
[**testBodyWithQueryParams**](FakeApiInterface.md#testBodyWithQueryParams) | **PUT** /fake/body-with-query-params | 
[**testClientModel**](FakeApiInterface.md#testClientModel) | **PATCH** /fake | To test \&quot;client\&quot; model
[**testEndpointParameters**](FakeApiInterface.md#testEndpointParameters) | **POST** /fake | Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트
[**testEnumParameters**](FakeApiInterface.md#testEnumParameters) | **GET** /fake | To test enum parameters
[**testGroupParameters**](FakeApiInterface.md#testGroupParameters) | **DELETE** /fake | Fake endpoint to test group parameters (optional)
[**testInlineAdditionalProperties**](FakeApiInterface.md#testInlineAdditionalProperties) | **POST** /fake/inline-additionalProperties | test inline additionalProperties
[**testJsonFormData**](FakeApiInterface.md#testJsonFormData) | **GET** /fake/jsonFormData | test json serialization of form data
[**testQueryParameterCollectionFormat**](FakeApiInterface.md#testQueryParameterCollectionFormat) | **PUT** /fake/test-query-paramters | 


## Service Declaration
```yaml
# src/Acme/MyBundle/Resources/services.yml
services:
    # ...
    acme.my_bundle.api.fake:
        class: Acme\MyBundle\Api\FakeApi
        tags:
            - { name: "open_api_server.api", api: "fake" }
    # ...
```

## **fakeHealthGet**
> OpenAPI\Server\Model\HealthCheckResult fakeHealthGet()

Health check endpoint

### Example Implementation
```php
<?php
// src/Acme/MyBundle/Api/FakeApiInterface.php

namespace Acme\MyBundle\Api;

use OpenAPI\Server\Api\FakeApiInterface;

class FakeApi implements FakeApiInterface
{

    // ...

    /**
     * Implementation of FakeApiInterface#fakeHealthGet
     */
    public function fakeHealthGet()
    {
        // Implement the operation ...
    }

    // ...
}
```

### Parameters
This endpoint does not need any parameter.

### Return type

[**OpenAPI\Server\Model\HealthCheckResult**](../Model/HealthCheckResult.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../../README.md#documentation-for-api-endpoints) [[Back to Model list]](../../README.md#documentation-for-models) [[Back to README]](../../README.md)

## **fakeHttpSignatureTest**
> fakeHttpSignatureTest($pet, $query1, $header1)

test http signature authentication

### Example Implementation
```php
<?php
// src/Acme/MyBundle/Api/FakeApiInterface.php

namespace Acme\MyBundle\Api;

use OpenAPI\Server\Api\FakeApiInterface;

class FakeApi implements FakeApiInterface
{

    // ...

    /**
     * Implementation of FakeApiInterface#fakeHttpSignatureTest
     */
    public function fakeHttpSignatureTest(Pet $pet, $query1 = null, $header1 = null)
    {
        // Implement the operation ...
    }

    // ...
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **pet** | [**OpenAPI\Server\Model\Pet**](../Model/Pet.md)| Pet object that needs to be added to the store |
 **query1** | **string**| query parameter | [optional]
 **header1** | **string**| header parameter | [optional]

### Return type

void (empty response body)

### Authorization

[http_signature_test](../../README.md#http_signature_test)

### HTTP request headers

 - **Content-Type**: application/json, application/xml
 - **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../../README.md#documentation-for-api-endpoints) [[Back to Model list]](../../README.md#documentation-for-models) [[Back to README]](../../README.md)

## **fakeOuterBooleanSerialize**
> bool fakeOuterBooleanSerialize($body)



Test serialization of outer boolean types

### Example Implementation
```php
<?php
// src/Acme/MyBundle/Api/FakeApiInterface.php

namespace Acme\MyBundle\Api;

use OpenAPI\Server\Api\FakeApiInterface;

class FakeApi implements FakeApiInterface
{

    // ...

    /**
     * Implementation of FakeApiInterface#fakeOuterBooleanSerialize
     */
    public function fakeOuterBooleanSerialize($body = null)
    {
        // Implement the operation ...
    }

    // ...
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

 - **Content-Type**: application/json
 - **Accept**: */*

[[Back to top]](#) [[Back to API list]](../../README.md#documentation-for-api-endpoints) [[Back to Model list]](../../README.md#documentation-for-models) [[Back to README]](../../README.md)

## **fakeOuterCompositeSerialize**
> OpenAPI\Server\Model\OuterComposite fakeOuterCompositeSerialize($outerComposite)



Test serialization of object with outer number type

### Example Implementation
```php
<?php
// src/Acme/MyBundle/Api/FakeApiInterface.php

namespace Acme\MyBundle\Api;

use OpenAPI\Server\Api\FakeApiInterface;

class FakeApi implements FakeApiInterface
{

    // ...

    /**
     * Implementation of FakeApiInterface#fakeOuterCompositeSerialize
     */
    public function fakeOuterCompositeSerialize(OuterComposite $outerComposite = null)
    {
        // Implement the operation ...
    }

    // ...
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **outerComposite** | [**OpenAPI\Server\Model\OuterComposite**](../Model/OuterComposite.md)| Input composite as post body | [optional]

### Return type

[**OpenAPI\Server\Model\OuterComposite**](../Model/OuterComposite.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: */*

[[Back to top]](#) [[Back to API list]](../../README.md#documentation-for-api-endpoints) [[Back to Model list]](../../README.md#documentation-for-models) [[Back to README]](../../README.md)

## **fakeOuterNumberSerialize**
> float fakeOuterNumberSerialize($body)



Test serialization of outer number types

### Example Implementation
```php
<?php
// src/Acme/MyBundle/Api/FakeApiInterface.php

namespace Acme\MyBundle\Api;

use OpenAPI\Server\Api\FakeApiInterface;

class FakeApi implements FakeApiInterface
{

    // ...

    /**
     * Implementation of FakeApiInterface#fakeOuterNumberSerialize
     */
    public function fakeOuterNumberSerialize($body = null)
    {
        // Implement the operation ...
    }

    // ...
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

 - **Content-Type**: application/json
 - **Accept**: */*

[[Back to top]](#) [[Back to API list]](../../README.md#documentation-for-api-endpoints) [[Back to Model list]](../../README.md#documentation-for-models) [[Back to README]](../../README.md)

## **fakeOuterStringSerialize**
> string fakeOuterStringSerialize($body)



Test serialization of outer string types

### Example Implementation
```php
<?php
// src/Acme/MyBundle/Api/FakeApiInterface.php

namespace Acme\MyBundle\Api;

use OpenAPI\Server\Api\FakeApiInterface;

class FakeApi implements FakeApiInterface
{

    // ...

    /**
     * Implementation of FakeApiInterface#fakeOuterStringSerialize
     */
    public function fakeOuterStringSerialize($body = null)
    {
        // Implement the operation ...
    }

    // ...
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

 - **Content-Type**: application/json
 - **Accept**: */*

[[Back to top]](#) [[Back to API list]](../../README.md#documentation-for-api-endpoints) [[Back to Model list]](../../README.md#documentation-for-models) [[Back to README]](../../README.md)

## **testBodyWithFileSchema**
> testBodyWithFileSchema($fileSchemaTestClass)



For this test, the body for this request much reference a schema named `File`.

### Example Implementation
```php
<?php
// src/Acme/MyBundle/Api/FakeApiInterface.php

namespace Acme\MyBundle\Api;

use OpenAPI\Server\Api\FakeApiInterface;

class FakeApi implements FakeApiInterface
{

    // ...

    /**
     * Implementation of FakeApiInterface#testBodyWithFileSchema
     */
    public function testBodyWithFileSchema(FileSchemaTestClass $fileSchemaTestClass)
    {
        // Implement the operation ...
    }

    // ...
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **fileSchemaTestClass** | [**OpenAPI\Server\Model\FileSchemaTestClass**](../Model/FileSchemaTestClass.md)|  |

### Return type

void (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../../README.md#documentation-for-api-endpoints) [[Back to Model list]](../../README.md#documentation-for-models) [[Back to README]](../../README.md)

## **testBodyWithQueryParams**
> testBodyWithQueryParams($query, $user)



### Example Implementation
```php
<?php
// src/Acme/MyBundle/Api/FakeApiInterface.php

namespace Acme\MyBundle\Api;

use OpenAPI\Server\Api\FakeApiInterface;

class FakeApi implements FakeApiInterface
{

    // ...

    /**
     * Implementation of FakeApiInterface#testBodyWithQueryParams
     */
    public function testBodyWithQueryParams($query, User $user)
    {
        // Implement the operation ...
    }

    // ...
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **query** | **string**|  |
 **user** | [**OpenAPI\Server\Model\User**](../Model/User.md)|  |

### Return type

void (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../../README.md#documentation-for-api-endpoints) [[Back to Model list]](../../README.md#documentation-for-models) [[Back to README]](../../README.md)

## **testClientModel**
> OpenAPI\Server\Model\Client testClientModel($client)

To test \"client\" model

To test \"client\" model

### Example Implementation
```php
<?php
// src/Acme/MyBundle/Api/FakeApiInterface.php

namespace Acme\MyBundle\Api;

use OpenAPI\Server\Api\FakeApiInterface;

class FakeApi implements FakeApiInterface
{

    // ...

    /**
     * Implementation of FakeApiInterface#testClientModel
     */
    public function testClientModel(Client $client)
    {
        // Implement the operation ...
    }

    // ...
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **client** | [**OpenAPI\Server\Model\Client**](../Model/Client.md)| client model |

### Return type

[**OpenAPI\Server\Model\Client**](../Model/Client.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../../README.md#documentation-for-api-endpoints) [[Back to Model list]](../../README.md#documentation-for-models) [[Back to README]](../../README.md)

## **testEndpointParameters**
> testEndpointParameters($number, $double, $patternWithoutDelimiter, $byte, $integer, $int32, $int64, $float, $string, $binary, $date, $dateTime, $password, $callback)

Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트

Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트

### Example Implementation
```php
<?php
// src/Acme/MyBundle/Api/FakeApiInterface.php

namespace Acme\MyBundle\Api;

use OpenAPI\Server\Api\FakeApiInterface;

class FakeApi implements FakeApiInterface
{

    // ...

    /**
     * Implementation of FakeApiInterface#testEndpointParameters
     */
    public function testEndpointParameters($number, $double, $patternWithoutDelimiter, $byte, $integer = null, $int32 = null, $int64 = null, $float = null, $string = null, UploadedFile $binary = null, \DateTime $date = null, \DateTime $dateTime = null, $password = null, $callback = null)
    {
        // Implement the operation ...
    }

    // ...
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **number** | **float**| None |
 **double** | **double**| None |
 **patternWithoutDelimiter** | **string**| None |
 **byte** | **string**| None |
 **integer** | **int**| None | [optional]
 **int32** | **int**| None | [optional]
 **int64** | **int**| None | [optional]
 **float** | **float**| None | [optional]
 **string** | **string**| None | [optional]
 **binary** | **UploadedFile****UploadedFile**| None | [optional]
 **date** | **\DateTime**| None | [optional]
 **dateTime** | **\DateTime**| None | [optional]
 **password** | **string**| None | [optional]
 **callback** | **string**| None | [optional]

### Return type

void (empty response body)

### Authorization

[http_basic_test](../../README.md#http_basic_test)

### HTTP request headers

 - **Content-Type**: application/x-www-form-urlencoded
 - **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../../README.md#documentation-for-api-endpoints) [[Back to Model list]](../../README.md#documentation-for-models) [[Back to README]](../../README.md)

## **testEnumParameters**
> testEnumParameters($enumHeaderStringArray, $enumHeaderString, $enumQueryStringArray, $enumQueryString, $enumQueryInteger, $enumQueryDouble, $enumFormStringArray, $enumFormString)

To test enum parameters

To test enum parameters

### Example Implementation
```php
<?php
// src/Acme/MyBundle/Api/FakeApiInterface.php

namespace Acme\MyBundle\Api;

use OpenAPI\Server\Api\FakeApiInterface;

class FakeApi implements FakeApiInterface
{

    // ...

    /**
     * Implementation of FakeApiInterface#testEnumParameters
     */
    public function testEnumParameters(array $enumHeaderStringArray = null, $enumHeaderString = '''-efg''', array $enumQueryStringArray = null, $enumQueryString = '''-efg''', $enumQueryInteger = null, $enumQueryDouble = null, array $enumFormStringArray = '''$''', $enumFormString = '''-efg''')
    {
        // Implement the operation ...
    }

    // ...
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **enumHeaderStringArray** | [**string**](../Model/string.md)| Header parameter enum test (string array) | [optional]
 **enumHeaderString** | **string**| Header parameter enum test (string) | [optional] [default to &#39;&#39;-efg&#39;&#39;]
 **enumQueryStringArray** | [**string**](../Model/string.md)| Query parameter enum test (string array) | [optional]
 **enumQueryString** | **string**| Query parameter enum test (string) | [optional] [default to &#39;&#39;-efg&#39;&#39;]
 **enumQueryInteger** | **int**| Query parameter enum test (double) | [optional]
 **enumQueryDouble** | **double**| Query parameter enum test (double) | [optional]
 **enumFormStringArray** | [**string**](../Model/string.md)| Form parameter enum test (string array) | [optional] [default to &#39;&#39;$&#39;&#39;]
 **enumFormString** | **string**| Form parameter enum test (string) | [optional] [default to &#39;&#39;-efg&#39;&#39;]

### Return type

void (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/x-www-form-urlencoded
 - **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../../README.md#documentation-for-api-endpoints) [[Back to Model list]](../../README.md#documentation-for-models) [[Back to README]](../../README.md)

## **testGroupParameters**
> testGroupParameters($requiredStringGroup, $requiredBooleanGroup, $requiredInt64Group, $stringGroup, $booleanGroup, $int64Group)

Fake endpoint to test group parameters (optional)

Fake endpoint to test group parameters (optional)

### Example Implementation
```php
<?php
// src/Acme/MyBundle/Api/FakeApiInterface.php

namespace Acme\MyBundle\Api;

use OpenAPI\Server\Api\FakeApiInterface;

class FakeApi implements FakeApiInterface
{

    // ...

    /**
     * Implementation of FakeApiInterface#testGroupParameters
     */
    public function testGroupParameters($requiredStringGroup, $requiredBooleanGroup, $requiredInt64Group, $stringGroup = null, $booleanGroup = null, $int64Group = null)
    {
        // Implement the operation ...
    }

    // ...
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **requiredStringGroup** | **int**| Required String in group parameters |
 **requiredBooleanGroup** | **bool**| Required Boolean in group parameters |
 **requiredInt64Group** | **int**| Required Integer in group parameters |
 **stringGroup** | **int**| String in group parameters | [optional]
 **booleanGroup** | **bool**| Boolean in group parameters | [optional]
 **int64Group** | **int**| Integer in group parameters | [optional]

### Return type

void (empty response body)

### Authorization

[bearer_test](../../README.md#bearer_test)

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../../README.md#documentation-for-api-endpoints) [[Back to Model list]](../../README.md#documentation-for-models) [[Back to README]](../../README.md)

## **testInlineAdditionalProperties**
> testInlineAdditionalProperties($requestBody)

test inline additionalProperties

### Example Implementation
```php
<?php
// src/Acme/MyBundle/Api/FakeApiInterface.php

namespace Acme\MyBundle\Api;

use OpenAPI\Server\Api\FakeApiInterface;

class FakeApi implements FakeApiInterface
{

    // ...

    /**
     * Implementation of FakeApiInterface#testInlineAdditionalProperties
     */
    public function testInlineAdditionalProperties(array $requestBody)
    {
        // Implement the operation ...
    }

    // ...
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **requestBody** | [**string**](../Model/string.md)| request body |

### Return type

void (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../../README.md#documentation-for-api-endpoints) [[Back to Model list]](../../README.md#documentation-for-models) [[Back to README]](../../README.md)

## **testJsonFormData**
> testJsonFormData($param, $param2)

test json serialization of form data

### Example Implementation
```php
<?php
// src/Acme/MyBundle/Api/FakeApiInterface.php

namespace Acme\MyBundle\Api;

use OpenAPI\Server\Api\FakeApiInterface;

class FakeApi implements FakeApiInterface
{

    // ...

    /**
     * Implementation of FakeApiInterface#testJsonFormData
     */
    public function testJsonFormData($param, $param2)
    {
        // Implement the operation ...
    }

    // ...
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

 - **Content-Type**: application/x-www-form-urlencoded
 - **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../../README.md#documentation-for-api-endpoints) [[Back to Model list]](../../README.md#documentation-for-models) [[Back to README]](../../README.md)

## **testQueryParameterCollectionFormat**
> testQueryParameterCollectionFormat($pipe, $ioutil, $http, $url, $context)



To test the collection format in query parameters

### Example Implementation
```php
<?php
// src/Acme/MyBundle/Api/FakeApiInterface.php

namespace Acme\MyBundle\Api;

use OpenAPI\Server\Api\FakeApiInterface;

class FakeApi implements FakeApiInterface
{

    // ...

    /**
     * Implementation of FakeApiInterface#testQueryParameterCollectionFormat
     */
    public function testQueryParameterCollectionFormat(array $pipe, array $ioutil, array $http, array $url, array $context)
    {
        // Implement the operation ...
    }

    // ...
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **pipe** | [**string**](../Model/string.md)|  |
 **ioutil** | [**string**](../Model/string.md)|  |
 **http** | [**string**](../Model/string.md)|  |
 **url** | [**string**](../Model/string.md)|  |
 **context** | [**string**](../Model/string.md)|  |

### Return type

void (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../../README.md#documentation-for-api-endpoints) [[Back to Model list]](../../README.md#documentation-for-models) [[Back to README]](../../README.md)

