# FakeApi

All URIs are relative to *http://petstore.swagger.io:80/v2*

| Method | HTTP request | Description |
|------------- | ------------- | -------------|
| [**fakeBigDecimalMap**](FakeApi.md#fakeBigDecimalMap) | **GET** /fake/BigDecimalMap |  |
| [**fakeHealthGet**](FakeApi.md#fakeHealthGet) | **GET** /fake/health | Health check endpoint |
| [**fakeHttpSignatureTest**](FakeApi.md#fakeHttpSignatureTest) | **GET** /fake/http-signature-test | test http signature authentication |
| [**fakeOuterBooleanSerialize**](FakeApi.md#fakeOuterBooleanSerialize) | **POST** /fake/outer/boolean |  |
| [**fakeOuterCompositeSerialize**](FakeApi.md#fakeOuterCompositeSerialize) | **POST** /fake/outer/composite |  |
| [**fakeOuterNumberSerialize**](FakeApi.md#fakeOuterNumberSerialize) | **POST** /fake/outer/number |  |
| [**fakeOuterStringSerialize**](FakeApi.md#fakeOuterStringSerialize) | **POST** /fake/outer/string |  |
| [**fakePropertyEnumIntegerSerialize**](FakeApi.md#fakePropertyEnumIntegerSerialize) | **POST** /fake/property/enum-int |  |
| [**testBodyWithBinary**](FakeApi.md#testBodyWithBinary) | **PUT** /fake/body-with-binary |  |
| [**testBodyWithFileSchema**](FakeApi.md#testBodyWithFileSchema) | **PUT** /fake/body-with-file-schema |  |
| [**testBodyWithQueryParams**](FakeApi.md#testBodyWithQueryParams) | **PUT** /fake/body-with-query-params |  |
| [**testClientModel**](FakeApi.md#testClientModel) | **PATCH** /fake | To test \&quot;client\&quot; model |
| [**testEndpointParameters**](FakeApi.md#testEndpointParameters) | **POST** /fake | Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트  |
| [**testEnumParameters**](FakeApi.md#testEnumParameters) | **GET** /fake | To test enum parameters |
| [**testGroupParameters**](FakeApi.md#testGroupParameters) | **DELETE** /fake | Fake endpoint to test group parameters (optional) |
| [**testInlineAdditionalProperties**](FakeApi.md#testInlineAdditionalProperties) | **POST** /fake/inline-additionalProperties | test inline additionalProperties |
| [**testJsonFormData**](FakeApi.md#testJsonFormData) | **GET** /fake/jsonFormData | test json serialization of form data |
| [**testNullable**](FakeApi.md#testNullable) | **POST** /fake/nullable | test nullable parent property |
| [**testQueryParameterCollectionFormat**](FakeApi.md#testQueryParameterCollectionFormat) | **PUT** /fake/test-query-parameters |  |



## fakeBigDecimalMap

> FakeBigDecimalMap200Response fakeBigDecimalMap()



for Java apache and Java native, test toUrlQueryString for maps with BegDecimal keys

### Parameters

This endpoint does not need any parameter.

### Return type

[**FakeBigDecimalMap200Response**](FakeBigDecimalMap200Response.md)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: */*


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **200** | successful operation |  -  |


## fakeHealthGet

> HealthCheckResult fakeHealthGet()

Health check endpoint

### Parameters

This endpoint does not need any parameter.

### Return type

[**HealthCheckResult**](HealthCheckResult.md)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: application/json


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **200** | The instance started successfully |  -  |


## fakeHttpSignatureTest

> void fakeHttpSignatureTest(pet, query1, header1)

test http signature authentication

### Parameters


| Name | Type | Description  | Notes |
|------------- | ------------- | ------------- | -------------|
| **pet** | [**Pet**](Pet.md)| Pet object that needs to be added to the store | |
| **query1** | **String**| query parameter | [optional] |
| **header1** | **String**| header parameter | [optional] |

### Return type

[**void**](Void.md)

### Authorization

[http_signature_test](../README.md#http_signature_test)

### HTTP request headers

- **Content-Type**: application/json, application/xml
- **Accept**: Not defined


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **200** | The instance started successfully |  -  |


## fakeOuterBooleanSerialize

> Boolean fakeOuterBooleanSerialize(body)



Test serialization of outer boolean types

### Parameters


| Name | Type | Description  | Notes |
|------------- | ------------- | ------------- | -------------|
| **body** | **Boolean**| Input boolean as post body | [optional] |

### Return type

**Boolean**

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: application/json
- **Accept**: */*


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **200** | Output boolean |  -  |


## fakeOuterCompositeSerialize

> OuterComposite fakeOuterCompositeSerialize(outerComposite)



Test serialization of object with outer number type

### Parameters


| Name | Type | Description  | Notes |
|------------- | ------------- | ------------- | -------------|
| **outerComposite** | [**OuterComposite**](OuterComposite.md)| Input composite as post body | [optional] |

### Return type

[**OuterComposite**](OuterComposite.md)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: application/json
- **Accept**: */*


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **200** | Output composite |  -  |


## fakeOuterNumberSerialize

> BigDecimal fakeOuterNumberSerialize(body)



Test serialization of outer number types

### Parameters


| Name | Type | Description  | Notes |
|------------- | ------------- | ------------- | -------------|
| **body** | **BigDecimal**| Input number as post body | [optional] |

### Return type

[**BigDecimal**](BigDecimal.md)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: application/json
- **Accept**: */*


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **200** | Output number |  -  |


## fakeOuterStringSerialize

> String fakeOuterStringSerialize(body)



Test serialization of outer string types

### Parameters


| Name | Type | Description  | Notes |
|------------- | ------------- | ------------- | -------------|
| **body** | **String**| Input string as post body | [optional] |

### Return type

**String**

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: application/json
- **Accept**: */*


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **200** | Output string |  -  |


## fakePropertyEnumIntegerSerialize

> OuterObjectWithEnumProperty fakePropertyEnumIntegerSerialize(outerObjectWithEnumProperty)



Test serialization of enum (int) properties with examples

### Parameters


| Name | Type | Description  | Notes |
|------------- | ------------- | ------------- | -------------|
| **outerObjectWithEnumProperty** | [**OuterObjectWithEnumProperty**](OuterObjectWithEnumProperty.md)| Input enum (int) as post body | |

### Return type

[**OuterObjectWithEnumProperty**](OuterObjectWithEnumProperty.md)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: application/json
- **Accept**: */*


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **200** | Output enum (int) |  -  |


## testBodyWithBinary

> void testBodyWithBinary(body)



For this test, the body has to be a binary file.

### Parameters


| Name | Type | Description  | Notes |
|------------- | ------------- | ------------- | -------------|
| **body** | **File**| image to upload | |

### Return type

[**void**](Void.md)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: image/png
- **Accept**: Not defined


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **200** | Success |  -  |


## testBodyWithFileSchema

> void testBodyWithFileSchema(fileSchemaTestClass)



For this test, the body for this request must reference a schema named &#x60;File&#x60;.

### Parameters


| Name | Type | Description  | Notes |
|------------- | ------------- | ------------- | -------------|
| **fileSchemaTestClass** | [**FileSchemaTestClass**](FileSchemaTestClass.md)|  | |

### Return type

[**void**](Void.md)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: application/json
- **Accept**: Not defined


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **200** | Success |  -  |


## testBodyWithQueryParams

> void testBodyWithQueryParams(query, user)



### Parameters


| Name | Type | Description  | Notes |
|------------- | ------------- | ------------- | -------------|
| **query** | **String**|  | |
| **user** | [**User**](User.md)|  | |

### Return type

[**void**](Void.md)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: application/json
- **Accept**: Not defined


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **200** | Success |  -  |


## testClientModel

> Client testClientModel(client)

To test \&quot;client\&quot; model

To test \&quot;client\&quot; model

### Parameters


| Name | Type | Description  | Notes |
|------------- | ------------- | ------------- | -------------|
| **client** | [**Client**](Client.md)| client model | |

### Return type

[**Client**](Client.md)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: application/json
- **Accept**: application/json


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **200** | successful operation |  -  |


## testEndpointParameters

> void testEndpointParameters(number, _double, patternWithoutDelimiter, _byte, integer, int32, int64, _float, string, binary, date, dateTime, password, paramCallback)

Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 

Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 

### Parameters


| Name | Type | Description  | Notes |
|------------- | ------------- | ------------- | -------------|
| **number** | **BigDecimal**| None | |
| **_double** | **Double**| None | |
| **patternWithoutDelimiter** | **String**| None | |
| **_byte** | **byte[]**| None | |
| **integer** | **Integer**| None | [optional] |
| **int32** | **Integer**| None | [optional] |
| **int64** | **Long**| None | [optional] |
| **_float** | **Float**| None | [optional] |
| **string** | **String**| None | [optional] |
| **binary** | **File**| None | [optional] |
| **date** | **LocalDate**| None | [optional] |
| **dateTime** | **OffsetDateTime**| None | [optional] |
| **password** | **String**| None | [optional] |
| **paramCallback** | **String**| None | [optional] |

### Return type

[**void**](Void.md)

### Authorization

[http_basic_test](../README.md#http_basic_test)

### HTTP request headers

- **Content-Type**: application/x-www-form-urlencoded
- **Accept**: Not defined


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **400** | Invalid username supplied |  -  |
| **404** | User not found |  -  |


## testEnumParameters

> void testEnumParameters(enumHeaderStringArray, enumHeaderString, enumQueryStringArray, enumQueryString, enumQueryInteger, enumQueryDouble, enumQueryModelArray, enumFormStringArray, enumFormString)

To test enum parameters

To test enum parameters

### Parameters


| Name | Type | Description  | Notes |
|------------- | ------------- | ------------- | -------------|
| **enumHeaderStringArray** | [**List&lt;String&gt;**](String.md)| Header parameter enum test (string array) | [optional] [enum: >, $] |
| **enumHeaderString** | **String**| Header parameter enum test (string) | [optional] [default to -efg] [enum: _abc, -efg, (xyz)] |
| **enumQueryStringArray** | [**List&lt;String&gt;**](String.md)| Query parameter enum test (string array) | [optional] [enum: >, $] |
| **enumQueryString** | **String**| Query parameter enum test (string) | [optional] [default to -efg] [enum: _abc, -efg, (xyz)] |
| **enumQueryInteger** | **Integer**| Query parameter enum test (double) | [optional] [enum: 1, -2] |
| **enumQueryDouble** | **Double**| Query parameter enum test (double) | [optional] [enum: 1.1, -1.2] |
| **enumQueryModelArray** | [**List&lt;EnumClass&gt;**](EnumClass.md)|  | [optional] |
| **enumFormStringArray** | [**List&lt;String&gt;**](String.md)| Form parameter enum test (string array) | [optional] [enum: >, $] |
| **enumFormString** | **String**| Form parameter enum test (string) | [optional] [default to -efg] [enum: _abc, -efg, (xyz)] |

### Return type

[**void**](Void.md)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: application/x-www-form-urlencoded
- **Accept**: Not defined


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **400** | Invalid request |  -  |
| **404** | Not found |  -  |


## testGroupParameters

> void testGroupParameters(requiredStringGroup, requiredBooleanGroup, requiredInt64Group, stringGroup, booleanGroup, int64Group)

Fake endpoint to test group parameters (optional)

Fake endpoint to test group parameters (optional)

### Parameters


| Name | Type | Description  | Notes |
|------------- | ------------- | ------------- | -------------|
| **requiredStringGroup** | **Integer**| Required String in group parameters | |
| **requiredBooleanGroup** | **Boolean**| Required Boolean in group parameters | |
| **requiredInt64Group** | **Long**| Required Integer in group parameters | |
| **stringGroup** | **Integer**| String in group parameters | [optional] |
| **booleanGroup** | **Boolean**| Boolean in group parameters | [optional] |
| **int64Group** | **Long**| Integer in group parameters | [optional] |

### Return type

[**void**](Void.md)

### Authorization

[bearer_test](../README.md#bearer_test)

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: Not defined


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **400** | Something wrong |  -  |


## testInlineAdditionalProperties

> void testInlineAdditionalProperties(requestBody)

test inline additionalProperties



### Parameters


| Name | Type | Description  | Notes |
|------------- | ------------- | ------------- | -------------|
| **requestBody** | [**Map&lt;String, String&gt;**](String.md)| request body | |

### Return type

[**void**](Void.md)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: application/json
- **Accept**: Not defined


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **200** | successful operation |  -  |


## testJsonFormData

> void testJsonFormData(param, param2)

test json serialization of form data



### Parameters


| Name | Type | Description  | Notes |
|------------- | ------------- | ------------- | -------------|
| **param** | **String**| field1 | |
| **param2** | **String**| field2 | |

### Return type

[**void**](Void.md)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: application/x-www-form-urlencoded
- **Accept**: Not defined


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **200** | successful operation |  -  |


## testNullable

> void testNullable(childWithNullable)

test nullable parent property



### Parameters


| Name | Type | Description  | Notes |
|------------- | ------------- | ------------- | -------------|
| **childWithNullable** | [**ChildWithNullable**](ChildWithNullable.md)| request body | |

### Return type

[**void**](Void.md)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: application/json
- **Accept**: Not defined


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **200** | successful operation |  -  |


## testQueryParameterCollectionFormat

> void testQueryParameterCollectionFormat(pipe, ioutil, http, url, context, allowEmpty, language)



To test the collection format in query parameters

### Parameters


| Name | Type | Description  | Notes |
|------------- | ------------- | ------------- | -------------|
| **pipe** | [**List&lt;String&gt;**](String.md)|  | |
| **ioutil** | [**List&lt;String&gt;**](String.md)|  | |
| **http** | [**List&lt;String&gt;**](String.md)|  | |
| **url** | [**List&lt;String&gt;**](String.md)|  | |
| **context** | [**List&lt;String&gt;**](String.md)|  | |
| **allowEmpty** | **String**|  | |
| **language** | [**Map&lt;String, String&gt;**](String.md)|  | [optional] |

### Return type

[**void**](Void.md)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: Not defined


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **200** | Success |  -  |

