# FakeApi

All URIs are relative to *http://petstore.swagger.io:80/v2*

| Method | HTTP request | Description |
|------------- | ------------- | -------------|
| [**fakeBigDecimalMap**](FakeApi.md#fakebigdecimalmap) | **GET** /fake/BigDecimalMap |  |
| [**fakeHealthGet**](FakeApi.md#fakehealthget) | **GET** /fake/health | Health check endpoint |
| [**fakeHttpSignatureTest**](FakeApi.md#fakehttpsignaturetest) | **GET** /fake/http-signature-test | test http signature authentication |
| [**fakeOuterBooleanSerialize**](FakeApi.md#fakeouterbooleanserialize) | **POST** /fake/outer/boolean |  |
| [**fakeOuterCompositeSerialize**](FakeApi.md#fakeoutercompositeserialize) | **POST** /fake/outer/composite |  |
| [**fakeOuterNumberSerialize**](FakeApi.md#fakeouternumberserialize) | **POST** /fake/outer/number |  |
| [**fakeOuterStringSerialize**](FakeApi.md#fakeouterstringserialize) | **POST** /fake/outer/string |  |
| [**fakePropertyEnumIntegerSerialize**](FakeApi.md#fakepropertyenumintegerserialize) | **POST** /fake/property/enum-int |  |
| [**testBodyWithBinary**](FakeApi.md#testbodywithbinary) | **PUT** /fake/body-with-binary |  |
| [**testBodyWithFileSchema**](FakeApi.md#testbodywithfileschema) | **PUT** /fake/body-with-file-schema |  |
| [**testBodyWithQueryParams**](FakeApi.md#testbodywithqueryparams) | **PUT** /fake/body-with-query-params |  |
| [**testClientModel**](FakeApi.md#testclientmodel) | **PATCH** /fake | To test \&quot;client\&quot; model |
| [**testEndpointParameters**](FakeApi.md#testendpointparameters) | **POST** /fake | Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트  |
| [**testEnumParameters**](FakeApi.md#testenumparameters) | **GET** /fake | To test enum parameters |
| [**testGroupParameters**](FakeApi.md#testgroupparameters) | **DELETE** /fake | Fake endpoint to test group parameters (optional) |
| [**testInlineAdditionalProperties**](FakeApi.md#testinlineadditionalproperties) | **POST** /fake/inline-additionalProperties | test inline additionalProperties |
| [**testJsonFormData**](FakeApi.md#testjsonformdata) | **GET** /fake/jsonFormData | test json serialization of form data |
| [**testQueryParameterCollectionFormat**](FakeApi.md#testqueryparametercollectionformat) | **PUT** /fake/test-query-parameters |  |



## fakeBigDecimalMap

> FakeBigDecimalMap200Response fakeBigDecimalMap()



for Java apache and Java native, test toUrlQueryString for maps with BegDecimal keys

### Example

```ts
import {
  Configuration,
  FakeApi,
} from '';
import type { FakeBigDecimalMapRequest } from '';

async function example() {
  console.log("🚀 Testing  SDK...");
  const api = new FakeApi();

  try {
    const data = await api.fakeBigDecimalMap();
    console.log(data);
  } catch (error) {
    console.error(error);
  }
}

// Run the test
example().catch(console.error);
```

### Parameters

This endpoint does not need any parameter.

### Return type

[**FakeBigDecimalMap200Response**](FakeBigDecimalMap200Response.md)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: `*/*`


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **200** | successful operation |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#api-endpoints) [[Back to Model list]](../README.md#models) [[Back to README]](../README.md)


## fakeHealthGet

> HealthCheckResult fakeHealthGet()

Health check endpoint

### Example

```ts
import {
  Configuration,
  FakeApi,
} from '';
import type { FakeHealthGetRequest } from '';

async function example() {
  console.log("🚀 Testing  SDK...");
  const api = new FakeApi();

  try {
    const data = await api.fakeHealthGet();
    console.log(data);
  } catch (error) {
    console.error(error);
  }
}

// Run the test
example().catch(console.error);
```

### Parameters

This endpoint does not need any parameter.

### Return type

[**HealthCheckResult**](HealthCheckResult.md)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: `application/json`


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **200** | The instance started successfully |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#api-endpoints) [[Back to Model list]](../README.md#models) [[Back to README]](../README.md)


## fakeHttpSignatureTest

> fakeHttpSignatureTest(pet, query1, header1)

test http signature authentication

### Example

```ts
import {
  Configuration,
  FakeApi,
} from '';
import type { FakeHttpSignatureTestRequest } from '';

async function example() {
  console.log("🚀 Testing  SDK...");
  const config = new Configuration({ 
    // To configure HTTP signature authorization: http_signature_test
    headers: { "YOUR HEADER NAME": "YOUR SIGNATURE" },
  });
  const api = new FakeApi(config);

  const body = {
    // Pet | Pet object that needs to be added to the store
    pet: ...,
    // string | query parameter (optional)
    query1: query1_example,
    // string | header parameter (optional)
    header1: header1_example,
  } satisfies FakeHttpSignatureTestRequest;

  try {
    const data = await api.fakeHttpSignatureTest(body);
    console.log(data);
  } catch (error) {
    console.error(error);
  }
}

// Run the test
example().catch(console.error);
```

### Parameters


| Name | Type | Description  | Notes |
|------------- | ------------- | ------------- | -------------|
| **pet** | [Pet](Pet.md) | Pet object that needs to be added to the store | |
| **query1** | `string` | query parameter | [Optional] [Defaults to `undefined`] |
| **header1** | `string` | header parameter | [Optional] [Defaults to `undefined`] |

### Return type

`void` (Empty response body)

### Authorization

[http_signature_test](../README.md#http_signature_test)

### HTTP request headers

- **Content-Type**: `application/json`, `application/xml`
- **Accept**: Not defined


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **200** | The instance started successfully |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#api-endpoints) [[Back to Model list]](../README.md#models) [[Back to README]](../README.md)


## fakeOuterBooleanSerialize

> boolean fakeOuterBooleanSerialize(body)



Test serialization of outer boolean types

### Example

```ts
import {
  Configuration,
  FakeApi,
} from '';
import type { FakeOuterBooleanSerializeRequest } from '';

async function example() {
  console.log("🚀 Testing  SDK...");
  const api = new FakeApi();

  const body = {
    // boolean | Input boolean as post body (optional)
    body: true,
  } satisfies FakeOuterBooleanSerializeRequest;

  try {
    const data = await api.fakeOuterBooleanSerialize(body);
    console.log(data);
  } catch (error) {
    console.error(error);
  }
}

// Run the test
example().catch(console.error);
```

### Parameters


| Name | Type | Description  | Notes |
|------------- | ------------- | ------------- | -------------|
| **body** | `boolean` | Input boolean as post body | [Optional] |

### Return type

**boolean**

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: `application/json`
- **Accept**: `*/*`


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **200** | Output boolean |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#api-endpoints) [[Back to Model list]](../README.md#models) [[Back to README]](../README.md)


## fakeOuterCompositeSerialize

> OuterComposite fakeOuterCompositeSerialize(outerComposite)



Test serialization of object with outer number type

### Example

```ts
import {
  Configuration,
  FakeApi,
} from '';
import type { FakeOuterCompositeSerializeRequest } from '';

async function example() {
  console.log("🚀 Testing  SDK...");
  const api = new FakeApi();

  const body = {
    // OuterComposite | Input composite as post body (optional)
    outerComposite: ...,
  } satisfies FakeOuterCompositeSerializeRequest;

  try {
    const data = await api.fakeOuterCompositeSerialize(body);
    console.log(data);
  } catch (error) {
    console.error(error);
  }
}

// Run the test
example().catch(console.error);
```

### Parameters


| Name | Type | Description  | Notes |
|------------- | ------------- | ------------- | -------------|
| **outerComposite** | [OuterComposite](OuterComposite.md) | Input composite as post body | [Optional] |

### Return type

[**OuterComposite**](OuterComposite.md)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: `application/json`
- **Accept**: `*/*`


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **200** | Output composite |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#api-endpoints) [[Back to Model list]](../README.md#models) [[Back to README]](../README.md)


## fakeOuterNumberSerialize

> number fakeOuterNumberSerialize(body)



Test serialization of outer number types

### Example

```ts
import {
  Configuration,
  FakeApi,
} from '';
import type { FakeOuterNumberSerializeRequest } from '';

async function example() {
  console.log("🚀 Testing  SDK...");
  const api = new FakeApi();

  const body = {
    // number | Input number as post body (optional)
    body: 8.14,
  } satisfies FakeOuterNumberSerializeRequest;

  try {
    const data = await api.fakeOuterNumberSerialize(body);
    console.log(data);
  } catch (error) {
    console.error(error);
  }
}

// Run the test
example().catch(console.error);
```

### Parameters


| Name | Type | Description  | Notes |
|------------- | ------------- | ------------- | -------------|
| **body** | `number` | Input number as post body | [Optional] |

### Return type

**number**

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: `application/json`
- **Accept**: `*/*`


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **200** | Output number |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#api-endpoints) [[Back to Model list]](../README.md#models) [[Back to README]](../README.md)


## fakeOuterStringSerialize

> string fakeOuterStringSerialize(body)



Test serialization of outer string types

### Example

```ts
import {
  Configuration,
  FakeApi,
} from '';
import type { FakeOuterStringSerializeRequest } from '';

async function example() {
  console.log("🚀 Testing  SDK...");
  const api = new FakeApi();

  const body = {
    // string | Input string as post body (optional)
    body: body_example,
  } satisfies FakeOuterStringSerializeRequest;

  try {
    const data = await api.fakeOuterStringSerialize(body);
    console.log(data);
  } catch (error) {
    console.error(error);
  }
}

// Run the test
example().catch(console.error);
```

### Parameters


| Name | Type | Description  | Notes |
|------------- | ------------- | ------------- | -------------|
| **body** | `string` | Input string as post body | [Optional] |

### Return type

**string**

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: `application/json`
- **Accept**: `*/*`


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **200** | Output string |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#api-endpoints) [[Back to Model list]](../README.md#models) [[Back to README]](../README.md)


## fakePropertyEnumIntegerSerialize

> OuterObjectWithEnumProperty fakePropertyEnumIntegerSerialize(outerObjectWithEnumProperty)



Test serialization of enum (int) properties with examples

### Example

```ts
import {
  Configuration,
  FakeApi,
} from '';
import type { FakePropertyEnumIntegerSerializeRequest } from '';

async function example() {
  console.log("🚀 Testing  SDK...");
  const api = new FakeApi();

  const body = {
    // OuterObjectWithEnumProperty | Input enum (int) as post body
    outerObjectWithEnumProperty: ...,
  } satisfies FakePropertyEnumIntegerSerializeRequest;

  try {
    const data = await api.fakePropertyEnumIntegerSerialize(body);
    console.log(data);
  } catch (error) {
    console.error(error);
  }
}

// Run the test
example().catch(console.error);
```

### Parameters


| Name | Type | Description  | Notes |
|------------- | ------------- | ------------- | -------------|
| **outerObjectWithEnumProperty** | [OuterObjectWithEnumProperty](OuterObjectWithEnumProperty.md) | Input enum (int) as post body | |

### Return type

[**OuterObjectWithEnumProperty**](OuterObjectWithEnumProperty.md)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: `application/json`
- **Accept**: `*/*`


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **200** | Output enum (int) |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#api-endpoints) [[Back to Model list]](../README.md#models) [[Back to README]](../README.md)


## testBodyWithBinary

> testBodyWithBinary(body)



For this test, the body has to be a binary file.

### Example

```ts
import {
  Configuration,
  FakeApi,
} from '';
import type { TestBodyWithBinaryRequest } from '';

async function example() {
  console.log("🚀 Testing  SDK...");
  const api = new FakeApi();

  const body = {
    // Blob | image to upload
    body: BINARY_DATA_HERE,
  } satisfies TestBodyWithBinaryRequest;

  try {
    const data = await api.testBodyWithBinary(body);
    console.log(data);
  } catch (error) {
    console.error(error);
  }
}

// Run the test
example().catch(console.error);
```

### Parameters


| Name | Type | Description  | Notes |
|------------- | ------------- | ------------- | -------------|
| **body** | `Blob` | image to upload | |

### Return type

`void` (Empty response body)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: `image/png`
- **Accept**: Not defined


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **200** | Success |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#api-endpoints) [[Back to Model list]](../README.md#models) [[Back to README]](../README.md)


## testBodyWithFileSchema

> testBodyWithFileSchema(fileSchemaTestClass)



For this test, the body for this request must reference a schema named &#x60;File&#x60;.

### Example

```ts
import {
  Configuration,
  FakeApi,
} from '';
import type { TestBodyWithFileSchemaRequest } from '';

async function example() {
  console.log("🚀 Testing  SDK...");
  const api = new FakeApi();

  const body = {
    // FileSchemaTestClass
    fileSchemaTestClass: ...,
  } satisfies TestBodyWithFileSchemaRequest;

  try {
    const data = await api.testBodyWithFileSchema(body);
    console.log(data);
  } catch (error) {
    console.error(error);
  }
}

// Run the test
example().catch(console.error);
```

### Parameters


| Name | Type | Description  | Notes |
|------------- | ------------- | ------------- | -------------|
| **fileSchemaTestClass** | [FileSchemaTestClass](FileSchemaTestClass.md) |  | |

### Return type

`void` (Empty response body)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: `application/json`
- **Accept**: Not defined


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **200** | Success |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#api-endpoints) [[Back to Model list]](../README.md#models) [[Back to README]](../README.md)


## testBodyWithQueryParams

> testBodyWithQueryParams(query, user)



### Example

```ts
import {
  Configuration,
  FakeApi,
} from '';
import type { TestBodyWithQueryParamsRequest } from '';

async function example() {
  console.log("🚀 Testing  SDK...");
  const api = new FakeApi();

  const body = {
    // string
    query: query_example,
    // User
    user: ...,
  } satisfies TestBodyWithQueryParamsRequest;

  try {
    const data = await api.testBodyWithQueryParams(body);
    console.log(data);
  } catch (error) {
    console.error(error);
  }
}

// Run the test
example().catch(console.error);
```

### Parameters


| Name | Type | Description  | Notes |
|------------- | ------------- | ------------- | -------------|
| **query** | `string` |  | [Defaults to `undefined`] |
| **user** | [User](User.md) |  | |

### Return type

`void` (Empty response body)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: `application/json`
- **Accept**: Not defined


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **200** | Success |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#api-endpoints) [[Back to Model list]](../README.md#models) [[Back to README]](../README.md)


## testClientModel

> Client testClientModel(client)

To test \&quot;client\&quot; model

To test \&quot;client\&quot; model

### Example

```ts
import {
  Configuration,
  FakeApi,
} from '';
import type { TestClientModelRequest } from '';

async function example() {
  console.log("🚀 Testing  SDK...");
  const api = new FakeApi();

  const body = {
    // Client | client model
    client: ...,
  } satisfies TestClientModelRequest;

  try {
    const data = await api.testClientModel(body);
    console.log(data);
  } catch (error) {
    console.error(error);
  }
}

// Run the test
example().catch(console.error);
```

### Parameters


| Name | Type | Description  | Notes |
|------------- | ------------- | ------------- | -------------|
| **client** | [Client](Client.md) | client model | |

### Return type

[**Client**](Client.md)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: `application/json`
- **Accept**: `application/json`


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **200** | successful operation |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#api-endpoints) [[Back to Model list]](../README.md#models) [[Back to README]](../README.md)


## testEndpointParameters

> testEndpointParameters(number, _double, patternWithoutDelimiter, _byte, integer, int32, int64, _float, string, binary, date, dateTime, password, callback)

Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 

Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 

### Example

```ts
import {
  Configuration,
  FakeApi,
} from '';
import type { TestEndpointParametersRequest } from '';

async function example() {
  console.log("🚀 Testing  SDK...");
  const config = new Configuration({ 
    // To configure HTTP basic authorization: http_basic_test
    username: "YOUR USERNAME",
    password: "YOUR PASSWORD",
  });
  const api = new FakeApi(config);

  const body = {
    // number | None
    number: 8.14,
    // number | None
    _double: 1.2,
    // string | None
    patternWithoutDelimiter: patternWithoutDelimiter_example,
    // string | None
    _byte: BYTE_ARRAY_DATA_HERE,
    // number | None (optional)
    integer: 56,
    // number | None (optional)
    int32: 56,
    // number | None (optional)
    int64: 789,
    // number | None (optional)
    _float: 3.4,
    // string | None (optional)
    string: string_example,
    // Blob | None (optional)
    binary: BINARY_DATA_HERE,
    // Date | None (optional)
    date: 2013-10-20,
    // Date | None (optional)
    dateTime: 2013-10-20T19:20:30+01:00,
    // string | None (optional)
    password: password_example,
    // string | None (optional)
    callback: callback_example,
  } satisfies TestEndpointParametersRequest;

  try {
    const data = await api.testEndpointParameters(body);
    console.log(data);
  } catch (error) {
    console.error(error);
  }
}

// Run the test
example().catch(console.error);
```

### Parameters


| Name | Type | Description  | Notes |
|------------- | ------------- | ------------- | -------------|
| **number** | `number` | None | [Defaults to `undefined`] |
| **_double** | `number` | None | [Defaults to `undefined`] |
| **patternWithoutDelimiter** | `string` | None | [Defaults to `undefined`] |
| **_byte** | `string` | None | [Defaults to `undefined`] |
| **integer** | `number` | None | [Optional] [Defaults to `undefined`] |
| **int32** | `number` | None | [Optional] [Defaults to `undefined`] |
| **int64** | `number` | None | [Optional] [Defaults to `undefined`] |
| **_float** | `number` | None | [Optional] [Defaults to `undefined`] |
| **string** | `string` | None | [Optional] [Defaults to `undefined`] |
| **binary** | `Blob` | None | [Optional] [Defaults to `undefined`] |
| **date** | `Date` | None | [Optional] [Defaults to `undefined`] |
| **dateTime** | `Date` | None | [Optional] [Defaults to `undefined`] |
| **password** | `string` | None | [Optional] [Defaults to `undefined`] |
| **callback** | `string` | None | [Optional] [Defaults to `undefined`] |

### Return type

`void` (Empty response body)

### Authorization

[http_basic_test](../README.md#http_basic_test)

### HTTP request headers

- **Content-Type**: `application/x-www-form-urlencoded`
- **Accept**: Not defined


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **400** | Invalid username supplied |  -  |
| **404** | User not found |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#api-endpoints) [[Back to Model list]](../README.md#models) [[Back to README]](../README.md)


## testEnumParameters

> testEnumParameters(enumHeaderStringArray, enumHeaderString, enumQueryStringArray, enumQueryString, enumQueryInteger, enumQueryDouble, enumQueryModelArray, enumFormStringArray, enumFormString)

To test enum parameters

To test enum parameters

### Example

```ts
import {
  Configuration,
  FakeApi,
} from '';
import type { TestEnumParametersRequest } from '';

async function example() {
  console.log("🚀 Testing  SDK...");
  const api = new FakeApi();

  const body = {
    // Array<'>' | '$'> | Header parameter enum test (string array) (optional)
    enumHeaderStringArray: ...,
    // '_abc' | '-efg' | '(xyz)' | Header parameter enum test (string) (optional)
    enumHeaderString: enumHeaderString_example,
    // Array<'>' | '$'> | Query parameter enum test (string array) (optional)
    enumQueryStringArray: ...,
    // '_abc' | '-efg' | '(xyz)' | Query parameter enum test (string) (optional)
    enumQueryString: enumQueryString_example,
    // 1 | -2 | Query parameter enum test (double) (optional)
    enumQueryInteger: 56,
    // 1.1 | -1.2 | Query parameter enum test (double) (optional)
    enumQueryDouble: 1.2,
    // Array<EnumClass> (optional)
    enumQueryModelArray: ...,
    // Array<string> | Form parameter enum test (string array) (optional)
    enumFormStringArray: ...,
    // string | Form parameter enum test (string) (optional)
    enumFormString: enumFormString_example,
  } satisfies TestEnumParametersRequest;

  try {
    const data = await api.testEnumParameters(body);
    console.log(data);
  } catch (error) {
    console.error(error);
  }
}

// Run the test
example().catch(console.error);
```

### Parameters


| Name | Type | Description  | Notes |
|------------- | ------------- | ------------- | -------------|
| **enumHeaderStringArray** | `>`, `$` | Header parameter enum test (string array) | [Optional] [Enum: >, $] |
| **enumHeaderString** | `_abc`, `-efg`, `(xyz)` | Header parameter enum test (string) | [Optional] [Defaults to `&#39;-efg&#39;`] [Enum: _abc, -efg, (xyz)] |
| **enumQueryStringArray** | `>`, `$` | Query parameter enum test (string array) | [Optional] [Enum: >, $] |
| **enumQueryString** | `_abc`, `-efg`, `(xyz)` | Query parameter enum test (string) | [Optional] [Defaults to `&#39;-efg&#39;`] [Enum: _abc, -efg, (xyz)] |
| **enumQueryInteger** | `1`, `-2` | Query parameter enum test (double) | [Optional] [Defaults to `undefined`] [Enum: 1, -2] |
| **enumQueryDouble** | `1.1`, `-1.2` | Query parameter enum test (double) | [Optional] [Defaults to `undefined`] [Enum: 1.1, -1.2] |
| **enumQueryModelArray** | `Array<EnumClass>` |  | [Optional] |
| **enumFormStringArray** | `>`, `$` | Form parameter enum test (string array) | [Optional] [Enum: >, $] |
| **enumFormString** | `_abc`, `-efg`, `(xyz)` | Form parameter enum test (string) | [Optional] [Defaults to `&#39;-efg&#39;`] [Enum: _abc, -efg, (xyz)] |

### Return type

`void` (Empty response body)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: `application/x-www-form-urlencoded`
- **Accept**: Not defined


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **400** | Invalid request |  -  |
| **404** | Not found |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#api-endpoints) [[Back to Model list]](../README.md#models) [[Back to README]](../README.md)


## testGroupParameters

> testGroupParameters(requiredStringGroup, requiredBooleanGroup, requiredInt64Group, stringGroup, booleanGroup, int64Group)

Fake endpoint to test group parameters (optional)

Fake endpoint to test group parameters (optional)

### Example

```ts
import {
  Configuration,
  FakeApi,
} from '';
import type { TestGroupParametersRequest } from '';

async function example() {
  console.log("🚀 Testing  SDK...");
  const config = new Configuration({ 
    // Configure HTTP bearer authorization: bearer_test
    accessToken: "YOUR BEARER TOKEN",
  });
  const api = new FakeApi(config);

  const body = {
    // number | Required String in group parameters
    requiredStringGroup: 56,
    // boolean | Required Boolean in group parameters
    requiredBooleanGroup: true,
    // number | Required Integer in group parameters
    requiredInt64Group: 789,
    // number | String in group parameters (optional)
    stringGroup: 56,
    // boolean | Boolean in group parameters (optional)
    booleanGroup: true,
    // number | Integer in group parameters (optional)
    int64Group: 789,
  } satisfies TestGroupParametersRequest;

  try {
    const data = await api.testGroupParameters(body);
    console.log(data);
  } catch (error) {
    console.error(error);
  }
}

// Run the test
example().catch(console.error);
```

### Parameters


| Name | Type | Description  | Notes |
|------------- | ------------- | ------------- | -------------|
| **requiredStringGroup** | `number` | Required String in group parameters | [Defaults to `undefined`] |
| **requiredBooleanGroup** | `boolean` | Required Boolean in group parameters | [Defaults to `undefined`] |
| **requiredInt64Group** | `number` | Required Integer in group parameters | [Defaults to `undefined`] |
| **stringGroup** | `number` | String in group parameters | [Optional] [Defaults to `undefined`] |
| **booleanGroup** | `boolean` | Boolean in group parameters | [Optional] [Defaults to `undefined`] |
| **int64Group** | `number` | Integer in group parameters | [Optional] [Defaults to `undefined`] |

### Return type

`void` (Empty response body)

### Authorization

[bearer_test](../README.md#bearer_test)

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: Not defined


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **400** | Something wrong |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#api-endpoints) [[Back to Model list]](../README.md#models) [[Back to README]](../README.md)


## testInlineAdditionalProperties

> testInlineAdditionalProperties(requestBody)

test inline additionalProperties



### Example

```ts
import {
  Configuration,
  FakeApi,
} from '';
import type { TestInlineAdditionalPropertiesRequest } from '';

async function example() {
  console.log("🚀 Testing  SDK...");
  const api = new FakeApi();

  const body = {
    // { [key: string]: string; } | request body
    requestBody: ...,
  } satisfies TestInlineAdditionalPropertiesRequest;

  try {
    const data = await api.testInlineAdditionalProperties(body);
    console.log(data);
  } catch (error) {
    console.error(error);
  }
}

// Run the test
example().catch(console.error);
```

### Parameters


| Name | Type | Description  | Notes |
|------------- | ------------- | ------------- | -------------|
| **requestBody** | `{ [key: string]: string; }` | request body | |

### Return type

`void` (Empty response body)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: `application/json`
- **Accept**: Not defined


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **200** | successful operation |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#api-endpoints) [[Back to Model list]](../README.md#models) [[Back to README]](../README.md)


## testJsonFormData

> testJsonFormData(param, param2)

test json serialization of form data



### Example

```ts
import {
  Configuration,
  FakeApi,
} from '';
import type { TestJsonFormDataRequest } from '';

async function example() {
  console.log("🚀 Testing  SDK...");
  const api = new FakeApi();

  const body = {
    // string | field1
    param: param_example,
    // string | field2
    param2: param2_example,
  } satisfies TestJsonFormDataRequest;

  try {
    const data = await api.testJsonFormData(body);
    console.log(data);
  } catch (error) {
    console.error(error);
  }
}

// Run the test
example().catch(console.error);
```

### Parameters


| Name | Type | Description  | Notes |
|------------- | ------------- | ------------- | -------------|
| **param** | `string` | field1 | [Defaults to `undefined`] |
| **param2** | `string` | field2 | [Defaults to `undefined`] |

### Return type

`void` (Empty response body)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: `application/x-www-form-urlencoded`
- **Accept**: Not defined


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **200** | successful operation |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#api-endpoints) [[Back to Model list]](../README.md#models) [[Back to README]](../README.md)


## testQueryParameterCollectionFormat

> testQueryParameterCollectionFormat(pipe, ioutil, http, url, context, allowEmpty, language)



To test the collection format in query parameters

### Example

```ts
import {
  Configuration,
  FakeApi,
} from '';
import type { TestQueryParameterCollectionFormatRequest } from '';

async function example() {
  console.log("🚀 Testing  SDK...");
  const api = new FakeApi();

  const body = {
    // Array<string>
    pipe: ...,
    // Array<string>
    ioutil: ...,
    // Array<string>
    http: ...,
    // Array<string>
    url: ...,
    // Array<string>
    context: ...,
    // string
    allowEmpty: allowEmpty_example,
    // { [key: string]: string; } (optional)
    language: ...,
  } satisfies TestQueryParameterCollectionFormatRequest;

  try {
    const data = await api.testQueryParameterCollectionFormat(body);
    console.log(data);
  } catch (error) {
    console.error(error);
  }
}

// Run the test
example().catch(console.error);
```

### Parameters


| Name | Type | Description  | Notes |
|------------- | ------------- | ------------- | -------------|
| **pipe** | `Array<string>` |  | |
| **ioutil** | `Array<string>` |  | |
| **http** | `Array<string>` |  | |
| **url** | `Array<string>` |  | |
| **context** | `Array<string>` |  | |
| **allowEmpty** | `string` |  | [Defaults to `undefined`] |
| **language** | `{ [key: string]: string; }` |  | [Optional] |

### Return type

`void` (Empty response body)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: Not defined


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **200** | Success |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#api-endpoints) [[Back to Model list]](../README.md#models) [[Back to README]](../README.md)

