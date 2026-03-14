# petstore.FakeApi

All URIs are relative to *http://petstore.swagger.io:80/v2*

Method | HTTP request | Description
------------- | ------------- | -------------
[**fakeBigDecimalMap**](FakeApi.md#fakeBigDecimalMap) | **GET** /fake/BigDecimalMap | 
[**fakeHealthGet**](FakeApi.md#fakeHealthGet) | **GET** /fake/health | Health check endpoint
[**fakeHttpSignatureTest**](FakeApi.md#fakeHttpSignatureTest) | **GET** /fake/http-signature-test | test http signature authentication
[**fakeOuterBooleanSerialize**](FakeApi.md#fakeOuterBooleanSerialize) | **POST** /fake/outer/boolean | 
[**fakeOuterCompositeSerialize**](FakeApi.md#fakeOuterCompositeSerialize) | **POST** /fake/outer/composite | 
[**fakeOuterNumberSerialize**](FakeApi.md#fakeOuterNumberSerialize) | **POST** /fake/outer/number | 
[**fakeOuterStringSerialize**](FakeApi.md#fakeOuterStringSerialize) | **POST** /fake/outer/string | 
[**fakePropertyEnumIntegerSerialize**](FakeApi.md#fakePropertyEnumIntegerSerialize) | **POST** /fake/property/enum-int | 
[**testBodyWithBinary**](FakeApi.md#testBodyWithBinary) | **PUT** /fake/body-with-binary | 
[**testBodyWithFileSchema**](FakeApi.md#testBodyWithFileSchema) | **PUT** /fake/body-with-file-schema | 
[**testBodyWithQueryParams**](FakeApi.md#testBodyWithQueryParams) | **PUT** /fake/body-with-query-params | 
[**testClientModel**](FakeApi.md#testClientModel) | **PATCH** /fake | To test \&quot;client\&quot; model
[**testEndpointParameters**](FakeApi.md#testEndpointParameters) | **POST** /fake | Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 
[**testEnumParameters**](FakeApi.md#testEnumParameters) | **GET** /fake | To test enum parameters
[**testGroupParameters**](FakeApi.md#testGroupParameters) | **DELETE** /fake | Fake endpoint to test group parameters (optional)
[**testInlineAdditionalProperties**](FakeApi.md#testInlineAdditionalProperties) | **POST** /fake/inline-additionalProperties | test inline additionalProperties
[**testJsonFormData**](FakeApi.md#testJsonFormData) | **GET** /fake/jsonFormData | test json serialization of form data
[**testQueryParameterCollectionFormat**](FakeApi.md#testQueryParameterCollectionFormat) | **PUT** /fake/test-query-parameters | 


# **fakeBigDecimalMap**
> FakeBigDecimalMap200Response fakeBigDecimalMap()

for Java apache and Java native, test toUrlQueryString for maps with BegDecimal keys

### Example


```typescript
import { createConfiguration, FakeApi } from 'ts-petstore-client';

const configuration = createConfiguration();
const apiInstance = new FakeApi(configuration);

const request = {};

const data = await apiInstance.fakeBigDecimalMap(request);
console.log('API called successfully. Returned data:', data);
```


### Parameters
This endpoint does not need any parameter.


### Return type

**FakeBigDecimalMap200Response**

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: */*


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
**200** | successful operation |  -  |

[[Back to top]](#) [[Back to API list]](README.md#documentation-for-api-endpoints) [[Back to Model list]](README.md#documentation-for-models) [[Back to README]](README.md)

# **fakeHealthGet**
> HealthCheckResult fakeHealthGet()


### Example


```typescript
import { createConfiguration, FakeApi } from 'ts-petstore-client';

const configuration = createConfiguration();
const apiInstance = new FakeApi(configuration);

const request = {};

const data = await apiInstance.fakeHealthGet(request);
console.log('API called successfully. Returned data:', data);
```


### Parameters
This endpoint does not need any parameter.


### Return type

**HealthCheckResult**

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/json


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
**200** | The instance started successfully |  -  |

[[Back to top]](#) [[Back to API list]](README.md#documentation-for-api-endpoints) [[Back to Model list]](README.md#documentation-for-models) [[Back to README]](README.md)

# **fakeHttpSignatureTest**
> void fakeHttpSignatureTest(pet)


### Example


```typescript
import { createConfiguration, FakeApi } from 'ts-petstore-client';
import type { FakeApiFakeHttpSignatureTestRequest } from 'ts-petstore-client';

const configuration = createConfiguration();
const apiInstance = new FakeApi(configuration);

const request: FakeApiFakeHttpSignatureTestRequest = {
    // Pet object that needs to be added to the store
  pet: {
    id: 1,
    category: {
      id: 1,
      name: "default-name",
    },
    name: "doggie",
    photoUrls: [
      "photoUrls_example",
    ],
    tags: [
      {
        id: 1,
        name: "name_example",
      },
    ],
    status: "available",
  },
    // query parameter (optional)
  query1: "query_1_example",
    // header parameter (optional)
  header1: "header_1_example",
};

const data = await apiInstance.fakeHttpSignatureTest(request);
console.log('API called successfully. Returned data:', data);
```


### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **pet** | **Pet**| Pet object that needs to be added to the store |
 **query1** | [**string**] | query parameter | (optional) defaults to undefined
 **header1** | [**string**] | header parameter | (optional) defaults to undefined


### Return type

**void**

### Authorization

[http_signature_test](README.md#http_signature_test)

### HTTP request headers

 - **Content-Type**: application/json, application/xml
 - **Accept**: Not defined


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
**200** | The instance started successfully |  -  |

[[Back to top]](#) [[Back to API list]](README.md#documentation-for-api-endpoints) [[Back to Model list]](README.md#documentation-for-models) [[Back to README]](README.md)

# **fakeOuterBooleanSerialize**
> boolean fakeOuterBooleanSerialize()

Test serialization of outer boolean types

### Example


```typescript
import { createConfiguration, FakeApi } from 'ts-petstore-client';
import type { FakeApiFakeOuterBooleanSerializeRequest } from 'ts-petstore-client';

const configuration = createConfiguration();
const apiInstance = new FakeApi(configuration);

const request: FakeApiFakeOuterBooleanSerializeRequest = {
    // Input boolean as post body (optional)
  body: true,
};

const data = await apiInstance.fakeOuterBooleanSerialize(request);
console.log('API called successfully. Returned data:', data);
```


### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | **boolean**| Input boolean as post body |


### Return type

**boolean**

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: */*


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
**200** | Output boolean |  -  |

[[Back to top]](#) [[Back to API list]](README.md#documentation-for-api-endpoints) [[Back to Model list]](README.md#documentation-for-models) [[Back to README]](README.md)

# **fakeOuterCompositeSerialize**
> OuterComposite fakeOuterCompositeSerialize()

Test serialization of object with outer number type

### Example


```typescript
import { createConfiguration, FakeApi } from 'ts-petstore-client';
import type { FakeApiFakeOuterCompositeSerializeRequest } from 'ts-petstore-client';

const configuration = createConfiguration();
const apiInstance = new FakeApi(configuration);

const request: FakeApiFakeOuterCompositeSerializeRequest = {
    // Input composite as post body (optional)
  outerComposite: {
    myNumber: 3.14,
    myString: "myString_example",
    myBoolean: true,
  },
};

const data = await apiInstance.fakeOuterCompositeSerialize(request);
console.log('API called successfully. Returned data:', data);
```


### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **outerComposite** | **OuterComposite**| Input composite as post body |


### Return type

**OuterComposite**

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: */*


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
**200** | Output composite |  -  |

[[Back to top]](#) [[Back to API list]](README.md#documentation-for-api-endpoints) [[Back to Model list]](README.md#documentation-for-models) [[Back to README]](README.md)

# **fakeOuterNumberSerialize**
> number fakeOuterNumberSerialize()

Test serialization of outer number types

### Example


```typescript
import { createConfiguration, FakeApi } from 'ts-petstore-client';
import type { FakeApiFakeOuterNumberSerializeRequest } from 'ts-petstore-client';

const configuration = createConfiguration();
const apiInstance = new FakeApi(configuration);

const request: FakeApiFakeOuterNumberSerializeRequest = {
    // Input number as post body (optional)
  body: 3.14,
};

const data = await apiInstance.fakeOuterNumberSerialize(request);
console.log('API called successfully. Returned data:', data);
```


### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | **number**| Input number as post body |


### Return type

**number**

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: */*


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
**200** | Output number |  -  |

[[Back to top]](#) [[Back to API list]](README.md#documentation-for-api-endpoints) [[Back to Model list]](README.md#documentation-for-models) [[Back to README]](README.md)

# **fakeOuterStringSerialize**
> string fakeOuterStringSerialize()

Test serialization of outer string types

### Example


```typescript
import { createConfiguration, FakeApi } from 'ts-petstore-client';
import type { FakeApiFakeOuterStringSerializeRequest } from 'ts-petstore-client';

const configuration = createConfiguration();
const apiInstance = new FakeApi(configuration);

const request: FakeApiFakeOuterStringSerializeRequest = {
    // Input string as post body (optional)
  body: "body_example",
};

const data = await apiInstance.fakeOuterStringSerialize(request);
console.log('API called successfully. Returned data:', data);
```


### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | **string**| Input string as post body |


### Return type

**string**

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: */*


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
**200** | Output string |  -  |

[[Back to top]](#) [[Back to API list]](README.md#documentation-for-api-endpoints) [[Back to Model list]](README.md#documentation-for-models) [[Back to README]](README.md)

# **fakePropertyEnumIntegerSerialize**
> OuterObjectWithEnumProperty fakePropertyEnumIntegerSerialize(outerObjectWithEnumProperty)

Test serialization of enum (int) properties with examples

### Example


```typescript
import { createConfiguration, FakeApi } from 'ts-petstore-client';
import type { FakeApiFakePropertyEnumIntegerSerializeRequest } from 'ts-petstore-client';

const configuration = createConfiguration();
const apiInstance = new FakeApi(configuration);

const request: FakeApiFakePropertyEnumIntegerSerializeRequest = {
    // Input enum (int) as post body
  outerObjectWithEnumProperty: {
    value: 2,
  },
};

const data = await apiInstance.fakePropertyEnumIntegerSerialize(request);
console.log('API called successfully. Returned data:', data);
```


### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **outerObjectWithEnumProperty** | **OuterObjectWithEnumProperty**| Input enum (int) as post body |


### Return type

**OuterObjectWithEnumProperty**

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: */*


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
**200** | Output enum (int) |  -  |

[[Back to top]](#) [[Back to API list]](README.md#documentation-for-api-endpoints) [[Back to Model list]](README.md#documentation-for-models) [[Back to README]](README.md)

# **testBodyWithBinary**
> void testBodyWithBinary(body)

For this test, the body has to be a binary file.

### Example


```typescript
import { createConfiguration, FakeApi } from 'ts-petstore-client';
import type { FakeApiTestBodyWithBinaryRequest } from 'ts-petstore-client';

const configuration = createConfiguration();
const apiInstance = new FakeApi(configuration);

const request: FakeApiTestBodyWithBinaryRequest = {
    // image to upload
  body: { data: Buffer.from(fs.readFileSync('/path/to/file', 'utf-8')), name: '/path/to/file' },
};

const data = await apiInstance.testBodyWithBinary(request);
console.log('API called successfully. Returned data:', data);
```


### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | **HttpFile**| image to upload |


### Return type

**void**

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: image/png
 - **Accept**: Not defined


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
**200** | Success |  -  |

[[Back to top]](#) [[Back to API list]](README.md#documentation-for-api-endpoints) [[Back to Model list]](README.md#documentation-for-models) [[Back to README]](README.md)

# **testBodyWithFileSchema**
> void testBodyWithFileSchema(fileSchemaTestClass)

For this test, the body for this request must reference a schema named `File`.

### Example


```typescript
import { createConfiguration, FakeApi } from 'ts-petstore-client';
import type { FakeApiTestBodyWithFileSchemaRequest } from 'ts-petstore-client';

const configuration = createConfiguration();
const apiInstance = new FakeApi(configuration);

const request: FakeApiTestBodyWithFileSchemaRequest = {
  
  fileSchemaTestClass: {
    file: {
      sourceURI: "sourceURI_example",
    },
    files: [
      {
        sourceURI: "sourceURI_example",
      },
    ],
  },
};

const data = await apiInstance.testBodyWithFileSchema(request);
console.log('API called successfully. Returned data:', data);
```


### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **fileSchemaTestClass** | **FileSchemaTestClass**|  |


### Return type

**void**

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: Not defined


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
**200** | Success |  -  |

[[Back to top]](#) [[Back to API list]](README.md#documentation-for-api-endpoints) [[Back to Model list]](README.md#documentation-for-models) [[Back to README]](README.md)

# **testBodyWithQueryParams**
> void testBodyWithQueryParams(user)


### Example


```typescript
import { createConfiguration, FakeApi } from 'ts-petstore-client';
import type { FakeApiTestBodyWithQueryParamsRequest } from 'ts-petstore-client';

const configuration = createConfiguration();
const apiInstance = new FakeApi(configuration);

const request: FakeApiTestBodyWithQueryParamsRequest = {
  
  query: "query_example",
  
  user: {
    id: 1,
    username: "username_example",
    firstName: "firstName_example",
    lastName: "lastName_example",
    email: "email_example",
    password: "password_example",
    phone: "phone_example",
    userStatus: 1,
  },
};

const data = await apiInstance.testBodyWithQueryParams(request);
console.log('API called successfully. Returned data:', data);
```


### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **user** | **User**|  |
 **query** | [**string**] |  | defaults to undefined


### Return type

**void**

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: Not defined


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
**200** | Success |  -  |

[[Back to top]](#) [[Back to API list]](README.md#documentation-for-api-endpoints) [[Back to Model list]](README.md#documentation-for-models) [[Back to README]](README.md)

# **testClientModel**
> Client testClientModel(client)

To test \"client\" model

### Example


```typescript
import { createConfiguration, FakeApi } from 'ts-petstore-client';
import type { FakeApiTestClientModelRequest } from 'ts-petstore-client';

const configuration = createConfiguration();
const apiInstance = new FakeApi(configuration);

const request: FakeApiTestClientModelRequest = {
    // client model
  client: {
    client: "client_example",
  },
};

const data = await apiInstance.testClientModel(request);
console.log('API called successfully. Returned data:', data);
```


### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **client** | **Client**| client model |


### Return type

**Client**

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: application/json


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
**200** | successful operation |  -  |

[[Back to top]](#) [[Back to API list]](README.md#documentation-for-api-endpoints) [[Back to Model list]](README.md#documentation-for-models) [[Back to README]](README.md)

# **testEndpointParameters**
> testEndpointParameters()

Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 

### Example


```typescript
import { createConfiguration, FakeApi } from 'ts-petstore-client';
import type { FakeApiTestEndpointParametersRequest } from 'ts-petstore-client';

const configuration = createConfiguration();
const apiInstance = new FakeApi(configuration);

const request: FakeApiTestEndpointParametersRequest = {
    // None
  number: 32.1,
    // None
  _double: 67.8,
    // None
  patternWithoutDelimiter: "AUR,rZ#UM/?R,Fp^l6$ARjbhJk C>",
    // None
  _byte: 'YQ==',
    // None (optional)
  integer: 10,
    // None (optional)
  int32: 20,
    // None (optional)
  int64: 1,
    // None (optional)
  _float: 3.14,
    // None (optional)
  string: "/a/i",
    // None (optional)
  binary: { data: Buffer.from(fs.readFileSync('/path/to/file', 'utf-8')), name: '/path/to/file' },
    // None (optional)
  date: new Date('1970-01-01').toISOString().split('T')[0];,
    // None (optional)
  dateTime: new Date('1970-01-01T00:00:00.00Z'),
    // None (optional)
  password: "password_example",
    // None (optional)
  callback: "callback_example",
};

const data = await apiInstance.testEndpointParameters(request);
console.log('API called successfully. Returned data:', data);
```


### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **number** | [**number**] | None | defaults to undefined
 **_double** | [**number**] | None | defaults to undefined
 **patternWithoutDelimiter** | [**string**] | None | defaults to undefined
 **_byte** | [**string**] | None | defaults to undefined
 **integer** | [**number**] | None | (optional) defaults to undefined
 **int32** | [**number**] | None | (optional) defaults to undefined
 **int64** | [**number**] | None | (optional) defaults to undefined
 **_float** | [**number**] | None | (optional) defaults to undefined
 **string** | [**string**] | None | (optional) defaults to undefined
 **binary** | [**HttpFile**] | None | (optional) defaults to undefined
 **date** | [**string**] | None | (optional) defaults to undefined
 **dateTime** | [**Date**] | None | (optional) defaults to undefined
 **password** | [**string**] | None | (optional) defaults to undefined
 **callback** | [**string**] | None | (optional) defaults to undefined


### Return type

void (empty response body)

### Authorization

[http_basic_test](README.md#http_basic_test)

### HTTP request headers

 - **Content-Type**: application/x-www-form-urlencoded
 - **Accept**: Not defined


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
**400** | Invalid username supplied |  -  |
**404** | User not found |  -  |

[[Back to top]](#) [[Back to API list]](README.md#documentation-for-api-endpoints) [[Back to Model list]](README.md#documentation-for-models) [[Back to README]](README.md)

# **testEnumParameters**
> testEnumParameters()

To test enum parameters

### Example


```typescript
import { createConfiguration, FakeApi } from 'ts-petstore-client';
import type { FakeApiTestEnumParametersRequest } from 'ts-petstore-client';

const configuration = createConfiguration();
const apiInstance = new FakeApi(configuration);

const request: FakeApiTestEnumParametersRequest = {
    // Header parameter enum test (string array) (optional)
  enumHeaderStringArray: [
    "$",
  ],
    // Header parameter enum test (string) (optional)
  enumHeaderString: "-efg",
    // Query parameter enum test (string array) (optional)
  enumQueryStringArray: [
    "$",
  ],
    // Query parameter enum test (string) (optional)
  enumQueryString: "-efg",
    // Query parameter enum test (double) (optional)
  enumQueryInteger: 1,
    // Query parameter enum test (double) (optional)
  enumQueryDouble: 1.1,
  
  enumQueryModelArray: [
    "-efg",
  ],
    // Form parameter enum test (string array) (optional)
  enumFormStringArray: [
    "$",
  ],
    // Form parameter enum test (string) (optional)
  enumFormString: "-efg",
};

const data = await apiInstance.testEnumParameters(request);
console.log('API called successfully. Returned data:', data);
```


### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **enumHeaderStringArray** | **Array<&#39;&gt;&#39; &#124; &#39;$&#39;>** | Header parameter enum test (string array) | (optional) defaults to undefined
 **enumHeaderString** | [**&#39;_abc&#39; | &#39;-efg&#39; | &#39;(xyz)&#39;**]**Array<&#39;_abc&#39; &#124; &#39;-efg&#39; &#124; &#39;(xyz)&#39;>** | Header parameter enum test (string) | (optional) defaults to '-efg'
 **enumQueryStringArray** | **Array<&#39;&gt;&#39; &#124; &#39;$&#39;>** | Query parameter enum test (string array) | (optional) defaults to undefined
 **enumQueryString** | [**&#39;_abc&#39; | &#39;-efg&#39; | &#39;(xyz)&#39;**]**Array<&#39;_abc&#39; &#124; &#39;-efg&#39; &#124; &#39;(xyz)&#39;>** | Query parameter enum test (string) | (optional) defaults to '-efg'
 **enumQueryInteger** | [**1 | -2**]**Array<1 &#124; -2>** | Query parameter enum test (double) | (optional) defaults to undefined
 **enumQueryDouble** | [**1.1 | -1.2**]**Array<1.1 &#124; -1.2>** | Query parameter enum test (double) | (optional) defaults to undefined
 **enumQueryModelArray** | **Array&lt;EnumClass&gt;** |  | (optional) defaults to undefined
 **enumFormStringArray** | **Array<&#39;&gt;&#39; &#124; &#39;$&#39;>** | Form parameter enum test (string array) | (optional) defaults to '$'
 **enumFormString** | [**string**]**Array<&#39;_abc&#39; &#124; &#39;-efg&#39; &#124; &#39;(xyz)&#39;>** | Form parameter enum test (string) | (optional) defaults to '-efg'


### Return type

void (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/x-www-form-urlencoded
 - **Accept**: Not defined


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
**400** | Invalid request |  -  |
**404** | Not found |  -  |

[[Back to top]](#) [[Back to API list]](README.md#documentation-for-api-endpoints) [[Back to Model list]](README.md#documentation-for-models) [[Back to README]](README.md)

# **testGroupParameters**
> testGroupParameters()

Fake endpoint to test group parameters (optional)

### Example


```typescript
import { createConfiguration, FakeApi } from 'ts-petstore-client';
import type { FakeApiTestGroupParametersRequest } from 'ts-petstore-client';

const configuration = createConfiguration();
const apiInstance = new FakeApi(configuration);

const request: FakeApiTestGroupParametersRequest = {
    // Required String in group parameters
  requiredStringGroup: 1,
    // Required Boolean in group parameters
  requiredBooleanGroup: true,
    // Required Integer in group parameters
  requiredInt64Group: 1,
    // String in group parameters (optional)
  stringGroup: 1,
    // Boolean in group parameters (optional)
  booleanGroup: true,
    // Integer in group parameters (optional)
  int64Group: 1,
};

const data = await apiInstance.testGroupParameters(request);
console.log('API called successfully. Returned data:', data);
```


### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **requiredStringGroup** | [**number**] | Required String in group parameters | defaults to undefined
 **requiredBooleanGroup** | [**boolean**] | Required Boolean in group parameters | defaults to undefined
 **requiredInt64Group** | [**number**] | Required Integer in group parameters | defaults to undefined
 **stringGroup** | [**number**] | String in group parameters | (optional) defaults to undefined
 **booleanGroup** | [**boolean**] | Boolean in group parameters | (optional) defaults to undefined
 **int64Group** | [**number**] | Integer in group parameters | (optional) defaults to undefined


### Return type

void (empty response body)

### Authorization

[bearer_test](README.md#bearer_test)

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: Not defined


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
**400** | Something wrong |  -  |

[[Back to top]](#) [[Back to API list]](README.md#documentation-for-api-endpoints) [[Back to Model list]](README.md#documentation-for-models) [[Back to README]](README.md)

# **testInlineAdditionalProperties**
> void testInlineAdditionalProperties(requestBody)



### Example


```typescript
import { createConfiguration, FakeApi } from 'ts-petstore-client';
import type { FakeApiTestInlineAdditionalPropertiesRequest } from 'ts-petstore-client';

const configuration = createConfiguration();
const apiInstance = new FakeApi(configuration);

const request: FakeApiTestInlineAdditionalPropertiesRequest = {
    // request body
  requestBody: {
    "key": "key_example",
  },
};

const data = await apiInstance.testInlineAdditionalProperties(request);
console.log('API called successfully. Returned data:', data);
```


### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **requestBody** | **{ [key: string]: string; }**| request body |


### Return type

**void**

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: Not defined


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
**200** | successful operation |  -  |

[[Back to top]](#) [[Back to API list]](README.md#documentation-for-api-endpoints) [[Back to Model list]](README.md#documentation-for-models) [[Back to README]](README.md)

# **testJsonFormData**
> void testJsonFormData()



### Example


```typescript
import { createConfiguration, FakeApi } from 'ts-petstore-client';
import type { FakeApiTestJsonFormDataRequest } from 'ts-petstore-client';

const configuration = createConfiguration();
const apiInstance = new FakeApi(configuration);

const request: FakeApiTestJsonFormDataRequest = {
    // field1
  param: "param_example",
    // field2
  param2: "param2_example",
};

const data = await apiInstance.testJsonFormData(request);
console.log('API called successfully. Returned data:', data);
```


### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **param** | [**string**] | field1 | defaults to undefined
 **param2** | [**string**] | field2 | defaults to undefined


### Return type

**void**

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/x-www-form-urlencoded
 - **Accept**: Not defined


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
**200** | successful operation |  -  |

[[Back to top]](#) [[Back to API list]](README.md#documentation-for-api-endpoints) [[Back to Model list]](README.md#documentation-for-models) [[Back to README]](README.md)

# **testQueryParameterCollectionFormat**
> void testQueryParameterCollectionFormat()

To test the collection format in query parameters

### Example


```typescript
import { createConfiguration, FakeApi } from 'ts-petstore-client';
import type { FakeApiTestQueryParameterCollectionFormatRequest } from 'ts-petstore-client';

const configuration = createConfiguration();
const apiInstance = new FakeApi(configuration);

const request: FakeApiTestQueryParameterCollectionFormatRequest = {
  
  pipe: [
    "pipe_example",
  ],
  
  ioutil: [
    "ioutil_example",
  ],
  
  http: [
    "http_example",
  ],
  
  url: [
    "url_example",
  ],
  
  context: [
    "context_example",
  ],
  
  allowEmpty: "allowEmpty_example",
  
  language: {
    "key": "key_example",
  },
};

const data = await apiInstance.testQueryParameterCollectionFormat(request);
console.log('API called successfully. Returned data:', data);
```


### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **pipe** | **Array&lt;string&gt;** |  | defaults to undefined
 **ioutil** | **Array&lt;string&gt;** |  | defaults to undefined
 **http** | **Array&lt;string&gt;** |  | defaults to undefined
 **url** | **Array&lt;string&gt;** |  | defaults to undefined
 **context** | **Array&lt;string&gt;** |  | defaults to undefined
 **allowEmpty** | [**string**] |  | defaults to undefined
 **language** | **{ [key: string]: string; }** |  | (optional) defaults to undefined


### Return type

**void**

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: Not defined


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
**200** | Success |  -  |

[[Back to top]](#) [[Back to API list]](README.md#documentation-for-api-endpoints) [[Back to Model list]](README.md#documentation-for-models) [[Back to README]](README.md)


