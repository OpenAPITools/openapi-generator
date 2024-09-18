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
import { petstore } from 'ts-petstore-client';
import * as fs from 'fs';

const configuration = petstore.createConfiguration();
const apiInstance = new petstore.FakeApi(configuration);

let body:any = {};

apiInstance.fakeBigDecimalMap(body).then((data:any) => {
  console.log('API called successfully. Returned data: ' + data);
}).catch((error:any) => console.error(error));
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
import { petstore } from 'ts-petstore-client';
import * as fs from 'fs';

const configuration = petstore.createConfiguration();
const apiInstance = new petstore.FakeApi(configuration);

let body:any = {};

apiInstance.fakeHealthGet(body).then((data:any) => {
  console.log('API called successfully. Returned data: ' + data);
}).catch((error:any) => console.error(error));
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
import { petstore } from 'ts-petstore-client';
import * as fs from 'fs';

const configuration = petstore.createConfiguration();
const apiInstance = new petstore.FakeApi(configuration);

let body:petstore.FakeApiFakeHttpSignatureTestRequest = {
  // Pet | Pet object that needs to be added to the store
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
  // string | query parameter (optional)
  query1: "query_1_example",
  // string | header parameter (optional)
  header1: "header_1_example",
};

apiInstance.fakeHttpSignatureTest(body).then((data:any) => {
  console.log('API called successfully. Returned data: ' + data);
}).catch((error:any) => console.error(error));
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
import { petstore } from 'ts-petstore-client';
import * as fs from 'fs';

const configuration = petstore.createConfiguration();
const apiInstance = new petstore.FakeApi(configuration);

let body:petstore.FakeApiFakeOuterBooleanSerializeRequest = {
  // boolean | Input boolean as post body (optional)
  body: true,
};

apiInstance.fakeOuterBooleanSerialize(body).then((data:any) => {
  console.log('API called successfully. Returned data: ' + data);
}).catch((error:any) => console.error(error));
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
import { petstore } from 'ts-petstore-client';
import * as fs from 'fs';

const configuration = petstore.createConfiguration();
const apiInstance = new petstore.FakeApi(configuration);

let body:petstore.FakeApiFakeOuterCompositeSerializeRequest = {
  // OuterComposite | Input composite as post body (optional)
  outerComposite: {
    myNumber: 3.14,
    myString: "myString_example",
    myBoolean: true,
  },
};

apiInstance.fakeOuterCompositeSerialize(body).then((data:any) => {
  console.log('API called successfully. Returned data: ' + data);
}).catch((error:any) => console.error(error));
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
import { petstore } from 'ts-petstore-client';
import * as fs from 'fs';

const configuration = petstore.createConfiguration();
const apiInstance = new petstore.FakeApi(configuration);

let body:petstore.FakeApiFakeOuterNumberSerializeRequest = {
  // number | Input number as post body (optional)
  body: 3.14,
};

apiInstance.fakeOuterNumberSerialize(body).then((data:any) => {
  console.log('API called successfully. Returned data: ' + data);
}).catch((error:any) => console.error(error));
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
import { petstore } from 'ts-petstore-client';
import * as fs from 'fs';

const configuration = petstore.createConfiguration();
const apiInstance = new petstore.FakeApi(configuration);

let body:petstore.FakeApiFakeOuterStringSerializeRequest = {
  // string | Input string as post body (optional)
  body: "body_example",
};

apiInstance.fakeOuterStringSerialize(body).then((data:any) => {
  console.log('API called successfully. Returned data: ' + data);
}).catch((error:any) => console.error(error));
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
import { petstore } from 'ts-petstore-client';
import * as fs from 'fs';

const configuration = petstore.createConfiguration();
const apiInstance = new petstore.FakeApi(configuration);

let body:petstore.FakeApiFakePropertyEnumIntegerSerializeRequest = {
  // OuterObjectWithEnumProperty | Input enum (int) as post body
  outerObjectWithEnumProperty: {
    value: 2,
  },
};

apiInstance.fakePropertyEnumIntegerSerialize(body).then((data:any) => {
  console.log('API called successfully. Returned data: ' + data);
}).catch((error:any) => console.error(error));
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
import { petstore } from 'ts-petstore-client';
import * as fs from 'fs';

const configuration = petstore.createConfiguration();
const apiInstance = new petstore.FakeApi(configuration);

let body:petstore.FakeApiTestBodyWithBinaryRequest = {
  // HttpFile | image to upload
  body: { data: Buffer.from(fs.readFileSync('/path/to/file', 'utf-8')), name: '/path/to/file' },
};

apiInstance.testBodyWithBinary(body).then((data:any) => {
  console.log('API called successfully. Returned data: ' + data);
}).catch((error:any) => console.error(error));
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
import { petstore } from 'ts-petstore-client';
import * as fs from 'fs';

const configuration = petstore.createConfiguration();
const apiInstance = new petstore.FakeApi(configuration);

let body:petstore.FakeApiTestBodyWithFileSchemaRequest = {
  // FileSchemaTestClass
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

apiInstance.testBodyWithFileSchema(body).then((data:any) => {
  console.log('API called successfully. Returned data: ' + data);
}).catch((error:any) => console.error(error));
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
import { petstore } from 'ts-petstore-client';
import * as fs from 'fs';

const configuration = petstore.createConfiguration();
const apiInstance = new petstore.FakeApi(configuration);

let body:petstore.FakeApiTestBodyWithQueryParamsRequest = {
  // string
  query: "query_example",
  // User
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

apiInstance.testBodyWithQueryParams(body).then((data:any) => {
  console.log('API called successfully. Returned data: ' + data);
}).catch((error:any) => console.error(error));
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
import { petstore } from 'ts-petstore-client';
import * as fs from 'fs';

const configuration = petstore.createConfiguration();
const apiInstance = new petstore.FakeApi(configuration);

let body:petstore.FakeApiTestClientModelRequest = {
  // Client | client model
  client: {
    client: "client_example",
  },
};

apiInstance.testClientModel(body).then((data:any) => {
  console.log('API called successfully. Returned data: ' + data);
}).catch((error:any) => console.error(error));
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
import { petstore } from 'ts-petstore-client';
import * as fs from 'fs';

const configuration = petstore.createConfiguration();
const apiInstance = new petstore.FakeApi(configuration);

let body:petstore.FakeApiTestEndpointParametersRequest = {
  // number | None
  number: 32.1,
  // number | None
  _double: 67.8,
  // string | None
  patternWithoutDelimiter: "AUR,rZ#UM/?R,Fp^l6$ARjbhJk C>",
  // string | None
  _byte: 'YQ==',
  // number | None (optional)
  integer: 10,
  // number | None (optional)
  int32: 20,
  // number | None (optional)
  int64: 1,
  // number | None (optional)
  _float: 3.14,
  // string | None (optional)
  string: "/a/i",
  // HttpFile | None (optional)
  binary: { data: Buffer.from(fs.readFileSync('/path/to/file', 'utf-8')), name: '/path/to/file' },
  // string | None (optional)
  date: new Date('1970-01-01').toISOString().split('T')[0];,
  // Date | None (optional)
  dateTime: new Date('1970-01-01T00:00:00.00Z'),
  // string | None (optional)
  password: "password_example",
  // string | None (optional)
  callback: "callback_example",
};

apiInstance.testEndpointParameters(body).then((data:any) => {
  console.log('API called successfully. Returned data: ' + data);
}).catch((error:any) => console.error(error));
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
import { petstore } from 'ts-petstore-client';
import * as fs from 'fs';

const configuration = petstore.createConfiguration();
const apiInstance = new petstore.FakeApi(configuration);

let body:petstore.FakeApiTestEnumParametersRequest = {
  // Array<'>' | '$'> | Header parameter enum test (string array) (optional)
  enumHeaderStringArray: [
    "$",
  ],
  // '_abc' | '-efg' | '(xyz)' | Header parameter enum test (string) (optional)
  enumHeaderString: "-efg",
  // Array<'>' | '$'> | Query parameter enum test (string array) (optional)
  enumQueryStringArray: [
    "$",
  ],
  // '_abc' | '-efg' | '(xyz)' | Query parameter enum test (string) (optional)
  enumQueryString: "-efg",
  // 1 | -2 | Query parameter enum test (double) (optional)
  enumQueryInteger: 1,
  // 1.1 | -1.2 | Query parameter enum test (double) (optional)
  enumQueryDouble: 1.1,
  // Array<EnumClass> (optional)
  enumQueryModelArray: [
    "-efg",
  ],
  // Array<string> | Form parameter enum test (string array) (optional)
  enumFormStringArray: [
    "$",
  ],
  // string | Form parameter enum test (string) (optional)
  enumFormString: "-efg",
};

apiInstance.testEnumParameters(body).then((data:any) => {
  console.log('API called successfully. Returned data: ' + data);
}).catch((error:any) => console.error(error));
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
import { petstore } from 'ts-petstore-client';
import * as fs from 'fs';

const configuration = petstore.createConfiguration();
const apiInstance = new petstore.FakeApi(configuration);

let body:petstore.FakeApiTestGroupParametersRequest = {
  // number | Required String in group parameters
  requiredStringGroup: 1,
  // boolean | Required Boolean in group parameters
  requiredBooleanGroup: true,
  // number | Required Integer in group parameters
  requiredInt64Group: 1,
  // number | String in group parameters (optional)
  stringGroup: 1,
  // boolean | Boolean in group parameters (optional)
  booleanGroup: true,
  // number | Integer in group parameters (optional)
  int64Group: 1,
};

apiInstance.testGroupParameters(body).then((data:any) => {
  console.log('API called successfully. Returned data: ' + data);
}).catch((error:any) => console.error(error));
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
import { petstore } from 'ts-petstore-client';
import * as fs from 'fs';

const configuration = petstore.createConfiguration();
const apiInstance = new petstore.FakeApi(configuration);

let body:petstore.FakeApiTestInlineAdditionalPropertiesRequest = {
  // { [key: string]: string; } | request body
  requestBody: {
    "key": "key_example",
  },
};

apiInstance.testInlineAdditionalProperties(body).then((data:any) => {
  console.log('API called successfully. Returned data: ' + data);
}).catch((error:any) => console.error(error));
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
import { petstore } from 'ts-petstore-client';
import * as fs from 'fs';

const configuration = petstore.createConfiguration();
const apiInstance = new petstore.FakeApi(configuration);

let body:petstore.FakeApiTestJsonFormDataRequest = {
  // string | field1
  param: "param_example",
  // string | field2
  param2: "param2_example",
};

apiInstance.testJsonFormData(body).then((data:any) => {
  console.log('API called successfully. Returned data: ' + data);
}).catch((error:any) => console.error(error));
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
import { petstore } from 'ts-petstore-client';
import * as fs from 'fs';

const configuration = petstore.createConfiguration();
const apiInstance = new petstore.FakeApi(configuration);

let body:petstore.FakeApiTestQueryParameterCollectionFormatRequest = {
  // Array<string>
  pipe: [
    "pipe_example",
  ],
  // Array<string>
  ioutil: [
    "ioutil_example",
  ],
  // Array<string>
  http: [
    "http_example",
  ],
  // Array<string>
  url: [
    "url_example",
  ],
  // Array<string>
  context: [
    "context_example",
  ],
  // string
  allowEmpty: "allowEmpty_example",
  // { [key: string]: string; } (optional)
  language: {
    "key": "key_example",
  },
};

apiInstance.testQueryParameterCollectionFormat(body).then((data:any) => {
  console.log('API called successfully. Returned data: ' + data);
}).catch((error:any) => console.error(error));
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


