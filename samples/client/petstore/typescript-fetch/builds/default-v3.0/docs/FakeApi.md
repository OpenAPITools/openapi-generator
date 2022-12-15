# .FakeApi

All URIs are relative to *http://petstore.swagger.io:80/v2*

|Method | HTTP request | Description|
|------------- | ------------- | -------------|
|[**fakeHealthGet**](FakeApi.md#fakeHealthGet) | **GET** /fake/health | Health check endpoint|
|[**fakeHttpSignatureTest**](FakeApi.md#fakeHttpSignatureTest) | **GET** /fake/http-signature-test | test http signature authentication|
|[**fakeOuterBooleanSerialize**](FakeApi.md#fakeOuterBooleanSerialize) | **POST** /fake/outer/boolean | |
|[**fakeOuterCompositeSerialize**](FakeApi.md#fakeOuterCompositeSerialize) | **POST** /fake/outer/composite | |
|[**fakeOuterNumberSerialize**](FakeApi.md#fakeOuterNumberSerialize) | **POST** /fake/outer/number | |
|[**fakeOuterStringSerialize**](FakeApi.md#fakeOuterStringSerialize) | **POST** /fake/outer/string | |
|[**fakePropertyEnumIntegerSerialize**](FakeApi.md#fakePropertyEnumIntegerSerialize) | **POST** /fake/property/enum-int | |
|[**testBodyWithBinary**](FakeApi.md#testBodyWithBinary) | **PUT** /fake/body-with-binary | |
|[**testBodyWithFileSchema**](FakeApi.md#testBodyWithFileSchema) | **PUT** /fake/body-with-file-schema | |
|[**testBodyWithQueryParams**](FakeApi.md#testBodyWithQueryParams) | **PUT** /fake/body-with-query-params | |
|[**testClientModel**](FakeApi.md#testClientModel) | **PATCH** /fake | To test \&quot;client\&quot; model|
|[**testEndpointParameters**](FakeApi.md#testEndpointParameters) | **POST** /fake | Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 |
|[**testEnumParameters**](FakeApi.md#testEnumParameters) | **GET** /fake | To test enum parameters|
|[**testGroupParameters**](FakeApi.md#testGroupParameters) | **DELETE** /fake | Fake endpoint to test group parameters (optional)|
|[**testInlineAdditionalProperties**](FakeApi.md#testInlineAdditionalProperties) | **POST** /fake/inline-additionalProperties | test inline additionalProperties|
|[**testJsonFormData**](FakeApi.md#testJsonFormData) | **GET** /fake/jsonFormData | test json serialization of form data|
|[**testQueryParameterCollectionFormat**](FakeApi.md#testQueryParameterCollectionFormat) | **PUT** /fake/test-query-parameters | |

# **fakeHealthGet**
> HealthCheckResult fakeHealthGet()


### Example

```typescript
import * as ApiModule from '';

const configuration = ApiModule.createConfiguration();
const apiInstance = new ApiModule.FakeApi(configuration);

let body: any = {};
apiInstance.fakeHealthGet(body).then((data:any) => {
  console.log('API called successfully. Returned data: ', data);
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
|**200** | The instance started successfully |  -  |

[[Back to top]](#) [[Back to API list]](README.md#documentation-for-api-endpoints) [[Back to Model list]](README.md#documentation-for-models) [[Back to README]](README.md)

# **fakeHttpSignatureTest**
> fakeHttpSignatureTest(pet)


### Example

```typescript
import * as ApiModule from '';

const configuration = ApiModule.createConfiguration();
const apiInstance = new ApiModule.FakeApi(configuration);

let body: ApiModule.FakeApiFakeHttpSignatureTestRequest = {
  // Pet | Pet object that needs to be added to the store
  pet: ,
  // string | query parameter (optional)
  query1: query1_example,
  // string | header parameter (optional)
  header1: header1_example,
};
apiInstance.fakeHttpSignatureTest(body).then((data:any) => {
  console.log('API called successfully. Returned data: ', data);
}).catch((error:any) => console.error(error));
```

### Parameters

|Name | Type | Description  | Notes|
|------------- | ------------- | ------------- | -------------|
| **pet** | **Pet**| Pet object that needs to be added to the store | |
| **query1** | [**string**] | query parameter | (optional) defaults to undefined|
| **header1** | [**string**] | header parameter | (optional) defaults to undefined|


### Return type

void (empty response body)

### Authorization

[http_signature_test](README.md#http_signature_test)

### HTTP request headers

 - **Content-Type**: application/json, application/xml
 - **Accept**: Not defined


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
|**200** | The instance started successfully |  -  |

[[Back to top]](#) [[Back to API list]](README.md#documentation-for-api-endpoints) [[Back to Model list]](README.md#documentation-for-models) [[Back to README]](README.md)

# **fakeOuterBooleanSerialize**
> boolean fakeOuterBooleanSerialize()

Test serialization of outer boolean types

### Example

```typescript
import * as ApiModule from '';

const configuration = ApiModule.createConfiguration();
const apiInstance = new ApiModule.FakeApi(configuration);

let body: ApiModule.FakeApiFakeOuterBooleanSerializeRequest = {
  // boolean | Input boolean as post body (optional)
  body: true,
};
apiInstance.fakeOuterBooleanSerialize(body).then((data:any) => {
  console.log('API called successfully. Returned data: ', data);
}).catch((error:any) => console.error(error));
```

### Parameters

|Name | Type | Description  | Notes|
|------------- | ------------- | ------------- | -------------|
| **body** | **boolean**| Input boolean as post body | |


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
|**200** | Output boolean |  -  |

[[Back to top]](#) [[Back to API list]](README.md#documentation-for-api-endpoints) [[Back to Model list]](README.md#documentation-for-models) [[Back to README]](README.md)

# **fakeOuterCompositeSerialize**
> OuterComposite fakeOuterCompositeSerialize()

Test serialization of object with outer number type

### Example

```typescript
import * as ApiModule from '';

const configuration = ApiModule.createConfiguration();
const apiInstance = new ApiModule.FakeApi(configuration);

let body: ApiModule.FakeApiFakeOuterCompositeSerializeRequest = {
  // OuterComposite | Input composite as post body (optional)
  outerComposite: ,
};
apiInstance.fakeOuterCompositeSerialize(body).then((data:any) => {
  console.log('API called successfully. Returned data: ', data);
}).catch((error:any) => console.error(error));
```

### Parameters

|Name | Type | Description  | Notes|
|------------- | ------------- | ------------- | -------------|
| **outerComposite** | **OuterComposite**| Input composite as post body | |


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
|**200** | Output composite |  -  |

[[Back to top]](#) [[Back to API list]](README.md#documentation-for-api-endpoints) [[Back to Model list]](README.md#documentation-for-models) [[Back to README]](README.md)

# **fakeOuterNumberSerialize**
> number fakeOuterNumberSerialize()

Test serialization of outer number types

### Example

```typescript
import * as ApiModule from '';

const configuration = ApiModule.createConfiguration();
const apiInstance = new ApiModule.FakeApi(configuration);

let body: ApiModule.FakeApiFakeOuterNumberSerializeRequest = {
  // number | Input number as post body (optional)
  body: 8.14,
};
apiInstance.fakeOuterNumberSerialize(body).then((data:any) => {
  console.log('API called successfully. Returned data: ', data);
}).catch((error:any) => console.error(error));
```

### Parameters

|Name | Type | Description  | Notes|
|------------- | ------------- | ------------- | -------------|
| **body** | **number**| Input number as post body | |


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
|**200** | Output number |  -  |

[[Back to top]](#) [[Back to API list]](README.md#documentation-for-api-endpoints) [[Back to Model list]](README.md#documentation-for-models) [[Back to README]](README.md)

# **fakeOuterStringSerialize**
> string fakeOuterStringSerialize()

Test serialization of outer string types

### Example

```typescript
import * as ApiModule from '';

const configuration = ApiModule.createConfiguration();
const apiInstance = new ApiModule.FakeApi(configuration);

let body: ApiModule.FakeApiFakeOuterStringSerializeRequest = {
  // string | Input string as post body (optional)
  body: body_example,
};
apiInstance.fakeOuterStringSerialize(body).then((data:any) => {
  console.log('API called successfully. Returned data: ', data);
}).catch((error:any) => console.error(error));
```

### Parameters

|Name | Type | Description  | Notes|
|------------- | ------------- | ------------- | -------------|
| **body** | **string**| Input string as post body | |


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
|**200** | Output string |  -  |

[[Back to top]](#) [[Back to API list]](README.md#documentation-for-api-endpoints) [[Back to Model list]](README.md#documentation-for-models) [[Back to README]](README.md)

# **fakePropertyEnumIntegerSerialize**
> OuterObjectWithEnumProperty fakePropertyEnumIntegerSerialize(outerObjectWithEnumProperty)

Test serialization of enum (int) properties with examples

### Example

```typescript
import * as ApiModule from '';

const configuration = ApiModule.createConfiguration();
const apiInstance = new ApiModule.FakeApi(configuration);

let body: ApiModule.FakeApiFakePropertyEnumIntegerSerializeRequest = {
  // OuterObjectWithEnumProperty | Input enum (int) as post body
  outerObjectWithEnumProperty: ,
};
apiInstance.fakePropertyEnumIntegerSerialize(body).then((data:any) => {
  console.log('API called successfully. Returned data: ', data);
}).catch((error:any) => console.error(error));
```

### Parameters

|Name | Type | Description  | Notes|
|------------- | ------------- | ------------- | -------------|
| **outerObjectWithEnumProperty** | **OuterObjectWithEnumProperty**| Input enum (int) as post body | |


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
|**200** | Output enum (int) |  -  |

[[Back to top]](#) [[Back to API list]](README.md#documentation-for-api-endpoints) [[Back to Model list]](README.md#documentation-for-models) [[Back to README]](README.md)

# **testBodyWithBinary**
> testBodyWithBinary(body)

For this test, the body has to be a binary file.

### Example

```typescript
import * as ApiModule from '';

const configuration = ApiModule.createConfiguration();
const apiInstance = new ApiModule.FakeApi(configuration);

let body: ApiModule.FakeApiTestBodyWithBinaryRequest = {
  // Blob | image to upload
  body: BINARY_DATA_HERE,
};
apiInstance.testBodyWithBinary(body).then((data:any) => {
  console.log('API called successfully. Returned data: ', data);
}).catch((error:any) => console.error(error));
```

### Parameters

|Name | Type | Description  | Notes|
|------------- | ------------- | ------------- | -------------|
| **body** | **Blob**| image to upload | |


### Return type

void (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: image/png
 - **Accept**: Not defined


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
|**200** | Success |  -  |

[[Back to top]](#) [[Back to API list]](README.md#documentation-for-api-endpoints) [[Back to Model list]](README.md#documentation-for-models) [[Back to README]](README.md)

# **testBodyWithFileSchema**
> testBodyWithFileSchema(fileSchemaTestClass)

For this test, the body for this request must reference a schema named `File`.

### Example

```typescript
import * as ApiModule from '';

const configuration = ApiModule.createConfiguration();
const apiInstance = new ApiModule.FakeApi(configuration);

let body: ApiModule.FakeApiTestBodyWithFileSchemaRequest = {
  // FileSchemaTestClass
  fileSchemaTestClass: ,
};
apiInstance.testBodyWithFileSchema(body).then((data:any) => {
  console.log('API called successfully. Returned data: ', data);
}).catch((error:any) => console.error(error));
```

### Parameters

|Name | Type | Description  | Notes|
|------------- | ------------- | ------------- | -------------|
| **fileSchemaTestClass** | **FileSchemaTestClass**|  | |


### Return type

void (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: Not defined


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
|**200** | Success |  -  |

[[Back to top]](#) [[Back to API list]](README.md#documentation-for-api-endpoints) [[Back to Model list]](README.md#documentation-for-models) [[Back to README]](README.md)

# **testBodyWithQueryParams**
> testBodyWithQueryParams(user)


### Example

```typescript
import * as ApiModule from '';

const configuration = ApiModule.createConfiguration();
const apiInstance = new ApiModule.FakeApi(configuration);

let body: ApiModule.FakeApiTestBodyWithQueryParamsRequest = {
  // string
  query: query_example,
  // User
  user: ,
};
apiInstance.testBodyWithQueryParams(body).then((data:any) => {
  console.log('API called successfully. Returned data: ', data);
}).catch((error:any) => console.error(error));
```

### Parameters

|Name | Type | Description  | Notes|
|------------- | ------------- | ------------- | -------------|
| **user** | **User**|  | |
| **query** | [**string**] |  | defaults to undefined|


### Return type

void (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: Not defined


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
|**200** | Success |  -  |

[[Back to top]](#) [[Back to API list]](README.md#documentation-for-api-endpoints) [[Back to Model list]](README.md#documentation-for-models) [[Back to README]](README.md)

# **testClientModel**
> Client testClientModel(client)

To test \"client\" model

### Example

```typescript
import * as ApiModule from '';

const configuration = ApiModule.createConfiguration();
const apiInstance = new ApiModule.FakeApi(configuration);

let body: ApiModule.FakeApiTestClientModelRequest = {
  // Client | client model
  client: ,
};
apiInstance.testClientModel(body).then((data:any) => {
  console.log('API called successfully. Returned data: ', data);
}).catch((error:any) => console.error(error));
```

### Parameters

|Name | Type | Description  | Notes|
|------------- | ------------- | ------------- | -------------|
| **client** | **Client**| client model | |


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
|**200** | successful operation |  -  |

[[Back to top]](#) [[Back to API list]](README.md#documentation-for-api-endpoints) [[Back to Model list]](README.md#documentation-for-models) [[Back to README]](README.md)

# **testEndpointParameters**
> testEndpointParameters()

Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 

### Example

```typescript
import * as ApiModule from '';

const configuration = ApiModule.createConfiguration();
const apiInstance = new ApiModule.FakeApi(configuration);

let body: ApiModule.FakeApiTestEndpointParametersRequest = {
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
};
apiInstance.testEndpointParameters(body).then((data:any) => {
  console.log('API called successfully. Returned data: ', data);
}).catch((error:any) => console.error(error));
```

### Parameters

|Name | Type | Description  | Notes|
|------------- | ------------- | ------------- | -------------|
| **number** | [**number**] | None | defaults to undefined|
| **_double** | [**number**] | None | defaults to undefined|
| **patternWithoutDelimiter** | [**string**] | None | defaults to undefined|
| **_byte** | [**string**] | None | defaults to undefined|
| **integer** | [**number**] | None | (optional) defaults to undefined|
| **int32** | [**number**] | None | (optional) defaults to undefined|
| **int64** | [**number**] | None | (optional) defaults to undefined|
| **_float** | [**number**] | None | (optional) defaults to undefined|
| **string** | [**string**] | None | (optional) defaults to undefined|
| **binary** | [**Blob**] | None | (optional) defaults to undefined|
| **date** | [**Date**] | None | (optional) defaults to undefined|
| **dateTime** | [**Date**] | None | (optional) defaults to undefined|
| **password** | [**string**] | None | (optional) defaults to undefined|
| **callback** | [**string**] | None | (optional) defaults to undefined|


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
|**400** | Invalid username supplied |  -  |
|**404** | User not found |  -  |

[[Back to top]](#) [[Back to API list]](README.md#documentation-for-api-endpoints) [[Back to Model list]](README.md#documentation-for-models) [[Back to README]](README.md)

# **testEnumParameters**
> testEnumParameters()

To test enum parameters

### Example

```typescript
import * as ApiModule from '';

const configuration = ApiModule.createConfiguration();
const apiInstance = new ApiModule.FakeApi(configuration);

let body: ApiModule.FakeApiTestEnumParametersRequest = {
  // Array<'>' | '$'> | Header parameter enum test (string array) (optional)
  enumHeaderStringArray: ,
  // '_abc' | '-efg' | '(xyz)' | Header parameter enum test (string) (optional)
  enumHeaderString: enumHeaderString_example,
  // Array<'>' | '$'> | Query parameter enum test (string array) (optional)
  enumQueryStringArray: ,
  // '_abc' | '-efg' | '(xyz)' | Query parameter enum test (string) (optional)
  enumQueryString: enumQueryString_example,
  // 1 | -2 | Query parameter enum test (double) (optional)
  enumQueryInteger: 56,
  // 1.1 | -1.2 | Query parameter enum test (double) (optional)
  enumQueryDouble: 1.2,
  // Array<EnumClass> (optional)
  enumQueryModelArray: ,
  // Array<string> | Form parameter enum test (string array) (optional)
  enumFormStringArray: ,
  // string | Form parameter enum test (string) (optional)
  enumFormString: enumFormString_example,
};
apiInstance.testEnumParameters(body).then((data:any) => {
  console.log('API called successfully. Returned data: ', data);
}).catch((error:any) => console.error(error));
```

### Parameters

|Name | Type | Description  | Notes|
|------------- | ------------- | ------------- | -------------|
| **enumHeaderStringArray** | **Array<&#39;&gt;&#39; &#124; &#39;$&#39;>** | Header parameter enum test (string array) | (optional) defaults to undefined|
| **enumHeaderString** | [**&#39;_abc&#39; | &#39;-efg&#39; | &#39;(xyz)&#39;**]**Array<&#39;_abc&#39; &#124; &#39;-efg&#39; &#124; &#39;(xyz)&#39;>** | Header parameter enum test (string) | (optional) defaults to '-efg'|
| **enumQueryStringArray** | **Array<&#39;&gt;&#39; &#124; &#39;$&#39;>** | Query parameter enum test (string array) | (optional) defaults to undefined|
| **enumQueryString** | [**&#39;_abc&#39; | &#39;-efg&#39; | &#39;(xyz)&#39;**]**Array<&#39;_abc&#39; &#124; &#39;-efg&#39; &#124; &#39;(xyz)&#39;>** | Query parameter enum test (string) | (optional) defaults to '-efg'|
| **enumQueryInteger** | [**1 | -2**]**Array<1 &#124; -2>** | Query parameter enum test (double) | (optional) defaults to undefined|
| **enumQueryDouble** | [**1.1 | -1.2**]**Array<1.1 &#124; -1.2>** | Query parameter enum test (double) | (optional) defaults to undefined|
| **enumQueryModelArray** | **Array&lt;EnumClass&gt;** |  | (optional) defaults to undefined|
| **enumFormStringArray** | **Array<&#39;&gt;&#39; &#124; &#39;$&#39;>** | Form parameter enum test (string array) | (optional) defaults to '$'|
| **enumFormString** | [**string**]**Array<&#39;_abc&#39; &#124; &#39;-efg&#39; &#124; &#39;(xyz)&#39;>** | Form parameter enum test (string) | (optional) defaults to '-efg'|


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
|**400** | Invalid request |  -  |
|**404** | Not found |  -  |

[[Back to top]](#) [[Back to API list]](README.md#documentation-for-api-endpoints) [[Back to Model list]](README.md#documentation-for-models) [[Back to README]](README.md)

# **testGroupParameters**
> testGroupParameters()

Fake endpoint to test group parameters (optional)

### Example

```typescript
import * as ApiModule from '';

const configuration = ApiModule.createConfiguration();
const apiInstance = new ApiModule.FakeApi(configuration);

let body: ApiModule.FakeApiTestGroupParametersRequest = {
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
};
apiInstance.testGroupParameters(body).then((data:any) => {
  console.log('API called successfully. Returned data: ', data);
}).catch((error:any) => console.error(error));
```

### Parameters

|Name | Type | Description  | Notes|
|------------- | ------------- | ------------- | -------------|
| **requiredStringGroup** | [**number**] | Required String in group parameters | defaults to undefined|
| **requiredBooleanGroup** | [**boolean**] | Required Boolean in group parameters | defaults to undefined|
| **requiredInt64Group** | [**number**] | Required Integer in group parameters | defaults to undefined|
| **stringGroup** | [**number**] | String in group parameters | (optional) defaults to undefined|
| **booleanGroup** | [**boolean**] | Boolean in group parameters | (optional) defaults to undefined|
| **int64Group** | [**number**] | Integer in group parameters | (optional) defaults to undefined|


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
|**400** | Something wrong |  -  |

[[Back to top]](#) [[Back to API list]](README.md#documentation-for-api-endpoints) [[Back to Model list]](README.md#documentation-for-models) [[Back to README]](README.md)

# **testInlineAdditionalProperties**
> testInlineAdditionalProperties(requestBody)



### Example

```typescript
import * as ApiModule from '';

const configuration = ApiModule.createConfiguration();
const apiInstance = new ApiModule.FakeApi(configuration);

let body: ApiModule.FakeApiTestInlineAdditionalPropertiesRequest = {
  // { [key: string]: string; } | request body
  requestBody: ,
};
apiInstance.testInlineAdditionalProperties(body).then((data:any) => {
  console.log('API called successfully. Returned data: ', data);
}).catch((error:any) => console.error(error));
```

### Parameters

|Name | Type | Description  | Notes|
|------------- | ------------- | ------------- | -------------|
| **requestBody** | **{ [key: string]: string; }**| request body | |


### Return type

void (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: Not defined


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
|**200** | successful operation |  -  |

[[Back to top]](#) [[Back to API list]](README.md#documentation-for-api-endpoints) [[Back to Model list]](README.md#documentation-for-models) [[Back to README]](README.md)

# **testJsonFormData**
> testJsonFormData()



### Example

```typescript
import * as ApiModule from '';

const configuration = ApiModule.createConfiguration();
const apiInstance = new ApiModule.FakeApi(configuration);

let body: ApiModule.FakeApiTestJsonFormDataRequest = {
  // string | field1
  param: param_example,
  // string | field2
  param2: param2_example,
};
apiInstance.testJsonFormData(body).then((data:any) => {
  console.log('API called successfully. Returned data: ', data);
}).catch((error:any) => console.error(error));
```

### Parameters

|Name | Type | Description  | Notes|
|------------- | ------------- | ------------- | -------------|
| **param** | [**string**] | field1 | defaults to undefined|
| **param2** | [**string**] | field2 | defaults to undefined|


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
|**200** | successful operation |  -  |

[[Back to top]](#) [[Back to API list]](README.md#documentation-for-api-endpoints) [[Back to Model list]](README.md#documentation-for-models) [[Back to README]](README.md)

# **testQueryParameterCollectionFormat**
> testQueryParameterCollectionFormat()

To test the collection format in query parameters

### Example

```typescript
import * as ApiModule from '';

const configuration = ApiModule.createConfiguration();
const apiInstance = new ApiModule.FakeApi(configuration);

let body: ApiModule.FakeApiTestQueryParameterCollectionFormatRequest = {
  // Array<string>
  pipe: ,
  // Array<string>
  ioutil: ,
  // Array<string>
  http: ,
  // Array<string>
  url: ,
  // Array<string>
  context: ,
  // string
  allowEmpty: allowEmpty_example,
  // { [key: string]: string; } (optional)
  language: ,
};
apiInstance.testQueryParameterCollectionFormat(body).then((data:any) => {
  console.log('API called successfully. Returned data: ', data);
}).catch((error:any) => console.error(error));
```

### Parameters

|Name | Type | Description  | Notes|
|------------- | ------------- | ------------- | -------------|
| **pipe** | **Array&lt;string&gt;** |  | defaults to undefined|
| **ioutil** | **Array&lt;string&gt;** |  | defaults to undefined|
| **http** | **Array&lt;string&gt;** |  | defaults to undefined|
| **url** | **Array&lt;string&gt;** |  | defaults to undefined|
| **context** | **Array&lt;string&gt;** |  | defaults to undefined|
| **allowEmpty** | [**string**] |  | defaults to undefined|
| **language** | **{ [key: string]: string; }** |  | (optional) defaults to undefined|


### Return type

void (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: Not defined


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
|**200** | Success |  -  |

[[Back to top]](#) [[Back to API list]](README.md#documentation-for-api-endpoints) [[Back to Model list]](README.md#documentation-for-models) [[Back to README]](README.md)


