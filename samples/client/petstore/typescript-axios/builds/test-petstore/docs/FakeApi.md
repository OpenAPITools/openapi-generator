# .FakeApi

All URIs are relative to *http://petstore.swagger.io:80/v2*

|Method | HTTP request | Description|
|------------- | ------------- | -------------|
|[**fakeHealthGet**](FakeApi.md#fakeHealthGet) | **GET** /fake/health | Health check endpoint|
|[**fakeOuterBooleanSerialize**](FakeApi.md#fakeOuterBooleanSerialize) | **POST** /fake/outer/boolean | |
|[**fakeOuterCompositeSerialize**](FakeApi.md#fakeOuterCompositeSerialize) | **POST** /fake/outer/composite | |
|[**fakeOuterNumberSerialize**](FakeApi.md#fakeOuterNumberSerialize) | **POST** /fake/outer/number | |
|[**fakeOuterStringSerialize**](FakeApi.md#fakeOuterStringSerialize) | **POST** /fake/outer/string | |
|[**getArrayOfEnums**](FakeApi.md#getArrayOfEnums) | **GET** /fake/array-of-enums | Array of Enums|
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

const configuration: ApiModule.Configuration = new ApiModule.Configuration();
const apiInstance: ApiModule.FakeApi = new ApiModule.FakeApi(configuration);

//let body: ApiModule. = {};

apiInstance.fakeHealthGet().then((data: any) => {
  console.log('API called successfully. Returned data: ', data);
}).catch((error: any) => {
  console.error(error);
});
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

# **fakeOuterBooleanSerialize**
> boolean fakeOuterBooleanSerialize()

Test serialization of outer boolean types

### Example

```typescript
import * as ApiModule from '';

const configuration: ApiModule.Configuration = new ApiModule.Configuration();
const apiInstance: ApiModule.FakeApi = new ApiModule.FakeApi(configuration);

const body: ApiModule.boolean = {
  // set here some attributes...
};  // boolean | Input boolean as post body

apiInstance.fakeOuterBooleanSerialize(body).then((data: any) => {
  console.log('API called successfully. Returned data: ', data);
}).catch((error: any) => {
  console.error(error);
});
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

const configuration: ApiModule.Configuration = new ApiModule.Configuration();
const apiInstance: ApiModule.FakeApi = new ApiModule.FakeApi(configuration);

const outerComposite: ApiModule.OuterComposite = {
  // set here some attributes...
};  // OuterComposite | Input composite as post body

apiInstance.fakeOuterCompositeSerialize(outerComposite).then((data: any) => {
  console.log('API called successfully. Returned data: ', data);
}).catch((error: any) => {
  console.error(error);
});
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

const configuration: ApiModule.Configuration = new ApiModule.Configuration();
const apiInstance: ApiModule.FakeApi = new ApiModule.FakeApi(configuration);

const body: ApiModule.number = {
  // set here some attributes...
};  // number | Input number as post body

apiInstance.fakeOuterNumberSerialize(body).then((data: any) => {
  console.log('API called successfully. Returned data: ', data);
}).catch((error: any) => {
  console.error(error);
});
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

const configuration: ApiModule.Configuration = new ApiModule.Configuration();
const apiInstance: ApiModule.FakeApi = new ApiModule.FakeApi(configuration);

const body: ApiModule.string = {
  // set here some attributes...
};  // string | Input string as post body

apiInstance.fakeOuterStringSerialize(body).then((data: any) => {
  console.log('API called successfully. Returned data: ', data);
}).catch((error: any) => {
  console.error(error);
});
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

# **getArrayOfEnums**
> Array<OuterEnum> getArrayOfEnums()


### Example

```typescript
import * as ApiModule from '';

const configuration: ApiModule.Configuration = new ApiModule.Configuration();
const apiInstance: ApiModule.FakeApi = new ApiModule.FakeApi(configuration);

//let body: ApiModule. = {};

apiInstance.getArrayOfEnums().then((data: any) => {
  console.log('API called successfully. Returned data: ', data);
}).catch((error: any) => {
  console.error(error);
});
```

### Parameters
This endpoint does not need any parameter.


### Return type

**Array<OuterEnum>**

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/json


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
|**200** | Got named array of enums |  -  |

[[Back to top]](#) [[Back to API list]](README.md#documentation-for-api-endpoints) [[Back to Model list]](README.md#documentation-for-models) [[Back to README]](README.md)

# **testBodyWithFileSchema**
> testBodyWithFileSchema(fileSchemaTestClass)

For this test, the body for this request much reference a schema named `File`.

### Example

```typescript
import * as ApiModule from '';

const configuration: ApiModule.Configuration = new ApiModule.Configuration();
const apiInstance: ApiModule.FakeApi = new ApiModule.FakeApi(configuration);

let fileSchemaTestClass: ApiModule.FileSchemaTestClass = ; // FileSchemaTestClass | 

apiInstance.testBodyWithFileSchema(fileSchemaTestClass).then(() => {
  console.log('API called successfully.');
}).catch((error: any) => {
  console.error(error);
});
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

const configuration: ApiModule.Configuration = new ApiModule.Configuration();
const apiInstance: ApiModule.FakeApi = new ApiModule.FakeApi(configuration);

let query: string = query_example; // string | 
let user: ApiModule.User = ; // User | 

apiInstance.testBodyWithQueryParams(query, user).then(() => {
  console.log('API called successfully.');
}).catch((error: any) => {
  console.error(error);
});
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

const configuration: ApiModule.Configuration = new ApiModule.Configuration();
const apiInstance: ApiModule.FakeApi = new ApiModule.FakeApi(configuration);

let client: ApiModule.Client = ; // Client | client model

apiInstance.testClientModel(client).then((data: any) => {
  console.log('API called successfully. Returned data: ', data);
}).catch((error: any) => {
  console.error(error);
});
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

const configuration: ApiModule.Configuration = new ApiModule.Configuration();
const apiInstance: ApiModule.FakeApi = new ApiModule.FakeApi(configuration);

let number: number = 8.14; // number | None
let _double: number = 1.2; // number | None
let patternWithoutDelimiter: string = patternWithoutDelimiter_example; // string | None
let _byte: string = BYTE_ARRAY_DATA_HERE; // string | None
const integer: ApiModule.number = {
  // set here some attributes...
}; , // number | None
const int32: ApiModule.number = {
  // set here some attributes...
}; , // number | None
const int64: ApiModule.number = {
  // set here some attributes...
}; , // number | None
const _float: ApiModule.number = {
  // set here some attributes...
}; , // number | None
const string: ApiModule.string = {
  // set here some attributes...
}; , // string | None
const binary: ApiModule.File = {
  // set here some attributes...
}; , // File | None
const date: ApiModule.string = {
  // set here some attributes...
}; , // string | None
const dateTime: ApiModule.string = {
  // set here some attributes...
}; , // string | None
const password: ApiModule.string = {
  // set here some attributes...
}; , // string | None
const callback: ApiModule.string = {
  // set here some attributes...
};  // string | None

apiInstance.testEndpointParameters(number, _double, patternWithoutDelimiter, _byte, callback).then(() => {
  console.log('API called successfully.');
}).catch((error: any) => {
  console.error(error);
});
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
| **binary** | [**File**] | None | (optional) defaults to undefined|
| **date** | [**string**] | None | (optional) defaults to undefined|
| **dateTime** | [**string**] | None | (optional) defaults to 2010-02-01T10:20:10.111110+01:00|
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

const configuration: ApiModule.Configuration = new ApiModule.Configuration();
const apiInstance: ApiModule.FakeApi = new ApiModule.FakeApi(configuration);

const enumHeaderStringArray: ApiModule.Array<'>' | '$'> = {
  // set here some attributes...
}; , // Array<'>' | '$'> | Header parameter enum test (string array)
const enumHeaderString: ApiModule.'_abc' | '-efg' | '(xyz)' = {
  // set here some attributes...
}; , // '_abc' | '-efg' | '(xyz)' | Header parameter enum test (string)
const enumQueryStringArray: ApiModule.Array<'>' | '$'> = {
  // set here some attributes...
}; , // Array<'>' | '$'> | Query parameter enum test (string array)
const enumQueryString: ApiModule.'_abc' | '-efg' | '(xyz)' = {
  // set here some attributes...
}; , // '_abc' | '-efg' | '(xyz)' | Query parameter enum test (string)
const enumQueryInteger: ApiModule.1 | -2 = {
  // set here some attributes...
}; , // 1 | -2 | Query parameter enum test (double)
const enumQueryDouble: ApiModule.1.1 | -1.2 = {
  // set here some attributes...
}; , // 1.1 | -1.2 | Query parameter enum test (double)
const enumFormStringArray: ApiModule.Array<string> = {
  // set here some attributes...
}; , // Array<string> | Form parameter enum test (string array)
const enumFormString: ApiModule.string = {
  // set here some attributes...
};  // string | Form parameter enum test (string)

apiInstance.testEnumParameters(enumFormString).then(() => {
  console.log('API called successfully.');
}).catch((error: any) => {
  console.error(error);
});
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

const configuration: ApiModule.Configuration = new ApiModule.Configuration();
const apiInstance: ApiModule.FakeApi = new ApiModule.FakeApi(configuration);

let requiredStringGroup: number = 56; // number | Required String in group parameters
let requiredBooleanGroup: boolean = true; // boolean | Required Boolean in group parameters
let requiredInt64Group: number = 789; // number | Required Integer in group parameters
const stringGroup: ApiModule.number = {
  // set here some attributes...
}; , // number | String in group parameters
const booleanGroup: ApiModule.boolean = {
  // set here some attributes...
}; , // boolean | Boolean in group parameters
const int64Group: ApiModule.number = {
  // set here some attributes...
};  // number | Integer in group parameters

apiInstance.testGroupParameters(requiredStringGroup, requiredBooleanGroup, requiredInt64Group, int64Group).then(() => {
  console.log('API called successfully.');
}).catch((error: any) => {
  console.error(error);
});
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

const configuration: ApiModule.Configuration = new ApiModule.Configuration();
const apiInstance: ApiModule.FakeApi = new ApiModule.FakeApi(configuration);

let requestBody: ApiModule.{ [key: string]: string; } = ; // { [key: string]: string; } | request body

apiInstance.testInlineAdditionalProperties(requestBody).then(() => {
  console.log('API called successfully.');
}).catch((error: any) => {
  console.error(error);
});
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

const configuration: ApiModule.Configuration = new ApiModule.Configuration();
const apiInstance: ApiModule.FakeApi = new ApiModule.FakeApi(configuration);

let param: string = param_example; // string | field1
let param2: string = param2_example; // string | field2

apiInstance.testJsonFormData(param, param2).then(() => {
  console.log('API called successfully.');
}).catch((error: any) => {
  console.error(error);
});
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

const configuration: ApiModule.Configuration = new ApiModule.Configuration();
const apiInstance: ApiModule.FakeApi = new ApiModule.FakeApi(configuration);

let pipe: ApiModule.Array<string> = ; // Array<string> | 
let ioutil: ApiModule.Array<string> = ; // Array<string> | 
let http: ApiModule.Array<string> = ; // Array<string> | 
let url: ApiModule.Array<string> = ; // Array<string> | 
let context: ApiModule.Array<string> = ; // Array<string> | 

apiInstance.testQueryParameterCollectionFormat(pipe, ioutil, http, url, context).then(() => {
  console.log('API called successfully.');
}).catch((error: any) => {
  console.error(error);
});
```

### Parameters

|Name | Type | Description  | Notes|
|------------- | ------------- | ------------- | -------------|
| **pipe** | **Array&lt;string&gt;** |  | defaults to undefined|
| **ioutil** | **Array&lt;string&gt;** |  | defaults to undefined|
| **http** | **Array&lt;string&gt;** |  | defaults to undefined|
| **url** | **Array&lt;string&gt;** |  | defaults to undefined|
| **context** | **Array&lt;string&gt;** |  | defaults to undefined|


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


