# FakeApi

All URIs are relative to *http://petstore.swagger.io:80/v2*

|Method | HTTP request | Description|
|------------- | ------------- | -------------|
|[**fakeHealthGet**](#fakehealthget) | **GET** /fake/health | Health check endpoint|
|[**fakeOuterBooleanSerialize**](#fakeouterbooleanserialize) | **POST** /fake/outer/boolean | |
|[**fakeOuterCompositeSerialize**](#fakeoutercompositeserialize) | **POST** /fake/outer/composite | |
|[**fakeOuterNumberSerialize**](#fakeouternumberserialize) | **POST** /fake/outer/number | |
|[**fakeOuterStringSerialize**](#fakeouterstringserialize) | **POST** /fake/outer/string | |
|[**testAdditionalPropertiesReference**](#testadditionalpropertiesreference) | **POST** /fake/additionalProperties-reference | test referenced additionalProperties|
|[**testBodyWithFileSchema**](#testbodywithfileschema) | **PUT** /fake/body-with-file-schema | |
|[**testBodyWithQueryParams**](#testbodywithqueryparams) | **PUT** /fake/body-with-query-params | |
|[**testClientModel**](#testclientmodel) | **PATCH** /fake | To test \&quot;client\&quot; model|
|[**testEndpointParameters**](#testendpointparameters) | **POST** /fake | Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 |
|[**testEnumParameters**](#testenumparameters) | **GET** /fake | To test enum parameters|
|[**testGroupParameters**](#testgroupparameters) | **DELETE** /fake | Fake endpoint to test group parameters (optional)|
|[**testInlineAdditionalProperties**](#testinlineadditionalproperties) | **POST** /fake/inline-additionalProperties | test inline additionalProperties|
|[**testInlineFreeformAdditionalProperties**](#testinlinefreeformadditionalproperties) | **POST** /fake/inline-freeform-additionalProperties | test inline free-form additionalProperties|
|[**testJsonFormData**](#testjsonformdata) | **GET** /fake/jsonFormData | test json serialization of form data|
|[**testQueryParameterCollectionFormat**](#testqueryparametercollectionformat) | **PUT** /fake/test-query-parameters | |
|[**testStringMapReference**](#teststringmapreference) | **POST** /fake/stringMap-reference | test referenced string map|
|[**testUniqueItemsHeaderAndQueryParameterCollectionFormat**](#testuniqueitemsheaderandqueryparametercollectionformat) | **PUT** /fake/test-unique-parameters | |

# **fakeHealthGet**
> HealthCheckResult fakeHealthGet()


### Example

```typescript
import {
    FakeApi,
    Configuration
} from './api';

const configuration = new Configuration();
const apiInstance = new FakeApi(configuration);

const { status, data } = await apiInstance.fakeHealthGet();
```

### Parameters
This endpoint does not have any parameters.


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

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **fakeOuterBooleanSerialize**
> boolean fakeOuterBooleanSerialize()

Test serialization of outer boolean types

### Example

```typescript
import {
    FakeApi,
    Configuration
} from './api';

const configuration = new Configuration();
const apiInstance = new FakeApi(configuration);

let body: boolean; //Input boolean as post body (optional)

const { status, data } = await apiInstance.fakeOuterBooleanSerialize(
    body
);
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

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **fakeOuterCompositeSerialize**
> OuterComposite fakeOuterCompositeSerialize()

Test serialization of object with outer number type

### Example

```typescript
import {
    FakeApi,
    Configuration,
    OuterComposite
} from './api';

const configuration = new Configuration();
const apiInstance = new FakeApi(configuration);

let outerComposite: OuterComposite; //Input composite as post body (optional)

const { status, data } = await apiInstance.fakeOuterCompositeSerialize(
    outerComposite
);
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

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **fakeOuterNumberSerialize**
> number fakeOuterNumberSerialize()

Test serialization of outer number types

### Example

```typescript
import {
    FakeApi,
    Configuration
} from './api';

const configuration = new Configuration();
const apiInstance = new FakeApi(configuration);

let body: number; //Input number as post body (optional)

const { status, data } = await apiInstance.fakeOuterNumberSerialize(
    body
);
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

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **fakeOuterStringSerialize**
> string fakeOuterStringSerialize()

Test serialization of outer string types

### Example

```typescript
import {
    FakeApi,
    Configuration
} from './api';

const configuration = new Configuration();
const apiInstance = new FakeApi(configuration);

let body: string; //Input string as post body (optional)

const { status, data } = await apiInstance.fakeOuterStringSerialize(
    body
);
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

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **testAdditionalPropertiesReference**
> testAdditionalPropertiesReference(requestBody)



### Example

```typescript
import {
    FakeApi,
    Configuration
} from './api';

const configuration = new Configuration();
const apiInstance = new FakeApi(configuration);

let requestBody: { [key: string]: any; }; //request body

const { status, data } = await apiInstance.testAdditionalPropertiesReference(
    requestBody
);
```

### Parameters

|Name | Type | Description  | Notes|
|------------- | ------------- | ------------- | -------------|
| **requestBody** | **{ [key: string]: any; }**| request body | |


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

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **testBodyWithFileSchema**
> testBodyWithFileSchema(fileSchemaTestClass)

For this test, the body for this request much reference a schema named `File`.

### Example

```typescript
import {
    FakeApi,
    Configuration,
    FileSchemaTestClass
} from './api';

const configuration = new Configuration();
const apiInstance = new FakeApi(configuration);

let fileSchemaTestClass: FileSchemaTestClass; //

const { status, data } = await apiInstance.testBodyWithFileSchema(
    fileSchemaTestClass
);
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

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **testBodyWithQueryParams**
> testBodyWithQueryParams(user)


### Example

```typescript
import {
    FakeApi,
    Configuration,
    User
} from './api';

const configuration = new Configuration();
const apiInstance = new FakeApi(configuration);

let query: string; // (default to undefined)
let user: User; //

const { status, data } = await apiInstance.testBodyWithQueryParams(
    query,
    user
);
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

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **testClientModel**
> Client testClientModel(client)

To test \"client\" model

### Example

```typescript
import {
    FakeApi,
    Configuration,
    Client
} from './api';

const configuration = new Configuration();
const apiInstance = new FakeApi(configuration);

let client: Client; //client model

const { status, data } = await apiInstance.testClientModel(
    client
);
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

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **testEndpointParameters**
> testEndpointParameters()

Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 

### Example

```typescript
import {
    FakeApi,
    Configuration
} from './api';

const configuration = new Configuration();
const apiInstance = new FakeApi(configuration);

let number: number; //None (default to undefined)
let _double: number; //None (default to undefined)
let patternWithoutDelimiter: string; //None (default to undefined)
let _byte: string; //None (default to undefined)
let integer: number; //None (optional) (default to undefined)
let int32: number; //None (optional) (default to undefined)
let int64: number; //None (optional) (default to undefined)
let _float: number; //None (optional) (default to undefined)
let string: string; //None (optional) (default to undefined)
let binary: File; //None (optional) (default to undefined)
let date: string; //None (optional) (default to undefined)
let dateTime: string; //None (optional) (default to undefined)
let password: string; //None (optional) (default to undefined)
let callback: string; //None (optional) (default to undefined)

const { status, data } = await apiInstance.testEndpointParameters(
    number,
    _double,
    patternWithoutDelimiter,
    _byte,
    integer,
    int32,
    int64,
    _float,
    string,
    binary,
    date,
    dateTime,
    password,
    callback
);
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
| **dateTime** | [**string**] | None | (optional) defaults to undefined|
| **password** | [**string**] | None | (optional) defaults to undefined|
| **callback** | [**string**] | None | (optional) defaults to undefined|


### Return type

void (empty response body)

### Authorization

[http_basic_test](../README.md#http_basic_test)

### HTTP request headers

 - **Content-Type**: application/x-www-form-urlencoded
 - **Accept**: Not defined


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
|**400** | Invalid username supplied |  -  |
|**404** | User not found |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **testEnumParameters**
> testEnumParameters()

To test enum parameters

### Example

```typescript
import {
    FakeApi,
    Configuration
} from './api';

const configuration = new Configuration();
const apiInstance = new FakeApi(configuration);

let enumHeaderStringArray: Array<'>' | '$'>; //Header parameter enum test (string array) (optional) (default to undefined)
let enumHeaderString: '_abc' | '-efg' | '(xyz)'; //Header parameter enum test (string) (optional) (default to '-efg')
let enumQueryStringArray: Array<'>' | '$'>; //Query parameter enum test (string array) (optional) (default to undefined)
let enumQueryString: '_abc' | '-efg' | '(xyz)'; //Query parameter enum test (string) (optional) (default to '-efg')
let enumQueryInteger: 1 | -2; //Query parameter enum test (double) (optional) (default to undefined)
let enumQueryDouble: 1.1 | -1.2; //Query parameter enum test (double) (optional) (default to undefined)
let enumFormStringArray: Array<string>; //Form parameter enum test (string array) (optional) (default to '$')
let enumFormString: string; //Form parameter enum test (string) (optional) (default to '-efg')

const { status, data } = await apiInstance.testEnumParameters(
    enumHeaderStringArray,
    enumHeaderString,
    enumQueryStringArray,
    enumQueryString,
    enumQueryInteger,
    enumQueryDouble,
    enumFormStringArray,
    enumFormString
);
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

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **testGroupParameters**
> testGroupParameters()

Fake endpoint to test group parameters (optional)

### Example

```typescript
import {
    FakeApi,
    Configuration
} from './api';

const configuration = new Configuration();
const apiInstance = new FakeApi(configuration);

let requiredStringGroup: number; //Required String in group parameters (default to undefined)
let requiredBooleanGroup: boolean; //Required Boolean in group parameters (default to undefined)
let requiredInt64Group: number; //Required Integer in group parameters (default to undefined)
let stringGroup: number; //String in group parameters (optional) (default to undefined)
let booleanGroup: boolean; //Boolean in group parameters (optional) (default to undefined)
let int64Group: number; //Integer in group parameters (optional) (default to undefined)

const { status, data } = await apiInstance.testGroupParameters(
    requiredStringGroup,
    requiredBooleanGroup,
    requiredInt64Group,
    stringGroup,
    booleanGroup,
    int64Group
);
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

[bearer_test](../README.md#bearer_test)

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: Not defined


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
|**400** | Something wrong |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **testInlineAdditionalProperties**
> testInlineAdditionalProperties(requestBody)



### Example

```typescript
import {
    FakeApi,
    Configuration
} from './api';

const configuration = new Configuration();
const apiInstance = new FakeApi(configuration);

let requestBody: { [key: string]: string; }; //request body

const { status, data } = await apiInstance.testInlineAdditionalProperties(
    requestBody
);
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

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **testInlineFreeformAdditionalProperties**
> testInlineFreeformAdditionalProperties(testInlineFreeformAdditionalPropertiesRequest)



### Example

```typescript
import {
    FakeApi,
    Configuration,
    TestInlineFreeformAdditionalPropertiesRequest
} from './api';

const configuration = new Configuration();
const apiInstance = new FakeApi(configuration);

let testInlineFreeformAdditionalPropertiesRequest: TestInlineFreeformAdditionalPropertiesRequest; //request body

const { status, data } = await apiInstance.testInlineFreeformAdditionalProperties(
    testInlineFreeformAdditionalPropertiesRequest
);
```

### Parameters

|Name | Type | Description  | Notes|
|------------- | ------------- | ------------- | -------------|
| **testInlineFreeformAdditionalPropertiesRequest** | **TestInlineFreeformAdditionalPropertiesRequest**| request body | |


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

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **testJsonFormData**
> testJsonFormData()



### Example

```typescript
import {
    FakeApi,
    Configuration
} from './api';

const configuration = new Configuration();
const apiInstance = new FakeApi(configuration);

let param: string; //field1 (default to undefined)
let param2: string; //field2 (default to undefined)

const { status, data } = await apiInstance.testJsonFormData(
    param,
    param2
);
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

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **testQueryParameterCollectionFormat**
> testQueryParameterCollectionFormat()

To test the collection format in query parameters

### Example

```typescript
import {
    FakeApi,
    Configuration
} from './api';

const configuration = new Configuration();
const apiInstance = new FakeApi(configuration);

let pipe: Array<string>; // (default to undefined)
let ioutil: Array<string>; // (default to undefined)
let http: Array<string>; // (default to undefined)
let url: Array<string>; // (default to undefined)
let context: Array<string>; // (default to undefined)

const { status, data } = await apiInstance.testQueryParameterCollectionFormat(
    pipe,
    ioutil,
    http,
    url,
    context
);
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

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **testStringMapReference**
> testStringMapReference(requestBody)



### Example

```typescript
import {
    FakeApi,
    Configuration
} from './api';

const configuration = new Configuration();
const apiInstance = new FakeApi(configuration);

let requestBody: { [key: string]: string; }; //request body

const { status, data } = await apiInstance.testStringMapReference(
    requestBody
);
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

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **testUniqueItemsHeaderAndQueryParameterCollectionFormat**
> Set<Pet> testUniqueItemsHeaderAndQueryParameterCollectionFormat()

To test unique items in header and query parameters

### Example

```typescript
import {
    FakeApi,
    Configuration
} from './api';

const configuration = new Configuration();
const apiInstance = new FakeApi(configuration);

let queryUnique: Set<string>; // (default to undefined)
let headerUnique: Set<string>; // (default to undefined)

const { status, data } = await apiInstance.testUniqueItemsHeaderAndQueryParameterCollectionFormat(
    queryUnique,
    headerUnique
);
```

### Parameters

|Name | Type | Description  | Notes|
|------------- | ------------- | ------------- | -------------|
| **queryUnique** | **Set&lt;string&gt;** |  | defaults to undefined|
| **headerUnique** | **Set&lt;string&gt;** |  | defaults to undefined|


### Return type

**Set<Pet>**

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/json


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
|**200** | Success |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

