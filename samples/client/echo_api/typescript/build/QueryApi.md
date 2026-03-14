# .QueryApi

All URIs are relative to *http://localhost:3000*

Method | HTTP request | Description
------------- | ------------- | -------------
[**deprecatedTest**](QueryApi.md#deprecatedTest) | **GET** /test/deprecated | Test deprecation
[**testEnumRefString**](QueryApi.md#testEnumRefString) | **GET** /query/enum_ref_string | Test query parameter(s)
[**testQueryDatetimeDateString**](QueryApi.md#testQueryDatetimeDateString) | **GET** /query/datetime/date/string | Test query parameter(s)
[**testQueryIntegerBooleanString**](QueryApi.md#testQueryIntegerBooleanString) | **GET** /query/integer/boolean/string | Test query parameter(s)
[**testQueryStyleDeepObjectExplodeTrueObject**](QueryApi.md#testQueryStyleDeepObjectExplodeTrueObject) | **GET** /query/style_deepObject/explode_true/object | Test query parameter(s)
[**testQueryStyleDeepObjectExplodeTrueObjectAllOf**](QueryApi.md#testQueryStyleDeepObjectExplodeTrueObjectAllOf) | **GET** /query/style_deepObject/explode_true/object/allOf | Test query parameter(s)
[**testQueryStyleFormExplodeFalseArrayInteger**](QueryApi.md#testQueryStyleFormExplodeFalseArrayInteger) | **GET** /query/style_form/explode_false/array_integer | Test query parameter(s)
[**testQueryStyleFormExplodeFalseArrayString**](QueryApi.md#testQueryStyleFormExplodeFalseArrayString) | **GET** /query/style_form/explode_false/array_string | Test query parameter(s)
[**testQueryStyleFormExplodeTrueArrayString**](QueryApi.md#testQueryStyleFormExplodeTrueArrayString) | **GET** /query/style_form/explode_true/array_string | Test query parameter(s)
[**testQueryStyleFormExplodeTrueObject**](QueryApi.md#testQueryStyleFormExplodeTrueObject) | **GET** /query/style_form/explode_true/object | Test query parameter(s)
[**testQueryStyleFormExplodeTrueObjectAllOf**](QueryApi.md#testQueryStyleFormExplodeTrueObjectAllOf) | **GET** /query/style_form/explode_true/object/allOf | Test query parameter(s)


# **deprecatedTest**
> string deprecatedTest()


### Example


```typescript
import { createConfiguration, QueryApi } from '';
import type { QueryApiDeprecatedTestRequest } from '';

const configuration = createConfiguration();
const apiInstance = new QueryApi(configuration);

const request: QueryApiDeprecatedTestRequest = {
    // name of pet (optional)
  name: "name_example",
};

const data = await apiInstance.deprecatedTest(request);
console.log('API called successfully. Returned data:', data);
```


### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **name** | [**string**] | name of pet | (optional) defaults to undefined


### Return type

**string**

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: text/plain


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
**200** | Successful operation |  -  |

[[Back to top]](#) [[Back to API list]](README.md#documentation-for-api-endpoints) [[Back to Model list]](README.md#documentation-for-models) [[Back to README]](README.md)

# **testEnumRefString**
> string testEnumRefString()

Test query parameter(s)

### Example


```typescript
import { createConfiguration, QueryApi } from '';
import type { QueryApiTestEnumRefStringRequest } from '';

const configuration = createConfiguration();
const apiInstance = new QueryApi(configuration);

const request: QueryApiTestEnumRefStringRequest = {
  
  enumNonrefStringQuery: "success",
  
  enumRefStringQuery: "success",
};

const data = await apiInstance.testEnumRefString(request);
console.log('API called successfully. Returned data:', data);
```


### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **enumNonrefStringQuery** | [**&#39;success&#39; | &#39;failure&#39; | &#39;unclassified&#39;**]**Array<&#39;success&#39; &#124; &#39;failure&#39; &#124; &#39;unclassified&#39;>** |  | (optional) defaults to undefined
 **enumRefStringQuery** | **StringEnumRef** |  | (optional) defaults to undefined


### Return type

**string**

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: text/plain


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
**200** | Successful operation |  -  |

[[Back to top]](#) [[Back to API list]](README.md#documentation-for-api-endpoints) [[Back to Model list]](README.md#documentation-for-models) [[Back to README]](README.md)

# **testQueryDatetimeDateString**
> string testQueryDatetimeDateString()

Test query parameter(s)

### Example


```typescript
import { createConfiguration, QueryApi } from '';
import type { QueryApiTestQueryDatetimeDateStringRequest } from '';

const configuration = createConfiguration();
const apiInstance = new QueryApi(configuration);

const request: QueryApiTestQueryDatetimeDateStringRequest = {
  
  datetimeQuery: new Date('1970-01-01T00:00:00.00Z'),
  
  dateQuery: new Date('1970-01-01').toISOString().split('T')[0];,
  
  stringQuery: "string_query_example",
};

const data = await apiInstance.testQueryDatetimeDateString(request);
console.log('API called successfully. Returned data:', data);
```


### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **datetimeQuery** | [**Date**] |  | (optional) defaults to undefined
 **dateQuery** | [**string**] |  | (optional) defaults to undefined
 **stringQuery** | [**string**] |  | (optional) defaults to undefined


### Return type

**string**

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: text/plain


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
**200** | Successful operation |  -  |

[[Back to top]](#) [[Back to API list]](README.md#documentation-for-api-endpoints) [[Back to Model list]](README.md#documentation-for-models) [[Back to README]](README.md)

# **testQueryIntegerBooleanString**
> string testQueryIntegerBooleanString()

Test query parameter(s)

### Example


```typescript
import { createConfiguration, QueryApi } from '';
import type { QueryApiTestQueryIntegerBooleanStringRequest } from '';

const configuration = createConfiguration();
const apiInstance = new QueryApi(configuration);

const request: QueryApiTestQueryIntegerBooleanStringRequest = {
  
  integerQuery: 1,
  
  booleanQuery: true,
  
  stringQuery: "string_query_example",
};

const data = await apiInstance.testQueryIntegerBooleanString(request);
console.log('API called successfully. Returned data:', data);
```


### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **integerQuery** | [**number**] |  | (optional) defaults to undefined
 **booleanQuery** | [**boolean**] |  | (optional) defaults to undefined
 **stringQuery** | [**string**] |  | (optional) defaults to undefined


### Return type

**string**

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: text/plain


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
**200** | Successful operation |  -  |

[[Back to top]](#) [[Back to API list]](README.md#documentation-for-api-endpoints) [[Back to Model list]](README.md#documentation-for-models) [[Back to README]](README.md)

# **testQueryStyleDeepObjectExplodeTrueObject**
> string testQueryStyleDeepObjectExplodeTrueObject()

Test query parameter(s)

### Example


```typescript
import { createConfiguration, QueryApi } from '';
import type { QueryApiTestQueryStyleDeepObjectExplodeTrueObjectRequest } from '';

const configuration = createConfiguration();
const apiInstance = new QueryApi(configuration);

const request: QueryApiTestQueryStyleDeepObjectExplodeTrueObjectRequest = {
  
  queryObject: {
    id: 10,
    name: "doggie",
    category: {
      id: 1,
      name: "Dogs",
    },
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
};

const data = await apiInstance.testQueryStyleDeepObjectExplodeTrueObject(request);
console.log('API called successfully. Returned data:', data);
```


### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **queryObject** | **Pet** |  | (optional) defaults to undefined


### Return type

**string**

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: text/plain


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
**200** | Successful operation |  -  |

[[Back to top]](#) [[Back to API list]](README.md#documentation-for-api-endpoints) [[Back to Model list]](README.md#documentation-for-models) [[Back to README]](README.md)

# **testQueryStyleDeepObjectExplodeTrueObjectAllOf**
> string testQueryStyleDeepObjectExplodeTrueObjectAllOf()

Test query parameter(s)

### Example


```typescript
import { createConfiguration, QueryApi } from '';
import type { QueryApiTestQueryStyleDeepObjectExplodeTrueObjectAllOfRequest } from '';

const configuration = createConfiguration();
const apiInstance = new QueryApi(configuration);

const request: QueryApiTestQueryStyleDeepObjectExplodeTrueObjectAllOfRequest = {
  
  queryObject: null,
};

const data = await apiInstance.testQueryStyleDeepObjectExplodeTrueObjectAllOf(request);
console.log('API called successfully. Returned data:', data);
```


### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **queryObject** | **TestQueryStyleDeepObjectExplodeTrueObjectAllOfQueryObjectParameter** |  | (optional) defaults to undefined


### Return type

**string**

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: text/plain


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
**200** | Successful operation |  -  |

[[Back to top]](#) [[Back to API list]](README.md#documentation-for-api-endpoints) [[Back to Model list]](README.md#documentation-for-models) [[Back to README]](README.md)

# **testQueryStyleFormExplodeFalseArrayInteger**
> string testQueryStyleFormExplodeFalseArrayInteger()

Test query parameter(s)

### Example


```typescript
import { createConfiguration, QueryApi } from '';
import type { QueryApiTestQueryStyleFormExplodeFalseArrayIntegerRequest } from '';

const configuration = createConfiguration();
const apiInstance = new QueryApi(configuration);

const request: QueryApiTestQueryStyleFormExplodeFalseArrayIntegerRequest = {
  
  queryObject: [
    1,
  ],
};

const data = await apiInstance.testQueryStyleFormExplodeFalseArrayInteger(request);
console.log('API called successfully. Returned data:', data);
```


### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **queryObject** | **Array&lt;number&gt;** |  | (optional) defaults to undefined


### Return type

**string**

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: text/plain


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
**200** | Successful operation |  -  |

[[Back to top]](#) [[Back to API list]](README.md#documentation-for-api-endpoints) [[Back to Model list]](README.md#documentation-for-models) [[Back to README]](README.md)

# **testQueryStyleFormExplodeFalseArrayString**
> string testQueryStyleFormExplodeFalseArrayString()

Test query parameter(s)

### Example


```typescript
import { createConfiguration, QueryApi } from '';
import type { QueryApiTestQueryStyleFormExplodeFalseArrayStringRequest } from '';

const configuration = createConfiguration();
const apiInstance = new QueryApi(configuration);

const request: QueryApiTestQueryStyleFormExplodeFalseArrayStringRequest = {
  
  queryObject: [
    "query_object_example",
  ],
};

const data = await apiInstance.testQueryStyleFormExplodeFalseArrayString(request);
console.log('API called successfully. Returned data:', data);
```


### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **queryObject** | **Array&lt;string&gt;** |  | (optional) defaults to undefined


### Return type

**string**

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: text/plain


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
**200** | Successful operation |  -  |

[[Back to top]](#) [[Back to API list]](README.md#documentation-for-api-endpoints) [[Back to Model list]](README.md#documentation-for-models) [[Back to README]](README.md)

# **testQueryStyleFormExplodeTrueArrayString**
> string testQueryStyleFormExplodeTrueArrayString()

Test query parameter(s)

### Example


```typescript
import { createConfiguration, QueryApi } from '';
import type { QueryApiTestQueryStyleFormExplodeTrueArrayStringRequest } from '';

const configuration = createConfiguration();
const apiInstance = new QueryApi(configuration);

const request: QueryApiTestQueryStyleFormExplodeTrueArrayStringRequest = {
  
  queryObject: {
    values: [
      "values_example",
    ],
  },
};

const data = await apiInstance.testQueryStyleFormExplodeTrueArrayString(request);
console.log('API called successfully. Returned data:', data);
```


### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **queryObject** | **TestQueryStyleFormExplodeTrueArrayStringQueryObjectParameter** |  | (optional) defaults to undefined


### Return type

**string**

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: text/plain


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
**200** | Successful operation |  -  |

[[Back to top]](#) [[Back to API list]](README.md#documentation-for-api-endpoints) [[Back to Model list]](README.md#documentation-for-models) [[Back to README]](README.md)

# **testQueryStyleFormExplodeTrueObject**
> string testQueryStyleFormExplodeTrueObject()

Test query parameter(s)

### Example


```typescript
import { createConfiguration, QueryApi } from '';
import type { QueryApiTestQueryStyleFormExplodeTrueObjectRequest } from '';

const configuration = createConfiguration();
const apiInstance = new QueryApi(configuration);

const request: QueryApiTestQueryStyleFormExplodeTrueObjectRequest = {
  
  queryObject: {
    id: 10,
    name: "doggie",
    category: {
      id: 1,
      name: "Dogs",
    },
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
};

const data = await apiInstance.testQueryStyleFormExplodeTrueObject(request);
console.log('API called successfully. Returned data:', data);
```


### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **queryObject** | **Pet** |  | (optional) defaults to undefined


### Return type

**string**

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: text/plain


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
**200** | Successful operation |  -  |

[[Back to top]](#) [[Back to API list]](README.md#documentation-for-api-endpoints) [[Back to Model list]](README.md#documentation-for-models) [[Back to README]](README.md)

# **testQueryStyleFormExplodeTrueObjectAllOf**
> string testQueryStyleFormExplodeTrueObjectAllOf()

Test query parameter(s)

### Example


```typescript
import { createConfiguration, QueryApi } from '';
import type { QueryApiTestQueryStyleFormExplodeTrueObjectAllOfRequest } from '';

const configuration = createConfiguration();
const apiInstance = new QueryApi(configuration);

const request: QueryApiTestQueryStyleFormExplodeTrueObjectAllOfRequest = {
  
  queryObject: null,
};

const data = await apiInstance.testQueryStyleFormExplodeTrueObjectAllOf(request);
console.log('API called successfully. Returned data:', data);
```


### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **queryObject** | **DataQuery** |  | (optional) defaults to undefined


### Return type

**string**

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: text/plain


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
**200** | Successful operation |  -  |

[[Back to top]](#) [[Back to API list]](README.md#documentation-for-api-endpoints) [[Back to Model list]](README.md#documentation-for-models) [[Back to README]](README.md)


