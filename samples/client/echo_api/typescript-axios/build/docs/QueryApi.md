# QueryApi

All URIs are relative to *http://localhost:3000*

|Method | HTTP request | Description|
|------------- | ------------- | -------------|
|[**testEnumRefString**](#testenumrefstring) | **GET** /query/enum_ref_string | Test query parameter(s)|
|[**testQueryDatetimeDateString**](#testquerydatetimedatestring) | **GET** /query/datetime/date/string | Test query parameter(s)|
|[**testQueryIntegerBooleanString**](#testqueryintegerbooleanstring) | **GET** /query/integer/boolean/string | Test query parameter(s)|
|[**testQueryStyleDeepObjectExplodeTrueObject**](#testquerystyledeepobjectexplodetrueobject) | **GET** /query/style_deepObject/explode_true/object | Test query parameter(s)|
|[**testQueryStyleDeepObjectExplodeTrueObjectAllOf**](#testquerystyledeepobjectexplodetrueobjectallof) | **GET** /query/style_deepObject/explode_true/object/allOf | Test query parameter(s)|
|[**testQueryStyleFormExplodeFalseArrayInteger**](#testquerystyleformexplodefalsearrayinteger) | **GET** /query/style_form/explode_false/array_integer | Test query parameter(s)|
|[**testQueryStyleFormExplodeFalseArrayString**](#testquerystyleformexplodefalsearraystring) | **GET** /query/style_form/explode_false/array_string | Test query parameter(s)|
|[**testQueryStyleFormExplodeTrueArrayString**](#testquerystyleformexplodetruearraystring) | **GET** /query/style_form/explode_true/array_string | Test query parameter(s)|
|[**testQueryStyleFormExplodeTrueObject**](#testquerystyleformexplodetrueobject) | **GET** /query/style_form/explode_true/object | Test query parameter(s)|
|[**testQueryStyleFormExplodeTrueObjectAllOf**](#testquerystyleformexplodetrueobjectallof) | **GET** /query/style_form/explode_true/object/allOf | Test query parameter(s)|

# **testEnumRefString**
> string testEnumRefString()

Test query parameter(s)

### Example

```typescript
import {
    QueryApi,
    Configuration
} from '@openapitools/typescript-axios-echo-api';

const configuration = new Configuration();
const apiInstance = new QueryApi(configuration);

let enumNonrefStringQuery: 'success' | 'failure' | 'unclassified'; // (optional) (default to undefined)
let enumRefStringQuery: StringEnumRef; // (optional) (default to undefined)

const { status, data } = await apiInstance.testEnumRefString(
    enumNonrefStringQuery,
    enumRefStringQuery
);
```

### Parameters

|Name | Type | Description  | Notes|
|------------- | ------------- | ------------- | -------------|
| **enumNonrefStringQuery** | [**&#39;success&#39; | &#39;failure&#39; | &#39;unclassified&#39;**]**Array<&#39;success&#39; &#124; &#39;failure&#39; &#124; &#39;unclassified&#39;>** |  | (optional) defaults to undefined|
| **enumRefStringQuery** | **StringEnumRef** |  | (optional) defaults to undefined|


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
|**200** | Successful operation |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **testQueryDatetimeDateString**
> string testQueryDatetimeDateString()

Test query parameter(s)

### Example

```typescript
import {
    QueryApi,
    Configuration
} from '@openapitools/typescript-axios-echo-api';

const configuration = new Configuration();
const apiInstance = new QueryApi(configuration);

let datetimeQuery: string; // (optional) (default to undefined)
let dateQuery: string; // (optional) (default to undefined)
let stringQuery: string; // (optional) (default to undefined)

const { status, data } = await apiInstance.testQueryDatetimeDateString(
    datetimeQuery,
    dateQuery,
    stringQuery
);
```

### Parameters

|Name | Type | Description  | Notes|
|------------- | ------------- | ------------- | -------------|
| **datetimeQuery** | [**string**] |  | (optional) defaults to undefined|
| **dateQuery** | [**string**] |  | (optional) defaults to undefined|
| **stringQuery** | [**string**] |  | (optional) defaults to undefined|


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
|**200** | Successful operation |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **testQueryIntegerBooleanString**
> string testQueryIntegerBooleanString()

Test query parameter(s)

### Example

```typescript
import {
    QueryApi,
    Configuration
} from '@openapitools/typescript-axios-echo-api';

const configuration = new Configuration();
const apiInstance = new QueryApi(configuration);

let integerQuery: number; // (optional) (default to undefined)
let booleanQuery: boolean; // (optional) (default to undefined)
let stringQuery: string; // (optional) (default to undefined)

const { status, data } = await apiInstance.testQueryIntegerBooleanString(
    integerQuery,
    booleanQuery,
    stringQuery
);
```

### Parameters

|Name | Type | Description  | Notes|
|------------- | ------------- | ------------- | -------------|
| **integerQuery** | [**number**] |  | (optional) defaults to undefined|
| **booleanQuery** | [**boolean**] |  | (optional) defaults to undefined|
| **stringQuery** | [**string**] |  | (optional) defaults to undefined|


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
|**200** | Successful operation |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **testQueryStyleDeepObjectExplodeTrueObject**
> string testQueryStyleDeepObjectExplodeTrueObject()

Test query parameter(s)

### Example

```typescript
import {
    QueryApi,
    Configuration,
    Pet
} from '@openapitools/typescript-axios-echo-api';

const configuration = new Configuration();
const apiInstance = new QueryApi(configuration);

let queryObject: Pet; // (optional) (default to undefined)

const { status, data } = await apiInstance.testQueryStyleDeepObjectExplodeTrueObject(
    queryObject
);
```

### Parameters

|Name | Type | Description  | Notes|
|------------- | ------------- | ------------- | -------------|
| **queryObject** | **Pet** |  | (optional) defaults to undefined|


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
|**200** | Successful operation |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **testQueryStyleDeepObjectExplodeTrueObjectAllOf**
> string testQueryStyleDeepObjectExplodeTrueObjectAllOf()

Test query parameter(s)

### Example

```typescript
import {
    QueryApi,
    Configuration,
    TestQueryStyleDeepObjectExplodeTrueObjectAllOfQueryObjectParameter
} from '@openapitools/typescript-axios-echo-api';

const configuration = new Configuration();
const apiInstance = new QueryApi(configuration);

let queryObject: TestQueryStyleDeepObjectExplodeTrueObjectAllOfQueryObjectParameter; // (optional) (default to undefined)

const { status, data } = await apiInstance.testQueryStyleDeepObjectExplodeTrueObjectAllOf(
    queryObject
);
```

### Parameters

|Name | Type | Description  | Notes|
|------------- | ------------- | ------------- | -------------|
| **queryObject** | **TestQueryStyleDeepObjectExplodeTrueObjectAllOfQueryObjectParameter** |  | (optional) defaults to undefined|


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
|**200** | Successful operation |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **testQueryStyleFormExplodeFalseArrayInteger**
> string testQueryStyleFormExplodeFalseArrayInteger()

Test query parameter(s)

### Example

```typescript
import {
    QueryApi,
    Configuration
} from '@openapitools/typescript-axios-echo-api';

const configuration = new Configuration();
const apiInstance = new QueryApi(configuration);

let queryObject: Array<number>; // (optional) (default to undefined)

const { status, data } = await apiInstance.testQueryStyleFormExplodeFalseArrayInteger(
    queryObject
);
```

### Parameters

|Name | Type | Description  | Notes|
|------------- | ------------- | ------------- | -------------|
| **queryObject** | **Array&lt;number&gt;** |  | (optional) defaults to undefined|


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
|**200** | Successful operation |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **testQueryStyleFormExplodeFalseArrayString**
> string testQueryStyleFormExplodeFalseArrayString()

Test query parameter(s)

### Example

```typescript
import {
    QueryApi,
    Configuration
} from '@openapitools/typescript-axios-echo-api';

const configuration = new Configuration();
const apiInstance = new QueryApi(configuration);

let queryObject: Array<string>; // (optional) (default to undefined)

const { status, data } = await apiInstance.testQueryStyleFormExplodeFalseArrayString(
    queryObject
);
```

### Parameters

|Name | Type | Description  | Notes|
|------------- | ------------- | ------------- | -------------|
| **queryObject** | **Array&lt;string&gt;** |  | (optional) defaults to undefined|


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
|**200** | Successful operation |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **testQueryStyleFormExplodeTrueArrayString**
> string testQueryStyleFormExplodeTrueArrayString()

Test query parameter(s)

### Example

```typescript
import {
    QueryApi,
    Configuration,
    TestQueryStyleFormExplodeTrueArrayStringQueryObjectParameter
} from '@openapitools/typescript-axios-echo-api';

const configuration = new Configuration();
const apiInstance = new QueryApi(configuration);

let queryObject: TestQueryStyleFormExplodeTrueArrayStringQueryObjectParameter; // (optional) (default to undefined)

const { status, data } = await apiInstance.testQueryStyleFormExplodeTrueArrayString(
    queryObject
);
```

### Parameters

|Name | Type | Description  | Notes|
|------------- | ------------- | ------------- | -------------|
| **queryObject** | **TestQueryStyleFormExplodeTrueArrayStringQueryObjectParameter** |  | (optional) defaults to undefined|


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
|**200** | Successful operation |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **testQueryStyleFormExplodeTrueObject**
> string testQueryStyleFormExplodeTrueObject()

Test query parameter(s)

### Example

```typescript
import {
    QueryApi,
    Configuration,
    Pet
} from '@openapitools/typescript-axios-echo-api';

const configuration = new Configuration();
const apiInstance = new QueryApi(configuration);

let queryObject: Pet; // (optional) (default to undefined)

const { status, data } = await apiInstance.testQueryStyleFormExplodeTrueObject(
    queryObject
);
```

### Parameters

|Name | Type | Description  | Notes|
|------------- | ------------- | ------------- | -------------|
| **queryObject** | **Pet** |  | (optional) defaults to undefined|


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
|**200** | Successful operation |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **testQueryStyleFormExplodeTrueObjectAllOf**
> string testQueryStyleFormExplodeTrueObjectAllOf()

Test query parameter(s)

### Example

```typescript
import {
    QueryApi,
    Configuration,
    DataQuery
} from '@openapitools/typescript-axios-echo-api';

const configuration = new Configuration();
const apiInstance = new QueryApi(configuration);

let queryObject: DataQuery; // (optional) (default to undefined)

const { status, data } = await apiInstance.testQueryStyleFormExplodeTrueObjectAllOf(
    queryObject
);
```

### Parameters

|Name | Type | Description  | Notes|
|------------- | ------------- | ------------- | -------------|
| **queryObject** | **DataQuery** |  | (optional) defaults to undefined|


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
|**200** | Successful operation |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

