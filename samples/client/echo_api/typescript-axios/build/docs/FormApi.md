# FormApi

All URIs are relative to *http://localhost:3000*

|Method | HTTP request | Description|
|------------- | ------------- | -------------|
|[**testFormIntegerBooleanString**](#testformintegerbooleanstring) | **POST** /form/integer/boolean/string | Test form parameter(s)|
|[**testFormObjectMultipart**](#testformobjectmultipart) | **POST** /form/object/multipart | Test form parameter(s) for multipart schema|
|[**testFormOneof**](#testformoneof) | **POST** /form/oneof | Test form parameter(s) for oneOf schema|

# **testFormIntegerBooleanString**
> string testFormIntegerBooleanString()

Test form parameter(s)

### Example

```typescript
import {
    FormApi,
    Configuration
} from '@openapitools/typescript-axios-echo-api';

const configuration = new Configuration();
const apiInstance = new FormApi(configuration);

let integerForm: number; // (optional) (default to undefined)
let booleanForm: boolean; // (optional) (default to undefined)
let stringForm: string; // (optional) (default to undefined)

const { status, data } = await apiInstance.testFormIntegerBooleanString(
    integerForm,
    booleanForm,
    stringForm
);
```

### Parameters

|Name | Type | Description  | Notes|
|------------- | ------------- | ------------- | -------------|
| **integerForm** | [**number**] |  | (optional) defaults to undefined|
| **booleanForm** | [**boolean**] |  | (optional) defaults to undefined|
| **stringForm** | [**string**] |  | (optional) defaults to undefined|


### Return type

**string**

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/x-www-form-urlencoded
 - **Accept**: text/plain


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
|**200** | Successful operation |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **testFormObjectMultipart**
> string testFormObjectMultipart()

Test form parameter(s) for multipart schema

### Example

```typescript
import {
    FormApi,
    Configuration,
    TestFormObjectMultipartRequestMarker
} from '@openapitools/typescript-axios-echo-api';

const configuration = new Configuration();
const apiInstance = new FormApi(configuration);

let marker: TestFormObjectMultipartRequestMarker; // (default to undefined)

const { status, data } = await apiInstance.testFormObjectMultipart(
    marker
);
```

### Parameters

|Name | Type | Description  | Notes|
|------------- | ------------- | ------------- | -------------|
| **marker** | **TestFormObjectMultipartRequestMarker** |  | defaults to undefined|


### Return type

**string**

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: multipart/form-data
 - **Accept**: text/plain


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
|**200** | Successful operation |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **testFormOneof**
> string testFormOneof()

Test form parameter(s) for oneOf schema

### Example

```typescript
import {
    FormApi,
    Configuration
} from '@openapitools/typescript-axios-echo-api';

const configuration = new Configuration();
const apiInstance = new FormApi(configuration);

let form1: string; // (optional) (default to undefined)
let form2: number; // (optional) (default to undefined)
let form3: string; // (optional) (default to undefined)
let form4: boolean; // (optional) (default to undefined)
let id: number; // (optional) (default to undefined)
let name: string; // (optional) (default to undefined)

const { status, data } = await apiInstance.testFormOneof(
    form1,
    form2,
    form3,
    form4,
    id,
    name
);
```

### Parameters

|Name | Type | Description  | Notes|
|------------- | ------------- | ------------- | -------------|
| **form1** | [**string**] |  | (optional) defaults to undefined|
| **form2** | [**number**] |  | (optional) defaults to undefined|
| **form3** | [**string**] |  | (optional) defaults to undefined|
| **form4** | [**boolean**] |  | (optional) defaults to undefined|
| **id** | [**number**] |  | (optional) defaults to undefined|
| **name** | [**string**] |  | (optional) defaults to undefined|


### Return type

**string**

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/x-www-form-urlencoded
 - **Accept**: text/plain


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
|**200** | Successful operation |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

