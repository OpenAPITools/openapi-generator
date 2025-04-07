# HeaderApi

All URIs are relative to *http://localhost:3000*

|Method | HTTP request | Description|
|------------- | ------------- | -------------|
|[**testHeaderIntegerBooleanStringEnums**](#testheaderintegerbooleanstringenums) | **GET** /header/integer/boolean/string/enums | Test header parameter(s)|

# **testHeaderIntegerBooleanStringEnums**
> string testHeaderIntegerBooleanStringEnums()

Test header parameter(s)

### Example

```typescript
import {
    HeaderApi,
    Configuration
} from '@openapitools/typescript-axios-echo-api';

const configuration = new Configuration();
const apiInstance = new HeaderApi(configuration);

let integerHeader: number; // (optional) (default to undefined)
let booleanHeader: boolean; // (optional) (default to undefined)
let stringHeader: string; // (optional) (default to undefined)
let enumNonrefStringHeader: 'success' | 'failure' | 'unclassified'; // (optional) (default to undefined)
let enumRefStringHeader: StringEnumRef; // (optional) (default to undefined)

const { status, data } = await apiInstance.testHeaderIntegerBooleanStringEnums(
    integerHeader,
    booleanHeader,
    stringHeader,
    enumNonrefStringHeader,
    enumRefStringHeader
);
```

### Parameters

|Name | Type | Description  | Notes|
|------------- | ------------- | ------------- | -------------|
| **integerHeader** | [**number**] |  | (optional) defaults to undefined|
| **booleanHeader** | [**boolean**] |  | (optional) defaults to undefined|
| **stringHeader** | [**string**] |  | (optional) defaults to undefined|
| **enumNonrefStringHeader** | [**&#39;success&#39; | &#39;failure&#39; | &#39;unclassified&#39;**]**Array<&#39;success&#39; &#124; &#39;failure&#39; &#124; &#39;unclassified&#39;>** |  | (optional) defaults to undefined|
| **enumRefStringHeader** | **StringEnumRef** |  | (optional) defaults to undefined|


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

