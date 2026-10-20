# PathApi

All URIs are relative to *http://localhost:3000*

|Method | HTTP request | Description|
|------------- | ------------- | -------------|
|[**testsPathStringPathStringIntegerPathIntegerEnumNonrefStringPathEnumRefStringPath**](#testspathstringpathstringintegerpathintegerenumnonrefstringpathenumrefstringpath) | **GET** /path/string/{path_string}/integer/{path_integer}/{enum_nonref_string_path}/{enum_ref_string_path} | Test path parameter(s)|

# **testsPathStringPathStringIntegerPathIntegerEnumNonrefStringPathEnumRefStringPath**
> string testsPathStringPathStringIntegerPathIntegerEnumNonrefStringPathEnumRefStringPath()

Test path parameter(s)

### Example

```typescript
import {
    PathApi,
    Configuration
} from '@openapitools/typescript-axios-echo-api';

const configuration = new Configuration();
const apiInstance = new PathApi(configuration);

let pathString: string; // (default to undefined)
let pathInteger: number; // (default to undefined)
let enumNonrefStringPath: 'success' | 'failure' | 'unclassified'; // (default to undefined)
let enumRefStringPath: StringEnumRef; // (default to undefined)

const { status, data } = await apiInstance.testsPathStringPathStringIntegerPathIntegerEnumNonrefStringPathEnumRefStringPath(
    pathString,
    pathInteger,
    enumNonrefStringPath,
    enumRefStringPath
);
```

### Parameters

|Name | Type | Description  | Notes|
|------------- | ------------- | ------------- | -------------|
| **pathString** | [**string**] |  | defaults to undefined|
| **pathInteger** | [**number**] |  | defaults to undefined|
| **enumNonrefStringPath** | [**&#39;success&#39; | &#39;failure&#39; | &#39;unclassified&#39;**]**Array<&#39;success&#39; &#124; &#39;failure&#39; &#124; &#39;unclassified&#39;>** |  | defaults to undefined|
| **enumRefStringPath** | **StringEnumRef** |  | defaults to undefined|


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

