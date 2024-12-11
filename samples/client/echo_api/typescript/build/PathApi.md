# .PathApi

All URIs are relative to *http://localhost:3000*

Method | HTTP request | Description
------------- | ------------- | -------------
[**testsPathStringPathStringIntegerPathIntegerEnumNonrefStringPathEnumRefStringPath**](PathApi.md#testsPathStringPathStringIntegerPathIntegerEnumNonrefStringPathEnumRefStringPath) | **GET** /path/string/{path_string}/integer/{path_integer}/{enum_nonref_string_path}/{enum_ref_string_path} | Test path parameter(s)


# **testsPathStringPathStringIntegerPathIntegerEnumNonrefStringPathEnumRefStringPath**
> string testsPathStringPathStringIntegerPathIntegerEnumNonrefStringPathEnumRefStringPath()

Test path parameter(s)

### Example


```typescript
import { createConfiguration, PathApi } from '';
import type { PathApiTestsPathStringPathStringIntegerPathIntegerEnumNonrefStringPathEnumRefStringPathRequest } from '';

const configuration = createConfiguration();
const apiInstance = new PathApi(configuration);

const request: PathApiTestsPathStringPathStringIntegerPathIntegerEnumNonrefStringPathEnumRefStringPathRequest = {
  
  pathString: "path_string_example",
  
  pathInteger: 1,
  
  enumNonrefStringPath: "success",
  
  enumRefStringPath: "success",
};

const data = await apiInstance.testsPathStringPathStringIntegerPathIntegerEnumNonrefStringPathEnumRefStringPath(request);
console.log('API called successfully. Returned data:', data);
```


### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **pathString** | [**string**] |  | defaults to undefined
 **pathInteger** | [**number**] |  | defaults to undefined
 **enumNonrefStringPath** | [**&#39;success&#39; | &#39;failure&#39; | &#39;unclassified&#39;**]**Array<&#39;success&#39; &#124; &#39;failure&#39; &#124; &#39;unclassified&#39;>** |  | defaults to undefined
 **enumRefStringPath** | **StringEnumRef** |  | defaults to undefined


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


