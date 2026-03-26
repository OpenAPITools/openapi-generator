# .HeaderApi

All URIs are relative to *http://localhost:3000*

Method | HTTP request | Description
------------- | ------------- | -------------
[**testHeaderIntegerBooleanStringEnums**](HeaderApi.md#testHeaderIntegerBooleanStringEnums) | **GET** /header/integer/boolean/string/enums | Test header parameter(s)


# **testHeaderIntegerBooleanStringEnums**
> string testHeaderIntegerBooleanStringEnums()

Test header parameter(s)

### Example


```typescript
import { createConfiguration, HeaderApi } from '';
import type { HeaderApiTestHeaderIntegerBooleanStringEnumsRequest } from '';

const configuration = createConfiguration();
const apiInstance = new HeaderApi(configuration);

const request: HeaderApiTestHeaderIntegerBooleanStringEnumsRequest = {
  
  integerHeader: 1,
  
  booleanHeader: true,
  
  stringHeader: "string_header_example",
  
  enumNonrefStringHeader: "success",
  
  enumRefStringHeader: "success",
};

const data = await apiInstance.testHeaderIntegerBooleanStringEnums(request);
console.log('API called successfully. Returned data:', data);
```


### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **integerHeader** | [**number**] |  | (optional) defaults to undefined
 **booleanHeader** | [**boolean**] |  | (optional) defaults to undefined
 **stringHeader** | [**string**] |  | (optional) defaults to undefined
 **enumNonrefStringHeader** | [**&#39;success&#39; | &#39;failure&#39; | &#39;unclassified&#39;**]**Array<&#39;success&#39; &#124; &#39;failure&#39; &#124; &#39;unclassified&#39;>** |  | (optional) defaults to undefined
 **enumRefStringHeader** | **StringEnumRef** |  | (optional) defaults to undefined


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


