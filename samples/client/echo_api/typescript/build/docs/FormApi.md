# .FormApi

All URIs are relative to *http://localhost:3000*

Method | HTTP request | Description
------------- | ------------- | -------------
[**testFormIntegerBooleanString**](FormApi.md#testFormIntegerBooleanString) | **POST** /form/integer/boolean/string | Test form parameter(s)
[**testFormObjectMultipart**](FormApi.md#testFormObjectMultipart) | **POST** /form/object/multipart | Test form parameter(s) for multipart schema
[**testFormOneof**](FormApi.md#testFormOneof) | **POST** /form/oneof | Test form parameter(s) for oneOf schema


# **testFormIntegerBooleanString**
> string testFormIntegerBooleanString()

Test form parameter(s)

### Example


```typescript
import { createConfiguration, FormApi } from '';
import type { FormApiTestFormIntegerBooleanStringRequest } from '';

const configuration = createConfiguration();
const apiInstance = new FormApi(configuration);

const request: FormApiTestFormIntegerBooleanStringRequest = {
  
  integerForm: 1,
  
  booleanForm: true,
  
  stringForm: "stringForm_example",
};

const data = await apiInstance.testFormIntegerBooleanString(request);
console.log('API called successfully. Returned data:', data);
```


### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **integerForm** | [**number**] |  | (optional) defaults to undefined
 **booleanForm** | [**boolean**] |  | (optional) defaults to undefined
 **stringForm** | [**string**] |  | (optional) defaults to undefined


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
**200** | Successful operation |  -  |

[[Back to top]](#) [[Back to API list]](README.md#documentation-for-api-endpoints) [[Back to Model list]](README.md#documentation-for-models) [[Back to README]](README.md)

# **testFormObjectMultipart**
> string testFormObjectMultipart()

Test form parameter(s) for multipart schema

### Example


```typescript
import { createConfiguration, FormApi } from '';
import type { FormApiTestFormObjectMultipartRequest } from '';

const configuration = createConfiguration();
const apiInstance = new FormApi(configuration);

const request: FormApiTestFormObjectMultipartRequest = {
  
  marker: {
    name: "name_example",
  },
};

const data = await apiInstance.testFormObjectMultipart(request);
console.log('API called successfully. Returned data:', data);
```


### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **marker** | **TestFormObjectMultipartRequestMarker** |  | defaults to undefined


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
**200** | Successful operation |  -  |

[[Back to top]](#) [[Back to API list]](README.md#documentation-for-api-endpoints) [[Back to Model list]](README.md#documentation-for-models) [[Back to README]](README.md)

# **testFormOneof**
> string testFormOneof()

Test form parameter(s) for oneOf schema

### Example


```typescript
import { createConfiguration, FormApi } from '';
import type { FormApiTestFormOneofRequest } from '';

const configuration = createConfiguration();
const apiInstance = new FormApi(configuration);

const request: FormApiTestFormOneofRequest = {
  
  form1: "form1_example",
  
  form2: 1,
  
  form3: "form3_example",
  
  form4: true,
  
  id: 1,
  
  name: "name_example",
};

const data = await apiInstance.testFormOneof(request);
console.log('API called successfully. Returned data:', data);
```


### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **form1** | [**string**] |  | (optional) defaults to undefined
 **form2** | [**number**] |  | (optional) defaults to undefined
 **form3** | [**string**] |  | (optional) defaults to undefined
 **form4** | [**boolean**] |  | (optional) defaults to undefined
 **id** | [**number**] |  | (optional) defaults to undefined
 **name** | [**string**] |  | (optional) defaults to undefined


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
**200** | Successful operation |  -  |

[[Back to top]](#) [[Back to API list]](README.md#documentation-for-api-endpoints) [[Back to Model list]](README.md#documentation-for-models) [[Back to README]](README.md)


