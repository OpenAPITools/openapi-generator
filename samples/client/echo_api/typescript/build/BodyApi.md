# .BodyApi

All URIs are relative to *http://localhost:3000*

Method | HTTP request | Description
------------- | ------------- | -------------
[**testBinaryGif**](BodyApi.md#testBinaryGif) | **POST** /binary/gif | Test binary (gif) response body
[**testBodyApplicationOctetstreamBinary**](BodyApi.md#testBodyApplicationOctetstreamBinary) | **POST** /body/application/octetstream/binary | Test body parameter(s)
[**testBodyMultipartFormdataArrayOfBinary**](BodyApi.md#testBodyMultipartFormdataArrayOfBinary) | **POST** /body/application/octetstream/array_of_binary | Test array of binary in multipart mime
[**testBodyMultipartFormdataSingleBinary**](BodyApi.md#testBodyMultipartFormdataSingleBinary) | **POST** /body/application/octetstream/single_binary | Test single binary in multipart mime
[**testEchoBodyAllOfPet**](BodyApi.md#testEchoBodyAllOfPet) | **POST** /echo/body/allOf/Pet | Test body parameter(s)
[**testEchoBodyFreeFormObjectResponseString**](BodyApi.md#testEchoBodyFreeFormObjectResponseString) | **POST** /echo/body/FreeFormObject/response_string | Test free form object
[**testEchoBodyPet**](BodyApi.md#testEchoBodyPet) | **POST** /echo/body/Pet | Test body parameter(s)
[**testEchoBodyPetResponseString**](BodyApi.md#testEchoBodyPetResponseString) | **POST** /echo/body/Pet/response_string | Test empty response body
[**testEchoBodyStringEnum**](BodyApi.md#testEchoBodyStringEnum) | **POST** /echo/body/string_enum | Test string enum response body
[**testEchoBodyTagResponseString**](BodyApi.md#testEchoBodyTagResponseString) | **POST** /echo/body/Tag/response_string | Test empty json (request body)


# **testBinaryGif**
> HttpFile testBinaryGif()

Test binary (gif) response body

### Example


```typescript
import { createConfiguration, BodyApi } from '';

const configuration = createConfiguration();
const apiInstance = new BodyApi(configuration);

const request = {};

const data = await apiInstance.testBinaryGif(request);
console.log('API called successfully. Returned data:', data);
```


### Parameters
This endpoint does not need any parameter.


### Return type

**HttpFile**

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: image/gif


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
**200** | Successful operation |  -  |

[[Back to top]](#) [[Back to API list]](README.md#documentation-for-api-endpoints) [[Back to Model list]](README.md#documentation-for-models) [[Back to README]](README.md)

# **testBodyApplicationOctetstreamBinary**
> string testBodyApplicationOctetstreamBinary()

Test body parameter(s)

### Example


```typescript
import { createConfiguration, BodyApi } from '';
import type { BodyApiTestBodyApplicationOctetstreamBinaryRequest } from '';

const configuration = createConfiguration();
const apiInstance = new BodyApi(configuration);

const request: BodyApiTestBodyApplicationOctetstreamBinaryRequest = {
  
  body: { data: Buffer.from(fs.readFileSync('/path/to/file', 'utf-8')), name: '/path/to/file' },
};

const data = await apiInstance.testBodyApplicationOctetstreamBinary(request);
console.log('API called successfully. Returned data:', data);
```


### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | **HttpFile**|  |


### Return type

**string**

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/octet-stream
 - **Accept**: text/plain


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
**200** | Successful operation |  -  |

[[Back to top]](#) [[Back to API list]](README.md#documentation-for-api-endpoints) [[Back to Model list]](README.md#documentation-for-models) [[Back to README]](README.md)

# **testBodyMultipartFormdataArrayOfBinary**
> string testBodyMultipartFormdataArrayOfBinary()

Test array of binary in multipart mime

### Example


```typescript
import { createConfiguration, BodyApi } from '';
import type { BodyApiTestBodyMultipartFormdataArrayOfBinaryRequest } from '';

const configuration = createConfiguration();
const apiInstance = new BodyApi(configuration);

const request: BodyApiTestBodyMultipartFormdataArrayOfBinaryRequest = {
  
  files: [
    { data: Buffer.from(fs.readFileSync('/path/to/file', 'utf-8')), name: '/path/to/file' },
  ],
};

const data = await apiInstance.testBodyMultipartFormdataArrayOfBinary(request);
console.log('API called successfully. Returned data:', data);
```


### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **files** | **Array&lt;HttpFile&gt;** |  | defaults to undefined


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

# **testBodyMultipartFormdataSingleBinary**
> string testBodyMultipartFormdataSingleBinary()

Test single binary in multipart mime

### Example


```typescript
import { createConfiguration, BodyApi } from '';
import type { BodyApiTestBodyMultipartFormdataSingleBinaryRequest } from '';

const configuration = createConfiguration();
const apiInstance = new BodyApi(configuration);

const request: BodyApiTestBodyMultipartFormdataSingleBinaryRequest = {
  
  myFile: { data: Buffer.from(fs.readFileSync('/path/to/file', 'utf-8')), name: '/path/to/file' },
};

const data = await apiInstance.testBodyMultipartFormdataSingleBinary(request);
console.log('API called successfully. Returned data:', data);
```


### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **myFile** | [**HttpFile**] |  | (optional) defaults to undefined


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

# **testEchoBodyAllOfPet**
> Pet testEchoBodyAllOfPet()

Test body parameter(s)

### Example


```typescript
import { createConfiguration, BodyApi } from '';
import type { BodyApiTestEchoBodyAllOfPetRequest } from '';

const configuration = createConfiguration();
const apiInstance = new BodyApi(configuration);

const request: BodyApiTestEchoBodyAllOfPetRequest = {
    // Pet object that needs to be added to the store (optional)
  pet: null,
};

const data = await apiInstance.testEchoBodyAllOfPet(request);
console.log('API called successfully. Returned data:', data);
```


### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **pet** | **Pet**| Pet object that needs to be added to the store |


### Return type

**Pet**

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: application/json


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
**200** | Successful operation |  -  |

[[Back to top]](#) [[Back to API list]](README.md#documentation-for-api-endpoints) [[Back to Model list]](README.md#documentation-for-models) [[Back to README]](README.md)

# **testEchoBodyFreeFormObjectResponseString**
> string testEchoBodyFreeFormObjectResponseString()

Test free form object

### Example


```typescript
import { createConfiguration, BodyApi } from '';
import type { BodyApiTestEchoBodyFreeFormObjectResponseStringRequest } from '';

const configuration = createConfiguration();
const apiInstance = new BodyApi(configuration);

const request: BodyApiTestEchoBodyFreeFormObjectResponseStringRequest = {
    // Free form object (optional)
  body: {},
};

const data = await apiInstance.testEchoBodyFreeFormObjectResponseString(request);
console.log('API called successfully. Returned data:', data);
```


### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | **any**| Free form object |


### Return type

**string**

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: text/plain


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
**200** | Successful operation |  -  |

[[Back to top]](#) [[Back to API list]](README.md#documentation-for-api-endpoints) [[Back to Model list]](README.md#documentation-for-models) [[Back to README]](README.md)

# **testEchoBodyPet**
> Pet testEchoBodyPet()

Test body parameter(s)

### Example


```typescript
import { createConfiguration, BodyApi } from '';
import type { BodyApiTestEchoBodyPetRequest } from '';

const configuration = createConfiguration();
const apiInstance = new BodyApi(configuration);

const request: BodyApiTestEchoBodyPetRequest = {
    // Pet object that needs to be added to the store (optional)
  pet: {
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

const data = await apiInstance.testEchoBodyPet(request);
console.log('API called successfully. Returned data:', data);
```


### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **pet** | **Pet**| Pet object that needs to be added to the store |


### Return type

**Pet**

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: application/json


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
**200** | Successful operation |  -  |

[[Back to top]](#) [[Back to API list]](README.md#documentation-for-api-endpoints) [[Back to Model list]](README.md#documentation-for-models) [[Back to README]](README.md)

# **testEchoBodyPetResponseString**
> string testEchoBodyPetResponseString()

Test empty response body

### Example


```typescript
import { createConfiguration, BodyApi } from '';
import type { BodyApiTestEchoBodyPetResponseStringRequest } from '';

const configuration = createConfiguration();
const apiInstance = new BodyApi(configuration);

const request: BodyApiTestEchoBodyPetResponseStringRequest = {
    // Pet object that needs to be added to the store (optional)
  pet: {
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

const data = await apiInstance.testEchoBodyPetResponseString(request);
console.log('API called successfully. Returned data:', data);
```


### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **pet** | **Pet**| Pet object that needs to be added to the store |


### Return type

**string**

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: text/plain


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
**200** | Successful operation |  -  |

[[Back to top]](#) [[Back to API list]](README.md#documentation-for-api-endpoints) [[Back to Model list]](README.md#documentation-for-models) [[Back to README]](README.md)

# **testEchoBodyStringEnum**
> StringEnumRef testEchoBodyStringEnum()

Test string enum response body

### Example


```typescript
import { createConfiguration, BodyApi } from '';
import type { BodyApiTestEchoBodyStringEnumRequest } from '';

const configuration = createConfiguration();
const apiInstance = new BodyApi(configuration);

const request: BodyApiTestEchoBodyStringEnumRequest = {
    // String enum (optional)
  body: "success",
};

const data = await apiInstance.testEchoBodyStringEnum(request);
console.log('API called successfully. Returned data:', data);
```


### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | **string**| String enum |


### Return type

**StringEnumRef**

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: application/json


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
**200** | Successful operation |  -  |

[[Back to top]](#) [[Back to API list]](README.md#documentation-for-api-endpoints) [[Back to Model list]](README.md#documentation-for-models) [[Back to README]](README.md)

# **testEchoBodyTagResponseString**
> string testEchoBodyTagResponseString()

Test empty json (request body)

### Example


```typescript
import { createConfiguration, BodyApi } from '';
import type { BodyApiTestEchoBodyTagResponseStringRequest } from '';

const configuration = createConfiguration();
const apiInstance = new BodyApi(configuration);

const request: BodyApiTestEchoBodyTagResponseStringRequest = {
    // Tag object (optional)
  tag: {
    id: 1,
    name: "name_example",
  },
};

const data = await apiInstance.testEchoBodyTagResponseString(request);
console.log('API called successfully. Returned data:', data);
```


### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **tag** | **Tag**| Tag object |


### Return type

**string**

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: text/plain


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
**200** | Successful operation |  -  |

[[Back to top]](#) [[Back to API list]](README.md#documentation-for-api-endpoints) [[Back to Model list]](README.md#documentation-for-models) [[Back to README]](README.md)


