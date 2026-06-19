# BodyApi

All URIs are relative to *http://localhost:3000*

|Method | HTTP request | Description|
|------------- | ------------- | -------------|
|[**testBinaryGif**](#testbinarygif) | **POST** /binary/gif | Test binary (gif) response body|
|[**testBodyApplicationOctetstreamBinary**](#testbodyapplicationoctetstreambinary) | **POST** /body/application/octetstream/binary | Test body parameter(s)|
|[**testBodyMultipartFormdataArrayOfBinary**](#testbodymultipartformdataarrayofbinary) | **POST** /body/application/octetstream/array_of_binary | Test array of binary in multipart mime|
|[**testBodyMultipartFormdataSingleBinary**](#testbodymultipartformdatasinglebinary) | **POST** /body/application/octetstream/single_binary | Test single binary in multipart mime|
|[**testEchoBodyAllOfPet**](#testechobodyallofpet) | **POST** /echo/body/allOf/Pet | Test body parameter(s)|
|[**testEchoBodyFreeFormObjectResponseString**](#testechobodyfreeformobjectresponsestring) | **POST** /echo/body/FreeFormObject/response_string | Test free form object|
|[**testEchoBodyPet**](#testechobodypet) | **POST** /echo/body/Pet | Test body parameter(s)|
|[**testEchoBodyPetResponseString**](#testechobodypetresponsestring) | **POST** /echo/body/Pet/response_string | Test empty response body|
|[**testEchoBodyStringEnum**](#testechobodystringenum) | **POST** /echo/body/string_enum | Test string enum response body|
|[**testEchoBodyTagResponseString**](#testechobodytagresponsestring) | **POST** /echo/body/Tag/response_string | Test empty json (request body)|

# **testBinaryGif**
> File testBinaryGif()

Test binary (gif) response body

### Example

```typescript
import {
    BodyApi,
    Configuration
} from '@openapitools/typescript-axios-echo-api';

const configuration = new Configuration();
const apiInstance = new BodyApi(configuration);

const { status, data } = await apiInstance.testBinaryGif();
```

### Parameters
This endpoint does not have any parameters.


### Return type

**File**

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: image/gif


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
|**200** | Successful operation |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **testBodyApplicationOctetstreamBinary**
> string testBodyApplicationOctetstreamBinary()

Test body parameter(s)

### Example

```typescript
import {
    BodyApi,
    Configuration
} from '@openapitools/typescript-axios-echo-api';

const configuration = new Configuration();
const apiInstance = new BodyApi(configuration);

let body: File; // (optional)

const { status, data } = await apiInstance.testBodyApplicationOctetstreamBinary(
    body
);
```

### Parameters

|Name | Type | Description  | Notes|
|------------- | ------------- | ------------- | -------------|
| **body** | **File**|  | |


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
|**200** | Successful operation |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **testBodyMultipartFormdataArrayOfBinary**
> string testBodyMultipartFormdataArrayOfBinary()

Test array of binary in multipart mime

### Example

```typescript
import {
    BodyApi,
    Configuration
} from '@openapitools/typescript-axios-echo-api';

const configuration = new Configuration();
const apiInstance = new BodyApi(configuration);

let files: Array<File>; // (default to undefined)

const { status, data } = await apiInstance.testBodyMultipartFormdataArrayOfBinary(
    files
);
```

### Parameters

|Name | Type | Description  | Notes|
|------------- | ------------- | ------------- | -------------|
| **files** | **Array&lt;File&gt;** |  | defaults to undefined|


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

# **testBodyMultipartFormdataSingleBinary**
> string testBodyMultipartFormdataSingleBinary()

Test single binary in multipart mime

### Example

```typescript
import {
    BodyApi,
    Configuration
} from '@openapitools/typescript-axios-echo-api';

const configuration = new Configuration();
const apiInstance = new BodyApi(configuration);

let myFile: File; // (optional) (default to undefined)

const { status, data } = await apiInstance.testBodyMultipartFormdataSingleBinary(
    myFile
);
```

### Parameters

|Name | Type | Description  | Notes|
|------------- | ------------- | ------------- | -------------|
| **myFile** | [**File**] |  | (optional) defaults to undefined|


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

# **testEchoBodyAllOfPet**
> Pet testEchoBodyAllOfPet()

Test body parameter(s)

### Example

```typescript
import {
    BodyApi,
    Configuration,
    Pet
} from '@openapitools/typescript-axios-echo-api';

const configuration = new Configuration();
const apiInstance = new BodyApi(configuration);

let pet: Pet; //Pet object that needs to be added to the store (optional)

const { status, data } = await apiInstance.testEchoBodyAllOfPet(
    pet
);
```

### Parameters

|Name | Type | Description  | Notes|
|------------- | ------------- | ------------- | -------------|
| **pet** | **Pet**| Pet object that needs to be added to the store | |


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
|**200** | Successful operation |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **testEchoBodyFreeFormObjectResponseString**
> string testEchoBodyFreeFormObjectResponseString()

Test free form object

### Example

```typescript
import {
    BodyApi,
    Configuration
} from '@openapitools/typescript-axios-echo-api';

const configuration = new Configuration();
const apiInstance = new BodyApi(configuration);

let body: object; //Free form object (optional)

const { status, data } = await apiInstance.testEchoBodyFreeFormObjectResponseString(
    body
);
```

### Parameters

|Name | Type | Description  | Notes|
|------------- | ------------- | ------------- | -------------|
| **body** | **object**| Free form object | |


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
|**200** | Successful operation |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **testEchoBodyPet**
> Pet testEchoBodyPet()

Test body parameter(s)

### Example

```typescript
import {
    BodyApi,
    Configuration,
    Pet
} from '@openapitools/typescript-axios-echo-api';

const configuration = new Configuration();
const apiInstance = new BodyApi(configuration);

let pet: Pet; //Pet object that needs to be added to the store (optional)

const { status, data } = await apiInstance.testEchoBodyPet(
    pet
);
```

### Parameters

|Name | Type | Description  | Notes|
|------------- | ------------- | ------------- | -------------|
| **pet** | **Pet**| Pet object that needs to be added to the store | |


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
|**200** | Successful operation |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **testEchoBodyPetResponseString**
> string testEchoBodyPetResponseString()

Test empty response body

### Example

```typescript
import {
    BodyApi,
    Configuration,
    Pet
} from '@openapitools/typescript-axios-echo-api';

const configuration = new Configuration();
const apiInstance = new BodyApi(configuration);

let pet: Pet; //Pet object that needs to be added to the store (optional)

const { status, data } = await apiInstance.testEchoBodyPetResponseString(
    pet
);
```

### Parameters

|Name | Type | Description  | Notes|
|------------- | ------------- | ------------- | -------------|
| **pet** | **Pet**| Pet object that needs to be added to the store | |


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
|**200** | Successful operation |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **testEchoBodyStringEnum**
> StringEnumRef testEchoBodyStringEnum()

Test string enum response body

### Example

```typescript
import {
    BodyApi,
    Configuration
} from '@openapitools/typescript-axios-echo-api';

const configuration = new Configuration();
const apiInstance = new BodyApi(configuration);

let body: string; //String enum (optional)

const { status, data } = await apiInstance.testEchoBodyStringEnum(
    body
);
```

### Parameters

|Name | Type | Description  | Notes|
|------------- | ------------- | ------------- | -------------|
| **body** | **string**| String enum | |


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
|**200** | Successful operation |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **testEchoBodyTagResponseString**
> string testEchoBodyTagResponseString()

Test empty json (request body)

### Example

```typescript
import {
    BodyApi,
    Configuration,
    Tag
} from '@openapitools/typescript-axios-echo-api';

const configuration = new Configuration();
const apiInstance = new BodyApi(configuration);

let tag: Tag; //Tag object (optional)

const { status, data } = await apiInstance.testEchoBodyTagResponseString(
    tag
);
```

### Parameters

|Name | Type | Description  | Notes|
|------------- | ------------- | ------------- | -------------|
| **tag** | **Tag**| Tag object | |


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
|**200** | Successful operation |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

