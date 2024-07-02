# BodyApi

All URIs are relative to *http://localhost:3000*

| Method | HTTP request | Description |
| ------------- | ------------- | ------------- |
| [**testBinaryGif**](BodyApi.md#testBinaryGif) | **POST** /binary/gif | Test binary (gif) response body |
| [**testBodyApplicationOctetstreamBinary**](BodyApi.md#testBodyApplicationOctetstreamBinary) | **POST** /body/application/octetstream/binary | Test body parameter(s) |
| [**testBodyMultipartFormdataArrayOfBinary**](BodyApi.md#testBodyMultipartFormdataArrayOfBinary) | **POST** /body/application/octetstream/array_of_binary | Test array of binary in multipart mime |
| [**testBodyMultipartFormdataSingleBinary**](BodyApi.md#testBodyMultipartFormdataSingleBinary) | **POST** /body/application/octetstream/single_binary | Test single binary in multipart mime |
| [**testEchoBodyFreeFormObjectResponseString**](BodyApi.md#testEchoBodyFreeFormObjectResponseString) | **POST** /echo/body/FreeFormObject/response_string | Test free form object |
| [**testEchoBodyPet**](BodyApi.md#testEchoBodyPet) | **POST** /echo/body/Pet | Test body parameter(s) |
| [**testEchoBodyPetResponseString**](BodyApi.md#testEchoBodyPetResponseString) | **POST** /echo/body/Pet/response_string | Test empty response body |
| [**testEchoBodyTagResponseString**](BodyApi.md#testEchoBodyTagResponseString) | **POST** /echo/body/Tag/response_string | Test empty json (request body) |


<a id="testBinaryGif"></a>
# **testBinaryGif**
> java.io.File testBinaryGif()

Test binary (gif) response body

Test binary (gif) response body

### Example
```kotlin
// Import classes:
//import org.openapitools.client.infrastructure.*
//import org.openapitools.client.models.*

val apiInstance = BodyApi()
try {
    val result : java.io.File = apiInstance.testBinaryGif()
    println(result)
} catch (e: ClientException) {
    println("4xx response calling BodyApi#testBinaryGif")
    e.printStackTrace()
} catch (e: ServerException) {
    println("5xx response calling BodyApi#testBinaryGif")
    e.printStackTrace()
}
```

### Parameters
This endpoint does not need any parameter.

### Return type

[**java.io.File**](java.io.File.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: image/gif

<a id="testBodyApplicationOctetstreamBinary"></a>
# **testBodyApplicationOctetstreamBinary**
> kotlin.String testBodyApplicationOctetstreamBinary(body)

Test body parameter(s)

Test body parameter(s)

### Example
```kotlin
// Import classes:
//import org.openapitools.client.infrastructure.*
//import org.openapitools.client.models.*

val apiInstance = BodyApi()
val body : java.io.File = BINARY_DATA_HERE // java.io.File | 
try {
    val result : kotlin.String = apiInstance.testBodyApplicationOctetstreamBinary(body)
    println(result)
} catch (e: ClientException) {
    println("4xx response calling BodyApi#testBodyApplicationOctetstreamBinary")
    e.printStackTrace()
} catch (e: ServerException) {
    println("5xx response calling BodyApi#testBodyApplicationOctetstreamBinary")
    e.printStackTrace()
}
```

### Parameters
| Name | Type | Description  | Notes |
| ------------- | ------------- | ------------- | ------------- |
| **body** | **java.io.File**|  | [optional] |

### Return type

**kotlin.String**

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/octet-stream
 - **Accept**: text/plain

<a id="testBodyMultipartFormdataArrayOfBinary"></a>
# **testBodyMultipartFormdataArrayOfBinary**
> kotlin.String testBodyMultipartFormdataArrayOfBinary(files)

Test array of binary in multipart mime

Test array of binary in multipart mime

### Example
```kotlin
// Import classes:
//import org.openapitools.client.infrastructure.*
//import org.openapitools.client.models.*

val apiInstance = BodyApi()
val files : kotlin.collections.List<java.io.File> = /path/to/file.txt // kotlin.collections.List<java.io.File> | 
try {
    val result : kotlin.String = apiInstance.testBodyMultipartFormdataArrayOfBinary(files)
    println(result)
} catch (e: ClientException) {
    println("4xx response calling BodyApi#testBodyMultipartFormdataArrayOfBinary")
    e.printStackTrace()
} catch (e: ServerException) {
    println("5xx response calling BodyApi#testBodyMultipartFormdataArrayOfBinary")
    e.printStackTrace()
}
```

### Parameters
| Name | Type | Description  | Notes |
| ------------- | ------------- | ------------- | ------------- |
| **files** | **kotlin.collections.List&lt;java.io.File&gt;**|  | |

### Return type

**kotlin.String**

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: multipart/form-data
 - **Accept**: text/plain

<a id="testBodyMultipartFormdataSingleBinary"></a>
# **testBodyMultipartFormdataSingleBinary**
> kotlin.String testBodyMultipartFormdataSingleBinary(myFile)

Test single binary in multipart mime

Test single binary in multipart mime

### Example
```kotlin
// Import classes:
//import org.openapitools.client.infrastructure.*
//import org.openapitools.client.models.*

val apiInstance = BodyApi()
val myFile : java.io.File = BINARY_DATA_HERE // java.io.File | 
try {
    val result : kotlin.String = apiInstance.testBodyMultipartFormdataSingleBinary(myFile)
    println(result)
} catch (e: ClientException) {
    println("4xx response calling BodyApi#testBodyMultipartFormdataSingleBinary")
    e.printStackTrace()
} catch (e: ServerException) {
    println("5xx response calling BodyApi#testBodyMultipartFormdataSingleBinary")
    e.printStackTrace()
}
```

### Parameters
| Name | Type | Description  | Notes |
| ------------- | ------------- | ------------- | ------------- |
| **myFile** | **java.io.File**|  | [optional] |

### Return type

**kotlin.String**

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: multipart/form-data
 - **Accept**: text/plain

<a id="testEchoBodyFreeFormObjectResponseString"></a>
# **testEchoBodyFreeFormObjectResponseString**
> kotlin.String testEchoBodyFreeFormObjectResponseString(body)

Test free form object

Test free form object

### Example
```kotlin
// Import classes:
//import org.openapitools.client.infrastructure.*
//import org.openapitools.client.models.*

val apiInstance = BodyApi()
val body : kotlin.Any = Object // kotlin.Any | Free form object
try {
    val result : kotlin.String = apiInstance.testEchoBodyFreeFormObjectResponseString(body)
    println(result)
} catch (e: ClientException) {
    println("4xx response calling BodyApi#testEchoBodyFreeFormObjectResponseString")
    e.printStackTrace()
} catch (e: ServerException) {
    println("5xx response calling BodyApi#testEchoBodyFreeFormObjectResponseString")
    e.printStackTrace()
}
```

### Parameters
| Name | Type | Description  | Notes |
| ------------- | ------------- | ------------- | ------------- |
| **body** | **kotlin.Any**| Free form object | [optional] |

### Return type

**kotlin.String**

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: text/plain

<a id="testEchoBodyPet"></a>
# **testEchoBodyPet**
> Pet testEchoBodyPet(pet)

Test body parameter(s)

Test body parameter(s)

### Example
```kotlin
// Import classes:
//import org.openapitools.client.infrastructure.*
//import org.openapitools.client.models.*

val apiInstance = BodyApi()
val pet : Pet =  // Pet | Pet object that needs to be added to the store
try {
    val result : Pet = apiInstance.testEchoBodyPet(pet)
    println(result)
} catch (e: ClientException) {
    println("4xx response calling BodyApi#testEchoBodyPet")
    e.printStackTrace()
} catch (e: ServerException) {
    println("5xx response calling BodyApi#testEchoBodyPet")
    e.printStackTrace()
}
```

### Parameters
| Name | Type | Description  | Notes |
| ------------- | ------------- | ------------- | ------------- |
| **pet** | [**Pet**](Pet.md)| Pet object that needs to be added to the store | [optional] |

### Return type

[**Pet**](Pet.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: application/json

<a id="testEchoBodyPetResponseString"></a>
# **testEchoBodyPetResponseString**
> kotlin.String testEchoBodyPetResponseString(pet)

Test empty response body

Test empty response body

### Example
```kotlin
// Import classes:
//import org.openapitools.client.infrastructure.*
//import org.openapitools.client.models.*

val apiInstance = BodyApi()
val pet : Pet =  // Pet | Pet object that needs to be added to the store
try {
    val result : kotlin.String = apiInstance.testEchoBodyPetResponseString(pet)
    println(result)
} catch (e: ClientException) {
    println("4xx response calling BodyApi#testEchoBodyPetResponseString")
    e.printStackTrace()
} catch (e: ServerException) {
    println("5xx response calling BodyApi#testEchoBodyPetResponseString")
    e.printStackTrace()
}
```

### Parameters
| Name | Type | Description  | Notes |
| ------------- | ------------- | ------------- | ------------- |
| **pet** | [**Pet**](Pet.md)| Pet object that needs to be added to the store | [optional] |

### Return type

**kotlin.String**

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: text/plain

<a id="testEchoBodyTagResponseString"></a>
# **testEchoBodyTagResponseString**
> kotlin.String testEchoBodyTagResponseString(tag)

Test empty json (request body)

Test empty json (request body)

### Example
```kotlin
// Import classes:
//import org.openapitools.client.infrastructure.*
//import org.openapitools.client.models.*

val apiInstance = BodyApi()
val tag : Tag =  // Tag | Tag object
try {
    val result : kotlin.String = apiInstance.testEchoBodyTagResponseString(tag)
    println(result)
} catch (e: ClientException) {
    println("4xx response calling BodyApi#testEchoBodyTagResponseString")
    e.printStackTrace()
} catch (e: ServerException) {
    println("5xx response calling BodyApi#testEchoBodyTagResponseString")
    e.printStackTrace()
}
```

### Parameters
| Name | Type | Description  | Notes |
| ------------- | ------------- | ------------- | ------------- |
| **tag** | [**Tag**](Tag.md)| Tag object | [optional] |

### Return type

**kotlin.String**

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: text/plain

