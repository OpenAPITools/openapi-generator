# BodyApi

All URIs are relative to *http://localhost:3000*

Method | HTTP request | Description
------------- | ------------- | -------------
[**testBinaryGif**](BodyApi.md#testBinaryGif) | **POST** binary/gif | Test binary (gif) response body
[**testBodyApplicationOctetstreamBinary**](BodyApi.md#testBodyApplicationOctetstreamBinary) | **POST** body/application/octetstream/binary | Test body parameter(s)
[**testBodyMultipartFormdataArrayOfBinary**](BodyApi.md#testBodyMultipartFormdataArrayOfBinary) | **POST** body/application/octetstream/array_of_binary | Test array of binary in multipart mime
[**testBodyMultipartFormdataSingleBinary**](BodyApi.md#testBodyMultipartFormdataSingleBinary) | **POST** body/application/octetstream/single_binary | Test single binary in multipart mime
[**testEchoBodyFreeFormObjectResponseString**](BodyApi.md#testEchoBodyFreeFormObjectResponseString) | **POST** echo/body/FreeFormObject/response_string | Test free form object
[**testEchoBodyPet**](BodyApi.md#testEchoBodyPet) | **POST** echo/body/Pet | Test body parameter(s)
[**testEchoBodyPetResponseString**](BodyApi.md#testEchoBodyPetResponseString) | **POST** echo/body/Pet/response_string | Test empty response body
[**testEchoBodyTagResponseString**](BodyApi.md#testEchoBodyTagResponseString) | **POST** echo/body/Tag/response_string | Test empty json (request body)



Test binary (gif) response body

Test binary (gif) response body

### Example
```kotlin
// Import classes:
//import org.openapitools.client.*
//import org.openapitools.client.infrastructure.*
//import org.openapitools.client.models.*

val apiClient = ApiClient()
val webService = apiClient.createWebservice(BodyApi::class.java)

launch(Dispatchers.IO) {
    val result : java.io.File = webService.testBinaryGif()
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


Test body parameter(s)

Test body parameter(s)

### Example
```kotlin
// Import classes:
//import org.openapitools.client.*
//import org.openapitools.client.infrastructure.*
//import org.openapitools.client.models.*

val apiClient = ApiClient()
val webService = apiClient.createWebservice(BodyApi::class.java)
val body : java.io.File = BINARY_DATA_HERE // java.io.File | 

launch(Dispatchers.IO) {
    val result : kotlin.String = webService.testBodyApplicationOctetstreamBinary(body)
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | **java.io.File**|  | [optional]

### Return type

**kotlin.String**

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/octet-stream
 - **Accept**: text/plain


Test array of binary in multipart mime

Test array of binary in multipart mime

### Example
```kotlin
// Import classes:
//import org.openapitools.client.*
//import org.openapitools.client.infrastructure.*
//import org.openapitools.client.models.*

val apiClient = ApiClient()
val webService = apiClient.createWebservice(BodyApi::class.java)
val files : kotlin.collections.List<java.io.File> = /path/to/file.txt // kotlin.collections.List<java.io.File> | 

launch(Dispatchers.IO) {
    val result : kotlin.String = webService.testBodyMultipartFormdataArrayOfBinary(files)
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **files** | **kotlin.collections.List&lt;java.io.File&gt;**|  |

### Return type

**kotlin.String**

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: multipart/form-data
 - **Accept**: text/plain


Test single binary in multipart mime

Test single binary in multipart mime

### Example
```kotlin
// Import classes:
//import org.openapitools.client.*
//import org.openapitools.client.infrastructure.*
//import org.openapitools.client.models.*

val apiClient = ApiClient()
val webService = apiClient.createWebservice(BodyApi::class.java)
val myFile : java.io.File = BINARY_DATA_HERE // java.io.File | 

launch(Dispatchers.IO) {
    val result : kotlin.String = webService.testBodyMultipartFormdataSingleBinary(myFile)
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **myFile** | **java.io.File**|  | [optional]

### Return type

**kotlin.String**

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: multipart/form-data
 - **Accept**: text/plain


Test free form object

Test free form object

### Example
```kotlin
// Import classes:
//import org.openapitools.client.*
//import org.openapitools.client.infrastructure.*
//import org.openapitools.client.models.*

val apiClient = ApiClient()
val webService = apiClient.createWebservice(BodyApi::class.java)
val body : kotlin.Any = Object // kotlin.Any | Free form object

launch(Dispatchers.IO) {
    val result : kotlin.String = webService.testEchoBodyFreeFormObjectResponseString(body)
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | **kotlin.Any**| Free form object | [optional]

### Return type

**kotlin.String**

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: text/plain


Test body parameter(s)

Test body parameter(s)

### Example
```kotlin
// Import classes:
//import org.openapitools.client.*
//import org.openapitools.client.infrastructure.*
//import org.openapitools.client.models.*

val apiClient = ApiClient()
val webService = apiClient.createWebservice(BodyApi::class.java)
val pet : Pet =  // Pet | Pet object that needs to be added to the store

launch(Dispatchers.IO) {
    val result : Pet = webService.testEchoBodyPet(pet)
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **pet** | [**Pet**](Pet.md)| Pet object that needs to be added to the store | [optional]

### Return type

[**Pet**](Pet.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: application/json


Test empty response body

Test empty response body

### Example
```kotlin
// Import classes:
//import org.openapitools.client.*
//import org.openapitools.client.infrastructure.*
//import org.openapitools.client.models.*

val apiClient = ApiClient()
val webService = apiClient.createWebservice(BodyApi::class.java)
val pet : Pet =  // Pet | Pet object that needs to be added to the store

launch(Dispatchers.IO) {
    val result : kotlin.String = webService.testEchoBodyPetResponseString(pet)
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **pet** | [**Pet**](Pet.md)| Pet object that needs to be added to the store | [optional]

### Return type

**kotlin.String**

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: text/plain


Test empty json (request body)

Test empty json (request body)

### Example
```kotlin
// Import classes:
//import org.openapitools.client.*
//import org.openapitools.client.infrastructure.*
//import org.openapitools.client.models.*

val apiClient = ApiClient()
val webService = apiClient.createWebservice(BodyApi::class.java)
val tag : Tag =  // Tag | Tag object

launch(Dispatchers.IO) {
    val result : kotlin.String = webService.testEchoBodyTagResponseString(tag)
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **tag** | [**Tag**](Tag.md)| Tag object | [optional]

### Return type

**kotlin.String**

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: text/plain

