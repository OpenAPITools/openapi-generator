# BodyApi

All URIs are relative to *http://localhost:3000*

| Method | HTTP request | Description |
| ------------- | ------------- | ------------- |
| [**testBodyMultipartFormdataWithJsonPart**](BodyApi.md#testBodyMultipartFormdataWithJsonPart) | **POST** /body/multipart/formdata/with_json_part | Test multipart with JSON part |


<a id="testBodyMultipartFormdataWithJsonPart"></a>
# **testBodyMultipartFormdataWithJsonPart**
> kotlin.String testBodyMultipartFormdataWithJsonPart(metadata, file)

Test multipart with JSON part

Test multipart/form-data with a part that has Content-Type application/json

### Example
```kotlin
// Import classes:
//import org.openapitools.client.infrastructure.*
//import org.openapitools.client.models.*

val apiInstance = BodyApi()
val metadata : FileMetadata =  // FileMetadata | 
val file : java.io.File = BINARY_DATA_HERE // java.io.File | File to upload
try {
    val result : kotlin.String = apiInstance.testBodyMultipartFormdataWithJsonPart(metadata, file)
    println(result)
} catch (e: ClientException) {
    println("4xx response calling BodyApi#testBodyMultipartFormdataWithJsonPart")
    e.printStackTrace()
} catch (e: ServerException) {
    println("5xx response calling BodyApi#testBodyMultipartFormdataWithJsonPart")
    e.printStackTrace()
}
```

### Parameters
| **metadata** | [**FileMetadata**](FileMetadata.md)|  | |
| Name | Type | Description  | Notes |
| ------------- | ------------- | ------------- | ------------- |
| **file** | **java.io.File**| File to upload | |

### Return type

**kotlin.String**

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: multipart/form-data
 - **Accept**: text/plain

