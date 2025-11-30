# DefaultApi

All URIs are relative to *http://petstore.swagger.io/v2*

| Method | HTTP request | Description |
| ------------- | ------------- | ------------- |
| [**itemsItemIdSomethingItemSubIdGet**](DefaultApi.md#itemsItemIdSomethingItemSubIdGet) | **GET** items/{item$Id}/something/{item$SubId} | SQ &#x3D; \&quot;; SBS &#x3D; \\; DBS &#x3D; \\\\; SD &#x3D; $some |
| [**itemsPost**](DefaultApi.md#itemsPost) | **POST** items | SQ &#x3D; \&quot;; SBS &#x3D; \\; DBS &#x3D; \\\\; SD &#x3D; $some |



SQ &#x3D; \&quot;; SBS &#x3D; \\; DBS &#x3D; \\\\; SD &#x3D; $some

SQ &#x3D; \&quot;; SBS &#x3D; \\; DBS &#x3D; \\\\; SD &#x3D; $some

### Example
```kotlin
// Import classes:
//import org.openapitools.client.*
//import org.openapitools.client.infrastructure.*
//import org.openapitools.client.models.*

val apiClient = ApiClient()
val webService = apiClient.createWebservice(DefaultApi::class.java)
val itemDollarId : kotlin.String = itemDollarId_example // kotlin.String | SQ = \"; SBS = \\; DBS = \\\\; SD = $some
val itemDollarSubId : kotlin.String = itemDollarSubId_example // kotlin.String | SQ = \"; SBS = \\; DBS = \\\\; SD = $some
val filterDollarType : kotlin.String = filterDollarType_example // kotlin.String | SQ = \"; SBS = \\; DBS = \\\\; SD = $some
val filterDollarSubType : kotlin.String = filterDollarSubType_example // kotlin.String | SQ = \"; SBS = \\; DBS = \\\\; SD = $some
val xCustomHeader : kotlin.String = xCustomHeader_example // kotlin.String | SQ = \"; SBS = \\; DBS = \\\\; SD = $some
val xCustomHeaderTwo : kotlin.String = xCustomHeaderTwo_example // kotlin.String | SQ = \"; SBS = \\; DBS = \\\\; SD = $some

val result : ItemsItemIdSomethingItemSubIdGet200Response = webService.itemsItemIdSomethingItemSubIdGet(itemDollarId, itemDollarSubId, filterDollarType, filterDollarSubType, xCustomHeader, xCustomHeaderTwo)
```

### Parameters
| **itemDollarId** | **kotlin.String**| SQ &#x3D; \&quot;; SBS &#x3D; \\; DBS &#x3D; \\\\; SD &#x3D; $some | |
| **itemDollarSubId** | **kotlin.String**| SQ &#x3D; \&quot;; SBS &#x3D; \\; DBS &#x3D; \\\\; SD &#x3D; $some | |
| **filterDollarType** | **kotlin.String**| SQ &#x3D; \&quot;; SBS &#x3D; \\; DBS &#x3D; \\\\; SD &#x3D; $some | [optional] [default to &quot;SQ &#x3D; \&quot;; SBS &#x3D; \\; DBS &#x3D; \\\\; SD &#x3D; $some&quot;] |
| **filterDollarSubType** | **kotlin.String**| SQ &#x3D; \&quot;; SBS &#x3D; \\; DBS &#x3D; \\\\; SD &#x3D; $some | [optional] [default to &quot;SQ &#x3D; \&quot;; SBS &#x3D; \\; DBS &#x3D; \\\\; SD &#x3D; $some&quot;] |
| **xCustomHeader** | **kotlin.String**| SQ &#x3D; \&quot;; SBS &#x3D; \\; DBS &#x3D; \\\\; SD &#x3D; $some | [optional] |
| Name | Type | Description  | Notes |
| ------------- | ------------- | ------------- | ------------- |
| **xCustomHeaderTwo** | **kotlin.String**| SQ &#x3D; \&quot;; SBS &#x3D; \\; DBS &#x3D; \\\\; SD &#x3D; $some | [optional] |

### Return type

[**ItemsItemIdSomethingItemSubIdGet200Response**](ItemsItemIdSomethingItemSubIdGet200Response.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/json


SQ &#x3D; \&quot;; SBS &#x3D; \\; DBS &#x3D; \\\\; SD &#x3D; $some

SQ &#x3D; \&quot;; SBS &#x3D; \\; DBS &#x3D; \\\\; SD &#x3D; $some

### Example
```kotlin
// Import classes:
//import org.openapitools.client.*
//import org.openapitools.client.infrastructure.*
//import org.openapitools.client.models.*

val apiClient = ApiClient()
val webService = apiClient.createWebservice(DefaultApi::class.java)
val xPostHeader : kotlin.String = xPostHeader_example // kotlin.String | SQ = \"; SBS = \\; DBS = \\\\; SD = $some
val formDollarName : kotlin.String = formDollarName_example // kotlin.String | SQ = \\\"; SBS = \\\\; DBS = \\\\\\\\; SD = $some
val formDollarValue : kotlin.String = formDollarValue_example // kotlin.String | SQ = \\\"; SBS = \\\\; DBS = \\\\\\\\; SD = $some

val result : ItemWithDollarAttributesAndExamples = webService.itemsPost(xPostHeader, formDollarName, formDollarValue)
```

### Parameters
| **xPostHeader** | **kotlin.String**| SQ &#x3D; \&quot;; SBS &#x3D; \\; DBS &#x3D; \\\\; SD &#x3D; $some | [optional] |
| **formDollarName** | **kotlin.String**| SQ &#x3D; \\\&quot;; SBS &#x3D; \\\\; DBS &#x3D; \\\\\\\\; SD &#x3D; $some | [optional] |
| Name | Type | Description  | Notes |
| ------------- | ------------- | ------------- | ------------- |
| **formDollarValue** | **kotlin.String**| SQ &#x3D; \\\&quot;; SBS &#x3D; \\\\; DBS &#x3D; \\\\\\\\; SD &#x3D; $some | [optional] [default to &quot;SQ &#x3D; \&quot;; SBS &#x3D; \\; DBS &#x3D; \\\\; SD &#x3D; $some&quot;] |

### Return type

[**ItemWithDollarAttributesAndExamples**](ItemWithDollarAttributesAndExamples.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/x-www-form-urlencoded
 - **Accept**: application/json

