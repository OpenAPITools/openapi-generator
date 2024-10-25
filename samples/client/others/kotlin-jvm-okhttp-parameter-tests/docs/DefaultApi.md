# DefaultApi

All URIs are relative to *http://localhost*

| Method | HTTP request | Description |
| ------------- | ------------- | ------------- |
| [**findPetsByStatus**](DefaultApi.md#findPetsByStatus) | **GET** /test/parameters/{path_default}/{path_nullable} | Finds Pets by status |


<a id="findPetsByStatus"></a>
# **findPetsByStatus**
> findPetsByStatus(pathDefault, pathNullable, queryDefault, queryDefaultEnum, queryDefaultInt, headerDefault, headerDefaultEnum, headerDefaultInt, cookieDefault, cookieDefaultEnum, cookieDefaultInt, queryNullable, headerNullable, cookieNullable, dollarQueryDollarDollarSign)

Finds Pets by status

Multiple status values can be provided with comma separated strings

### Example
```kotlin
// Import classes:
//import org.openapitools.client.infrastructure.*
//import org.openapitools.client.models.*

val apiInstance = DefaultApi()
val pathDefault : String = pathDefault_example // String | path default
val pathNullable : String = pathNullable_example // String | path_nullable
val queryDefault : String = queryDefault_example // String | query default
val queryDefaultEnum : String = queryDefaultEnum_example // String | query default enum
val queryDefaultInt : BigDecimal = 8.14 // BigDecimal | query default int
val headerDefault : String = headerDefault_example // String | header default
val headerDefaultEnum : String = headerDefaultEnum_example // String | header default enum
val headerDefaultInt : BigDecimal = 8.14 // BigDecimal | header default int
val cookieDefault : String = cookieDefault_example // String | cookie default
val cookieDefaultEnum : String = cookieDefaultEnum_example // String | cookie default enum
val cookieDefaultInt : BigDecimal = 8.14 // BigDecimal | cookie default int
val queryNullable : String = queryNullable_example // String | query nullable
val headerNullable : String = headerNullable_example // String | header nullable
val cookieNullable : String = cookieNullable_example // String | cookie_nullable
val dollarQueryDollarDollarSign : String = dollarQueryDollarDollarSign_example // String | query parameter with dollar sign
try {
    apiInstance.findPetsByStatus(pathDefault, pathNullable, queryDefault, queryDefaultEnum, queryDefaultInt, headerDefault, headerDefaultEnum, headerDefaultInt, cookieDefault, cookieDefaultEnum, cookieDefaultInt, queryNullable, headerNullable, cookieNullable, dollarQueryDollarDollarSign)
} catch (e: ClientException) {
    println("4xx response calling DefaultApi#findPetsByStatus")
    e.printStackTrace()
} catch (e: ServerException) {
    println("5xx response calling DefaultApi#findPetsByStatus")
    e.printStackTrace()
}
```

### Parameters
| **pathDefault** | **String**| path default | |
| **pathNullable** | **String**| path_nullable | |
| **queryDefault** | **String**| query default | [optional] [default to &quot;available&quot;] |
| **queryDefaultEnum** | **String**| query default enum | [optional] [default to B] [enum: A, B, C] |
| **queryDefaultInt** | **BigDecimal**| query default int | [optional] [default to 3] |
| **headerDefault** | **String**| header default | [optional] [default to &quot;available&quot;] |
| **headerDefaultEnum** | **String**| header default enum | [optional] [default to B] [enum: A, B, C] |
| **headerDefaultInt** | **BigDecimal**| header default int | [optional] [default to 3] |
| **cookieDefault** | **String**| cookie default | [optional] [default to &quot;available&quot;] |
| **cookieDefaultEnum** | **String**| cookie default enum | [optional] [default to B] [enum: A, B, C] |
| **cookieDefaultInt** | **BigDecimal**| cookie default int | [optional] [default to 3] |
| **queryNullable** | **String**| query nullable | [optional] |
| **headerNullable** | **String**| header nullable | [optional] |
| **cookieNullable** | **String**| cookie_nullable | [optional] |
| Name | Type | Description  | Notes |
| ------------- | ------------- | ------------- | ------------- |
| **dollarQueryDollarDollarSign** | **String**| query parameter with dollar sign | [optional] |

### Return type

null (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: Not defined

