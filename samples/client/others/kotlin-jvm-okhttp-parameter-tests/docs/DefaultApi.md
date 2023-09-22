# DefaultApi

All URIs are relative to *http://localhost*

Method | HTTP request | Description
------------- | ------------- | -------------
[**findPetsByStatus**](DefaultApi.md#findPetsByStatus) | **GET** /test/parameters/{path_default}/{path_nullable} | Finds Pets by status


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
val pathDefault : kotlin.String = pathDefault_example // kotlin.String | path default
val pathNullable : kotlin.String = pathNullable_example // kotlin.String | path_nullable
val queryDefault : kotlin.String = queryDefault_example // kotlin.String | query default
val queryDefaultEnum : kotlin.String = queryDefaultEnum_example // kotlin.String | query default enum
val queryDefaultInt : java.math.BigDecimal = 8.14 // java.math.BigDecimal | query default int
val headerDefault : kotlin.String = headerDefault_example // kotlin.String | header default
val headerDefaultEnum : kotlin.String = headerDefaultEnum_example // kotlin.String | header default enum
val headerDefaultInt : java.math.BigDecimal = 8.14 // java.math.BigDecimal | header default int
val cookieDefault : kotlin.String = cookieDefault_example // kotlin.String | cookie default
val cookieDefaultEnum : kotlin.String = cookieDefaultEnum_example // kotlin.String | cookie default enum
val cookieDefaultInt : java.math.BigDecimal = 8.14 // java.math.BigDecimal | cookie default int
val queryNullable : kotlin.String = queryNullable_example // kotlin.String | query nullable
val headerNullable : kotlin.String = headerNullable_example // kotlin.String | header nullable
val cookieNullable : kotlin.String = cookieNullable_example // kotlin.String | cookie_nullable
val dollarQueryDollarDollarSign : kotlin.String = dollarQueryDollarDollarSign_example // kotlin.String | query parameter with dollar sign
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

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **pathDefault** | **kotlin.String**| path default |
 **pathNullable** | **kotlin.String**| path_nullable |
 **queryDefault** | **kotlin.String**| query default | [optional] [default to &quot;available&quot;]
 **queryDefaultEnum** | **kotlin.String**| query default enum | [optional] [default to B] [enum: A, B, C]
 **queryDefaultInt** | **java.math.BigDecimal**| query default int | [optional] [default to 3]
 **headerDefault** | **kotlin.String**| header default | [optional] [default to &quot;available&quot;]
 **headerDefaultEnum** | **kotlin.String**| header default enum | [optional] [default to B] [enum: A, B, C]
 **headerDefaultInt** | **java.math.BigDecimal**| header default int | [optional] [default to 3]
 **cookieDefault** | **kotlin.String**| cookie default | [optional] [default to &quot;available&quot;]
 **cookieDefaultEnum** | **kotlin.String**| cookie default enum | [optional] [default to B] [enum: A, B, C]
 **cookieDefaultInt** | **java.math.BigDecimal**| cookie default int | [optional] [default to 3]
 **queryNullable** | **kotlin.String**| query nullable | [optional]
 **headerNullable** | **kotlin.String**| header nullable | [optional]
 **cookieNullable** | **kotlin.String**| cookie_nullable | [optional]
 **dollarQueryDollarDollarSign** | **kotlin.String**| query parameter with dollar sign | [optional]

### Return type

null (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: Not defined

