# DefaultApi

All URIs are relative to *http://localhost*

Method | HTTP request | Description
------------- | ------------- | -------------
[**test**](DefaultApi.md#test) | **POST** /test | Tests default values



Tests default values

Tests default values of different parameters

### Example
```kotlin
// Import classes:
//import org.openapitools.client.*
//import org.openapitools.client.infrastructure.*
//import org.openapitools.client.models.*

val apiClient = ApiClient()
val webService = apiClient.createWebservice(DefaultApi::class.java)
val pi0 : Int = 56 // Int | 
val pi1 : Int = 56 // Int | 
val pn0 : BigDecimal = 8.14 // BigDecimal | 
val pn1 : BigDecimal = 8.14 // BigDecimal | 
val qi0 : Int = 56 // Int | 
val qi1 : Int = 56 // Int | 
val qi2 : Int = 56 // Int | 
val qi3 : Int = 56 // Int | 
val qn0 : BigDecimal = 8.14 // BigDecimal | 
val qn1 : BigDecimal = 8.14 // BigDecimal | 
val qn2 : BigDecimal = 8.14 // BigDecimal | 
val qn3 : BigDecimal = 8.14 // BigDecimal | 
val hi0 : Int = 56 // Int | 
val hi1 : Int = 56 // Int | 
val hi2 : Int = 56 // Int | 
val hi3 : Int = 56 // Int | 
val hn0 : BigDecimal = 8.14 // BigDecimal | 
val hn1 : BigDecimal = 8.14 // BigDecimal | 
val hn2 : BigDecimal = 8.14 // BigDecimal | 
val hn3 : BigDecimal = 8.14 // BigDecimal | 
val fi0 : Int = 56 // Int | 
val fi1 : Int = 56 // Int | 
val fi2 : Int = 56 // Int | 
val fi3 : Int = 56 // Int | 
val fn0 : BigDecimal = 8.14 // BigDecimal | 
val fn1 : BigDecimal = 8.14 // BigDecimal | 
val fn2 : BigDecimal = 8.14 // BigDecimal | 
val fn3 : BigDecimal = 8.14 // BigDecimal | 
val fn4 : List<String> =  // List<String> | 

webService.test(pi0, pi1, pn0, pn1, qi0, qi1, qi2, qi3, qn0, qn1, qn2, qn3, hi0, hi1, hi2, hi3, hn0, hn1, hn2, hn3, fi0, fi1, fi2, fi3, fn0, fn1, fn2, fn3, fn4)
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **pi0** | **Int**|  | [default to 10]
 **pi1** | **Int**|  |
 **pn0** | **BigDecimal**|  | [default to 10.0]
 **pn1** | **BigDecimal**|  |
 **qi0** | **Int**|  | [optional] [default to 10]
 **qi1** | **Int**|  | [default to 71]
 **qi2** | **Int**|  | [optional]
 **qi3** | **Int**|  |
 **qn0** | **BigDecimal**|  | [optional] [default to 10.0]
 **qn1** | **BigDecimal**|  | [default to 71.0]
 **qn2** | **BigDecimal**|  | [optional]
 **qn3** | **BigDecimal**|  |
 **hi0** | **Int**|  | [optional] [default to 10]
 **hi1** | **Int**|  | [default to 71]
 **hi2** | **Int**|  | [optional]
 **hi3** | **Int**|  |
 **hn0** | **BigDecimal**|  | [optional] [default to 10.0]
 **hn1** | **BigDecimal**|  | [default to 71.0]
 **hn2** | **BigDecimal**|  | [optional]
 **hn3** | **BigDecimal**|  |
 **fi0** | **Int**|  | [optional] [default to 10]
 **fi1** | **Int**|  | [default to 71]
 **fi2** | **Int**|  | [optional]
 **fi3** | **Int**|  |
 **fn0** | **BigDecimal**|  | [optional] [default to 10.0]
 **fn1** | **BigDecimal**|  | [default to 71.0]
 **fn2** | **BigDecimal**|  | [optional]
 **fn3** | **BigDecimal**|  |
 **fn4** | [**List&lt;String&gt;**](String.md)|  |

### Return type

null (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: multipart/form-data
 - **Accept**: Not defined

