# DefaultApi

All URIs are relative to *http://localhost*

Method | HTTP request | Description
------------- | ------------- | -------------
[**test**](DefaultApi.md#test) | **POST** test | Tests default values



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
val pi0 : kotlin.Int = 56 // kotlin.Int | 
val pi1 : kotlin.Int = 56 // kotlin.Int | 
val pn0 : java.math.BigDecimal = 8.14 // java.math.BigDecimal | 
val pn1 : java.math.BigDecimal = 8.14 // java.math.BigDecimal | 
val qi0 : kotlin.Int = 56 // kotlin.Int | 
val qi1 : kotlin.Int = 56 // kotlin.Int | 
val qi2 : kotlin.Int = 56 // kotlin.Int | 
val qi3 : kotlin.Int = 56 // kotlin.Int | 
val qn0 : java.math.BigDecimal = 8.14 // java.math.BigDecimal | 
val qn1 : java.math.BigDecimal = 8.14 // java.math.BigDecimal | 
val qn2 : java.math.BigDecimal = 8.14 // java.math.BigDecimal | 
val qn3 : java.math.BigDecimal = 8.14 // java.math.BigDecimal | 
val hi0 : kotlin.Int = 56 // kotlin.Int | 
val hi1 : kotlin.Int = 56 // kotlin.Int | 
val hi2 : kotlin.Int = 56 // kotlin.Int | 
val hi3 : kotlin.Int = 56 // kotlin.Int | 
val hn0 : java.math.BigDecimal = 8.14 // java.math.BigDecimal | 
val hn1 : java.math.BigDecimal = 8.14 // java.math.BigDecimal | 
val hn2 : java.math.BigDecimal = 8.14 // java.math.BigDecimal | 
val hn3 : java.math.BigDecimal = 8.14 // java.math.BigDecimal | 
val fi0 : kotlin.Int = 56 // kotlin.Int | 
val fi1 : kotlin.Int = 56 // kotlin.Int | 
val fi2 : kotlin.Int = 56 // kotlin.Int | 
val fi3 : kotlin.Int = 56 // kotlin.Int | 
val fn0 : java.math.BigDecimal = 8.14 // java.math.BigDecimal | 
val fn1 : java.math.BigDecimal = 8.14 // java.math.BigDecimal | 
val fn2 : java.math.BigDecimal = 8.14 // java.math.BigDecimal | 
val fn3 : java.math.BigDecimal = 8.14 // java.math.BigDecimal | 
val fn4 : kotlin.collections.List<kotlin.String> =  // kotlin.collections.List<kotlin.String> | 

webService.test(pi0, pi1, pn0, pn1, qi0, qi1, qi2, qi3, qn0, qn1, qn2, qn3, hi0, hi1, hi2, hi3, hn0, hn1, hn2, hn3, fi0, fi1, fi2, fi3, fn0, fn1, fn2, fn3, fn4)
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **pi0** | **kotlin.Int**|  | [default to 10]
 **pi1** | **kotlin.Int**|  |
 **pn0** | **java.math.BigDecimal**|  | [default to 10.0]
 **pn1** | **java.math.BigDecimal**|  |
 **qi0** | **kotlin.Int**|  | [optional] [default to 10]
 **qi1** | **kotlin.Int**|  | [default to 71]
 **qi2** | **kotlin.Int**|  | [optional]
 **qi3** | **kotlin.Int**|  |
 **qn0** | **java.math.BigDecimal**|  | [optional] [default to 10.0]
 **qn1** | **java.math.BigDecimal**|  | [default to 71.0]
 **qn2** | **java.math.BigDecimal**|  | [optional]
 **qn3** | **java.math.BigDecimal**|  |
 **hi0** | **kotlin.Int**|  | [optional] [default to 10]
 **hi1** | **kotlin.Int**|  | [default to 71]
 **hi2** | **kotlin.Int**|  | [optional]
 **hi3** | **kotlin.Int**|  |
 **hn0** | **java.math.BigDecimal**|  | [optional] [default to 10.0]
 **hn1** | **java.math.BigDecimal**|  | [default to 71.0]
 **hn2** | **java.math.BigDecimal**|  | [optional]
 **hn3** | **java.math.BigDecimal**|  |
 **fi0** | **kotlin.Int**|  | [optional] [default to 10]
 **fi1** | **kotlin.Int**|  | [default to 71]
 **fi2** | **kotlin.Int**|  | [optional]
 **fi3** | **kotlin.Int**|  |
 **fn0** | **java.math.BigDecimal**|  | [optional] [default to 10.0]
 **fn1** | **java.math.BigDecimal**|  | [default to 71.0]
 **fn2** | **java.math.BigDecimal**|  | [optional]
 **fn3** | **java.math.BigDecimal**|  |
 **fn4** | [**kotlin.collections.List&lt;kotlin.String&gt;**](kotlin.String.md)|  |

### Return type

null (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: multipart/form-data
 - **Accept**: Not defined

