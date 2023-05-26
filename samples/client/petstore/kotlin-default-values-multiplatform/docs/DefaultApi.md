# DefaultApi

All URIs are relative to *http://localhost*

Method | HTTP request | Description
------------- | ------------- | -------------
[**test**](DefaultApi.md#test) | **POST** /test | Tests default values


<a id="test"></a>
# **test**
> test(pi0, pi1, pn0, pn1, qi0, qi1, qi2, qi3, qn0, qn1, qn2, qn3, hi0, hi1, hi2, hi3, hn0, hn1, hn2, hn3, fi0, fi1, fi2, fi3, fn0, fn1, fn2, fn3, fn4)

Tests default values

Tests default values of different parameters

### Example
```kotlin
// Import classes:
//import org.openapitools.client.infrastructure.*
//import org.openapitools.client.models.*

val apiInstance = DefaultApi()
val pi0 : kotlin.Int = 56 // kotlin.Int | 
val pi1 : kotlin.Int = 56 // kotlin.Int | 
val pn0 : kotlin.Double = 8.14 // kotlin.Double | 
val pn1 : kotlin.Double = 8.14 // kotlin.Double | 
val qi0 : kotlin.Int = 56 // kotlin.Int | 
val qi1 : kotlin.Int = 56 // kotlin.Int | 
val qi2 : kotlin.Int = 56 // kotlin.Int | 
val qi3 : kotlin.Int = 56 // kotlin.Int | 
val qn0 : kotlin.Double = 8.14 // kotlin.Double | 
val qn1 : kotlin.Double = 8.14 // kotlin.Double | 
val qn2 : kotlin.Double = 8.14 // kotlin.Double | 
val qn3 : kotlin.Double = 8.14 // kotlin.Double | 
val hi0 : kotlin.Int = 56 // kotlin.Int | 
val hi1 : kotlin.Int = 56 // kotlin.Int | 
val hi2 : kotlin.Int = 56 // kotlin.Int | 
val hi3 : kotlin.Int = 56 // kotlin.Int | 
val hn0 : kotlin.Double = 8.14 // kotlin.Double | 
val hn1 : kotlin.Double = 8.14 // kotlin.Double | 
val hn2 : kotlin.Double = 8.14 // kotlin.Double | 
val hn3 : kotlin.Double = 8.14 // kotlin.Double | 
val fi0 : kotlin.Int = 56 // kotlin.Int | 
val fi1 : kotlin.Int = 56 // kotlin.Int | 
val fi2 : kotlin.Int = 56 // kotlin.Int | 
val fi3 : kotlin.Int = 56 // kotlin.Int | 
val fn0 : kotlin.Double = 8.14 // kotlin.Double | 
val fn1 : kotlin.Double = 8.14 // kotlin.Double | 
val fn2 : kotlin.Double = 8.14 // kotlin.Double | 
val fn3 : kotlin.Double = 8.14 // kotlin.Double | 
val fn4 : kotlin.collections.List<kotlin.String> =  // kotlin.collections.List<kotlin.String> | 
try {
    apiInstance.test(pi0, pi1, pn0, pn1, qi0, qi1, qi2, qi3, qn0, qn1, qn2, qn3, hi0, hi1, hi2, hi3, hn0, hn1, hn2, hn3, fi0, fi1, fi2, fi3, fn0, fn1, fn2, fn3, fn4)
} catch (e: ClientException) {
    println("4xx response calling DefaultApi#test")
    e.printStackTrace()
} catch (e: ServerException) {
    println("5xx response calling DefaultApi#test")
    e.printStackTrace()
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **pi0** | **kotlin.Int**|  | [default to 10]
 **pi1** | **kotlin.Int**|  |
 **pn0** | **kotlin.Double**|  | [default to 10.0]
 **pn1** | **kotlin.Double**|  |
 **qi0** | **kotlin.Int**|  | [optional] [default to 10]
 **qi1** | **kotlin.Int**|  | [default to 71]
 **qi2** | **kotlin.Int**|  | [optional]
 **qi3** | **kotlin.Int**|  |
 **qn0** | **kotlin.Double**|  | [optional] [default to 10.0]
 **qn1** | **kotlin.Double**|  | [default to 71.0]
 **qn2** | **kotlin.Double**|  | [optional]
 **qn3** | **kotlin.Double**|  |
 **hi0** | **kotlin.Int**|  | [optional] [default to 10]
 **hi1** | **kotlin.Int**|  | [default to 71]
 **hi2** | **kotlin.Int**|  | [optional]
 **hi3** | **kotlin.Int**|  |
 **hn0** | **kotlin.Double**|  | [optional] [default to 10.0]
 **hn1** | **kotlin.Double**|  | [default to 71.0]
 **hn2** | **kotlin.Double**|  | [optional]
 **hn3** | **kotlin.Double**|  |
 **fi0** | **kotlin.Int**|  | [optional] [default to 10]
 **fi1** | **kotlin.Int**|  | [default to 71]
 **fi2** | **kotlin.Int**|  | [optional]
 **fi3** | **kotlin.Int**|  |
 **fn0** | **kotlin.Double**|  | [optional] [default to 10.0]
 **fn1** | **kotlin.Double**|  | [default to 71.0]
 **fn2** | **kotlin.Double**|  | [optional]
 **fn3** | **kotlin.Double**|  |
 **fn4** | [**kotlin.collections.List&lt;kotlin.String&gt;**](kotlin.String.md)|  |

### Return type

null (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: multipart/form-data
 - **Accept**: Not defined

