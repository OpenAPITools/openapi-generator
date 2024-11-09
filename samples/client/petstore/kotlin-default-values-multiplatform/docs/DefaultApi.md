# DefaultApi

All URIs are relative to *http://localhost*

| Method | HTTP request | Description |
| ------------- | ------------- | ------------- |
| [**test**](DefaultApi.md#test) | **POST** /test | Tests default values |


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
val pi0 : Int = 56 // Int | 
val pi1 : Int = 56 // Int | 
val pn0 : Double = 8.14 // Double | 
val pn1 : Double = 8.14 // Double | 
val qi0 : Int = 56 // Int | 
val qi1 : Int = 56 // Int | 
val qi2 : Int = 56 // Int | 
val qi3 : Int = 56 // Int | 
val qn0 : Double = 8.14 // Double | 
val qn1 : Double = 8.14 // Double | 
val qn2 : Double = 8.14 // Double | 
val qn3 : Double = 8.14 // Double | 
val hi0 : Int = 56 // Int | 
val hi1 : Int = 56 // Int | 
val hi2 : Int = 56 // Int | 
val hi3 : Int = 56 // Int | 
val hn0 : Double = 8.14 // Double | 
val hn1 : Double = 8.14 // Double | 
val hn2 : Double = 8.14 // Double | 
val hn3 : Double = 8.14 // Double | 
val fi0 : Int = 56 // Int | 
val fi1 : Int = 56 // Int | 
val fi2 : Int = 56 // Int | 
val fi3 : Int = 56 // Int | 
val fn0 : Double = 8.14 // Double | 
val fn1 : Double = 8.14 // Double | 
val fn2 : Double = 8.14 // Double | 
val fn3 : Double = 8.14 // Double | 
val fn4 : List<String> =  // List<String> | 
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
| **pi0** | **Int**|  | [default to 10] |
| **pi1** | **Int**|  | |
| **pn0** | **Double**|  | [default to 10.0] |
| **pn1** | **Double**|  | |
| **qi0** | **Int**|  | [optional] [default to 10] |
| **qi1** | **Int**|  | [default to 71] |
| **qi2** | **Int**|  | [optional] |
| **qi3** | **Int**|  | |
| **qn0** | **Double**|  | [optional] [default to 10.0] |
| **qn1** | **Double**|  | [default to 71.0] |
| **qn2** | **Double**|  | [optional] |
| **qn3** | **Double**|  | |
| **hi0** | **Int**|  | [optional] [default to 10] |
| **hi1** | **Int**|  | [default to 71] |
| **hi2** | **Int**|  | [optional] |
| **hi3** | **Int**|  | |
| **hn0** | **Double**|  | [optional] [default to 10.0] |
| **hn1** | **Double**|  | [default to 71.0] |
| **hn2** | **Double**|  | [optional] |
| **hn3** | **Double**|  | |
| **fi0** | **Int**|  | [optional] [default to 10] |
| **fi1** | **Int**|  | [default to 71] |
| **fi2** | **Int**|  | [optional] |
| **fi3** | **Int**|  | |
| **fn0** | **Double**|  | [optional] [default to 10.0] |
| **fn1** | **Double**|  | [default to 71.0] |
| **fn2** | **Double**|  | [optional] |
| **fn3** | **Double**|  | |
| Name | Type | Description  | Notes |
| ------------- | ------------- | ------------- | ------------- |
| **fn4** | [**List&lt;String&gt;**](String.md)|  | |

### Return type

null (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: multipart/form-data
 - **Accept**: Not defined

