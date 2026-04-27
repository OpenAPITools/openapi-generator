# EchoApi

All URIs are relative to *http://localhost:3000*

| Method | HTTP request | Description |
| ------------- | ------------- | ------------- |
| [**testsEchoStringEscapingParamName**](EchoApi.md#testsEchoStringEscapingParamName) | **GET** echo/string-escaping/{$paramName} | Test \$-in-path-param escaping |



Test \$-in-path-param escaping

Tests that path params with \$dollar, backslash \\ and quote " are properly escaped

### Example
```kotlin
// Import classes:
//import org.openapitools.client.*
//import org.openapitools.client.infrastructure.*
//import org.openapitools.client.models.*

val apiClient = ApiClient()
val webService = apiClient.createWebservice(EchoApi::class.java)
val dollarParamName : kotlin.String = dollarParamName_example // kotlin.String | 
val filterDollarType : kotlin.String = filterDollarType_example // kotlin.String | Filter with \$dollar in description and comment-close */

launch(Dispatchers.IO) {
    val result : kotlin.String = webService.testsEchoStringEscapingParamName(dollarParamName, filterDollarType)
}
```

### Parameters
| **dollarParamName** | **kotlin.String**|  | |
| Name | Type | Description  | Notes |
| ------------- | ------------- | ------------- | ------------- |
| **filterDollarType** | **kotlin.String**| Filter with \$dollar in description and comment-close */ | [optional] [default to "default\$Value with \\\\ and \\""] |

### Return type

**kotlin.String**

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: text/plain

