# FormApi

All URIs are relative to *http://localhost:3000*

| Method | HTTP request | Description |
|------------- | ------------- | -------------|
| [**testFormIntegerBooleanString**](FormApi.md#testFormIntegerBooleanString) | **POST** /form/integer/boolean/string | Test form parameter(s) |



## testFormIntegerBooleanString

> String testFormIntegerBooleanString(integerForm, booleanForm, stringForm)

Test form parameter(s)

Test form parameter(s)

### Example

```java
// Import classes:
import org.openapitools.client.ApiClient;
import org.openapitools.client.ApiException;
import org.openapitools.client.Configuration;
import org.openapitools.client.models.*;
import org.openapitools.client.api.FormApi;

public class Example {
    public static void main(String[] args) {
        ApiClient defaultClient = Configuration.getDefaultApiClient();
        defaultClient.setBasePath("http://localhost:3000");

        FormApi apiInstance = new FormApi(defaultClient);
        Integer integerForm = 56; // Integer | 
        Boolean booleanForm = true; // Boolean | 
        String stringForm = "stringForm_example"; // String | 
        try {
            String result = apiInstance.testFormIntegerBooleanString(integerForm, booleanForm, stringForm);
            System.out.println(result);
        } catch (ApiException e) {
            System.err.println("Exception when calling FormApi#testFormIntegerBooleanString");
            System.err.println("Status code: " + e.getCode());
            System.err.println("Reason: " + e.getResponseBody());
            System.err.println("Response headers: " + e.getResponseHeaders());
            e.printStackTrace();
        }
    }
}
```

### Parameters


| Name | Type | Description  | Notes |
|------------- | ------------- | ------------- | -------------|
| **integerForm** | **Integer**|  | [optional] |
| **booleanForm** | **Boolean**|  | [optional] |
| **stringForm** | **String**|  | [optional] |

### Return type

**String**

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: application/x-www-form-urlencoded
- **Accept**: text/plain


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **200** | Successful operation |  -  |

