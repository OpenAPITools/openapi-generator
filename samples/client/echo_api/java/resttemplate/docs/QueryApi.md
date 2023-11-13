# QueryApi

All URIs are relative to *http://localhost:3000*

| Method | HTTP request | Description |
|------------- | ------------- | -------------|
| [**testQueryIntegerBooleanString**](QueryApi.md#testQueryIntegerBooleanString) | **GET** /query/integer/boolean/string | Test query parameter(s) |
| [**testQueryStyleFormExplodeTrueArrayString**](QueryApi.md#testQueryStyleFormExplodeTrueArrayString) | **GET** /query/style_form/explode_true/array_string | Test query parameter(s) |
| [**testQueryStyleFormExplodeTrueObject**](QueryApi.md#testQueryStyleFormExplodeTrueObject) | **GET** /query/style_form/explode_true/object | Test query parameter(s) |



## testQueryIntegerBooleanString

> String testQueryIntegerBooleanString(integerQuery, booleanQuery, stringQuery)

Test query parameter(s)

Test query parameter(s)

### Example

```java
// Import classes:
import org.openapitools.client.ApiClient;
import org.openapitools.client.ApiException;
import org.openapitools.client.Configuration;
import org.openapitools.client.models.*;
import org.openapitools.client.api.QueryApi;

public class Example {
    public static void main(String[] args) {
        ApiClient defaultClient = Configuration.getDefaultApiClient();
        defaultClient.setBasePath("http://localhost:3000");

        QueryApi apiInstance = new QueryApi(defaultClient);
        Integer integerQuery = 56; // Integer | 
        Boolean booleanQuery = true; // Boolean | 
        String stringQuery = "stringQuery_example"; // String | 
        try {
            String result = apiInstance.testQueryIntegerBooleanString(integerQuery, booleanQuery, stringQuery);
            System.out.println(result);
        } catch (ApiException e) {
            System.err.println("Exception when calling QueryApi#testQueryIntegerBooleanString");
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
| **integerQuery** | **Integer**|  | [optional] |
| **booleanQuery** | **Boolean**|  | [optional] |
| **stringQuery** | **String**|  | [optional] |

### Return type

**String**

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: text/plain


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **200** | Successful operation |  -  |


## testQueryStyleFormExplodeTrueArrayString

> String testQueryStyleFormExplodeTrueArrayString(queryObject)

Test query parameter(s)

Test query parameter(s)

### Example

```java
// Import classes:
import org.openapitools.client.ApiClient;
import org.openapitools.client.ApiException;
import org.openapitools.client.Configuration;
import org.openapitools.client.models.*;
import org.openapitools.client.api.QueryApi;

public class Example {
    public static void main(String[] args) {
        ApiClient defaultClient = Configuration.getDefaultApiClient();
        defaultClient.setBasePath("http://localhost:3000");

        QueryApi apiInstance = new QueryApi(defaultClient);
        TestQueryStyleFormExplodeTrueArrayStringQueryObjectParameter queryObject = new HashMap(); // TestQueryStyleFormExplodeTrueArrayStringQueryObjectParameter | 
        try {
            String result = apiInstance.testQueryStyleFormExplodeTrueArrayString(queryObject);
            System.out.println(result);
        } catch (ApiException e) {
            System.err.println("Exception when calling QueryApi#testQueryStyleFormExplodeTrueArrayString");
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
| **queryObject** | [**TestQueryStyleFormExplodeTrueArrayStringQueryObjectParameter**](.md)|  | [optional] |

### Return type

**String**

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: text/plain


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **200** | Successful operation |  -  |


## testQueryStyleFormExplodeTrueObject

> String testQueryStyleFormExplodeTrueObject(queryObject)

Test query parameter(s)

Test query parameter(s)

### Example

```java
// Import classes:
import org.openapitools.client.ApiClient;
import org.openapitools.client.ApiException;
import org.openapitools.client.Configuration;
import org.openapitools.client.models.*;
import org.openapitools.client.api.QueryApi;

public class Example {
    public static void main(String[] args) {
        ApiClient defaultClient = Configuration.getDefaultApiClient();
        defaultClient.setBasePath("http://localhost:3000");

        QueryApi apiInstance = new QueryApi(defaultClient);
        Pet queryObject = new HashMap(); // Pet | 
        try {
            String result = apiInstance.testQueryStyleFormExplodeTrueObject(queryObject);
            System.out.println(result);
        } catch (ApiException e) {
            System.err.println("Exception when calling QueryApi#testQueryStyleFormExplodeTrueObject");
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
| **queryObject** | [**Pet**](.md)|  | [optional] |

### Return type

**String**

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: text/plain


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **200** | Successful operation |  -  |

