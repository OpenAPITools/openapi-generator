# DefaultApi

All URIs are relative to *http://petstore.swagger.io:80/v2*

| Method | HTTP request | Description |
|------------- | ------------- | -------------|
| [**fakeAnyOfWIthSameErasureGet**](DefaultApi.md#fakeAnyOfWIthSameErasureGet) | **GET** /fake/anyOfWIthSameErasure |  |
| [**fakeFreeFormQueryParametersGet**](DefaultApi.md#fakeFreeFormQueryParametersGet) | **GET** /fake/free-form-query-parameters |  |
| [**fakeOneOfWIthSameErasureGet**](DefaultApi.md#fakeOneOfWIthSameErasureGet) | **GET** /fake/oneOfWIthSameErasure |  |
| [**fooGet**](DefaultApi.md#fooGet) | **GET** /foo |  |


<a id="fakeAnyOfWIthSameErasureGet"></a>
# **fakeAnyOfWIthSameErasureGet**
> FakeAnyOfWIthSameErasureGet200Response fakeAnyOfWIthSameErasureGet()



Test route, this shouldn&#39;t cause a compiler error

### Example
```java
// Import classes:
import org.openapitools.client.ApiClient;
import org.openapitools.client.ApiException;
import org.openapitools.client.Configuration;
import org.openapitools.client.models.*;
import org.openapitools.client.api.DefaultApi;

public class Example {
  public static void main(String[] args) {
    ApiClient defaultClient = Configuration.getDefaultApiClient();
    defaultClient.setBasePath("http://petstore.swagger.io:80/v2");

    DefaultApi apiInstance = new DefaultApi(defaultClient);
    try {
      FakeAnyOfWIthSameErasureGet200Response result = apiInstance.fakeAnyOfWIthSameErasureGet();
      System.out.println(result);
    } catch (ApiException e) {
      System.err.println("Exception when calling DefaultApi#fakeAnyOfWIthSameErasureGet");
      System.err.println("Status code: " + e.getCode());
      System.err.println("Reason: " + e.getResponseBody());
      System.err.println("Response headers: " + e.getResponseHeaders());
      e.printStackTrace();
    }
  }
}
```

### Parameters
This endpoint does not need any parameter.

### Return type

[**FakeAnyOfWIthSameErasureGet200Response**](FakeAnyOfWIthSameErasureGet200Response.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/json

### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **200** | successful response |  -  |

<a id="fakeFreeFormQueryParametersGet"></a>
# **fakeFreeFormQueryParametersGet**
> fakeFreeFormQueryParametersGet(fixed, freeForm)



### Example
```java
// Import classes:
import org.openapitools.client.ApiClient;
import org.openapitools.client.ApiException;
import org.openapitools.client.Configuration;
import org.openapitools.client.models.*;
import org.openapitools.client.api.DefaultApi;

public class Example {
  public static void main(String[] args) {
    ApiClient defaultClient = Configuration.getDefaultApiClient();
    defaultClient.setBasePath("http://petstore.swagger.io:80/v2");

    DefaultApi apiInstance = new DefaultApi(defaultClient);
    String fixed = "fixed_example"; // String | 
    Object freeForm = null; // Object | 
    try {
      apiInstance.fakeFreeFormQueryParametersGet(fixed, freeForm);
    } catch (ApiException e) {
      System.err.println("Exception when calling DefaultApi#fakeFreeFormQueryParametersGet");
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
| **fixed** | **String**|  | [optional] |
| **freeForm** | [**Object**](.md)|  | [optional] |

### Return type

null (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: Not defined

### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **200** | OK |  -  |

<a id="fakeOneOfWIthSameErasureGet"></a>
# **fakeOneOfWIthSameErasureGet**
> FakeOneOfWIthSameErasureGet200Response fakeOneOfWIthSameErasureGet()



Test route, this shouldn&#39;t cause a compiler error

### Example
```java
// Import classes:
import org.openapitools.client.ApiClient;
import org.openapitools.client.ApiException;
import org.openapitools.client.Configuration;
import org.openapitools.client.models.*;
import org.openapitools.client.api.DefaultApi;

public class Example {
  public static void main(String[] args) {
    ApiClient defaultClient = Configuration.getDefaultApiClient();
    defaultClient.setBasePath("http://petstore.swagger.io:80/v2");

    DefaultApi apiInstance = new DefaultApi(defaultClient);
    try {
      FakeOneOfWIthSameErasureGet200Response result = apiInstance.fakeOneOfWIthSameErasureGet();
      System.out.println(result);
    } catch (ApiException e) {
      System.err.println("Exception when calling DefaultApi#fakeOneOfWIthSameErasureGet");
      System.err.println("Status code: " + e.getCode());
      System.err.println("Reason: " + e.getResponseBody());
      System.err.println("Response headers: " + e.getResponseHeaders());
      e.printStackTrace();
    }
  }
}
```

### Parameters
This endpoint does not need any parameter.

### Return type

[**FakeOneOfWIthSameErasureGet200Response**](FakeOneOfWIthSameErasureGet200Response.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/json

### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **200** | successful response |  -  |

<a id="fooGet"></a>
# **fooGet**
> FooGetDefaultResponse fooGet()



### Example
```java
// Import classes:
import org.openapitools.client.ApiClient;
import org.openapitools.client.ApiException;
import org.openapitools.client.Configuration;
import org.openapitools.client.models.*;
import org.openapitools.client.api.DefaultApi;

public class Example {
  public static void main(String[] args) {
    ApiClient defaultClient = Configuration.getDefaultApiClient();
    defaultClient.setBasePath("http://petstore.swagger.io:80/v2");

    DefaultApi apiInstance = new DefaultApi(defaultClient);
    try {
      FooGetDefaultResponse result = apiInstance.fooGet();
      System.out.println(result);
    } catch (ApiException e) {
      System.err.println("Exception when calling DefaultApi#fooGet");
      System.err.println("Status code: " + e.getCode());
      System.err.println("Reason: " + e.getResponseBody());
      System.err.println("Response headers: " + e.getResponseHeaders());
      e.printStackTrace();
    }
  }
}
```

### Parameters
This endpoint does not need any parameter.

### Return type

[**FooGetDefaultResponse**](FooGetDefaultResponse.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/json

### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **0** | response |  -  |

