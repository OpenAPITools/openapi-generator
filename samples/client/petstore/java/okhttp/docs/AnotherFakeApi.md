# AnotherFakeApi

All URIs are relative to *http://petstore.swagger.io:80/v2*

| Method | HTTP request | Description |
|------------- | ------------- | -------------|
| [**call123testSpecialTags**](AnotherFakeApi.md#call123testSpecialTags) | **PATCH** /another-fake/dummy | To test special tags |
| [**getParameterArrayNumber**](AnotherFakeApi.md#getParameterArrayNumber) | **GET** /fake/parameter-array-number | parameter array number default value |
| [**getParameterStringNumber**](AnotherFakeApi.md#getParameterStringNumber) | **GET** /fake/parameter-string-number | parameter string number |


<a id="call123testSpecialTags"></a>
# **call123testSpecialTags**
> Client call123testSpecialTags(client)

To test special tags

To test special tags and operation ID starting with number

### Example
```java
// Import classes:
import org.openapitools.client.ApiClient;
import org.openapitools.client.ApiException;
import org.openapitools.client.Configuration;
import org.openapitools.client.models.*;
import org.openapitools.client.api.AnotherFakeApi;

public class Example {
  public static void main(String[] args) {
    ApiClient defaultClient = Configuration.getDefaultApiClient();
    defaultClient.setBasePath("http://petstore.swagger.io:80/v2");

    AnotherFakeApi apiInstance = new AnotherFakeApi(defaultClient);
    Client client = new Client(); // Client | client model
    try {
      Client result = apiInstance.call123testSpecialTags(client);
      System.out.println(result);
    } catch (ApiException e) {
      System.err.println("Exception when calling AnotherFakeApi#call123testSpecialTags");
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
| **client** | [**Client**](Client.md)| client model | |

### Return type

[**Client**](Client.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: application/json

### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **200** | successful operation |  -  |

<a id="getParameterArrayNumber"></a>
# **getParameterArrayNumber**
> getParameterArrayNumber(array)

parameter array number default value

### Example
```java
// Import classes:
import org.openapitools.client.ApiClient;
import org.openapitools.client.ApiException;
import org.openapitools.client.Configuration;
import org.openapitools.client.models.*;
import org.openapitools.client.api.AnotherFakeApi;

public class Example {
  public static void main(String[] args) {
    ApiClient defaultClient = Configuration.getDefaultApiClient();
    defaultClient.setBasePath("http://petstore.swagger.io:80/v2");

    AnotherFakeApi apiInstance = new AnotherFakeApi(defaultClient);
    List<Integer> array = Arrays.asList(); // List<Integer> | array integer
    try {
      apiInstance.getParameterArrayNumber(array);
    } catch (ApiException e) {
      System.err.println("Exception when calling AnotherFakeApi#getParameterArrayNumber");
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
| **array** | [**List&lt;Integer&gt;**](Integer.md)| array integer | |

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

<a id="getParameterStringNumber"></a>
# **getParameterStringNumber**
> getParameterStringNumber(stringNumber)

parameter string number

### Example
```java
// Import classes:
import org.openapitools.client.ApiClient;
import org.openapitools.client.ApiException;
import org.openapitools.client.Configuration;
import org.openapitools.client.models.*;
import org.openapitools.client.api.AnotherFakeApi;

public class Example {
  public static void main(String[] args) {
    ApiClient defaultClient = Configuration.getDefaultApiClient();
    defaultClient.setBasePath("http://petstore.swagger.io:80/v2");

    AnotherFakeApi apiInstance = new AnotherFakeApi(defaultClient);
    BigDecimal stringNumber = new BigDecimal(78); // BigDecimal | string number
    try {
      apiInstance.getParameterStringNumber(stringNumber);
    } catch (ApiException e) {
      System.err.println("Exception when calling AnotherFakeApi#getParameterStringNumber");
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
| **stringNumber** | **BigDecimal**| string number | |

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

