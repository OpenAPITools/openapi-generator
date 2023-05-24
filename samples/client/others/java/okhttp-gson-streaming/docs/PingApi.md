# PingApi

All URIs are relative to *http://localhost:8082*

| Method | HTTP request | Description |
|------------- | ------------- | -------------|
| [**getPing**](PingApi.md#getPing) | **GET** /ping |  |
| [**postPing**](PingApi.md#postPing) | **POST** /ping |  |


<a id="getPing"></a>
# **getPing**
> SomeObj getPing(petId).name(name).status(status).execute();



### Example
```java
// Import classes:
import org.openapitools.client.ApiClient;
import org.openapitools.client.ApiException;
import org.openapitools.client.Configuration;
import org.openapitools.client.models.*;
import org.openapitools.client.api.PingApi;

public class Example {
  public static void main(String[] args) {
    ApiClient defaultClient = Configuration.getDefaultApiClient();
    defaultClient.setBasePath("http://localhost:8082");

    PingApi apiInstance = new PingApi(defaultClient);
    Long petId = 56L; // Long | ID of pet that needs to be updated
    String name = "name_example"; // String | Updated name of the pet
    String status = "status_example"; // String | Updated status of the pet
    try {
      SomeObj result = apiInstance.getPing(petId)
            .name(name)
            .status(status)
            .execute();
      System.out.println(result);
    } catch (ApiException e) {
      System.err.println("Exception when calling PingApi#getPing");
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
| **petId** | **Long**| ID of pet that needs to be updated | |
| **name** | **String**| Updated name of the pet | [optional] |
| **status** | **String**| Updated status of the pet | [optional] |

### Return type

[**SomeObj**](SomeObj.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/x-www-form-urlencoded
 - **Accept**: application/json

### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **200** | OK |  -  |

<a id="postPing"></a>
# **postPing**
> SomeObj postPing(someObj)



### Example
```java
// Import classes:
import org.openapitools.client.ApiClient;
import org.openapitools.client.ApiException;
import org.openapitools.client.Configuration;
import org.openapitools.client.models.*;
import org.openapitools.client.api.PingApi;

public class Example {
  public static void main(String[] args) {
    ApiClient defaultClient = Configuration.getDefaultApiClient();
    defaultClient.setBasePath("http://localhost:8082");

    PingApi apiInstance = new PingApi(defaultClient);
    SomeObj someObj = new SomeObj(); // SomeObj | 
    try {
      SomeObj result = apiInstance.postPing(someObj);
      System.out.println(result);
    } catch (ApiException e) {
      System.err.println("Exception when calling PingApi#postPing");
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
| **someObj** | [**SomeObj**](SomeObj.md)|  | [optional] |

### Return type

[**SomeObj**](SomeObj.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: application/json

### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **200** | OK |  -  |

