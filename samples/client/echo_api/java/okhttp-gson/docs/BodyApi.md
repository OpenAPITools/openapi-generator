# BodyApi

All URIs are relative to *http://localhost:3000*

| Method | HTTP request | Description |
|------------- | ------------- | -------------|
| [**testEchoBodyPet**](BodyApi.md#testEchoBodyPet) | **POST** /echo/body/Pet | Test body parameter(s) |


<a name="testEchoBodyPet"></a>
# **testEchoBodyPet**
> Pet testEchoBodyPet(pet)

Test body parameter(s)

Test body parameter(s)

### Example
```java
// Import classes:
import org.openapitools.client.ApiClient;
import org.openapitools.client.ApiException;
import org.openapitools.client.Configuration;
import org.openapitools.client.models.*;
import org.openapitools.client.api.BodyApi;

public class Example {
  public static void main(String[] args) {
    ApiClient defaultClient = Configuration.getDefaultApiClient();
    defaultClient.setBasePath("http://localhost:3000");

    BodyApi apiInstance = new BodyApi(defaultClient);
    Pet pet = new Pet(); // Pet | Pet object that needs to be added to the store
    try {
      Pet result = apiInstance.testEchoBodyPet(pet);
      System.out.println(result);
    } catch (ApiException e) {
      System.err.println("Exception when calling BodyApi#testEchoBodyPet");
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
| **pet** | [**Pet**](Pet.md)| Pet object that needs to be added to the store | [optional] |

### Return type

[**Pet**](Pet.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: application/json

### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **200** | Successful operation |  -  |

