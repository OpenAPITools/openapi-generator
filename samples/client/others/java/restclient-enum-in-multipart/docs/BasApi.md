# BasApi

All URIs are relative to *http://localhost:8080*

| Method | HTTP request | Description |
|------------- | ------------- | -------------|
| [**createMessage**](BasApi.md#createMessage) | **POST** /messages | Creates a new message |



## createMessage

> InlineObject createMessage(fileContent, idempotencyKey, dataDirection, dataChannel)

Creates a new message

Creates a new message

### Example

```java
// Import classes:
import org.openapitools.client.ApiClient;
import org.openapitools.client.ApiException;
import org.openapitools.client.Configuration;
import org.openapitools.client.auth.*;
import org.openapitools.client.models.*;
import org.openapitools.client.api.BasApi;

public class Example {
    public static void main(String[] args) {
        ApiClient defaultClient = Configuration.getDefaultApiClient();
        defaultClient.setBasePath("http://localhost:8080");
        
        // Configure HTTP basic authorization: basicAuth
        HttpBasicAuth basicAuth = (HttpBasicAuth) defaultClient.getAuthentication("basicAuth");
        basicAuth.setUsername("YOUR USERNAME");
        basicAuth.setPassword("YOUR PASSWORD");

        BasApi apiInstance = new BasApi(defaultClient);
        File fileContent = new File("/path/to/file"); // File | The message payload
        String idempotencyKey = "idempotencyKey_example"; // String | 
        DataDirection dataDirection = DataDirection.fromValue("INGOING"); // DataDirection | 
        DataChannel dataChannel = DataChannel.fromValue("BIKE"); // DataChannel | 
        try {
            InlineObject result = apiInstance.createMessage(fileContent, idempotencyKey, dataDirection, dataChannel);
            System.out.println(result);
        } catch (ApiException e) {
            System.err.println("Exception when calling BasApi#createMessage");
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
| **fileContent** | **File**| The message payload | |
| **idempotencyKey** | **String**|  | |
| **dataDirection** | [**DataDirection**](DataDirection.md)|  | [enum: INGOING, OUTGOING] |
| **dataChannel** | [**DataChannel**](DataChannel.md)|  | [enum: BIKE, CAR, BUS, PLANE] |

### Return type

[**InlineObject**](InlineObject.md)

### Authorization

[basicAuth](../README.md#basicAuth)

### HTTP request headers

- **Content-Type**: multipart/form-data
- **Accept**: application/json


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **201** | The message was created. |  -  |

