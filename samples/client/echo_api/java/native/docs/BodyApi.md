# BodyApi

All URIs are relative to *http://localhost:3000*

| Method | HTTP request | Description |
|------------- | ------------- | -------------|
| [**testEchoBodyPet**](BodyApi.md#testEchoBodyPet) | **POST** /echo/body/Pet | Test body parameter(s) |
| [**testEchoBodyPetWithHttpInfo**](BodyApi.md#testEchoBodyPetWithHttpInfo) | **POST** /echo/body/Pet | Test body parameter(s) |
| [**testEchoBodyPetResponseString**](BodyApi.md#testEchoBodyPetResponseString) | **POST** /echo/body/Pet/response_string | Test empty response body |
| [**testEchoBodyPetResponseStringWithHttpInfo**](BodyApi.md#testEchoBodyPetResponseStringWithHttpInfo) | **POST** /echo/body/Pet/response_string | Test empty response body |



## testEchoBodyPet

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

## testEchoBodyPetWithHttpInfo

> ApiResponse<Pet> testEchoBodyPet testEchoBodyPetWithHttpInfo(pet)

Test body parameter(s)

Test body parameter(s)

### Example

```java
// Import classes:
import org.openapitools.client.ApiClient;
import org.openapitools.client.ApiException;
import org.openapitools.client.ApiResponse;
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
            ApiResponse<Pet> response = apiInstance.testEchoBodyPetWithHttpInfo(pet);
            System.out.println("Status code: " + response.getStatusCode());
            System.out.println("Response headers: " + response.getHeaders());
            System.out.println("Response body: " + response.getData());
        } catch (ApiException e) {
            System.err.println("Exception when calling BodyApi#testEchoBodyPet");
            System.err.println("Status code: " + e.getCode());
            System.err.println("Response headers: " + e.getResponseHeaders());
            System.err.println("Reason: " + e.getResponseBody());
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

ApiResponse<[**Pet**](Pet.md)>


### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: application/json
- **Accept**: application/json

### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **200** | Successful operation |  -  |


## testEchoBodyPetResponseString

> String testEchoBodyPetResponseString(pet)

Test empty response body

Test empty response body

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
            String result = apiInstance.testEchoBodyPetResponseString(pet);
            System.out.println(result);
        } catch (ApiException e) {
            System.err.println("Exception when calling BodyApi#testEchoBodyPetResponseString");
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

**String**


### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: application/json
- **Accept**: text/plain

### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **200** | Successful operation |  -  |

## testEchoBodyPetResponseStringWithHttpInfo

> ApiResponse<String> testEchoBodyPetResponseString testEchoBodyPetResponseStringWithHttpInfo(pet)

Test empty response body

Test empty response body

### Example

```java
// Import classes:
import org.openapitools.client.ApiClient;
import org.openapitools.client.ApiException;
import org.openapitools.client.ApiResponse;
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
            ApiResponse<String> response = apiInstance.testEchoBodyPetResponseStringWithHttpInfo(pet);
            System.out.println("Status code: " + response.getStatusCode());
            System.out.println("Response headers: " + response.getHeaders());
            System.out.println("Response body: " + response.getData());
        } catch (ApiException e) {
            System.err.println("Exception when calling BodyApi#testEchoBodyPetResponseString");
            System.err.println("Status code: " + e.getCode());
            System.err.println("Response headers: " + e.getResponseHeaders());
            System.err.println("Reason: " + e.getResponseBody());
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

ApiResponse<**String**>


### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: application/json
- **Accept**: text/plain

### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **200** | Successful operation |  -  |

