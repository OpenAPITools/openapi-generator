# BodyApi

All URIs are relative to *http://localhost:3000*

| Method | HTTP request | Description |
|------------- | ------------- | -------------|
| [**testBinaryGif**](BodyApi.md#testBinaryGif) | **POST** /binary/gif | Test binary (gif) response body |
| [**testBodyApplicationOctetstreamBinary**](BodyApi.md#testBodyApplicationOctetstreamBinary) | **POST** /body/application/octetstream/binary | Test body parameter(s) |
| [**testEchoBodyFreeFormObjectResponseString**](BodyApi.md#testEchoBodyFreeFormObjectResponseString) | **POST** /echo/body/FreeFormObject/response_string | Test free form object |
| [**testEchoBodyPet**](BodyApi.md#testEchoBodyPet) | **POST** /echo/body/Pet | Test body parameter(s) |
| [**testEchoBodyPetResponseString**](BodyApi.md#testEchoBodyPetResponseString) | **POST** /echo/body/Pet/response_string | Test empty response body |
| [**testEchoBodyTagResponseString**](BodyApi.md#testEchoBodyTagResponseString) | **POST** /echo/body/Tag/response_string | Test empty json (request body) |



## testBinaryGif

> File testBinaryGif()

Test binary (gif) response body

Test binary (gif) response body

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
        try {
            File result = apiInstance.testBinaryGif();
            System.out.println(result);
        } catch (ApiException e) {
            System.err.println("Exception when calling BodyApi#testBinaryGif");
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

[**File**](File.md)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: image/gif


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **200** | Successful operation |  -  |


## testBodyApplicationOctetstreamBinary

> String testBodyApplicationOctetstreamBinary(body)

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
        File body = new File("/path/to/file"); // File | 
        try {
            String result = apiInstance.testBodyApplicationOctetstreamBinary(body);
            System.out.println(result);
        } catch (ApiException e) {
            System.err.println("Exception when calling BodyApi#testBodyApplicationOctetstreamBinary");
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
| **body** | **File**|  | [optional] |

### Return type

**String**

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: application/octet-stream
- **Accept**: text/plain


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **200** | Successful operation |  -  |


## testEchoBodyFreeFormObjectResponseString

> String testEchoBodyFreeFormObjectResponseString(body)

Test free form object

Test free form object

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
        Object body = null; // Object | Free form object
        try {
            String result = apiInstance.testEchoBodyFreeFormObjectResponseString(body);
            System.out.println(result);
        } catch (ApiException e) {
            System.err.println("Exception when calling BodyApi#testEchoBodyFreeFormObjectResponseString");
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
| **body** | **Object**| Free form object | [optional] |

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


## testEchoBodyTagResponseString

> String testEchoBodyTagResponseString(tag)

Test empty json (request body)

Test empty json (request body)

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
        Tag tag = new Tag(); // Tag | Tag object
        try {
            String result = apiInstance.testEchoBodyTagResponseString(tag);
            System.out.println(result);
        } catch (ApiException e) {
            System.err.println("Exception when calling BodyApi#testEchoBodyTagResponseString");
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
| **tag** | [**Tag**](Tag.md)| Tag object | [optional] |

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

