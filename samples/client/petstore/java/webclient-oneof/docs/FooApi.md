# FooApi

All URIs are relative to *http://localhost:8080*

| Method | HTTP request | Description |
|------------- | ------------- | -------------|
| [**createFoo**](FooApi.md#createFoo) | **POST** /foo | Create a Foo |
| [**getAllFoos**](FooApi.md#getAllFoos) | **GET** /foo | GET all Foos |



## createFoo

> FooRefOrValue createFoo(foo)

Create a Foo

### Example

```java
// Import classes:
import org.openapitools.client.ApiClient;
import org.openapitools.client.ApiException;
import org.openapitools.client.Configuration;
import org.openapitools.client.models.*;
import org.openapitools.client.api.FooApi;

public class Example {
    public static void main(String[] args) {
        ApiClient defaultClient = Configuration.getDefaultApiClient();
        defaultClient.setBasePath("http://localhost:8080");

        FooApi apiInstance = new FooApi(defaultClient);
        Foo foo = new Foo(); // Foo | The Foo to be created
        try {
            FooRefOrValue result = apiInstance.createFoo(foo);
            System.out.println(result);
        } catch (ApiException e) {
            System.err.println("Exception when calling FooApi#createFoo");
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
| **foo** | [**Foo**](Foo.md)| The Foo to be created | [optional] |

### Return type

[**FooRefOrValue**](FooRefOrValue.md)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: application/json;charset=utf-8
- **Accept**: application/json


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **201** | Error |  -  |


## getAllFoos

> List&lt;FooRefOrValue&gt; getAllFoos()

GET all Foos

### Example

```java
// Import classes:
import org.openapitools.client.ApiClient;
import org.openapitools.client.ApiException;
import org.openapitools.client.Configuration;
import org.openapitools.client.models.*;
import org.openapitools.client.api.FooApi;

public class Example {
    public static void main(String[] args) {
        ApiClient defaultClient = Configuration.getDefaultApiClient();
        defaultClient.setBasePath("http://localhost:8080");

        FooApi apiInstance = new FooApi(defaultClient);
        try {
            List<FooRefOrValue> result = apiInstance.getAllFoos();
            System.out.println(result);
        } catch (ApiException e) {
            System.err.println("Exception when calling FooApi#getAllFoos");
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

[**List&lt;FooRefOrValue&gt;**](FooRefOrValue.md)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: application/json;charset=utf-8


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **200** | Success |  -  |

