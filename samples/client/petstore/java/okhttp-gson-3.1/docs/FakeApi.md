# FakeApi

All URIs are relative to *http://petstore.swagger.io/v2*

| Method | HTTP request | Description |
|------------- | ------------- | -------------|
| [**op1**](FakeApi.md#op1) | **POST** /fake/api/changeowner | op1 |
| [**op2**](FakeApi.md#op2) | **POST** /fake/api/changename | op2 |
| [**op3**](FakeApi.md#op3) | **POST** /fake/api/query/enum | op3 |
| [**refToRefParameter**](FakeApi.md#refToRefParameter) | **GET** /ref/ref_to_parameter |  |
| [**responseNoRef**](FakeApi.md#responseNoRef) | **GET** /no_ref |  |
| [**responseRefToNoRef**](FakeApi.md#responseRefToNoRef) | **GET** /ref/no_ref |  |
| [**responseRefToRef**](FakeApi.md#responseRefToRef) | **GET** /ref/ref |  |


<a id="op1"></a>
# **op1**
> Object op1()

op1

### Example
```java
// Import classes:
import org.openapitools.client.ApiClient;
import org.openapitools.client.ApiException;
import org.openapitools.client.Configuration;
import org.openapitools.client.models.*;
import org.openapitools.client.api.FakeApi;

public class Example {
  public static void main(String[] args) {
    ApiClient defaultClient = Configuration.getDefaultApiClient();
    defaultClient.setBasePath("http://petstore.swagger.io/v2");

    FakeApi apiInstance = new FakeApi(defaultClient);
    try {
      Object result = apiInstance.op1();
      System.out.println(result);
    } catch (ApiException e) {
      System.err.println("Exception when calling FakeApi#op1");
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

**Object**

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/json

### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **201** | Successful Response |  -  |
| **422** | Validation Error |  -  |

<a id="op2"></a>
# **op2**
> Object op2()

op2

### Example
```java
// Import classes:
import org.openapitools.client.ApiClient;
import org.openapitools.client.ApiException;
import org.openapitools.client.Configuration;
import org.openapitools.client.models.*;
import org.openapitools.client.api.FakeApi;

public class Example {
  public static void main(String[] args) {
    ApiClient defaultClient = Configuration.getDefaultApiClient();
    defaultClient.setBasePath("http://petstore.swagger.io/v2");

    FakeApi apiInstance = new FakeApi(defaultClient);
    try {
      Object result = apiInstance.op2();
      System.out.println(result);
    } catch (ApiException e) {
      System.err.println("Exception when calling FakeApi#op2");
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

**Object**

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/json

### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **201** | Successful Response |  -  |
| **422** | Validation Error |  -  |

<a id="op3"></a>
# **op3**
> op3(queryEnum)

op3

### Example
```java
// Import classes:
import org.openapitools.client.ApiClient;
import org.openapitools.client.ApiException;
import org.openapitools.client.Configuration;
import org.openapitools.client.models.*;
import org.openapitools.client.api.FakeApi;

public class Example {
  public static void main(String[] args) {
    ApiClient defaultClient = Configuration.getDefaultApiClient();
    defaultClient.setBasePath("http://petstore.swagger.io/v2");

    FakeApi apiInstance = new FakeApi(defaultClient);
    List<CodesEnum> queryEnum = Arrays.asList(); // List<CodesEnum> | query enum test
    try {
      apiInstance.op3(queryEnum);
    } catch (ApiException e) {
      System.err.println("Exception when calling FakeApi#op3");
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
| **queryEnum** | [**List&lt;CodesEnum&gt;**](CodesEnum.md)| query enum test | |

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
| **200** | Successful Response |  -  |

<a id="refToRefParameter"></a>
# **refToRefParameter**
> String refToRefParameter(refToUuid)



### Example
```java
// Import classes:
import org.openapitools.client.ApiClient;
import org.openapitools.client.ApiException;
import org.openapitools.client.Configuration;
import org.openapitools.client.models.*;
import org.openapitools.client.api.FakeApi;

public class Example {
  public static void main(String[] args) {
    ApiClient defaultClient = Configuration.getDefaultApiClient();
    defaultClient.setBasePath("http://petstore.swagger.io/v2");

    FakeApi apiInstance = new FakeApi(defaultClient);
    UUID refToUuid = UUID.fromString("61864654-6e6b-4152-a62f-795fdd606bc2"); // UUID | to test ref to parameter (uuid)
    try {
      String result = apiInstance.refToRefParameter(refToUuid);
      System.out.println(result);
    } catch (ApiException e) {
      System.err.println("Exception when calling FakeApi#refToRefParameter");
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
| **refToUuid** | **UUID**| to test ref to parameter (uuid) | |

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
| **200** |  |  -  |

<a id="responseNoRef"></a>
# **responseNoRef**
> String responseNoRef()



### Example
```java
// Import classes:
import org.openapitools.client.ApiClient;
import org.openapitools.client.ApiException;
import org.openapitools.client.Configuration;
import org.openapitools.client.models.*;
import org.openapitools.client.api.FakeApi;

public class Example {
  public static void main(String[] args) {
    ApiClient defaultClient = Configuration.getDefaultApiClient();
    defaultClient.setBasePath("http://petstore.swagger.io/v2");

    FakeApi apiInstance = new FakeApi(defaultClient);
    try {
      String result = apiInstance.responseNoRef();
      System.out.println(result);
    } catch (ApiException e) {
      System.err.println("Exception when calling FakeApi#responseNoRef");
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

**String**

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: text/plain

### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **200** | required to pass validation |  -  |

<a id="responseRefToNoRef"></a>
# **responseRefToNoRef**
> String responseRefToNoRef()



### Example
```java
// Import classes:
import org.openapitools.client.ApiClient;
import org.openapitools.client.ApiException;
import org.openapitools.client.Configuration;
import org.openapitools.client.models.*;
import org.openapitools.client.api.FakeApi;

public class Example {
  public static void main(String[] args) {
    ApiClient defaultClient = Configuration.getDefaultApiClient();
    defaultClient.setBasePath("http://petstore.swagger.io/v2");

    FakeApi apiInstance = new FakeApi(defaultClient);
    try {
      String result = apiInstance.responseRefToNoRef();
      System.out.println(result);
    } catch (ApiException e) {
      System.err.println("Exception when calling FakeApi#responseRefToNoRef");
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

**String**

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: text/plain

### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **200** |  |  -  |

<a id="responseRefToRef"></a>
# **responseRefToRef**
> String responseRefToRef()



### Example
```java
// Import classes:
import org.openapitools.client.ApiClient;
import org.openapitools.client.ApiException;
import org.openapitools.client.Configuration;
import org.openapitools.client.models.*;
import org.openapitools.client.api.FakeApi;

public class Example {
  public static void main(String[] args) {
    ApiClient defaultClient = Configuration.getDefaultApiClient();
    defaultClient.setBasePath("http://petstore.swagger.io/v2");

    FakeApi apiInstance = new FakeApi(defaultClient);
    try {
      String result = apiInstance.responseRefToRef();
      System.out.println(result);
    } catch (ApiException e) {
      System.err.println("Exception when calling FakeApi#responseRefToRef");
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

**String**

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: text/plain

### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **200** |  |  -  |

