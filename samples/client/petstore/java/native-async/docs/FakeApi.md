# FakeApi

All URIs are relative to *http://petstore.swagger.io:80/v2*

Method | HTTP request | Description
------------- | ------------- | -------------
[**createXmlItem**](FakeApi.md#createXmlItem) | **POST** /fake/create_xml_item | creates an XmlItem
[**createXmlItemWithHttpInfo**](FakeApi.md#createXmlItemWithHttpInfo) | **POST** /fake/create_xml_item | creates an XmlItem
[**fakeOuterBooleanSerialize**](FakeApi.md#fakeOuterBooleanSerialize) | **POST** /fake/outer/boolean | 
[**fakeOuterBooleanSerializeWithHttpInfo**](FakeApi.md#fakeOuterBooleanSerializeWithHttpInfo) | **POST** /fake/outer/boolean | 
[**fakeOuterCompositeSerialize**](FakeApi.md#fakeOuterCompositeSerialize) | **POST** /fake/outer/composite | 
[**fakeOuterCompositeSerializeWithHttpInfo**](FakeApi.md#fakeOuterCompositeSerializeWithHttpInfo) | **POST** /fake/outer/composite | 
[**fakeOuterNumberSerialize**](FakeApi.md#fakeOuterNumberSerialize) | **POST** /fake/outer/number | 
[**fakeOuterNumberSerializeWithHttpInfo**](FakeApi.md#fakeOuterNumberSerializeWithHttpInfo) | **POST** /fake/outer/number | 
[**fakeOuterStringSerialize**](FakeApi.md#fakeOuterStringSerialize) | **POST** /fake/outer/string | 
[**fakeOuterStringSerializeWithHttpInfo**](FakeApi.md#fakeOuterStringSerializeWithHttpInfo) | **POST** /fake/outer/string | 
[**testBodyWithFileSchema**](FakeApi.md#testBodyWithFileSchema) | **PUT** /fake/body-with-file-schema | 
[**testBodyWithFileSchemaWithHttpInfo**](FakeApi.md#testBodyWithFileSchemaWithHttpInfo) | **PUT** /fake/body-with-file-schema | 
[**testBodyWithQueryParams**](FakeApi.md#testBodyWithQueryParams) | **PUT** /fake/body-with-query-params | 
[**testBodyWithQueryParamsWithHttpInfo**](FakeApi.md#testBodyWithQueryParamsWithHttpInfo) | **PUT** /fake/body-with-query-params | 
[**testClientModel**](FakeApi.md#testClientModel) | **PATCH** /fake | To test \&quot;client\&quot; model
[**testClientModelWithHttpInfo**](FakeApi.md#testClientModelWithHttpInfo) | **PATCH** /fake | To test \&quot;client\&quot; model
[**testEndpointParameters**](FakeApi.md#testEndpointParameters) | **POST** /fake | Fake endpoint for testing various parameters  假端點  偽のエンドポイント  가짜 엔드 포인트
[**testEndpointParametersWithHttpInfo**](FakeApi.md#testEndpointParametersWithHttpInfo) | **POST** /fake | Fake endpoint for testing various parameters  假端點  偽のエンドポイント  가짜 엔드 포인트
[**testEnumParameters**](FakeApi.md#testEnumParameters) | **GET** /fake | To test enum parameters
[**testEnumParametersWithHttpInfo**](FakeApi.md#testEnumParametersWithHttpInfo) | **GET** /fake | To test enum parameters
[**testGroupParameters**](FakeApi.md#testGroupParameters) | **DELETE** /fake | Fake endpoint to test group parameters (optional)
[**testGroupParametersWithHttpInfo**](FakeApi.md#testGroupParametersWithHttpInfo) | **DELETE** /fake | Fake endpoint to test group parameters (optional)
[**testInlineAdditionalProperties**](FakeApi.md#testInlineAdditionalProperties) | **POST** /fake/inline-additionalProperties | test inline additionalProperties
[**testInlineAdditionalPropertiesWithHttpInfo**](FakeApi.md#testInlineAdditionalPropertiesWithHttpInfo) | **POST** /fake/inline-additionalProperties | test inline additionalProperties
[**testJsonFormData**](FakeApi.md#testJsonFormData) | **GET** /fake/jsonFormData | test json serialization of form data
[**testJsonFormDataWithHttpInfo**](FakeApi.md#testJsonFormDataWithHttpInfo) | **GET** /fake/jsonFormData | test json serialization of form data
[**testQueryParameterCollectionFormat**](FakeApi.md#testQueryParameterCollectionFormat) | **PUT** /fake/test-query-parameters | 
[**testQueryParameterCollectionFormatWithHttpInfo**](FakeApi.md#testQueryParameterCollectionFormatWithHttpInfo) | **PUT** /fake/test-query-parameters | 



## createXmlItem

> CompletableFuture<Void> createXmlItem(xmlItem)

creates an XmlItem

this route creates an XmlItem

### Example

```java
// Import classes:
import org.openapitools.client.ApiClient;
import org.openapitools.client.ApiException;
import org.openapitools.client.Configuration;
import org.openapitools.client.models.*;
import org.openapitools.client.api.FakeApi;
import java.util.concurrent.CompletableFuture;

public class Example {
    public static void main(String[] args) {
        ApiClient defaultClient = Configuration.getDefaultApiClient();
        defaultClient.setBasePath("http://petstore.swagger.io:80/v2");

        FakeApi apiInstance = new FakeApi(defaultClient);
        XmlItem xmlItem = new XmlItem(); // XmlItem | XmlItem Body
        try {
            CompletableFuture<Void> result = apiInstance.createXmlItem(xmlItem);
        } catch (ApiException e) {
            System.err.println("Exception when calling FakeApi#createXmlItem");
            System.err.println("Status code: " + e.getCode());
            System.err.println("Reason: " + e.getResponseBody());
            System.err.println("Response headers: " + e.getResponseHeaders());
            e.printStackTrace();
        }
    }
}
```

### Parameters


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **xmlItem** | [**XmlItem**](XmlItem.md)| XmlItem Body |

### Return type


CompletableFuture<void> (empty response body)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: application/xml, application/xml; charset=utf-8, application/xml; charset=utf-16, text/xml, text/xml; charset=utf-8, text/xml; charset=utf-16
- **Accept**: Not defined

### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **200** | successful operation |  -  |

## createXmlItemWithHttpInfo

> CompletableFuture<ApiResponse<Void>> createXmlItem createXmlItemWithHttpInfo(xmlItem)

creates an XmlItem

this route creates an XmlItem

### Example

```java
// Import classes:
import org.openapitools.client.ApiClient;
import org.openapitools.client.ApiException;
import org.openapitools.client.ApiResponse;
import org.openapitools.client.Configuration;
import org.openapitools.client.models.*;
import org.openapitools.client.api.FakeApi;
import java.util.concurrent.CompletableFuture;

public class Example {
    public static void main(String[] args) {
        ApiClient defaultClient = Configuration.getDefaultApiClient();
        defaultClient.setBasePath("http://petstore.swagger.io:80/v2");

        FakeApi apiInstance = new FakeApi(defaultClient);
        XmlItem xmlItem = new XmlItem(); // XmlItem | XmlItem Body
        try {
            CompletableFuture<ApiResponse<Void>> response = apiInstance.createXmlItemWithHttpInfo(xmlItem);
            System.out.println("Status code: " + response.get().getStatusCode());
            System.out.println("Response headers: " + response.get().getHeaders());
        } catch (InterruptedException | ExecutionException e) {
            ApiException apiException = (ApiException)e.getCause();
            System.err.println("Exception when calling FakeApi#createXmlItem");
            System.err.println("Status code: " + apiException.getCode());
            System.err.println("Response headers: " + apiException.getResponseHeaders());
            System.err.println("Reason: " + apiException.getResponseBody());
            e.printStackTrace();
        } catch (ApiException e) {
            System.err.println("Exception when calling FakeApi#createXmlItem");
            System.err.println("Status code: " + e.getCode());
            System.err.println("Response headers: " + e.getResponseHeaders());
            System.err.println("Reason: " + e.getResponseBody());
            e.printStackTrace();
        }
    }
}
```

### Parameters


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **xmlItem** | [**XmlItem**](XmlItem.md)| XmlItem Body |

### Return type


CompletableFuture<ApiResponse<Void>>

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: application/xml, application/xml; charset=utf-8, application/xml; charset=utf-16, text/xml, text/xml; charset=utf-8, text/xml; charset=utf-16
- **Accept**: Not defined

### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **200** | successful operation |  -  |


## fakeOuterBooleanSerialize

> CompletableFuture<Boolean> fakeOuterBooleanSerialize(body)



Test serialization of outer boolean types

### Example

```java
// Import classes:
import org.openapitools.client.ApiClient;
import org.openapitools.client.ApiException;
import org.openapitools.client.Configuration;
import org.openapitools.client.models.*;
import org.openapitools.client.api.FakeApi;
import java.util.concurrent.CompletableFuture;

public class Example {
    public static void main(String[] args) {
        ApiClient defaultClient = Configuration.getDefaultApiClient();
        defaultClient.setBasePath("http://petstore.swagger.io:80/v2");

        FakeApi apiInstance = new FakeApi(defaultClient);
        Boolean body = true; // Boolean | Input boolean as post body
        try {
            CompletableFuture<Boolean> result = apiInstance.fakeOuterBooleanSerialize(body);
            System.out.println(result.get());
        } catch (ApiException e) {
            System.err.println("Exception when calling FakeApi#fakeOuterBooleanSerialize");
            System.err.println("Status code: " + e.getCode());
            System.err.println("Reason: " + e.getResponseBody());
            System.err.println("Response headers: " + e.getResponseHeaders());
            e.printStackTrace();
        }
    }
}
```

### Parameters


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | **Boolean**| Input boolean as post body | [optional]

### Return type

CompletableFuture<**Boolean**>


### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: */*

### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **200** | Output boolean |  -  |

## fakeOuterBooleanSerializeWithHttpInfo

> CompletableFuture<ApiResponse<Boolean>> fakeOuterBooleanSerialize fakeOuterBooleanSerializeWithHttpInfo(body)



Test serialization of outer boolean types

### Example

```java
// Import classes:
import org.openapitools.client.ApiClient;
import org.openapitools.client.ApiException;
import org.openapitools.client.ApiResponse;
import org.openapitools.client.Configuration;
import org.openapitools.client.models.*;
import org.openapitools.client.api.FakeApi;
import java.util.concurrent.CompletableFuture;

public class Example {
    public static void main(String[] args) {
        ApiClient defaultClient = Configuration.getDefaultApiClient();
        defaultClient.setBasePath("http://petstore.swagger.io:80/v2");

        FakeApi apiInstance = new FakeApi(defaultClient);
        Boolean body = true; // Boolean | Input boolean as post body
        try {
            CompletableFuture<ApiResponse<Boolean>> response = apiInstance.fakeOuterBooleanSerializeWithHttpInfo(body);
            System.out.println("Status code: " + response.get().getStatusCode());
            System.out.println("Response headers: " + response.get().getHeaders());
            System.out.println("Response body: " + response.get().getData());
        } catch (InterruptedException | ExecutionException e) {
            ApiException apiException = (ApiException)e.getCause();
            System.err.println("Exception when calling FakeApi#fakeOuterBooleanSerialize");
            System.err.println("Status code: " + apiException.getCode());
            System.err.println("Response headers: " + apiException.getResponseHeaders());
            System.err.println("Reason: " + apiException.getResponseBody());
            e.printStackTrace();
        } catch (ApiException e) {
            System.err.println("Exception when calling FakeApi#fakeOuterBooleanSerialize");
            System.err.println("Status code: " + e.getCode());
            System.err.println("Response headers: " + e.getResponseHeaders());
            System.err.println("Reason: " + e.getResponseBody());
            e.printStackTrace();
        }
    }
}
```

### Parameters


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | **Boolean**| Input boolean as post body | [optional]

### Return type

CompletableFuture<ApiResponse<**Boolean**>>


### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: */*

### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **200** | Output boolean |  -  |


## fakeOuterCompositeSerialize

> CompletableFuture<OuterComposite> fakeOuterCompositeSerialize(body)



Test serialization of object with outer number type

### Example

```java
// Import classes:
import org.openapitools.client.ApiClient;
import org.openapitools.client.ApiException;
import org.openapitools.client.Configuration;
import org.openapitools.client.models.*;
import org.openapitools.client.api.FakeApi;
import java.util.concurrent.CompletableFuture;

public class Example {
    public static void main(String[] args) {
        ApiClient defaultClient = Configuration.getDefaultApiClient();
        defaultClient.setBasePath("http://petstore.swagger.io:80/v2");

        FakeApi apiInstance = new FakeApi(defaultClient);
        OuterComposite body = new OuterComposite(); // OuterComposite | Input composite as post body
        try {
            CompletableFuture<OuterComposite> result = apiInstance.fakeOuterCompositeSerialize(body);
            System.out.println(result.get());
        } catch (ApiException e) {
            System.err.println("Exception when calling FakeApi#fakeOuterCompositeSerialize");
            System.err.println("Status code: " + e.getCode());
            System.err.println("Reason: " + e.getResponseBody());
            System.err.println("Response headers: " + e.getResponseHeaders());
            e.printStackTrace();
        }
    }
}
```

### Parameters


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | [**OuterComposite**](OuterComposite.md)| Input composite as post body | [optional]

### Return type

CompletableFuture<[**OuterComposite**](OuterComposite.md)>


### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: */*

### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **200** | Output composite |  -  |

## fakeOuterCompositeSerializeWithHttpInfo

> CompletableFuture<ApiResponse<OuterComposite>> fakeOuterCompositeSerialize fakeOuterCompositeSerializeWithHttpInfo(body)



Test serialization of object with outer number type

### Example

```java
// Import classes:
import org.openapitools.client.ApiClient;
import org.openapitools.client.ApiException;
import org.openapitools.client.ApiResponse;
import org.openapitools.client.Configuration;
import org.openapitools.client.models.*;
import org.openapitools.client.api.FakeApi;
import java.util.concurrent.CompletableFuture;

public class Example {
    public static void main(String[] args) {
        ApiClient defaultClient = Configuration.getDefaultApiClient();
        defaultClient.setBasePath("http://petstore.swagger.io:80/v2");

        FakeApi apiInstance = new FakeApi(defaultClient);
        OuterComposite body = new OuterComposite(); // OuterComposite | Input composite as post body
        try {
            CompletableFuture<ApiResponse<OuterComposite>> response = apiInstance.fakeOuterCompositeSerializeWithHttpInfo(body);
            System.out.println("Status code: " + response.get().getStatusCode());
            System.out.println("Response headers: " + response.get().getHeaders());
            System.out.println("Response body: " + response.get().getData());
        } catch (InterruptedException | ExecutionException e) {
            ApiException apiException = (ApiException)e.getCause();
            System.err.println("Exception when calling FakeApi#fakeOuterCompositeSerialize");
            System.err.println("Status code: " + apiException.getCode());
            System.err.println("Response headers: " + apiException.getResponseHeaders());
            System.err.println("Reason: " + apiException.getResponseBody());
            e.printStackTrace();
        } catch (ApiException e) {
            System.err.println("Exception when calling FakeApi#fakeOuterCompositeSerialize");
            System.err.println("Status code: " + e.getCode());
            System.err.println("Response headers: " + e.getResponseHeaders());
            System.err.println("Reason: " + e.getResponseBody());
            e.printStackTrace();
        }
    }
}
```

### Parameters


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | [**OuterComposite**](OuterComposite.md)| Input composite as post body | [optional]

### Return type

CompletableFuture<ApiResponse<[**OuterComposite**](OuterComposite.md)>>


### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: */*

### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **200** | Output composite |  -  |


## fakeOuterNumberSerialize

> CompletableFuture<BigDecimal> fakeOuterNumberSerialize(body)



Test serialization of outer number types

### Example

```java
// Import classes:
import org.openapitools.client.ApiClient;
import org.openapitools.client.ApiException;
import org.openapitools.client.Configuration;
import org.openapitools.client.models.*;
import org.openapitools.client.api.FakeApi;
import java.util.concurrent.CompletableFuture;

public class Example {
    public static void main(String[] args) {
        ApiClient defaultClient = Configuration.getDefaultApiClient();
        defaultClient.setBasePath("http://petstore.swagger.io:80/v2");

        FakeApi apiInstance = new FakeApi(defaultClient);
        BigDecimal body = new BigDecimal(78); // BigDecimal | Input number as post body
        try {
            CompletableFuture<BigDecimal> result = apiInstance.fakeOuterNumberSerialize(body);
            System.out.println(result.get());
        } catch (ApiException e) {
            System.err.println("Exception when calling FakeApi#fakeOuterNumberSerialize");
            System.err.println("Status code: " + e.getCode());
            System.err.println("Reason: " + e.getResponseBody());
            System.err.println("Response headers: " + e.getResponseHeaders());
            e.printStackTrace();
        }
    }
}
```

### Parameters


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | **BigDecimal**| Input number as post body | [optional]

### Return type

CompletableFuture<[**BigDecimal**](BigDecimal.md)>


### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: */*

### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **200** | Output number |  -  |

## fakeOuterNumberSerializeWithHttpInfo

> CompletableFuture<ApiResponse<BigDecimal>> fakeOuterNumberSerialize fakeOuterNumberSerializeWithHttpInfo(body)



Test serialization of outer number types

### Example

```java
// Import classes:
import org.openapitools.client.ApiClient;
import org.openapitools.client.ApiException;
import org.openapitools.client.ApiResponse;
import org.openapitools.client.Configuration;
import org.openapitools.client.models.*;
import org.openapitools.client.api.FakeApi;
import java.util.concurrent.CompletableFuture;

public class Example {
    public static void main(String[] args) {
        ApiClient defaultClient = Configuration.getDefaultApiClient();
        defaultClient.setBasePath("http://petstore.swagger.io:80/v2");

        FakeApi apiInstance = new FakeApi(defaultClient);
        BigDecimal body = new BigDecimal(78); // BigDecimal | Input number as post body
        try {
            CompletableFuture<ApiResponse<BigDecimal>> response = apiInstance.fakeOuterNumberSerializeWithHttpInfo(body);
            System.out.println("Status code: " + response.get().getStatusCode());
            System.out.println("Response headers: " + response.get().getHeaders());
            System.out.println("Response body: " + response.get().getData());
        } catch (InterruptedException | ExecutionException e) {
            ApiException apiException = (ApiException)e.getCause();
            System.err.println("Exception when calling FakeApi#fakeOuterNumberSerialize");
            System.err.println("Status code: " + apiException.getCode());
            System.err.println("Response headers: " + apiException.getResponseHeaders());
            System.err.println("Reason: " + apiException.getResponseBody());
            e.printStackTrace();
        } catch (ApiException e) {
            System.err.println("Exception when calling FakeApi#fakeOuterNumberSerialize");
            System.err.println("Status code: " + e.getCode());
            System.err.println("Response headers: " + e.getResponseHeaders());
            System.err.println("Reason: " + e.getResponseBody());
            e.printStackTrace();
        }
    }
}
```

### Parameters


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | **BigDecimal**| Input number as post body | [optional]

### Return type

CompletableFuture<ApiResponse<[**BigDecimal**](BigDecimal.md)>>


### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: */*

### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **200** | Output number |  -  |


## fakeOuterStringSerialize

> CompletableFuture<String> fakeOuterStringSerialize(body)



Test serialization of outer string types

### Example

```java
// Import classes:
import org.openapitools.client.ApiClient;
import org.openapitools.client.ApiException;
import org.openapitools.client.Configuration;
import org.openapitools.client.models.*;
import org.openapitools.client.api.FakeApi;
import java.util.concurrent.CompletableFuture;

public class Example {
    public static void main(String[] args) {
        ApiClient defaultClient = Configuration.getDefaultApiClient();
        defaultClient.setBasePath("http://petstore.swagger.io:80/v2");

        FakeApi apiInstance = new FakeApi(defaultClient);
        String body = "body_example"; // String | Input string as post body
        try {
            CompletableFuture<String> result = apiInstance.fakeOuterStringSerialize(body);
            System.out.println(result.get());
        } catch (ApiException e) {
            System.err.println("Exception when calling FakeApi#fakeOuterStringSerialize");
            System.err.println("Status code: " + e.getCode());
            System.err.println("Reason: " + e.getResponseBody());
            System.err.println("Response headers: " + e.getResponseHeaders());
            e.printStackTrace();
        }
    }
}
```

### Parameters


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | **String**| Input string as post body | [optional]

### Return type

CompletableFuture<**String**>


### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: */*

### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **200** | Output string |  -  |

## fakeOuterStringSerializeWithHttpInfo

> CompletableFuture<ApiResponse<String>> fakeOuterStringSerialize fakeOuterStringSerializeWithHttpInfo(body)



Test serialization of outer string types

### Example

```java
// Import classes:
import org.openapitools.client.ApiClient;
import org.openapitools.client.ApiException;
import org.openapitools.client.ApiResponse;
import org.openapitools.client.Configuration;
import org.openapitools.client.models.*;
import org.openapitools.client.api.FakeApi;
import java.util.concurrent.CompletableFuture;

public class Example {
    public static void main(String[] args) {
        ApiClient defaultClient = Configuration.getDefaultApiClient();
        defaultClient.setBasePath("http://petstore.swagger.io:80/v2");

        FakeApi apiInstance = new FakeApi(defaultClient);
        String body = "body_example"; // String | Input string as post body
        try {
            CompletableFuture<ApiResponse<String>> response = apiInstance.fakeOuterStringSerializeWithHttpInfo(body);
            System.out.println("Status code: " + response.get().getStatusCode());
            System.out.println("Response headers: " + response.get().getHeaders());
            System.out.println("Response body: " + response.get().getData());
        } catch (InterruptedException | ExecutionException e) {
            ApiException apiException = (ApiException)e.getCause();
            System.err.println("Exception when calling FakeApi#fakeOuterStringSerialize");
            System.err.println("Status code: " + apiException.getCode());
            System.err.println("Response headers: " + apiException.getResponseHeaders());
            System.err.println("Reason: " + apiException.getResponseBody());
            e.printStackTrace();
        } catch (ApiException e) {
            System.err.println("Exception when calling FakeApi#fakeOuterStringSerialize");
            System.err.println("Status code: " + e.getCode());
            System.err.println("Response headers: " + e.getResponseHeaders());
            System.err.println("Reason: " + e.getResponseBody());
            e.printStackTrace();
        }
    }
}
```

### Parameters


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | **String**| Input string as post body | [optional]

### Return type

CompletableFuture<ApiResponse<**String**>>


### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: */*

### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **200** | Output string |  -  |


## testBodyWithFileSchema

> CompletableFuture<Void> testBodyWithFileSchema(body)



For this test, the body for this request much reference a schema named &#x60;File&#x60;.

### Example

```java
// Import classes:
import org.openapitools.client.ApiClient;
import org.openapitools.client.ApiException;
import org.openapitools.client.Configuration;
import org.openapitools.client.models.*;
import org.openapitools.client.api.FakeApi;
import java.util.concurrent.CompletableFuture;

public class Example {
    public static void main(String[] args) {
        ApiClient defaultClient = Configuration.getDefaultApiClient();
        defaultClient.setBasePath("http://petstore.swagger.io:80/v2");

        FakeApi apiInstance = new FakeApi(defaultClient);
        FileSchemaTestClass body = new FileSchemaTestClass(); // FileSchemaTestClass | 
        try {
            CompletableFuture<Void> result = apiInstance.testBodyWithFileSchema(body);
        } catch (ApiException e) {
            System.err.println("Exception when calling FakeApi#testBodyWithFileSchema");
            System.err.println("Status code: " + e.getCode());
            System.err.println("Reason: " + e.getResponseBody());
            System.err.println("Response headers: " + e.getResponseHeaders());
            e.printStackTrace();
        }
    }
}
```

### Parameters


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | [**FileSchemaTestClass**](FileSchemaTestClass.md)|  |

### Return type


CompletableFuture<void> (empty response body)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: application/json
- **Accept**: Not defined

### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **200** | Success |  -  |

## testBodyWithFileSchemaWithHttpInfo

> CompletableFuture<ApiResponse<Void>> testBodyWithFileSchema testBodyWithFileSchemaWithHttpInfo(body)



For this test, the body for this request much reference a schema named &#x60;File&#x60;.

### Example

```java
// Import classes:
import org.openapitools.client.ApiClient;
import org.openapitools.client.ApiException;
import org.openapitools.client.ApiResponse;
import org.openapitools.client.Configuration;
import org.openapitools.client.models.*;
import org.openapitools.client.api.FakeApi;
import java.util.concurrent.CompletableFuture;

public class Example {
    public static void main(String[] args) {
        ApiClient defaultClient = Configuration.getDefaultApiClient();
        defaultClient.setBasePath("http://petstore.swagger.io:80/v2");

        FakeApi apiInstance = new FakeApi(defaultClient);
        FileSchemaTestClass body = new FileSchemaTestClass(); // FileSchemaTestClass | 
        try {
            CompletableFuture<ApiResponse<Void>> response = apiInstance.testBodyWithFileSchemaWithHttpInfo(body);
            System.out.println("Status code: " + response.get().getStatusCode());
            System.out.println("Response headers: " + response.get().getHeaders());
        } catch (InterruptedException | ExecutionException e) {
            ApiException apiException = (ApiException)e.getCause();
            System.err.println("Exception when calling FakeApi#testBodyWithFileSchema");
            System.err.println("Status code: " + apiException.getCode());
            System.err.println("Response headers: " + apiException.getResponseHeaders());
            System.err.println("Reason: " + apiException.getResponseBody());
            e.printStackTrace();
        } catch (ApiException e) {
            System.err.println("Exception when calling FakeApi#testBodyWithFileSchema");
            System.err.println("Status code: " + e.getCode());
            System.err.println("Response headers: " + e.getResponseHeaders());
            System.err.println("Reason: " + e.getResponseBody());
            e.printStackTrace();
        }
    }
}
```

### Parameters


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | [**FileSchemaTestClass**](FileSchemaTestClass.md)|  |

### Return type


CompletableFuture<ApiResponse<Void>>

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: application/json
- **Accept**: Not defined

### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **200** | Success |  -  |


## testBodyWithQueryParams

> CompletableFuture<Void> testBodyWithQueryParams(query, body)



### Example

```java
// Import classes:
import org.openapitools.client.ApiClient;
import org.openapitools.client.ApiException;
import org.openapitools.client.Configuration;
import org.openapitools.client.models.*;
import org.openapitools.client.api.FakeApi;
import java.util.concurrent.CompletableFuture;

public class Example {
    public static void main(String[] args) {
        ApiClient defaultClient = Configuration.getDefaultApiClient();
        defaultClient.setBasePath("http://petstore.swagger.io:80/v2");

        FakeApi apiInstance = new FakeApi(defaultClient);
        String query = "query_example"; // String | 
        User body = new User(); // User | 
        try {
            CompletableFuture<Void> result = apiInstance.testBodyWithQueryParams(query, body);
        } catch (ApiException e) {
            System.err.println("Exception when calling FakeApi#testBodyWithQueryParams");
            System.err.println("Status code: " + e.getCode());
            System.err.println("Reason: " + e.getResponseBody());
            System.err.println("Response headers: " + e.getResponseHeaders());
            e.printStackTrace();
        }
    }
}
```

### Parameters


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **query** | **String**|  |
 **body** | [**User**](User.md)|  |

### Return type


CompletableFuture<void> (empty response body)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: application/json
- **Accept**: Not defined

### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **200** | Success |  -  |

## testBodyWithQueryParamsWithHttpInfo

> CompletableFuture<ApiResponse<Void>> testBodyWithQueryParams testBodyWithQueryParamsWithHttpInfo(query, body)



### Example

```java
// Import classes:
import org.openapitools.client.ApiClient;
import org.openapitools.client.ApiException;
import org.openapitools.client.ApiResponse;
import org.openapitools.client.Configuration;
import org.openapitools.client.models.*;
import org.openapitools.client.api.FakeApi;
import java.util.concurrent.CompletableFuture;

public class Example {
    public static void main(String[] args) {
        ApiClient defaultClient = Configuration.getDefaultApiClient();
        defaultClient.setBasePath("http://petstore.swagger.io:80/v2");

        FakeApi apiInstance = new FakeApi(defaultClient);
        String query = "query_example"; // String | 
        User body = new User(); // User | 
        try {
            CompletableFuture<ApiResponse<Void>> response = apiInstance.testBodyWithQueryParamsWithHttpInfo(query, body);
            System.out.println("Status code: " + response.get().getStatusCode());
            System.out.println("Response headers: " + response.get().getHeaders());
        } catch (InterruptedException | ExecutionException e) {
            ApiException apiException = (ApiException)e.getCause();
            System.err.println("Exception when calling FakeApi#testBodyWithQueryParams");
            System.err.println("Status code: " + apiException.getCode());
            System.err.println("Response headers: " + apiException.getResponseHeaders());
            System.err.println("Reason: " + apiException.getResponseBody());
            e.printStackTrace();
        } catch (ApiException e) {
            System.err.println("Exception when calling FakeApi#testBodyWithQueryParams");
            System.err.println("Status code: " + e.getCode());
            System.err.println("Response headers: " + e.getResponseHeaders());
            System.err.println("Reason: " + e.getResponseBody());
            e.printStackTrace();
        }
    }
}
```

### Parameters


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **query** | **String**|  |
 **body** | [**User**](User.md)|  |

### Return type


CompletableFuture<ApiResponse<Void>>

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: application/json
- **Accept**: Not defined

### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **200** | Success |  -  |


## testClientModel

> CompletableFuture<Client> testClientModel(body)

To test \&quot;client\&quot; model

To test \&quot;client\&quot; model

### Example

```java
// Import classes:
import org.openapitools.client.ApiClient;
import org.openapitools.client.ApiException;
import org.openapitools.client.Configuration;
import org.openapitools.client.models.*;
import org.openapitools.client.api.FakeApi;
import java.util.concurrent.CompletableFuture;

public class Example {
    public static void main(String[] args) {
        ApiClient defaultClient = Configuration.getDefaultApiClient();
        defaultClient.setBasePath("http://petstore.swagger.io:80/v2");

        FakeApi apiInstance = new FakeApi(defaultClient);
        Client body = new Client(); // Client | client model
        try {
            CompletableFuture<Client> result = apiInstance.testClientModel(body);
            System.out.println(result.get());
        } catch (ApiException e) {
            System.err.println("Exception when calling FakeApi#testClientModel");
            System.err.println("Status code: " + e.getCode());
            System.err.println("Reason: " + e.getResponseBody());
            System.err.println("Response headers: " + e.getResponseHeaders());
            e.printStackTrace();
        }
    }
}
```

### Parameters


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | [**Client**](Client.md)| client model |

### Return type

CompletableFuture<[**Client**](Client.md)>


### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: application/json
- **Accept**: application/json

### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **200** | successful operation |  -  |

## testClientModelWithHttpInfo

> CompletableFuture<ApiResponse<Client>> testClientModel testClientModelWithHttpInfo(body)

To test \&quot;client\&quot; model

To test \&quot;client\&quot; model

### Example

```java
// Import classes:
import org.openapitools.client.ApiClient;
import org.openapitools.client.ApiException;
import org.openapitools.client.ApiResponse;
import org.openapitools.client.Configuration;
import org.openapitools.client.models.*;
import org.openapitools.client.api.FakeApi;
import java.util.concurrent.CompletableFuture;

public class Example {
    public static void main(String[] args) {
        ApiClient defaultClient = Configuration.getDefaultApiClient();
        defaultClient.setBasePath("http://petstore.swagger.io:80/v2");

        FakeApi apiInstance = new FakeApi(defaultClient);
        Client body = new Client(); // Client | client model
        try {
            CompletableFuture<ApiResponse<Client>> response = apiInstance.testClientModelWithHttpInfo(body);
            System.out.println("Status code: " + response.get().getStatusCode());
            System.out.println("Response headers: " + response.get().getHeaders());
            System.out.println("Response body: " + response.get().getData());
        } catch (InterruptedException | ExecutionException e) {
            ApiException apiException = (ApiException)e.getCause();
            System.err.println("Exception when calling FakeApi#testClientModel");
            System.err.println("Status code: " + apiException.getCode());
            System.err.println("Response headers: " + apiException.getResponseHeaders());
            System.err.println("Reason: " + apiException.getResponseBody());
            e.printStackTrace();
        } catch (ApiException e) {
            System.err.println("Exception when calling FakeApi#testClientModel");
            System.err.println("Status code: " + e.getCode());
            System.err.println("Response headers: " + e.getResponseHeaders());
            System.err.println("Reason: " + e.getResponseBody());
            e.printStackTrace();
        }
    }
}
```

### Parameters


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | [**Client**](Client.md)| client model |

### Return type

CompletableFuture<ApiResponse<[**Client**](Client.md)>>


### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: application/json
- **Accept**: application/json

### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **200** | successful operation |  -  |


## testEndpointParameters

> CompletableFuture<Void> testEndpointParameters(number, _double, patternWithoutDelimiter, _byte, integer, int32, int64, _float, string, binary, date, dateTime, password, paramCallback)

Fake endpoint for testing various parameters  假端點  偽のエンドポイント  가짜 엔드 포인트

Fake endpoint for testing various parameters  假端點  偽のエンドポイント  가짜 엔드 포인트

### Example

```java
// Import classes:
import org.openapitools.client.ApiClient;
import org.openapitools.client.ApiException;
import org.openapitools.client.Configuration;
import org.openapitools.client.auth.*;
import org.openapitools.client.models.*;
import org.openapitools.client.api.FakeApi;
import java.util.concurrent.CompletableFuture;

public class Example {
    public static void main(String[] args) {
        ApiClient defaultClient = Configuration.getDefaultApiClient();
        defaultClient.setBasePath("http://petstore.swagger.io:80/v2");
        
        // Configure HTTP basic authorization: http_basic_test
        HttpBasicAuth http_basic_test = (HttpBasicAuth) defaultClient.getAuthentication("http_basic_test");
        http_basic_test.setUsername("YOUR USERNAME");
        http_basic_test.setPassword("YOUR PASSWORD");

        FakeApi apiInstance = new FakeApi(defaultClient);
        BigDecimal number = new BigDecimal(78); // BigDecimal | None
        Double _double = 3.4D; // Double | None
        String patternWithoutDelimiter = "patternWithoutDelimiter_example"; // String | None
        byte[] _byte = null; // byte[] | None
        Integer integer = 56; // Integer | None
        Integer int32 = 56; // Integer | None
        Long int64 = 56L; // Long | None
        Float _float = 3.4F; // Float | None
        String string = "string_example"; // String | None
        File binary = new File("/path/to/file"); // File | None
        LocalDate date = LocalDate.now(); // LocalDate | None
        OffsetDateTime dateTime = OffsetDateTime.now(); // OffsetDateTime | None
        String password = "password_example"; // String | None
        String paramCallback = "paramCallback_example"; // String | None
        try {
            CompletableFuture<Void> result = apiInstance.testEndpointParameters(number, _double, patternWithoutDelimiter, _byte, integer, int32, int64, _float, string, binary, date, dateTime, password, paramCallback);
        } catch (ApiException e) {
            System.err.println("Exception when calling FakeApi#testEndpointParameters");
            System.err.println("Status code: " + e.getCode());
            System.err.println("Reason: " + e.getResponseBody());
            System.err.println("Response headers: " + e.getResponseHeaders());
            e.printStackTrace();
        }
    }
}
```

### Parameters


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **number** | **BigDecimal**| None |
 **_double** | **Double**| None |
 **patternWithoutDelimiter** | **String**| None |
 **_byte** | **byte[]**| None |
 **integer** | **Integer**| None | [optional]
 **int32** | **Integer**| None | [optional]
 **int64** | **Long**| None | [optional]
 **_float** | **Float**| None | [optional]
 **string** | **String**| None | [optional]
 **binary** | **File**| None | [optional]
 **date** | **LocalDate**| None | [optional]
 **dateTime** | **OffsetDateTime**| None | [optional]
 **password** | **String**| None | [optional]
 **paramCallback** | **String**| None | [optional]

### Return type


CompletableFuture<void> (empty response body)

### Authorization

[http_basic_test](../README.md#http_basic_test)

### HTTP request headers

- **Content-Type**: application/x-www-form-urlencoded
- **Accept**: Not defined

### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **400** | Invalid username supplied |  -  |
| **404** | User not found |  -  |

## testEndpointParametersWithHttpInfo

> CompletableFuture<ApiResponse<Void>> testEndpointParameters testEndpointParametersWithHttpInfo(number, _double, patternWithoutDelimiter, _byte, integer, int32, int64, _float, string, binary, date, dateTime, password, paramCallback)

Fake endpoint for testing various parameters  假端點  偽のエンドポイント  가짜 엔드 포인트

Fake endpoint for testing various parameters  假端點  偽のエンドポイント  가짜 엔드 포인트

### Example

```java
// Import classes:
import org.openapitools.client.ApiClient;
import org.openapitools.client.ApiException;
import org.openapitools.client.ApiResponse;
import org.openapitools.client.Configuration;
import org.openapitools.client.auth.*;
import org.openapitools.client.models.*;
import org.openapitools.client.api.FakeApi;
import java.util.concurrent.CompletableFuture;

public class Example {
    public static void main(String[] args) {
        ApiClient defaultClient = Configuration.getDefaultApiClient();
        defaultClient.setBasePath("http://petstore.swagger.io:80/v2");
        
        // Configure HTTP basic authorization: http_basic_test
        HttpBasicAuth http_basic_test = (HttpBasicAuth) defaultClient.getAuthentication("http_basic_test");
        http_basic_test.setUsername("YOUR USERNAME");
        http_basic_test.setPassword("YOUR PASSWORD");

        FakeApi apiInstance = new FakeApi(defaultClient);
        BigDecimal number = new BigDecimal(78); // BigDecimal | None
        Double _double = 3.4D; // Double | None
        String patternWithoutDelimiter = "patternWithoutDelimiter_example"; // String | None
        byte[] _byte = null; // byte[] | None
        Integer integer = 56; // Integer | None
        Integer int32 = 56; // Integer | None
        Long int64 = 56L; // Long | None
        Float _float = 3.4F; // Float | None
        String string = "string_example"; // String | None
        File binary = new File("/path/to/file"); // File | None
        LocalDate date = LocalDate.now(); // LocalDate | None
        OffsetDateTime dateTime = OffsetDateTime.now(); // OffsetDateTime | None
        String password = "password_example"; // String | None
        String paramCallback = "paramCallback_example"; // String | None
        try {
            CompletableFuture<ApiResponse<Void>> response = apiInstance.testEndpointParametersWithHttpInfo(number, _double, patternWithoutDelimiter, _byte, integer, int32, int64, _float, string, binary, date, dateTime, password, paramCallback);
            System.out.println("Status code: " + response.get().getStatusCode());
            System.out.println("Response headers: " + response.get().getHeaders());
        } catch (InterruptedException | ExecutionException e) {
            ApiException apiException = (ApiException)e.getCause();
            System.err.println("Exception when calling FakeApi#testEndpointParameters");
            System.err.println("Status code: " + apiException.getCode());
            System.err.println("Response headers: " + apiException.getResponseHeaders());
            System.err.println("Reason: " + apiException.getResponseBody());
            e.printStackTrace();
        } catch (ApiException e) {
            System.err.println("Exception when calling FakeApi#testEndpointParameters");
            System.err.println("Status code: " + e.getCode());
            System.err.println("Response headers: " + e.getResponseHeaders());
            System.err.println("Reason: " + e.getResponseBody());
            e.printStackTrace();
        }
    }
}
```

### Parameters


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **number** | **BigDecimal**| None |
 **_double** | **Double**| None |
 **patternWithoutDelimiter** | **String**| None |
 **_byte** | **byte[]**| None |
 **integer** | **Integer**| None | [optional]
 **int32** | **Integer**| None | [optional]
 **int64** | **Long**| None | [optional]
 **_float** | **Float**| None | [optional]
 **string** | **String**| None | [optional]
 **binary** | **File**| None | [optional]
 **date** | **LocalDate**| None | [optional]
 **dateTime** | **OffsetDateTime**| None | [optional]
 **password** | **String**| None | [optional]
 **paramCallback** | **String**| None | [optional]

### Return type


CompletableFuture<ApiResponse<Void>>

### Authorization

[http_basic_test](../README.md#http_basic_test)

### HTTP request headers

- **Content-Type**: application/x-www-form-urlencoded
- **Accept**: Not defined

### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **400** | Invalid username supplied |  -  |
| **404** | User not found |  -  |


## testEnumParameters

> CompletableFuture<Void> testEnumParameters(enumHeaderStringArray, enumHeaderString, enumQueryStringArray, enumQueryString, enumQueryInteger, enumQueryDouble, enumFormStringArray, enumFormString)

To test enum parameters

To test enum parameters

### Example

```java
// Import classes:
import org.openapitools.client.ApiClient;
import org.openapitools.client.ApiException;
import org.openapitools.client.Configuration;
import org.openapitools.client.models.*;
import org.openapitools.client.api.FakeApi;
import java.util.concurrent.CompletableFuture;

public class Example {
    public static void main(String[] args) {
        ApiClient defaultClient = Configuration.getDefaultApiClient();
        defaultClient.setBasePath("http://petstore.swagger.io:80/v2");

        FakeApi apiInstance = new FakeApi(defaultClient);
        List<String> enumHeaderStringArray = Arrays.asList("$"); // List<String> | Header parameter enum test (string array)
        String enumHeaderString = "_abc"; // String | Header parameter enum test (string)
        List<String> enumQueryStringArray = Arrays.asList("$"); // List<String> | Query parameter enum test (string array)
        String enumQueryString = "_abc"; // String | Query parameter enum test (string)
        Integer enumQueryInteger = 1; // Integer | Query parameter enum test (double)
        Double enumQueryDouble = 1.1D; // Double | Query parameter enum test (double)
        List<String> enumFormStringArray = Arrays.asList("$"); // List<String> | Form parameter enum test (string array)
        String enumFormString = "_abc"; // String | Form parameter enum test (string)
        try {
            CompletableFuture<Void> result = apiInstance.testEnumParameters(enumHeaderStringArray, enumHeaderString, enumQueryStringArray, enumQueryString, enumQueryInteger, enumQueryDouble, enumFormStringArray, enumFormString);
        } catch (ApiException e) {
            System.err.println("Exception when calling FakeApi#testEnumParameters");
            System.err.println("Status code: " + e.getCode());
            System.err.println("Reason: " + e.getResponseBody());
            System.err.println("Response headers: " + e.getResponseHeaders());
            e.printStackTrace();
        }
    }
}
```

### Parameters


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **enumHeaderStringArray** | [**List&lt;String&gt;**](String.md)| Header parameter enum test (string array) | [optional] [enum: >, $]
 **enumHeaderString** | **String**| Header parameter enum test (string) | [optional] [default to -efg] [enum: _abc, -efg, (xyz)]
 **enumQueryStringArray** | [**List&lt;String&gt;**](String.md)| Query parameter enum test (string array) | [optional] [enum: >, $]
 **enumQueryString** | **String**| Query parameter enum test (string) | [optional] [default to -efg] [enum: _abc, -efg, (xyz)]
 **enumQueryInteger** | **Integer**| Query parameter enum test (double) | [optional] [enum: 1, -2]
 **enumQueryDouble** | **Double**| Query parameter enum test (double) | [optional] [enum: 1.1, -1.2]
 **enumFormStringArray** | [**List&lt;String&gt;**](String.md)| Form parameter enum test (string array) | [optional] [enum: >, $]
 **enumFormString** | **String**| Form parameter enum test (string) | [optional] [default to -efg] [enum: _abc, -efg, (xyz)]

### Return type


CompletableFuture<void> (empty response body)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: application/x-www-form-urlencoded
- **Accept**: Not defined

### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **400** | Invalid request |  -  |
| **404** | Not found |  -  |

## testEnumParametersWithHttpInfo

> CompletableFuture<ApiResponse<Void>> testEnumParameters testEnumParametersWithHttpInfo(enumHeaderStringArray, enumHeaderString, enumQueryStringArray, enumQueryString, enumQueryInteger, enumQueryDouble, enumFormStringArray, enumFormString)

To test enum parameters

To test enum parameters

### Example

```java
// Import classes:
import org.openapitools.client.ApiClient;
import org.openapitools.client.ApiException;
import org.openapitools.client.ApiResponse;
import org.openapitools.client.Configuration;
import org.openapitools.client.models.*;
import org.openapitools.client.api.FakeApi;
import java.util.concurrent.CompletableFuture;

public class Example {
    public static void main(String[] args) {
        ApiClient defaultClient = Configuration.getDefaultApiClient();
        defaultClient.setBasePath("http://petstore.swagger.io:80/v2");

        FakeApi apiInstance = new FakeApi(defaultClient);
        List<String> enumHeaderStringArray = Arrays.asList("$"); // List<String> | Header parameter enum test (string array)
        String enumHeaderString = "_abc"; // String | Header parameter enum test (string)
        List<String> enumQueryStringArray = Arrays.asList("$"); // List<String> | Query parameter enum test (string array)
        String enumQueryString = "_abc"; // String | Query parameter enum test (string)
        Integer enumQueryInteger = 1; // Integer | Query parameter enum test (double)
        Double enumQueryDouble = 1.1D; // Double | Query parameter enum test (double)
        List<String> enumFormStringArray = Arrays.asList("$"); // List<String> | Form parameter enum test (string array)
        String enumFormString = "_abc"; // String | Form parameter enum test (string)
        try {
            CompletableFuture<ApiResponse<Void>> response = apiInstance.testEnumParametersWithHttpInfo(enumHeaderStringArray, enumHeaderString, enumQueryStringArray, enumQueryString, enumQueryInteger, enumQueryDouble, enumFormStringArray, enumFormString);
            System.out.println("Status code: " + response.get().getStatusCode());
            System.out.println("Response headers: " + response.get().getHeaders());
        } catch (InterruptedException | ExecutionException e) {
            ApiException apiException = (ApiException)e.getCause();
            System.err.println("Exception when calling FakeApi#testEnumParameters");
            System.err.println("Status code: " + apiException.getCode());
            System.err.println("Response headers: " + apiException.getResponseHeaders());
            System.err.println("Reason: " + apiException.getResponseBody());
            e.printStackTrace();
        } catch (ApiException e) {
            System.err.println("Exception when calling FakeApi#testEnumParameters");
            System.err.println("Status code: " + e.getCode());
            System.err.println("Response headers: " + e.getResponseHeaders());
            System.err.println("Reason: " + e.getResponseBody());
            e.printStackTrace();
        }
    }
}
```

### Parameters


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **enumHeaderStringArray** | [**List&lt;String&gt;**](String.md)| Header parameter enum test (string array) | [optional] [enum: >, $]
 **enumHeaderString** | **String**| Header parameter enum test (string) | [optional] [default to -efg] [enum: _abc, -efg, (xyz)]
 **enumQueryStringArray** | [**List&lt;String&gt;**](String.md)| Query parameter enum test (string array) | [optional] [enum: >, $]
 **enumQueryString** | **String**| Query parameter enum test (string) | [optional] [default to -efg] [enum: _abc, -efg, (xyz)]
 **enumQueryInteger** | **Integer**| Query parameter enum test (double) | [optional] [enum: 1, -2]
 **enumQueryDouble** | **Double**| Query parameter enum test (double) | [optional] [enum: 1.1, -1.2]
 **enumFormStringArray** | [**List&lt;String&gt;**](String.md)| Form parameter enum test (string array) | [optional] [enum: >, $]
 **enumFormString** | **String**| Form parameter enum test (string) | [optional] [default to -efg] [enum: _abc, -efg, (xyz)]

### Return type


CompletableFuture<ApiResponse<Void>>

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: application/x-www-form-urlencoded
- **Accept**: Not defined

### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **400** | Invalid request |  -  |
| **404** | Not found |  -  |


## testGroupParameters

> CompletableFuture<Void> testGroupParameters(testGroupParametersRequest)

Fake endpoint to test group parameters (optional)

Fake endpoint to test group parameters (optional)

### Example

```java
// Import classes:
import org.openapitools.client.ApiClient;
import org.openapitools.client.ApiException;
import org.openapitools.client.Configuration;
import org.openapitools.client.models.*;
import org.openapitools.client.api.FakeApi;
import org.openapitools.client.api.FakeApi.*;
import java.util.concurrent.CompletableFuture;

public class Example {
    public static void main(String[] args) {
        ApiClient defaultClient = Configuration.getDefaultApiClient();
        defaultClient.setBasePath("http://petstore.swagger.io:80/v2");

        FakeApi apiInstance = new FakeApi(defaultClient);
        Integer requiredStringGroup = 56; // Integer | Required String in group parameters
        Boolean requiredBooleanGroup = true; // Boolean | Required Boolean in group parameters
        Long requiredInt64Group = 56L; // Long | Required Integer in group parameters
        Integer stringGroup = 56; // Integer | String in group parameters
        Boolean booleanGroup = true; // Boolean | Boolean in group parameters
        Long int64Group = 56L; // Long | Integer in group parameters
        try {
            APItestGroupParametersRequest request = APItestGroupParametersRequest.newBuilder()
                .requiredStringGroup(requiredStringGroup)
                .requiredBooleanGroup(requiredBooleanGroup)
                .requiredInt64Group(requiredInt64Group)
                .stringGroup(stringGroup)
                .booleanGroup(booleanGroup)
                .int64Group(int64Group)
                .build();
            CompletableFuture<Void> result = apiInstance.testGroupParameters(request);
        } catch (ApiException e) {
            System.err.println("Exception when calling FakeApi#testGroupParameters");
            System.err.println("Status code: " + e.getCode());
            System.err.println("Reason: " + e.getResponseBody());
            System.err.println("Response headers: " + e.getResponseHeaders());
            e.printStackTrace();
        }
    }
}
```

### Parameters

|    Name      |    Type       | Description   |     Notes    |
|------------- | ------------- | ------------- | -------------|
| testGroupParametersRequest | [**APItestGroupParametersRequest**](FakeApi.md#APItestGroupParametersRequest)|-|-|

### Return type


CompletableFuture<void> (empty response body)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: Not defined

### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **400** | Someting wrong |  -  |

## testGroupParametersWithHttpInfo

> CompletableFuture<ApiResponse<Void>> testGroupParameters testGroupParametersWithHttpInfo(testGroupParametersRequest)

Fake endpoint to test group parameters (optional)

Fake endpoint to test group parameters (optional)

### Example

```java
// Import classes:
import org.openapitools.client.ApiClient;
import org.openapitools.client.ApiException;
import org.openapitools.client.ApiResponse;
import org.openapitools.client.Configuration;
import org.openapitools.client.models.*;
import org.openapitools.client.api.FakeApi;
import org.openapitools.client.api.FakeApi.*;
import java.util.concurrent.CompletableFuture;

public class Example {
    public static void main(String[] args) {
        ApiClient defaultClient = Configuration.getDefaultApiClient();
        defaultClient.setBasePath("http://petstore.swagger.io:80/v2");

        FakeApi apiInstance = new FakeApi(defaultClient);
        Integer requiredStringGroup = 56; // Integer | Required String in group parameters
        Boolean requiredBooleanGroup = true; // Boolean | Required Boolean in group parameters
        Long requiredInt64Group = 56L; // Long | Required Integer in group parameters
        Integer stringGroup = 56; // Integer | String in group parameters
        Boolean booleanGroup = true; // Boolean | Boolean in group parameters
        Long int64Group = 56L; // Long | Integer in group parameters
        try {
            APItestGroupParametersRequest request = APItestGroupParametersRequest.newBuilder()
                .requiredStringGroup(requiredStringGroup)
                .requiredBooleanGroup(requiredBooleanGroup)
                .requiredInt64Group(requiredInt64Group)
                .stringGroup(stringGroup)
                .booleanGroup(booleanGroup)
                .int64Group(int64Group)
                .build();
            CompletableFuture<ApiResponse<Void>> response = apiInstance.testGroupParametersWithHttpInfo(request);
            System.out.println("Status code: " + response.get().getStatusCode());
            System.out.println("Response headers: " + response.get().getHeaders());
        } catch (InterruptedException | ExecutionException e) {
            ApiException apiException = (ApiException)e.getCause();
            System.err.println("Exception when calling FakeApi#testGroupParameters");
            System.err.println("Status code: " + apiException.getCode());
            System.err.println("Response headers: " + apiException.getResponseHeaders());
            System.err.println("Reason: " + apiException.getResponseBody());
            e.printStackTrace();
        } catch (ApiException e) {
            System.err.println("Exception when calling FakeApi#testGroupParameters");
            System.err.println("Status code: " + e.getCode());
            System.err.println("Response headers: " + e.getResponseHeaders());
            System.err.println("Reason: " + e.getResponseBody());
            e.printStackTrace();
        }
    }
}
```

### Parameters

|    Name      |    Type       | Description   |     Notes    |
|------------- | ------------- | ------------- | -------------|
| testGroupParametersRequest | [**APItestGroupParametersRequest**](FakeApi.md#APItestGroupParametersRequest)|-|-|

### Return type


CompletableFuture<ApiResponse<Void>>

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: Not defined

### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **400** | Someting wrong |  -  |


<a name="APItestGroupParametersRequest"></a>
## APItestGroupParametersRequest
### Properties

|     Name      |    Type       | Description   |     Notes    |
| ------------- | ------------- | ------------- | -------------|
 **requiredStringGroup** | **Integer** | Required String in group parameters |
 **requiredBooleanGroup** | **Boolean** | Required Boolean in group parameters |
 **requiredInt64Group** | **Long** | Required Integer in group parameters |
 **stringGroup** | **Integer** | String in group parameters | [optional]
 **booleanGroup** | **Boolean** | Boolean in group parameters | [optional]
 **int64Group** | **Long** | Integer in group parameters | [optional]



## testInlineAdditionalProperties

> CompletableFuture<Void> testInlineAdditionalProperties(param)

test inline additionalProperties

### Example

```java
// Import classes:
import org.openapitools.client.ApiClient;
import org.openapitools.client.ApiException;
import org.openapitools.client.Configuration;
import org.openapitools.client.models.*;
import org.openapitools.client.api.FakeApi;
import java.util.concurrent.CompletableFuture;

public class Example {
    public static void main(String[] args) {
        ApiClient defaultClient = Configuration.getDefaultApiClient();
        defaultClient.setBasePath("http://petstore.swagger.io:80/v2");

        FakeApi apiInstance = new FakeApi(defaultClient);
        Map<String, String> param = new HashMap(); // Map<String, String> | request body
        try {
            CompletableFuture<Void> result = apiInstance.testInlineAdditionalProperties(param);
        } catch (ApiException e) {
            System.err.println("Exception when calling FakeApi#testInlineAdditionalProperties");
            System.err.println("Status code: " + e.getCode());
            System.err.println("Reason: " + e.getResponseBody());
            System.err.println("Response headers: " + e.getResponseHeaders());
            e.printStackTrace();
        }
    }
}
```

### Parameters


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **param** | [**Map&lt;String, String&gt;**](String.md)| request body |

### Return type


CompletableFuture<void> (empty response body)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: application/json
- **Accept**: Not defined

### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **200** | successful operation |  -  |

## testInlineAdditionalPropertiesWithHttpInfo

> CompletableFuture<ApiResponse<Void>> testInlineAdditionalProperties testInlineAdditionalPropertiesWithHttpInfo(param)

test inline additionalProperties

### Example

```java
// Import classes:
import org.openapitools.client.ApiClient;
import org.openapitools.client.ApiException;
import org.openapitools.client.ApiResponse;
import org.openapitools.client.Configuration;
import org.openapitools.client.models.*;
import org.openapitools.client.api.FakeApi;
import java.util.concurrent.CompletableFuture;

public class Example {
    public static void main(String[] args) {
        ApiClient defaultClient = Configuration.getDefaultApiClient();
        defaultClient.setBasePath("http://petstore.swagger.io:80/v2");

        FakeApi apiInstance = new FakeApi(defaultClient);
        Map<String, String> param = new HashMap(); // Map<String, String> | request body
        try {
            CompletableFuture<ApiResponse<Void>> response = apiInstance.testInlineAdditionalPropertiesWithHttpInfo(param);
            System.out.println("Status code: " + response.get().getStatusCode());
            System.out.println("Response headers: " + response.get().getHeaders());
        } catch (InterruptedException | ExecutionException e) {
            ApiException apiException = (ApiException)e.getCause();
            System.err.println("Exception when calling FakeApi#testInlineAdditionalProperties");
            System.err.println("Status code: " + apiException.getCode());
            System.err.println("Response headers: " + apiException.getResponseHeaders());
            System.err.println("Reason: " + apiException.getResponseBody());
            e.printStackTrace();
        } catch (ApiException e) {
            System.err.println("Exception when calling FakeApi#testInlineAdditionalProperties");
            System.err.println("Status code: " + e.getCode());
            System.err.println("Response headers: " + e.getResponseHeaders());
            System.err.println("Reason: " + e.getResponseBody());
            e.printStackTrace();
        }
    }
}
```

### Parameters


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **param** | [**Map&lt;String, String&gt;**](String.md)| request body |

### Return type


CompletableFuture<ApiResponse<Void>>

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: application/json
- **Accept**: Not defined

### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **200** | successful operation |  -  |


## testJsonFormData

> CompletableFuture<Void> testJsonFormData(param, param2)

test json serialization of form data

### Example

```java
// Import classes:
import org.openapitools.client.ApiClient;
import org.openapitools.client.ApiException;
import org.openapitools.client.Configuration;
import org.openapitools.client.models.*;
import org.openapitools.client.api.FakeApi;
import java.util.concurrent.CompletableFuture;

public class Example {
    public static void main(String[] args) {
        ApiClient defaultClient = Configuration.getDefaultApiClient();
        defaultClient.setBasePath("http://petstore.swagger.io:80/v2");

        FakeApi apiInstance = new FakeApi(defaultClient);
        String param = "param_example"; // String | field1
        String param2 = "param2_example"; // String | field2
        try {
            CompletableFuture<Void> result = apiInstance.testJsonFormData(param, param2);
        } catch (ApiException e) {
            System.err.println("Exception when calling FakeApi#testJsonFormData");
            System.err.println("Status code: " + e.getCode());
            System.err.println("Reason: " + e.getResponseBody());
            System.err.println("Response headers: " + e.getResponseHeaders());
            e.printStackTrace();
        }
    }
}
```

### Parameters


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **param** | **String**| field1 |
 **param2** | **String**| field2 |

### Return type


CompletableFuture<void> (empty response body)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: application/x-www-form-urlencoded
- **Accept**: Not defined

### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **200** | successful operation |  -  |

## testJsonFormDataWithHttpInfo

> CompletableFuture<ApiResponse<Void>> testJsonFormData testJsonFormDataWithHttpInfo(param, param2)

test json serialization of form data

### Example

```java
// Import classes:
import org.openapitools.client.ApiClient;
import org.openapitools.client.ApiException;
import org.openapitools.client.ApiResponse;
import org.openapitools.client.Configuration;
import org.openapitools.client.models.*;
import org.openapitools.client.api.FakeApi;
import java.util.concurrent.CompletableFuture;

public class Example {
    public static void main(String[] args) {
        ApiClient defaultClient = Configuration.getDefaultApiClient();
        defaultClient.setBasePath("http://petstore.swagger.io:80/v2");

        FakeApi apiInstance = new FakeApi(defaultClient);
        String param = "param_example"; // String | field1
        String param2 = "param2_example"; // String | field2
        try {
            CompletableFuture<ApiResponse<Void>> response = apiInstance.testJsonFormDataWithHttpInfo(param, param2);
            System.out.println("Status code: " + response.get().getStatusCode());
            System.out.println("Response headers: " + response.get().getHeaders());
        } catch (InterruptedException | ExecutionException e) {
            ApiException apiException = (ApiException)e.getCause();
            System.err.println("Exception when calling FakeApi#testJsonFormData");
            System.err.println("Status code: " + apiException.getCode());
            System.err.println("Response headers: " + apiException.getResponseHeaders());
            System.err.println("Reason: " + apiException.getResponseBody());
            e.printStackTrace();
        } catch (ApiException e) {
            System.err.println("Exception when calling FakeApi#testJsonFormData");
            System.err.println("Status code: " + e.getCode());
            System.err.println("Response headers: " + e.getResponseHeaders());
            System.err.println("Reason: " + e.getResponseBody());
            e.printStackTrace();
        }
    }
}
```

### Parameters


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **param** | **String**| field1 |
 **param2** | **String**| field2 |

### Return type


CompletableFuture<ApiResponse<Void>>

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: application/x-www-form-urlencoded
- **Accept**: Not defined

### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **200** | successful operation |  -  |


## testQueryParameterCollectionFormat

> CompletableFuture<Void> testQueryParameterCollectionFormat(pipe, ioutil, http, url, context)



To test the collection format in query parameters

### Example

```java
// Import classes:
import org.openapitools.client.ApiClient;
import org.openapitools.client.ApiException;
import org.openapitools.client.Configuration;
import org.openapitools.client.models.*;
import org.openapitools.client.api.FakeApi;
import java.util.concurrent.CompletableFuture;

public class Example {
    public static void main(String[] args) {
        ApiClient defaultClient = Configuration.getDefaultApiClient();
        defaultClient.setBasePath("http://petstore.swagger.io:80/v2");

        FakeApi apiInstance = new FakeApi(defaultClient);
        List<String> pipe = Arrays.asList(); // List<String> | 
        List<String> ioutil = Arrays.asList(); // List<String> | 
        List<String> http = Arrays.asList(); // List<String> | 
        List<String> url = Arrays.asList(); // List<String> | 
        List<String> context = Arrays.asList(); // List<String> | 
        try {
            CompletableFuture<Void> result = apiInstance.testQueryParameterCollectionFormat(pipe, ioutil, http, url, context);
        } catch (ApiException e) {
            System.err.println("Exception when calling FakeApi#testQueryParameterCollectionFormat");
            System.err.println("Status code: " + e.getCode());
            System.err.println("Reason: " + e.getResponseBody());
            System.err.println("Response headers: " + e.getResponseHeaders());
            e.printStackTrace();
        }
    }
}
```

### Parameters


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **pipe** | [**List&lt;String&gt;**](String.md)|  |
 **ioutil** | [**List&lt;String&gt;**](String.md)|  |
 **http** | [**List&lt;String&gt;**](String.md)|  |
 **url** | [**List&lt;String&gt;**](String.md)|  |
 **context** | [**List&lt;String&gt;**](String.md)|  |

### Return type


CompletableFuture<void> (empty response body)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: Not defined

### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **200** | Success |  -  |

## testQueryParameterCollectionFormatWithHttpInfo

> CompletableFuture<ApiResponse<Void>> testQueryParameterCollectionFormat testQueryParameterCollectionFormatWithHttpInfo(pipe, ioutil, http, url, context)



To test the collection format in query parameters

### Example

```java
// Import classes:
import org.openapitools.client.ApiClient;
import org.openapitools.client.ApiException;
import org.openapitools.client.ApiResponse;
import org.openapitools.client.Configuration;
import org.openapitools.client.models.*;
import org.openapitools.client.api.FakeApi;
import java.util.concurrent.CompletableFuture;

public class Example {
    public static void main(String[] args) {
        ApiClient defaultClient = Configuration.getDefaultApiClient();
        defaultClient.setBasePath("http://petstore.swagger.io:80/v2");

        FakeApi apiInstance = new FakeApi(defaultClient);
        List<String> pipe = Arrays.asList(); // List<String> | 
        List<String> ioutil = Arrays.asList(); // List<String> | 
        List<String> http = Arrays.asList(); // List<String> | 
        List<String> url = Arrays.asList(); // List<String> | 
        List<String> context = Arrays.asList(); // List<String> | 
        try {
            CompletableFuture<ApiResponse<Void>> response = apiInstance.testQueryParameterCollectionFormatWithHttpInfo(pipe, ioutil, http, url, context);
            System.out.println("Status code: " + response.get().getStatusCode());
            System.out.println("Response headers: " + response.get().getHeaders());
        } catch (InterruptedException | ExecutionException e) {
            ApiException apiException = (ApiException)e.getCause();
            System.err.println("Exception when calling FakeApi#testQueryParameterCollectionFormat");
            System.err.println("Status code: " + apiException.getCode());
            System.err.println("Response headers: " + apiException.getResponseHeaders());
            System.err.println("Reason: " + apiException.getResponseBody());
            e.printStackTrace();
        } catch (ApiException e) {
            System.err.println("Exception when calling FakeApi#testQueryParameterCollectionFormat");
            System.err.println("Status code: " + e.getCode());
            System.err.println("Response headers: " + e.getResponseHeaders());
            System.err.println("Reason: " + e.getResponseBody());
            e.printStackTrace();
        }
    }
}
```

### Parameters


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **pipe** | [**List&lt;String&gt;**](String.md)|  |
 **ioutil** | [**List&lt;String&gt;**](String.md)|  |
 **http** | [**List&lt;String&gt;**](String.md)|  |
 **url** | [**List&lt;String&gt;**](String.md)|  |
 **context** | [**List&lt;String&gt;**](String.md)|  |

### Return type


CompletableFuture<ApiResponse<Void>>

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: Not defined

### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **200** | Success |  -  |

