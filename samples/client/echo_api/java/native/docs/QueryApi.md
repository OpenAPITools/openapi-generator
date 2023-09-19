# QueryApi

All URIs are relative to *http://localhost:3000*

| Method | HTTP request | Description |
|------------- | ------------- | -------------|
| [**testEnumRefString**](QueryApi.md#testEnumRefString) | **GET** /query/enum_ref_string | Test query parameter(s) |
| [**testEnumRefStringWithHttpInfo**](QueryApi.md#testEnumRefStringWithHttpInfo) | **GET** /query/enum_ref_string | Test query parameter(s) |
| [**testQueryDatetimeDateString**](QueryApi.md#testQueryDatetimeDateString) | **GET** /query/datetime/date/string | Test query parameter(s) |
| [**testQueryDatetimeDateStringWithHttpInfo**](QueryApi.md#testQueryDatetimeDateStringWithHttpInfo) | **GET** /query/datetime/date/string | Test query parameter(s) |
| [**testQueryIntegerBooleanString**](QueryApi.md#testQueryIntegerBooleanString) | **GET** /query/integer/boolean/string | Test query parameter(s) |
| [**testQueryIntegerBooleanStringWithHttpInfo**](QueryApi.md#testQueryIntegerBooleanStringWithHttpInfo) | **GET** /query/integer/boolean/string | Test query parameter(s) |
| [**testQueryStyleDeepObjectExplodeTrueObject**](QueryApi.md#testQueryStyleDeepObjectExplodeTrueObject) | **GET** /query/style_deepObject/explode_true/object | Test query parameter(s) |
| [**testQueryStyleDeepObjectExplodeTrueObjectWithHttpInfo**](QueryApi.md#testQueryStyleDeepObjectExplodeTrueObjectWithHttpInfo) | **GET** /query/style_deepObject/explode_true/object | Test query parameter(s) |
| [**testQueryStyleDeepObjectExplodeTrueObjectAllOf**](QueryApi.md#testQueryStyleDeepObjectExplodeTrueObjectAllOf) | **GET** /query/style_deepObject/explode_true/object/allOf | Test query parameter(s) |
| [**testQueryStyleDeepObjectExplodeTrueObjectAllOfWithHttpInfo**](QueryApi.md#testQueryStyleDeepObjectExplodeTrueObjectAllOfWithHttpInfo) | **GET** /query/style_deepObject/explode_true/object/allOf | Test query parameter(s) |
| [**testQueryStyleFormExplodeTrueArrayString**](QueryApi.md#testQueryStyleFormExplodeTrueArrayString) | **GET** /query/style_form/explode_true/array_string | Test query parameter(s) |
| [**testQueryStyleFormExplodeTrueArrayStringWithHttpInfo**](QueryApi.md#testQueryStyleFormExplodeTrueArrayStringWithHttpInfo) | **GET** /query/style_form/explode_true/array_string | Test query parameter(s) |
| [**testQueryStyleFormExplodeTrueObject**](QueryApi.md#testQueryStyleFormExplodeTrueObject) | **GET** /query/style_form/explode_true/object | Test query parameter(s) |
| [**testQueryStyleFormExplodeTrueObjectWithHttpInfo**](QueryApi.md#testQueryStyleFormExplodeTrueObjectWithHttpInfo) | **GET** /query/style_form/explode_true/object | Test query parameter(s) |
| [**testQueryStyleFormExplodeTrueObjectAllOf**](QueryApi.md#testQueryStyleFormExplodeTrueObjectAllOf) | **GET** /query/style_form/explode_true/object/allOf | Test query parameter(s) |
| [**testQueryStyleFormExplodeTrueObjectAllOfWithHttpInfo**](QueryApi.md#testQueryStyleFormExplodeTrueObjectAllOfWithHttpInfo) | **GET** /query/style_form/explode_true/object/allOf | Test query parameter(s) |



## testEnumRefString

> String testEnumRefString(enumRefStringQuery)

Test query parameter(s)

Test query parameter(s)

### Example

```java
// Import classes:
import org.openapitools.client.ApiClient;
import org.openapitools.client.ApiException;
import org.openapitools.client.Configuration;
import org.openapitools.client.models.*;
import org.openapitools.client.api.QueryApi;

public class Example {
    public static void main(String[] args) {
        ApiClient defaultClient = Configuration.getDefaultApiClient();
        defaultClient.setBasePath("http://localhost:3000");

        QueryApi apiInstance = new QueryApi(defaultClient);
        StringEnumRef enumRefStringQuery = StringEnumRef.fromValue("success"); // StringEnumRef | 
        try {
            String result = apiInstance.testEnumRefString(enumRefStringQuery);
            System.out.println(result);
        } catch (ApiException e) {
            System.err.println("Exception when calling QueryApi#testEnumRefString");
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
| **enumRefStringQuery** | [**StringEnumRef**](.md)|  | [optional] [enum: success, failure, unclassified] |

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
| **200** | Successful operation |  -  |

## testEnumRefStringWithHttpInfo

> ApiResponse<String> testEnumRefString testEnumRefStringWithHttpInfo(enumRefStringQuery)

Test query parameter(s)

Test query parameter(s)

### Example

```java
// Import classes:
import org.openapitools.client.ApiClient;
import org.openapitools.client.ApiException;
import org.openapitools.client.ApiResponse;
import org.openapitools.client.Configuration;
import org.openapitools.client.models.*;
import org.openapitools.client.api.QueryApi;

public class Example {
    public static void main(String[] args) {
        ApiClient defaultClient = Configuration.getDefaultApiClient();
        defaultClient.setBasePath("http://localhost:3000");

        QueryApi apiInstance = new QueryApi(defaultClient);
        StringEnumRef enumRefStringQuery = StringEnumRef.fromValue("success"); // StringEnumRef | 
        try {
            ApiResponse<String> response = apiInstance.testEnumRefStringWithHttpInfo(enumRefStringQuery);
            System.out.println("Status code: " + response.getStatusCode());
            System.out.println("Response headers: " + response.getHeaders());
            System.out.println("Response body: " + response.getData());
        } catch (ApiException e) {
            System.err.println("Exception when calling QueryApi#testEnumRefString");
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
| **enumRefStringQuery** | [**StringEnumRef**](.md)|  | [optional] [enum: success, failure, unclassified] |

### Return type

ApiResponse<**String**>


### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: text/plain

### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **200** | Successful operation |  -  |


## testQueryDatetimeDateString

> String testQueryDatetimeDateString(datetimeQuery, dateQuery, stringQuery)

Test query parameter(s)

Test query parameter(s)

### Example

```java
// Import classes:
import org.openapitools.client.ApiClient;
import org.openapitools.client.ApiException;
import org.openapitools.client.Configuration;
import org.openapitools.client.models.*;
import org.openapitools.client.api.QueryApi;

public class Example {
    public static void main(String[] args) {
        ApiClient defaultClient = Configuration.getDefaultApiClient();
        defaultClient.setBasePath("http://localhost:3000");

        QueryApi apiInstance = new QueryApi(defaultClient);
        Instant datetimeQuery = new Instant(); // Instant | 
        LocalDate dateQuery = LocalDate.now(); // LocalDate | 
        String stringQuery = "stringQuery_example"; // String | 
        try {
            String result = apiInstance.testQueryDatetimeDateString(datetimeQuery, dateQuery, stringQuery);
            System.out.println(result);
        } catch (ApiException e) {
            System.err.println("Exception when calling QueryApi#testQueryDatetimeDateString");
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
| **datetimeQuery** | **Instant**|  | [optional] |
| **dateQuery** | **LocalDate**|  | [optional] |
| **stringQuery** | **String**|  | [optional] |

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
| **200** | Successful operation |  -  |

## testQueryDatetimeDateStringWithHttpInfo

> ApiResponse<String> testQueryDatetimeDateString testQueryDatetimeDateStringWithHttpInfo(datetimeQuery, dateQuery, stringQuery)

Test query parameter(s)

Test query parameter(s)

### Example

```java
// Import classes:
import org.openapitools.client.ApiClient;
import org.openapitools.client.ApiException;
import org.openapitools.client.ApiResponse;
import org.openapitools.client.Configuration;
import org.openapitools.client.models.*;
import org.openapitools.client.api.QueryApi;

public class Example {
    public static void main(String[] args) {
        ApiClient defaultClient = Configuration.getDefaultApiClient();
        defaultClient.setBasePath("http://localhost:3000");

        QueryApi apiInstance = new QueryApi(defaultClient);
        Instant datetimeQuery = new Instant(); // Instant | 
        LocalDate dateQuery = LocalDate.now(); // LocalDate | 
        String stringQuery = "stringQuery_example"; // String | 
        try {
            ApiResponse<String> response = apiInstance.testQueryDatetimeDateStringWithHttpInfo(datetimeQuery, dateQuery, stringQuery);
            System.out.println("Status code: " + response.getStatusCode());
            System.out.println("Response headers: " + response.getHeaders());
            System.out.println("Response body: " + response.getData());
        } catch (ApiException e) {
            System.err.println("Exception when calling QueryApi#testQueryDatetimeDateString");
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
| **datetimeQuery** | **Instant**|  | [optional] |
| **dateQuery** | **LocalDate**|  | [optional] |
| **stringQuery** | **String**|  | [optional] |

### Return type

ApiResponse<**String**>


### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: text/plain

### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **200** | Successful operation |  -  |


## testQueryIntegerBooleanString

> String testQueryIntegerBooleanString(integerQuery, booleanQuery, stringQuery)

Test query parameter(s)

Test query parameter(s)

### Example

```java
// Import classes:
import org.openapitools.client.ApiClient;
import org.openapitools.client.ApiException;
import org.openapitools.client.Configuration;
import org.openapitools.client.models.*;
import org.openapitools.client.api.QueryApi;

public class Example {
    public static void main(String[] args) {
        ApiClient defaultClient = Configuration.getDefaultApiClient();
        defaultClient.setBasePath("http://localhost:3000");

        QueryApi apiInstance = new QueryApi(defaultClient);
        Integer integerQuery = 56; // Integer | 
        Boolean booleanQuery = true; // Boolean | 
        String stringQuery = "stringQuery_example"; // String | 
        try {
            String result = apiInstance.testQueryIntegerBooleanString(integerQuery, booleanQuery, stringQuery);
            System.out.println(result);
        } catch (ApiException e) {
            System.err.println("Exception when calling QueryApi#testQueryIntegerBooleanString");
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
| **integerQuery** | **Integer**|  | [optional] |
| **booleanQuery** | **Boolean**|  | [optional] |
| **stringQuery** | **String**|  | [optional] |

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
| **200** | Successful operation |  -  |

## testQueryIntegerBooleanStringWithHttpInfo

> ApiResponse<String> testQueryIntegerBooleanString testQueryIntegerBooleanStringWithHttpInfo(integerQuery, booleanQuery, stringQuery)

Test query parameter(s)

Test query parameter(s)

### Example

```java
// Import classes:
import org.openapitools.client.ApiClient;
import org.openapitools.client.ApiException;
import org.openapitools.client.ApiResponse;
import org.openapitools.client.Configuration;
import org.openapitools.client.models.*;
import org.openapitools.client.api.QueryApi;

public class Example {
    public static void main(String[] args) {
        ApiClient defaultClient = Configuration.getDefaultApiClient();
        defaultClient.setBasePath("http://localhost:3000");

        QueryApi apiInstance = new QueryApi(defaultClient);
        Integer integerQuery = 56; // Integer | 
        Boolean booleanQuery = true; // Boolean | 
        String stringQuery = "stringQuery_example"; // String | 
        try {
            ApiResponse<String> response = apiInstance.testQueryIntegerBooleanStringWithHttpInfo(integerQuery, booleanQuery, stringQuery);
            System.out.println("Status code: " + response.getStatusCode());
            System.out.println("Response headers: " + response.getHeaders());
            System.out.println("Response body: " + response.getData());
        } catch (ApiException e) {
            System.err.println("Exception when calling QueryApi#testQueryIntegerBooleanString");
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
| **integerQuery** | **Integer**|  | [optional] |
| **booleanQuery** | **Boolean**|  | [optional] |
| **stringQuery** | **String**|  | [optional] |

### Return type

ApiResponse<**String**>


### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: text/plain

### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **200** | Successful operation |  -  |


## testQueryStyleDeepObjectExplodeTrueObject

> String testQueryStyleDeepObjectExplodeTrueObject(queryObject)

Test query parameter(s)

Test query parameter(s)

### Example

```java
// Import classes:
import org.openapitools.client.ApiClient;
import org.openapitools.client.ApiException;
import org.openapitools.client.Configuration;
import org.openapitools.client.models.*;
import org.openapitools.client.api.QueryApi;

public class Example {
    public static void main(String[] args) {
        ApiClient defaultClient = Configuration.getDefaultApiClient();
        defaultClient.setBasePath("http://localhost:3000");

        QueryApi apiInstance = new QueryApi(defaultClient);
        Pet queryObject = new Pet(); // Pet | 
        try {
            String result = apiInstance.testQueryStyleDeepObjectExplodeTrueObject(queryObject);
            System.out.println(result);
        } catch (ApiException e) {
            System.err.println("Exception when calling QueryApi#testQueryStyleDeepObjectExplodeTrueObject");
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
| **queryObject** | [**Pet**](.md)|  | [optional] |

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
| **200** | Successful operation |  -  |

## testQueryStyleDeepObjectExplodeTrueObjectWithHttpInfo

> ApiResponse<String> testQueryStyleDeepObjectExplodeTrueObject testQueryStyleDeepObjectExplodeTrueObjectWithHttpInfo(queryObject)

Test query parameter(s)

Test query parameter(s)

### Example

```java
// Import classes:
import org.openapitools.client.ApiClient;
import org.openapitools.client.ApiException;
import org.openapitools.client.ApiResponse;
import org.openapitools.client.Configuration;
import org.openapitools.client.models.*;
import org.openapitools.client.api.QueryApi;

public class Example {
    public static void main(String[] args) {
        ApiClient defaultClient = Configuration.getDefaultApiClient();
        defaultClient.setBasePath("http://localhost:3000");

        QueryApi apiInstance = new QueryApi(defaultClient);
        Pet queryObject = new Pet(); // Pet | 
        try {
            ApiResponse<String> response = apiInstance.testQueryStyleDeepObjectExplodeTrueObjectWithHttpInfo(queryObject);
            System.out.println("Status code: " + response.getStatusCode());
            System.out.println("Response headers: " + response.getHeaders());
            System.out.println("Response body: " + response.getData());
        } catch (ApiException e) {
            System.err.println("Exception when calling QueryApi#testQueryStyleDeepObjectExplodeTrueObject");
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
| **queryObject** | [**Pet**](.md)|  | [optional] |

### Return type

ApiResponse<**String**>


### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: text/plain

### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **200** | Successful operation |  -  |


## testQueryStyleDeepObjectExplodeTrueObjectAllOf

> String testQueryStyleDeepObjectExplodeTrueObjectAllOf(queryObject)

Test query parameter(s)

Test query parameter(s)

### Example

```java
// Import classes:
import org.openapitools.client.ApiClient;
import org.openapitools.client.ApiException;
import org.openapitools.client.Configuration;
import org.openapitools.client.models.*;
import org.openapitools.client.api.QueryApi;

public class Example {
    public static void main(String[] args) {
        ApiClient defaultClient = Configuration.getDefaultApiClient();
        defaultClient.setBasePath("http://localhost:3000");

        QueryApi apiInstance = new QueryApi(defaultClient);
        TestQueryStyleDeepObjectExplodeTrueObjectAllOfQueryObjectParameter queryObject = new TestQueryStyleDeepObjectExplodeTrueObjectAllOfQueryObjectParameter(); // TestQueryStyleDeepObjectExplodeTrueObjectAllOfQueryObjectParameter | 
        try {
            String result = apiInstance.testQueryStyleDeepObjectExplodeTrueObjectAllOf(queryObject);
            System.out.println(result);
        } catch (ApiException e) {
            System.err.println("Exception when calling QueryApi#testQueryStyleDeepObjectExplodeTrueObjectAllOf");
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
| **queryObject** | [**TestQueryStyleDeepObjectExplodeTrueObjectAllOfQueryObjectParameter**](.md)|  | [optional] |

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
| **200** | Successful operation |  -  |

## testQueryStyleDeepObjectExplodeTrueObjectAllOfWithHttpInfo

> ApiResponse<String> testQueryStyleDeepObjectExplodeTrueObjectAllOf testQueryStyleDeepObjectExplodeTrueObjectAllOfWithHttpInfo(queryObject)

Test query parameter(s)

Test query parameter(s)

### Example

```java
// Import classes:
import org.openapitools.client.ApiClient;
import org.openapitools.client.ApiException;
import org.openapitools.client.ApiResponse;
import org.openapitools.client.Configuration;
import org.openapitools.client.models.*;
import org.openapitools.client.api.QueryApi;

public class Example {
    public static void main(String[] args) {
        ApiClient defaultClient = Configuration.getDefaultApiClient();
        defaultClient.setBasePath("http://localhost:3000");

        QueryApi apiInstance = new QueryApi(defaultClient);
        TestQueryStyleDeepObjectExplodeTrueObjectAllOfQueryObjectParameter queryObject = new TestQueryStyleDeepObjectExplodeTrueObjectAllOfQueryObjectParameter(); // TestQueryStyleDeepObjectExplodeTrueObjectAllOfQueryObjectParameter | 
        try {
            ApiResponse<String> response = apiInstance.testQueryStyleDeepObjectExplodeTrueObjectAllOfWithHttpInfo(queryObject);
            System.out.println("Status code: " + response.getStatusCode());
            System.out.println("Response headers: " + response.getHeaders());
            System.out.println("Response body: " + response.getData());
        } catch (ApiException e) {
            System.err.println("Exception when calling QueryApi#testQueryStyleDeepObjectExplodeTrueObjectAllOf");
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
| **queryObject** | [**TestQueryStyleDeepObjectExplodeTrueObjectAllOfQueryObjectParameter**](.md)|  | [optional] |

### Return type

ApiResponse<**String**>


### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: text/plain

### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **200** | Successful operation |  -  |


## testQueryStyleFormExplodeTrueArrayString

> String testQueryStyleFormExplodeTrueArrayString(queryObject)

Test query parameter(s)

Test query parameter(s)

### Example

```java
// Import classes:
import org.openapitools.client.ApiClient;
import org.openapitools.client.ApiException;
import org.openapitools.client.Configuration;
import org.openapitools.client.models.*;
import org.openapitools.client.api.QueryApi;

public class Example {
    public static void main(String[] args) {
        ApiClient defaultClient = Configuration.getDefaultApiClient();
        defaultClient.setBasePath("http://localhost:3000");

        QueryApi apiInstance = new QueryApi(defaultClient);
        TestQueryStyleFormExplodeTrueArrayStringQueryObjectParameter queryObject = new TestQueryStyleFormExplodeTrueArrayStringQueryObjectParameter(); // TestQueryStyleFormExplodeTrueArrayStringQueryObjectParameter | 
        try {
            String result = apiInstance.testQueryStyleFormExplodeTrueArrayString(queryObject);
            System.out.println(result);
        } catch (ApiException e) {
            System.err.println("Exception when calling QueryApi#testQueryStyleFormExplodeTrueArrayString");
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
| **queryObject** | [**TestQueryStyleFormExplodeTrueArrayStringQueryObjectParameter**](.md)|  | [optional] |

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
| **200** | Successful operation |  -  |

## testQueryStyleFormExplodeTrueArrayStringWithHttpInfo

> ApiResponse<String> testQueryStyleFormExplodeTrueArrayString testQueryStyleFormExplodeTrueArrayStringWithHttpInfo(queryObject)

Test query parameter(s)

Test query parameter(s)

### Example

```java
// Import classes:
import org.openapitools.client.ApiClient;
import org.openapitools.client.ApiException;
import org.openapitools.client.ApiResponse;
import org.openapitools.client.Configuration;
import org.openapitools.client.models.*;
import org.openapitools.client.api.QueryApi;

public class Example {
    public static void main(String[] args) {
        ApiClient defaultClient = Configuration.getDefaultApiClient();
        defaultClient.setBasePath("http://localhost:3000");

        QueryApi apiInstance = new QueryApi(defaultClient);
        TestQueryStyleFormExplodeTrueArrayStringQueryObjectParameter queryObject = new TestQueryStyleFormExplodeTrueArrayStringQueryObjectParameter(); // TestQueryStyleFormExplodeTrueArrayStringQueryObjectParameter | 
        try {
            ApiResponse<String> response = apiInstance.testQueryStyleFormExplodeTrueArrayStringWithHttpInfo(queryObject);
            System.out.println("Status code: " + response.getStatusCode());
            System.out.println("Response headers: " + response.getHeaders());
            System.out.println("Response body: " + response.getData());
        } catch (ApiException e) {
            System.err.println("Exception when calling QueryApi#testQueryStyleFormExplodeTrueArrayString");
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
| **queryObject** | [**TestQueryStyleFormExplodeTrueArrayStringQueryObjectParameter**](.md)|  | [optional] |

### Return type

ApiResponse<**String**>


### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: text/plain

### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **200** | Successful operation |  -  |


## testQueryStyleFormExplodeTrueObject

> String testQueryStyleFormExplodeTrueObject(queryObject)

Test query parameter(s)

Test query parameter(s)

### Example

```java
// Import classes:
import org.openapitools.client.ApiClient;
import org.openapitools.client.ApiException;
import org.openapitools.client.Configuration;
import org.openapitools.client.models.*;
import org.openapitools.client.api.QueryApi;

public class Example {
    public static void main(String[] args) {
        ApiClient defaultClient = Configuration.getDefaultApiClient();
        defaultClient.setBasePath("http://localhost:3000");

        QueryApi apiInstance = new QueryApi(defaultClient);
        Pet queryObject = new Pet(); // Pet | 
        try {
            String result = apiInstance.testQueryStyleFormExplodeTrueObject(queryObject);
            System.out.println(result);
        } catch (ApiException e) {
            System.err.println("Exception when calling QueryApi#testQueryStyleFormExplodeTrueObject");
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
| **queryObject** | [**Pet**](.md)|  | [optional] |

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
| **200** | Successful operation |  -  |

## testQueryStyleFormExplodeTrueObjectWithHttpInfo

> ApiResponse<String> testQueryStyleFormExplodeTrueObject testQueryStyleFormExplodeTrueObjectWithHttpInfo(queryObject)

Test query parameter(s)

Test query parameter(s)

### Example

```java
// Import classes:
import org.openapitools.client.ApiClient;
import org.openapitools.client.ApiException;
import org.openapitools.client.ApiResponse;
import org.openapitools.client.Configuration;
import org.openapitools.client.models.*;
import org.openapitools.client.api.QueryApi;

public class Example {
    public static void main(String[] args) {
        ApiClient defaultClient = Configuration.getDefaultApiClient();
        defaultClient.setBasePath("http://localhost:3000");

        QueryApi apiInstance = new QueryApi(defaultClient);
        Pet queryObject = new Pet(); // Pet | 
        try {
            ApiResponse<String> response = apiInstance.testQueryStyleFormExplodeTrueObjectWithHttpInfo(queryObject);
            System.out.println("Status code: " + response.getStatusCode());
            System.out.println("Response headers: " + response.getHeaders());
            System.out.println("Response body: " + response.getData());
        } catch (ApiException e) {
            System.err.println("Exception when calling QueryApi#testQueryStyleFormExplodeTrueObject");
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
| **queryObject** | [**Pet**](.md)|  | [optional] |

### Return type

ApiResponse<**String**>


### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: text/plain

### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **200** | Successful operation |  -  |


## testQueryStyleFormExplodeTrueObjectAllOf

> String testQueryStyleFormExplodeTrueObjectAllOf(queryObject)

Test query parameter(s)

Test query parameter(s)

### Example

```java
// Import classes:
import org.openapitools.client.ApiClient;
import org.openapitools.client.ApiException;
import org.openapitools.client.Configuration;
import org.openapitools.client.models.*;
import org.openapitools.client.api.QueryApi;

public class Example {
    public static void main(String[] args) {
        ApiClient defaultClient = Configuration.getDefaultApiClient();
        defaultClient.setBasePath("http://localhost:3000");

        QueryApi apiInstance = new QueryApi(defaultClient);
        DataQuery queryObject = new DataQuery(); // DataQuery | 
        try {
            String result = apiInstance.testQueryStyleFormExplodeTrueObjectAllOf(queryObject);
            System.out.println(result);
        } catch (ApiException e) {
            System.err.println("Exception when calling QueryApi#testQueryStyleFormExplodeTrueObjectAllOf");
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
| **queryObject** | [**DataQuery**](.md)|  | [optional] |

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
| **200** | Successful operation |  -  |

## testQueryStyleFormExplodeTrueObjectAllOfWithHttpInfo

> ApiResponse<String> testQueryStyleFormExplodeTrueObjectAllOf testQueryStyleFormExplodeTrueObjectAllOfWithHttpInfo(queryObject)

Test query parameter(s)

Test query parameter(s)

### Example

```java
// Import classes:
import org.openapitools.client.ApiClient;
import org.openapitools.client.ApiException;
import org.openapitools.client.ApiResponse;
import org.openapitools.client.Configuration;
import org.openapitools.client.models.*;
import org.openapitools.client.api.QueryApi;

public class Example {
    public static void main(String[] args) {
        ApiClient defaultClient = Configuration.getDefaultApiClient();
        defaultClient.setBasePath("http://localhost:3000");

        QueryApi apiInstance = new QueryApi(defaultClient);
        DataQuery queryObject = new DataQuery(); // DataQuery | 
        try {
            ApiResponse<String> response = apiInstance.testQueryStyleFormExplodeTrueObjectAllOfWithHttpInfo(queryObject);
            System.out.println("Status code: " + response.getStatusCode());
            System.out.println("Response headers: " + response.getHeaders());
            System.out.println("Response body: " + response.getData());
        } catch (ApiException e) {
            System.err.println("Exception when calling QueryApi#testQueryStyleFormExplodeTrueObjectAllOf");
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
| **queryObject** | [**DataQuery**](.md)|  | [optional] |

### Return type

ApiResponse<**String**>


### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: text/plain

### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **200** | Successful operation |  -  |

