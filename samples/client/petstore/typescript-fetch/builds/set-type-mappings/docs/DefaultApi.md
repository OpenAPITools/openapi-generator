# DefaultApi

All URIs are relative to *http://localhost*

| Method | HTTP request | Description |
|------------- | ------------- | -------------|
| [**uniqueCallback**](DefaultApi.md#uniquecallback) | **POST** /unique-callback |  |
| [**uniqueCookieParam**](DefaultApi.md#uniquecookieparam) | **GET** /unique-cookie-param |  |
| [**uniqueHeaderParam**](DefaultApi.md#uniqueheaderparam) | **GET** /unique-header-param |  |
| [**uniquePathParam**](DefaultApi.md#uniquepathparam) | **GET** /unique-path-param/{values} |  |
| [**uniqueQueryParams**](DefaultApi.md#uniquequeryparams) | **GET** /unique-query-params |  |
| [**uniqueRequestBody**](DefaultApi.md#uniquerequestbody) | **POST** /unique-request-body |  |
| [**uniqueResponseBody**](DefaultApi.md#uniqueresponsebody) | **GET** /unique-response-body |  |



## uniqueCallback

> uniqueCallback()



### Example

```ts
import {
  Configuration,
  DefaultApi,
} from '';
import type { UniqueCallbackRequest } from '';

async function example() {
  console.log("🚀 Testing  SDK...");
  const api = new DefaultApi();

  try {
    const data = await api.uniqueCallback();
    console.log(data);
  } catch (error) {
    console.error(error);
  }
}

// Run the test
example().catch(console.error);
```

### Parameters

This endpoint does not need any parameter.

### Return type

`void` (Empty response body)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: Not defined


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **200** | OK |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#api-endpoints) [[Back to Model list]](../README.md#models) [[Back to README]](../README.md)


## uniqueCookieParam

> uniqueCookieParam(uniqueCookie)



### Example

```ts
import {
  Configuration,
  DefaultApi,
} from '';
import type { UniqueCookieParamRequest } from '';

async function example() {
  console.log("🚀 Testing  SDK...");
  const api = new DefaultApi();

  const body = {
    // Array<string> (optional)
    uniqueCookie: ...,
  } satisfies UniqueCookieParamRequest;

  try {
    const data = await api.uniqueCookieParam(body);
    console.log(data);
  } catch (error) {
    console.error(error);
  }
}

// Run the test
example().catch(console.error);
```

### Parameters


| Name | Type | Description  | Notes |
|------------- | ------------- | ------------- | -------------|
| **uniqueCookie** | `Array<string>` |  | [Optional] |

### Return type

`void` (Empty response body)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: Not defined


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **200** | OK |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#api-endpoints) [[Back to Model list]](../README.md#models) [[Back to README]](../README.md)


## uniqueHeaderParam

> uniqueHeaderParam(xUniqueHeader)



### Example

```ts
import {
  Configuration,
  DefaultApi,
} from '';
import type { UniqueHeaderParamRequest } from '';

async function example() {
  console.log("🚀 Testing  SDK...");
  const api = new DefaultApi();

  const body = {
    // Array<string> (optional)
    xUniqueHeader: ...,
  } satisfies UniqueHeaderParamRequest;

  try {
    const data = await api.uniqueHeaderParam(body);
    console.log(data);
  } catch (error) {
    console.error(error);
  }
}

// Run the test
example().catch(console.error);
```

### Parameters


| Name | Type | Description  | Notes |
|------------- | ------------- | ------------- | -------------|
| **xUniqueHeader** | `Array<string>` |  | [Optional] |

### Return type

`void` (Empty response body)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: Not defined


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **200** | OK |  * X-Unique-Response-Header -  <br>  |

[[Back to top]](#) [[Back to API list]](../README.md#api-endpoints) [[Back to Model list]](../README.md#models) [[Back to README]](../README.md)


## uniquePathParam

> uniquePathParam(values)



### Example

```ts
import {
  Configuration,
  DefaultApi,
} from '';
import type { UniquePathParamRequest } from '';

async function example() {
  console.log("🚀 Testing  SDK...");
  const api = new DefaultApi();

  const body = {
    // Array<string>
    values: ...,
  } satisfies UniquePathParamRequest;

  try {
    const data = await api.uniquePathParam(body);
    console.log(data);
  } catch (error) {
    console.error(error);
  }
}

// Run the test
example().catch(console.error);
```

### Parameters


| Name | Type | Description  | Notes |
|------------- | ------------- | ------------- | -------------|
| **values** | `Array<string>` |  | |

### Return type

`void` (Empty response body)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: Not defined


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **200** | OK |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#api-endpoints) [[Back to Model list]](../README.md#models) [[Back to README]](../README.md)


## uniqueQueryParams

> uniqueQueryParams(uniqueStrings, uniqueIntegers, uniqueNumbers, uniqueBooleans, uniqueObjects, uniqueArrays, uniqueRefs)



### Example

```ts
import {
  Configuration,
  DefaultApi,
} from '';
import type { UniqueQueryParamsRequest } from '';

async function example() {
  console.log("🚀 Testing  SDK...");
  const api = new DefaultApi();

  const body = {
    // Array<string> (optional)
    uniqueStrings: ...,
    // Array<number> (optional)
    uniqueIntegers: ...,
    // Array<number> (optional)
    uniqueNumbers: ...,
    // Array<boolean> (optional)
    uniqueBooleans: ...,
    // Array<object> (optional)
    uniqueObjects: ...,
    // Array<Array<string>> (optional)
    uniqueArrays: ...,
    // Array<Tag> (optional)
    uniqueRefs: ...,
  } satisfies UniqueQueryParamsRequest;

  try {
    const data = await api.uniqueQueryParams(body);
    console.log(data);
  } catch (error) {
    console.error(error);
  }
}

// Run the test
example().catch(console.error);
```

### Parameters


| Name | Type | Description  | Notes |
|------------- | ------------- | ------------- | -------------|
| **uniqueStrings** | `Array<string>` |  | [Optional] |
| **uniqueIntegers** | `Array<number>` |  | [Optional] |
| **uniqueNumbers** | `Array<number>` |  | [Optional] |
| **uniqueBooleans** | `Array<boolean>` |  | [Optional] |
| **uniqueObjects** | `Array<object>` |  | [Optional] |
| **uniqueArrays** | `Array<Array<string>>` |  | [Optional] |
| **uniqueRefs** | `Array<Tag>` |  | [Optional] |

### Return type

`void` (Empty response body)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: Not defined


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **200** | OK |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#api-endpoints) [[Back to Model list]](../README.md#models) [[Back to README]](../README.md)


## uniqueRequestBody

> uniqueRequestBody(tag)



### Example

```ts
import {
  Configuration,
  DefaultApi,
} from '';
import type { UniqueRequestBodyRequest } from '';

async function example() {
  console.log("🚀 Testing  SDK...");
  const api = new DefaultApi();

  const body = {
    // Array<Tag> | Request body that is an array with uniqueItems
    tag: ...,
  } satisfies UniqueRequestBodyRequest;

  try {
    const data = await api.uniqueRequestBody(body);
    console.log(data);
  } catch (error) {
    console.error(error);
  }
}

// Run the test
example().catch(console.error);
```

### Parameters


| Name | Type | Description  | Notes |
|------------- | ------------- | ------------- | -------------|
| **tag** | `Array<Tag>` | Request body that is an array with uniqueItems | |

### Return type

`void` (Empty response body)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: `application/json`, `application/xml`
- **Accept**: Not defined


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **200** | OK |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#api-endpoints) [[Back to Model list]](../README.md#models) [[Back to README]](../README.md)


## uniqueResponseBody

> Array&lt;Tag&gt; uniqueResponseBody()



### Example

```ts
import {
  Configuration,
  DefaultApi,
} from '';
import type { UniqueResponseBodyRequest } from '';

async function example() {
  console.log("🚀 Testing  SDK...");
  const api = new DefaultApi();

  try {
    const data = await api.uniqueResponseBody();
    console.log(data);
  } catch (error) {
    console.error(error);
  }
}

// Run the test
example().catch(console.error);
```

### Parameters

This endpoint does not need any parameter.

### Return type

[**Array&lt;Tag&gt;**](Tag.md)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: `application/json`, `application/xml`


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **200** | Response body that is an array with uniqueItems |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#api-endpoints) [[Back to Model list]](../README.md#models) [[Back to README]](../README.md)

