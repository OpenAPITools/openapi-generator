# DefaultApi

All URIs are relative to *http://localhost:3000*

| Method | HTTP request | Description |
|------------- | ------------- | -------------|
| [**fakeEnumRequestGetInline**](DefaultApi.md#fakeenumrequestgetinline) | **GET** /fake/enum-request-inline |  |
| [**fakeEnumRequestGetRef**](DefaultApi.md#fakeenumrequestgetref) | **GET** /fake/enum-request-ref |  |
| [**fakeEnumRequestPostInline**](DefaultApi.md#fakeenumrequestpostinline) | **POST** /fake/enum-request-inline |  |
| [**fakeEnumRequestPostRef**](DefaultApi.md#fakeenumrequestpostref) | **POST** /fake/enum-request-ref |  |



## fakeEnumRequestGetInline

> FakeEnumRequestGetInline200Response fakeEnumRequestGetInline(stringEnum, nullableStringEnum, numberEnum, nullableNumberEnum)



### Example

```ts
import {
  Configuration,
  DefaultApi,
} from '';
import type { FakeEnumRequestGetInlineRequest } from '';

async function example() {
  console.log("🚀 Testing  SDK...");
  const api = new DefaultApi();

  const body = {
    // 'one' | 'two' | 'three' (optional)
    stringEnum: stringEnum_example,
    // string (optional)
    nullableStringEnum: ...,
    // 1 | 2 | 3 (optional)
    numberEnum: 8.14,
    // number (optional)
    nullableNumberEnum: ...,
  } satisfies FakeEnumRequestGetInlineRequest;

  try {
    const data = await api.fakeEnumRequestGetInline(body);
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
| **stringEnum** | `one`, `two`, `three` |  | [Optional] [Defaults to `undefined`] [Enum: one, two, three] |
| **nullableStringEnum** | `one`, `two`, `three` |  | [Optional] [Defaults to `undefined`] [Enum: one, two, three] |
| **numberEnum** | `1`, `2`, `3` |  | [Optional] [Defaults to `undefined`] [Enum: 1, 2, 3] |
| **nullableNumberEnum** | `1`, `2`, `3` |  | [Optional] [Defaults to `undefined`] [Enum: 1, 2, 3] |

### Return type

[**FakeEnumRequestGetInline200Response**](FakeEnumRequestGetInline200Response.md)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: `application/json`


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **200** | OK |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#api-endpoints) [[Back to Model list]](../README.md#models) [[Back to README]](../README.md)


## fakeEnumRequestGetRef

> EnumPatternObject fakeEnumRequestGetRef(stringEnum, nullableStringEnum, numberEnum, nullableNumberEnum)



### Example

```ts
import {
  Configuration,
  DefaultApi,
} from '';
import type { FakeEnumRequestGetRefRequest } from '';

async function example() {
  console.log("🚀 Testing  SDK...");
  const api = new DefaultApi();

  const body = {
    // StringEnum (optional)
    stringEnum: ...,
    // StringEnum (optional)
    nullableStringEnum: ...,
    // NumberEnum (optional)
    numberEnum: ...,
    // NumberEnum (optional)
    nullableNumberEnum: ...,
  } satisfies FakeEnumRequestGetRefRequest;

  try {
    const data = await api.fakeEnumRequestGetRef(body);
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
| **stringEnum** | `StringEnum` |  | [Optional] [Defaults to `undefined`] [Enum: one, two, three] |
| **nullableStringEnum** | `StringEnum` |  | [Optional] [Defaults to `undefined`] [Enum: one, two, three] |
| **numberEnum** | `NumberEnum` |  | [Optional] [Defaults to `undefined`] [Enum: 1, 2, 3] |
| **nullableNumberEnum** | `NumberEnum` |  | [Optional] [Defaults to `undefined`] [Enum: 1, 2, 3] |

### Return type

[**EnumPatternObject**](EnumPatternObject.md)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: `application/json`


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **200** | OK |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#api-endpoints) [[Back to Model list]](../README.md#models) [[Back to README]](../README.md)


## fakeEnumRequestPostInline

> FakeEnumRequestGetInline200Response fakeEnumRequestPostInline(fakeEnumRequestGetInline200Response)



### Example

```ts
import {
  Configuration,
  DefaultApi,
} from '';
import type { FakeEnumRequestPostInlineRequest } from '';

async function example() {
  console.log("🚀 Testing  SDK...");
  const api = new DefaultApi();

  const body = {
    // FakeEnumRequestGetInline200Response (optional)
    fakeEnumRequestGetInline200Response: ...,
  } satisfies FakeEnumRequestPostInlineRequest;

  try {
    const data = await api.fakeEnumRequestPostInline(body);
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
| **fakeEnumRequestGetInline200Response** | [FakeEnumRequestGetInline200Response](FakeEnumRequestGetInline200Response.md) |  | [Optional] |

### Return type

[**FakeEnumRequestGetInline200Response**](FakeEnumRequestGetInline200Response.md)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: `application/json`
- **Accept**: `application/json`


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **200** | OK |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#api-endpoints) [[Back to Model list]](../README.md#models) [[Back to README]](../README.md)


## fakeEnumRequestPostRef

> EnumPatternObject fakeEnumRequestPostRef(enumPatternObject)



### Example

```ts
import {
  Configuration,
  DefaultApi,
} from '';
import type { FakeEnumRequestPostRefRequest } from '';

async function example() {
  console.log("🚀 Testing  SDK...");
  const api = new DefaultApi();

  const body = {
    // EnumPatternObject (optional)
    enumPatternObject: ...,
  } satisfies FakeEnumRequestPostRefRequest;

  try {
    const data = await api.fakeEnumRequestPostRef(body);
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
| **enumPatternObject** | [EnumPatternObject](EnumPatternObject.md) |  | [Optional] |

### Return type

[**EnumPatternObject**](EnumPatternObject.md)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: `application/json`
- **Accept**: `application/json`


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **200** | OK |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#api-endpoints) [[Back to Model list]](../README.md#models) [[Back to README]](../README.md)

