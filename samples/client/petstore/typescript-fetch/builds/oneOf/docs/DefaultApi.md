# DefaultApi

All URIs are relative to *http://localhost:3000*

| Method | HTTP request | Description |
|------------- | ------------- | -------------|
| [**test**](DefaultApi.md#test) | **GET** /test |  |
| [**testArray**](DefaultApi.md#testarray) | **GET** /test-array |  |
| [**testDashedDiscriminator**](DefaultApi.md#testdasheddiscriminator) | **GET** /test-dashed-discriminator |  |
| [**testDiscriminator**](DefaultApi.md#testdiscriminator) | **GET** /test-discriminator |  |
| [**testSnakeCaseDiscriminator**](DefaultApi.md#testsnakecasediscriminator) | **GET** /test-snake-case-discriminator |  |



## test

> TestResponse test()



### Example

```ts
import {
  Configuration,
  DefaultApi,
} from '';
import type { TestRequest } from '';

async function example() {
  console.log("🚀 Testing  SDK...");
  const api = new DefaultApi();

  try {
    const data = await api.test();
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

[**TestResponse**](TestResponse.md)

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


## testArray

> TestArrayResponse testArray()



### Example

```ts
import {
  Configuration,
  DefaultApi,
} from '';
import type { TestArrayRequest } from '';

async function example() {
  console.log("🚀 Testing  SDK...");
  const api = new DefaultApi();

  try {
    const data = await api.testArray();
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

[**TestArrayResponse**](TestArrayResponse.md)

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


## testDashedDiscriminator

> TestDashedDiscriminatorResponse testDashedDiscriminator()



### Example

```ts
import {
  Configuration,
  DefaultApi,
} from '';
import type { TestDashedDiscriminatorRequest } from '';

async function example() {
  console.log("🚀 Testing  SDK...");
  const api = new DefaultApi();

  try {
    const data = await api.testDashedDiscriminator();
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

[**TestDashedDiscriminatorResponse**](TestDashedDiscriminatorResponse.md)

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


## testDiscriminator

> TestDiscriminatorResponse testDiscriminator()



### Example

```ts
import {
  Configuration,
  DefaultApi,
} from '';
import type { TestDiscriminatorRequest } from '';

async function example() {
  console.log("🚀 Testing  SDK...");
  const api = new DefaultApi();

  try {
    const data = await api.testDiscriminator();
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

[**TestDiscriminatorResponse**](TestDiscriminatorResponse.md)

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


## testSnakeCaseDiscriminator

> TestSnakeCaseDiscriminatorResponse testSnakeCaseDiscriminator()



### Example

```ts
import {
  Configuration,
  DefaultApi,
} from '';
import type { TestSnakeCaseDiscriminatorRequest } from '';

async function example() {
  console.log("🚀 Testing  SDK...");
  const api = new DefaultApi();

  try {
    const data = await api.testSnakeCaseDiscriminator();
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

[**TestSnakeCaseDiscriminatorResponse**](TestSnakeCaseDiscriminatorResponse.md)

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

