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

> TestResponseResource test()



### Example

```ts
import {
  Configuration,
  DefaultApi,
} from '@openapitools/typescript-fetch-model-suffix';
import type { TestRequest } from '@openapitools/typescript-fetch-model-suffix';

async function example() {
  console.log("🚀 Testing @openapitools/typescript-fetch-model-suffix SDK...");
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

[**TestResponseResource**](TestResponseResource.md)

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

> TestArrayResponseResource testArray()



### Example

```ts
import {
  Configuration,
  DefaultApi,
} from '@openapitools/typescript-fetch-model-suffix';
import type { TestArrayRequest } from '@openapitools/typescript-fetch-model-suffix';

async function example() {
  console.log("🚀 Testing @openapitools/typescript-fetch-model-suffix SDK...");
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

[**TestArrayResponseResource**](TestArrayResponseResource.md)

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

> TestDashedDiscriminatorResponseResource testDashedDiscriminator()



### Example

```ts
import {
  Configuration,
  DefaultApi,
} from '@openapitools/typescript-fetch-model-suffix';
import type { TestDashedDiscriminatorRequest } from '@openapitools/typescript-fetch-model-suffix';

async function example() {
  console.log("🚀 Testing @openapitools/typescript-fetch-model-suffix SDK...");
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

[**TestDashedDiscriminatorResponseResource**](TestDashedDiscriminatorResponseResource.md)

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

> TestDiscriminatorResponseResource testDiscriminator()



### Example

```ts
import {
  Configuration,
  DefaultApi,
} from '@openapitools/typescript-fetch-model-suffix';
import type { TestDiscriminatorRequest } from '@openapitools/typescript-fetch-model-suffix';

async function example() {
  console.log("🚀 Testing @openapitools/typescript-fetch-model-suffix SDK...");
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

[**TestDiscriminatorResponseResource**](TestDiscriminatorResponseResource.md)

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

> TestSnakeCaseDiscriminatorResponseResource testSnakeCaseDiscriminator()



### Example

```ts
import {
  Configuration,
  DefaultApi,
} from '@openapitools/typescript-fetch-model-suffix';
import type { TestSnakeCaseDiscriminatorRequest } from '@openapitools/typescript-fetch-model-suffix';

async function example() {
  console.log("🚀 Testing @openapitools/typescript-fetch-model-suffix SDK...");
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

[**TestSnakeCaseDiscriminatorResponseResource**](TestSnakeCaseDiscriminatorResponseResource.md)

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

