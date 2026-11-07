# DefaultApi

All URIs are relative to *http://localhost*

| Method | HTTP request | Description |
|------------- | ------------- | -------------|
| [**createPet**](DefaultApi.md#createpet) | **POST** /pets |  |



## createPet

> PetData createPet(createPet)



### Example

```ts
import {
  Configuration,
  DefaultApi,
} from '@openapitools/typescript-fetch-petstore';
import type { CreatePetRequest } from '@openapitools/typescript-fetch-petstore';

async function example() {
  console.log("🚀 Testing @openapitools/typescript-fetch-petstore SDK...");
  const api = new DefaultApi();

  const body = {
    // CreatePet (optional)
    createPet: ...,
  } satisfies CreatePetRequest;

  try {
    const data = await api.createPet(body);
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
| **createPet** | [CreatePet](CreatePet.md) |  | [Optional] |

### Return type

[**PetData**](PetData.md)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: `application/json`
- **Accept**: `application/json`


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **200** | ok |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#api-endpoints) [[Back to Model list]](../README.md#models) [[Back to README]](../README.md)

