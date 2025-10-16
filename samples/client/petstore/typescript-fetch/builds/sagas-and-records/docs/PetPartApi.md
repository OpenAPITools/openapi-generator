# PetPartApi

All URIs are relative to *http://petstore.swagger.io/v2*

| Method | HTTP request | Description |
|------------- | ------------- | -------------|
| [**getFakePetPartType**](PetPartApi.md#getfakepetparttype) | **GET** /fake_petParts/{fake_petPart-id}/part-type | Returns single pet part type for the petPart id. |
| [**getMatchingParts**](PetPartApi.md#getmatchingparts) | **GET** /fake_petParts/{fake_petPart-id}/matching-parts | Get the matching parts for the given pet part. |



## getFakePetPartType

> GetPetPartTypeResponse getFakePetPartType(fakePetPartId)

Returns single pet part type for the petPart id.

### Example

```ts
import {
  Configuration,
  PetPartApi,
} from '@openapitools/typescript-fetch-petstore';
import type { GetFakePetPartTypeRequest } from '@openapitools/typescript-fetch-petstore';

async function example() {
  console.log("🚀 Testing @openapitools/typescript-fetch-petstore SDK...");
  const api = new PetPartApi();

  const body = {
    // number
    fakePetPartId: 789,
  } satisfies GetFakePetPartTypeRequest;

  try {
    const data = await api.getFakePetPartType(body);
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
| **fakePetPartId** | `number` |  | [Defaults to `undefined`] |

### Return type

[**GetPetPartTypeResponse**](GetPetPartTypeResponse.md)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: `*/*`


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **200** | OK |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#api-endpoints) [[Back to Model list]](../README.md#models) [[Back to README]](../README.md)


## getMatchingParts

> GetMatchingPartsResponse getMatchingParts(fakePetPartId, _long, smooth, _short, name, connectedPart)

Get the matching parts for the given pet part.

### Example

```ts
import {
  Configuration,
  PetPartApi,
} from '@openapitools/typescript-fetch-petstore';
import type { GetMatchingPartsRequest } from '@openapitools/typescript-fetch-petstore';

async function example() {
  console.log("🚀 Testing @openapitools/typescript-fetch-petstore SDK...");
  const api = new PetPartApi();

  const body = {
    // number
    fakePetPartId: 789,
    // boolean
    _long: true,
    // boolean
    smooth: true,
    // boolean
    _short: true,
    // string (optional)
    name: name_example,
    // string (optional)
    connectedPart: connectedPart_example,
  } satisfies GetMatchingPartsRequest;

  try {
    const data = await api.getMatchingParts(body);
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
| **fakePetPartId** | `number` |  | [Defaults to `undefined`] |
| **_long** | `boolean` |  | [Defaults to `undefined`] |
| **smooth** | `boolean` |  | [Defaults to `undefined`] |
| **_short** | `boolean` |  | [Defaults to `undefined`] |
| **name** | `string` |  | [Optional] [Defaults to `undefined`] |
| **connectedPart** | `string` |  | [Optional] [Defaults to `undefined`] |

### Return type

[**GetMatchingPartsResponse**](GetMatchingPartsResponse.md)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: `*/*`


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **200** | OK |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#api-endpoints) [[Back to Model list]](../README.md#models) [[Back to README]](../README.md)

