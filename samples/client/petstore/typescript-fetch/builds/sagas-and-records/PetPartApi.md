# .PetPartApi

All URIs are relative to *http://petstore.swagger.io/v2*

Method | HTTP request | Description
------------- | ------------- | -------------
[**getFakePetPartType**](PetPartApi.md#getfakepetparttype) | **GET** /fake_petParts/{fake_petPart-id}/part-type | Returns single pet part type for the petPart id.
[**getMatchingParts**](PetPartApi.md#getmatchingparts) | **GET** /fake_petParts/{fake_petPart-id}/matching-parts | Get the matching parts for the given pet part.


## **getFakePetPartType**
> GetPetPartTypeResponse getFakePetPartType()


### Example


```typescript
import { PetPartApi } from '';

const apiInstance = new .PetPartApi();

let body:.PetPartApiGetFakePetPartTypeRequest = {
    // number
    fakePetPartId: 789,
};

apiInstance.getFakePetPartType(body).then((data:any) => {
    console.log('API called successfully. Returned data: ' + data);
}).catch((error:any) => console.error(error));
```


### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **fakePetPartId** | [**number**] |  | defaults to undefined


### Return type

[**GetPetPartTypeResponse**](GetPetPartTypeResponse.md)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: */*


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
**200** | OK |  -  |

[[Back to top]](#) [[Back to API list]](README.md#documentation-for-api-endpoints) [[Back to Model list]](README.md#documentation-for-models) [[Back to README]](README.md)

## **getMatchingParts**
> GetMatchingPartsResponse getMatchingParts()


### Example


```typescript
import { PetPartApi } from '';

const apiInstance = new .PetPartApi();

let body:.PetPartApiGetMatchingPartsRequest = {
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
};

apiInstance.getMatchingParts(body).then((data:any) => {
    console.log('API called successfully. Returned data: ' + data);
}).catch((error:any) => console.error(error));
```


### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **fakePetPartId** | [**number**] |  | defaults to undefined
 **_long** | [**boolean**] |  | defaults to undefined
 **smooth** | [**boolean**] |  | defaults to undefined
 **_short** | [**boolean**] |  | defaults to undefined
 **name** | [**string**] |  | (optional) defaults to undefined
 **connectedPart** | [**string**] |  | (optional) defaults to undefined


### Return type

[**GetMatchingPartsResponse**](GetMatchingPartsResponse.md)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: */*


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
**200** | OK |  -  |

[[Back to top]](#) [[Back to API list]](README.md#documentation-for-api-endpoints) [[Back to Model list]](README.md#documentation-for-models) [[Back to README]](README.md)


