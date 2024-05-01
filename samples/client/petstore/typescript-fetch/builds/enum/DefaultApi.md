# .DefaultApi

All URIs are relative to *http://localhost:3000*

Method | HTTP request | Description
------------- | ------------- | -------------
[**fakeEnumRequestGetInline**](DefaultApi.md#fakeenumrequestgetinline) | **GET** /fake/enum-request-inline | 
[**fakeEnumRequestGetRef**](DefaultApi.md#fakeenumrequestgetref) | **GET** /fake/enum-request-ref | 
[**fakeEnumRequestPostInline**](DefaultApi.md#fakeenumrequestpostinline) | **POST** /fake/enum-request-inline | 
[**fakeEnumRequestPostRef**](DefaultApi.md#fakeenumrequestpostref) | **POST** /fake/enum-request-ref | 


## **fakeEnumRequestGetInline**
> FakeEnumRequestGetInline200Response fakeEnumRequestGetInline()


### Example


```typescript
import { DefaultApi } from '';

const apiInstance = new .DefaultApi();

let body:.DefaultApiFakeEnumRequestGetInlineRequest = {
    // 'one' | 'two' | 'three' (optional)
    stringEnum: stringEnum_example,
    // string (optional)
    nullableStringEnum: ,
    // 1 | 2 | 3 (optional)
    numberEnum: 8.14,
    // number (optional)
    nullableNumberEnum: ,
};

apiInstance.fakeEnumRequestGetInline(body).then((data:any) => {
    console.log('API called successfully. Returned data: ' + data);
}).catch((error:any) => console.error(error));
```


### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **stringEnum** | [**&#39;one&#39; | &#39;two&#39; | &#39;three&#39;**]**Array<&#39;one&#39; &#124; &#39;two&#39; &#124; &#39;three&#39;>** |  | (optional) defaults to undefined
 **nullableStringEnum** | **Array<&#39;one&#39; &#124; &#39;two&#39; &#124; &#39;three&#39;>** |  | (optional) defaults to undefined
 **numberEnum** | [**1 | 2 | 3**]**Array<1 &#124; 2 &#124; 3>** |  | (optional) defaults to undefined
 **nullableNumberEnum** | **Array<1 &#124; 2 &#124; 3>** |  | (optional) defaults to undefined


### Return type

[**FakeEnumRequestGetInline200Response**](FakeEnumRequestGetInline200Response.md)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: application/json


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
**200** | OK |  -  |

[[Back to top]](#) [[Back to API list]](README.md#documentation-for-api-endpoints) [[Back to Model list]](README.md#documentation-for-models) [[Back to README]](README.md)

## **fakeEnumRequestGetRef**
> EnumPatternObject fakeEnumRequestGetRef()


### Example


```typescript
import { DefaultApi } from '';

const apiInstance = new .DefaultApi();

let body:.DefaultApiFakeEnumRequestGetRefRequest = {
    // StringEnum (optional)
    stringEnum: ,
    // StringEnum (optional)
    nullableStringEnum: ,
    // NumberEnum (optional)
    numberEnum: ,
    // NumberEnum (optional)
    nullableNumberEnum: ,
};

apiInstance.fakeEnumRequestGetRef(body).then((data:any) => {
    console.log('API called successfully. Returned data: ' + data);
}).catch((error:any) => console.error(error));
```


### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **stringEnum** | **StringEnum** |  | (optional) defaults to undefined
 **nullableStringEnum** | **StringEnum** |  | (optional) defaults to undefined
 **numberEnum** | **NumberEnum** |  | (optional) defaults to undefined
 **nullableNumberEnum** | **NumberEnum** |  | (optional) defaults to undefined


### Return type

[**EnumPatternObject**](EnumPatternObject.md)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: application/json


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
**200** | OK |  -  |

[[Back to top]](#) [[Back to API list]](README.md#documentation-for-api-endpoints) [[Back to Model list]](README.md#documentation-for-models) [[Back to README]](README.md)

## **fakeEnumRequestPostInline**
> FakeEnumRequestGetInline200Response fakeEnumRequestPostInline()


### Example


```typescript
import { DefaultApi } from '';

const apiInstance = new .DefaultApi();

let body:.DefaultApiFakeEnumRequestPostInlineRequest = {
    // FakeEnumRequestGetInline200Response (optional)
    fakeEnumRequestGetInline200Response: ,
};

apiInstance.fakeEnumRequestPostInline(body).then((data:any) => {
    console.log('API called successfully. Returned data: ' + data);
}).catch((error:any) => console.error(error));
```


### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **fakeEnumRequestGetInline200Response** | **FakeEnumRequestGetInline200Response**|  |


### Return type

[**FakeEnumRequestGetInline200Response**](FakeEnumRequestGetInline200Response.md)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: application/json
- **Accept**: application/json


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
**200** | OK |  -  |

[[Back to top]](#) [[Back to API list]](README.md#documentation-for-api-endpoints) [[Back to Model list]](README.md#documentation-for-models) [[Back to README]](README.md)

## **fakeEnumRequestPostRef**
> EnumPatternObject fakeEnumRequestPostRef()


### Example


```typescript
import { DefaultApi } from '';

const apiInstance = new .DefaultApi();

let body:.DefaultApiFakeEnumRequestPostRefRequest = {
    // EnumPatternObject (optional)
    enumPatternObject: ,
};

apiInstance.fakeEnumRequestPostRef(body).then((data:any) => {
    console.log('API called successfully. Returned data: ' + data);
}).catch((error:any) => console.error(error));
```


### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **enumPatternObject** | **EnumPatternObject**|  |


### Return type

[**EnumPatternObject**](EnumPatternObject.md)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: application/json
- **Accept**: application/json


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
**200** | OK |  -  |

[[Back to top]](#) [[Back to API list]](README.md#documentation-for-api-endpoints) [[Back to Model list]](README.md#documentation-for-models) [[Back to README]](README.md)


