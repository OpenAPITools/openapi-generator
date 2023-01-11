# .DefaultApi

All URIs are relative to *http://petstore.swagger.io:80/v2*

|Method | HTTP request | Description|
|------------- | ------------- | -------------|
|[**fooGet**](DefaultApi.md#fooGet) | **GET** /foo | |

# **fooGet**
> FooGetDefaultResponse fooGet()


### Example

```typescript
import * as ApiModule from '';

const configuration: ApiModule.Configuration = new ApiModule.Configuration();
const apiInstance: ApiModule.DefaultApi = new ApiModule.DefaultApi(configuration);

//let body: ApiModule. = {};

apiInstance.fooGet().then((data: any) => {
  console.log('API called successfully. Returned data: ', data);
}).catch((error: any) => {
  console.error(error);
});
```

### Parameters
This endpoint does not need any parameter.


### Return type

**FooGetDefaultResponse**

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/json


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
|**404** | not found |  -  |
|**4XX** | client error |  -  |
|**0** | response |  -  |

[[Back to top]](#) [[Back to API list]](README.md#documentation-for-api-endpoints) [[Back to Model list]](README.md#documentation-for-models) [[Back to README]](README.md)


