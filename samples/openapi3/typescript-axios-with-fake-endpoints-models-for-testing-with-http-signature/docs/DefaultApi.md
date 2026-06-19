# DefaultApi

All URIs are relative to *http://petstore.swagger.io:80/v2*

|Method | HTTP request | Description|
|------------- | ------------- | -------------|
|[**fooGet**](#fooget) | **GET** /foo | |

# **fooGet**
> FooGetDefaultResponse fooGet()


### Example

```typescript
import {
    DefaultApi,
    Configuration
} from './api';

const configuration = new Configuration();
const apiInstance = new DefaultApi(configuration);

const { status, data } = await apiInstance.fooGet();
```

### Parameters
This endpoint does not have any parameters.


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

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

