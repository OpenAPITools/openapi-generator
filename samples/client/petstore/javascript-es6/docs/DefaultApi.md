# OpenApiPetstore.DefaultApi

All URIs are relative to *http://petstore.swagger.io:80/v2*

Method | HTTP request | Description
------------- | ------------- | -------------
[**fooGet**](DefaultApi.md#fooGet) | **GET** /foo | 



## fooGet

> InlineResponseDefault fooGet()



### Example

```javascript
import OpenApiPetstore from 'open_api_petstore';

let apiInstance = new OpenApiPetstore.DefaultApi();
apiInstance.fooGet((error, data, response) => {
  if (error) {
    console.error(error);
  } else {
    console.log('API called successfully. Returned data: ' + data);
  }
});
```

### Parameters

This endpoint does not need any parameter.

### Return type

[**InlineResponseDefault**](InlineResponseDefault.md)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: application/json

