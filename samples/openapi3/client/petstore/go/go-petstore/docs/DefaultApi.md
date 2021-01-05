# \DefaultApi

All URIs are relative to *http://petstore.swagger.io:80/v2*

Method | HTTP request | Description
------------- | ------------- | -------------
[**FooGet**](DefaultApi.md#FooGet) | **Get** /foo | 



## FooGet

> InlineResponseDefault FooGet(ctx).Execute()



### Example

```go
package main

import (
    "context"
    "fmt"
    "os"
    openapiclient "./openapi"
)

type openapiError interface {
    Model() interface{}
    Body() []byte 
}

func main() {

    configuration := openapiclient.NewConfiguration()
    api_client := openapiclient.NewAPIClient(configuration)
    resp, r, err := api_client.DefaultApi.FooGet(context.Background()).Execute()
    if err != nil {
        fmt.Fprintf(os.Stderr, "Error when calling `DefaultApi.FooGet``: %v\n", err)
        fmt.Fprintf(os.Stderr, "Full HTTP response: %v\n", r)
        if openapiErr, ok := err.(openapiError); ok {
            fmt.Fprintf(os.Stderr, "Model returned from error response: %v\n", openapiErr.Model())
            fmt.Fprintf(os.Stderr, "Raw response body: %v\n", openapiErr.Body())
        }
    }
    // response from `FooGet`: InlineResponseDefault
    fmt.Fprintf(os.Stdout, "Response from `DefaultApi.FooGet`: %v\n", resp)
}
```

### Path Parameters

This endpoint does not need any parameter.

### Other Parameters

Other parameters are passed through a pointer to a apiFooGetRequest struct via the builder pattern


### Return type

[**InlineResponseDefault**](inline_response_default.md), http.Response and error

The returned error provides `Body()` and `Model()` methods that can be accessed using a custom interface.

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints)
[[Back to Model list]](../README.md#documentation-for-models)
[[Back to README]](../README.md)

