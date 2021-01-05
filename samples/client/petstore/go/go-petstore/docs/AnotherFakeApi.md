# \AnotherFakeApi

All URIs are relative to *http://petstore.swagger.io:80/v2*

Method | HTTP request | Description
------------- | ------------- | -------------
[**Call123TestSpecialTags**](AnotherFakeApi.md#Call123TestSpecialTags) | **Patch** /another-fake/dummy | To test special tags



## Call123TestSpecialTags

> Client Call123TestSpecialTags(ctx).Body(body).Execute()

To test special tags



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
    body := *openapiclient.NewClient() // Client | client model

    configuration := openapiclient.NewConfiguration()
    api_client := openapiclient.NewAPIClient(configuration)
    resp, r, err := api_client.AnotherFakeApi.Call123TestSpecialTags(context.Background()).Body(body).Execute()
    if err != nil {
        fmt.Fprintf(os.Stderr, "Error when calling `AnotherFakeApi.Call123TestSpecialTags``: %v\n", err)
        fmt.Fprintf(os.Stderr, "Full HTTP response: %v\n", r)
        if openapiErr, ok := err.(openapiError); ok {
            fmt.Fprintf(os.Stderr, "Model returned from error response: %v\n", openapiErr.Model())
            fmt.Fprintf(os.Stderr, "Raw response body: %v\n", openapiErr.Body())
        }
    }
    // response from `Call123TestSpecialTags`: Client
    fmt.Fprintf(os.Stdout, "Response from `AnotherFakeApi.Call123TestSpecialTags`: %v\n", resp)
}
```

### Path Parameters



### Other Parameters

Other parameters are passed through a pointer to a apiCall123TestSpecialTagsRequest struct via the builder pattern


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | [**Client**](Client.md) | client model | 

### Return type

[**Client**](Client.md), http.Response and error

The returned error provides `Body()` and `Model()` methods that can be accessed using a custom interface.

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: application/json
- **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints)
[[Back to Model list]](../README.md#documentation-for-models)
[[Back to README]](../README.md)

