# \AnotherFakeApi

All URIs are relative to *http://petstore.swagger.io:80/v2*

Method | HTTP request | Description
------------- | ------------- | -------------
[**Call123TestSpecialTags**](AnotherFakeApi.md#Call123TestSpecialTags) | **Patch** /another-fake/dummy | To test special tags



## Call123TestSpecialTags

> Client Call123TestSpecialTags(ctx).Client(client).Execute()

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

func main() {
    client := *openapiclient.NewClient() // Client | client model

    configuration := openapiclient.NewConfiguration()
    api_client := openapiclient.NewAPIClient(configuration)
    resp, r, err := api_client.AnotherFakeApi.Call123TestSpecialTags(context.Background()).Client(client).Execute()
    if err != nil {
        fmt.Fprintf(os.Stderr, "Error when calling `AnotherFakeApi.Call123TestSpecialTags``: %v\n", err)
        fmt.Fprintf(os.Stderr, "Full HTTP response: %v\n", r)
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
 **client** | [**Client**](Client.md) | client model | 

### Return type

[**Client**](Client.md)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: application/json
- **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints)
[[Back to Model list]](../README.md#documentation-for-models)
[[Back to README]](../README.md)

