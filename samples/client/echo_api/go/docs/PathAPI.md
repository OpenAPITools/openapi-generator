# \PathAPI

All URIs are relative to *http://localhost:3000*

Method | HTTP request | Description
------------- | ------------- | -------------
[**TestsPathStringPathStringIntegerPathInteger**](PathAPI.md#TestsPathStringPathStringIntegerPathInteger) | **Get** /path/string/{path_string}/integer/{path_integer} | Test path parameter(s)



## TestsPathStringPathStringIntegerPathInteger

> string TestsPathStringPathStringIntegerPathInteger(ctx, pathString, pathInteger).Execute()

Test path parameter(s)



### Example

```go
package main

import (
    "context"
    "fmt"
    "os"
    openapiclient "github.com/GIT_USER_ID/GIT_REPO_ID"
)

func main() {
    pathString := "pathString_example" // string | 
    pathInteger := int32(56) // int32 | 

    configuration := openapiclient.NewConfiguration()
    apiClient := openapiclient.NewAPIClient(configuration)
    resp, r, err := apiClient.PathAPI.TestsPathStringPathStringIntegerPathInteger(context.Background(), pathString, pathInteger).Execute()
    if err != nil {
        fmt.Fprintf(os.Stderr, "Error when calling `PathAPI.TestsPathStringPathStringIntegerPathInteger``: %v\n", err)
        fmt.Fprintf(os.Stderr, "Full HTTP response: %v\n", r)
    }
    // response from `TestsPathStringPathStringIntegerPathInteger`: string
    fmt.Fprintf(os.Stdout, "Response from `PathAPI.TestsPathStringPathStringIntegerPathInteger`: %v\n", resp)
}
```

### Path Parameters


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
**ctx** | **context.Context** | context for authentication, logging, cancellation, deadlines, tracing, etc.
**pathString** | **string** |  | 
**pathInteger** | **int32** |  | 

### Other Parameters

Other parameters are passed through a pointer to a apiTestsPathStringPathStringIntegerPathIntegerRequest struct via the builder pattern


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------



### Return type

**string**

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: text/plain

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints)
[[Back to Model list]](../README.md#documentation-for-models)
[[Back to README]](../README.md)

