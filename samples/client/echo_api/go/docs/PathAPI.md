# \PathAPI

All URIs are relative to *http://localhost:3000*

Method | HTTP request | Description
------------- | ------------- | -------------
[**TestsPathStringPathStringIntegerPathIntegerEnumNonrefStringPathEnumRefStringPath**](PathAPI.md#TestsPathStringPathStringIntegerPathIntegerEnumNonrefStringPathEnumRefStringPath) | **Get** /path/string/{path_string}/integer/{path_integer}/{enum_nonref_string_path}/{enum_ref_string_path} | Test path parameter(s)



## TestsPathStringPathStringIntegerPathIntegerEnumNonrefStringPathEnumRefStringPath

> string TestsPathStringPathStringIntegerPathIntegerEnumNonrefStringPathEnumRefStringPath(ctx, pathString, pathInteger, enumNonrefStringPath, enumRefStringPath).Execute()

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
    enumNonrefStringPath := "enumNonrefStringPath_example" // string | 
    enumRefStringPath := openapiclient.StringEnumRef("success") // StringEnumRef | 

    configuration := openapiclient.NewConfiguration()
    apiClient := openapiclient.NewAPIClient(configuration)
    resp, r, err := apiClient.PathAPI.TestsPathStringPathStringIntegerPathIntegerEnumNonrefStringPathEnumRefStringPath(context.Background(), pathString, pathInteger, enumNonrefStringPath, enumRefStringPath).Execute()
    if err != nil {
        fmt.Fprintf(os.Stderr, "Error when calling `PathAPI.TestsPathStringPathStringIntegerPathIntegerEnumNonrefStringPathEnumRefStringPath``: %v\n", err)
        fmt.Fprintf(os.Stderr, "Full HTTP response: %v\n", r)
    }
    // response from `TestsPathStringPathStringIntegerPathIntegerEnumNonrefStringPathEnumRefStringPath`: string
    fmt.Fprintf(os.Stdout, "Response from `PathAPI.TestsPathStringPathStringIntegerPathIntegerEnumNonrefStringPathEnumRefStringPath`: %v\n", resp)
}
```

### Path Parameters


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
**ctx** | **context.Context** | context for authentication, logging, cancellation, deadlines, tracing, etc.
**pathString** | **string** |  | 
**pathInteger** | **int32** |  | 
**enumNonrefStringPath** | **string** |  | 
**enumRefStringPath** | [**StringEnumRef**](.md) |  | 

### Other Parameters

Other parameters are passed through a pointer to a apiTestsPathStringPathStringIntegerPathIntegerEnumNonrefStringPathEnumRefStringPathRequest struct via the builder pattern


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

