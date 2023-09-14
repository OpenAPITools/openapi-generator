# \HeaderAPI

All URIs are relative to *http://localhost:3000*

Method | HTTP request | Description
------------- | ------------- | -------------
[**TestHeaderIntegerBooleanString**](HeaderAPI.md#TestHeaderIntegerBooleanString) | **Get** /header/integer/boolean/string | Test header parameter(s)



## TestHeaderIntegerBooleanString

> string TestHeaderIntegerBooleanString(ctx).IntegerHeader(integerHeader).BooleanHeader(booleanHeader).StringHeader(stringHeader).Execute()

Test header parameter(s)



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
    integerHeader := int32(56) // int32 |  (optional)
    booleanHeader := true // bool |  (optional)
    stringHeader := "stringHeader_example" // string |  (optional)

    configuration := openapiclient.NewConfiguration()
    apiClient := openapiclient.NewAPIClient(configuration)
    resp, r, err := apiClient.HeaderAPI.TestHeaderIntegerBooleanString(context.Background()).IntegerHeader(integerHeader).BooleanHeader(booleanHeader).StringHeader(stringHeader).Execute()
    if err != nil {
        fmt.Fprintf(os.Stderr, "Error when calling `HeaderAPI.TestHeaderIntegerBooleanString``: %v\n", err)
        fmt.Fprintf(os.Stderr, "Full HTTP response: %v\n", r)
    }
    // response from `TestHeaderIntegerBooleanString`: string
    fmt.Fprintf(os.Stdout, "Response from `HeaderAPI.TestHeaderIntegerBooleanString`: %v\n", resp)
}
```

### Path Parameters



### Other Parameters

Other parameters are passed through a pointer to a apiTestHeaderIntegerBooleanStringRequest struct via the builder pattern


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **integerHeader** | **int32** |  | 
 **booleanHeader** | **bool** |  | 
 **stringHeader** | **string** |  | 

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

