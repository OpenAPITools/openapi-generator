# \HeaderAPI

All URIs are relative to *http://localhost:3000*

Method | HTTP request | Description
------------- | ------------- | -------------
[**TestHeaderIntegerBooleanStringEnums**](HeaderAPI.md#TestHeaderIntegerBooleanStringEnums) | **Get** /header/integer/boolean/string/enums | Test header parameter(s)



## TestHeaderIntegerBooleanStringEnums

> string TestHeaderIntegerBooleanStringEnums(ctx).IntegerHeader(integerHeader).BooleanHeader(booleanHeader).StringHeader(stringHeader).EnumNonrefStringHeader(enumNonrefStringHeader).EnumRefStringHeader(enumRefStringHeader).Execute()

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
    enumNonrefStringHeader := "enumNonrefStringHeader_example" // string |  (optional)
    enumRefStringHeader := openapiclient.StringEnumRef("success") // StringEnumRef |  (optional)

    configuration := openapiclient.NewConfiguration()
    apiClient := openapiclient.NewAPIClient(configuration)
    resp, r, err := apiClient.HeaderAPI.TestHeaderIntegerBooleanStringEnums(context.Background()).IntegerHeader(integerHeader).BooleanHeader(booleanHeader).StringHeader(stringHeader).EnumNonrefStringHeader(enumNonrefStringHeader).EnumRefStringHeader(enumRefStringHeader).Execute()
    if err != nil {
        fmt.Fprintf(os.Stderr, "Error when calling `HeaderAPI.TestHeaderIntegerBooleanStringEnums``: %v\n", err)
        fmt.Fprintf(os.Stderr, "Full HTTP response: %v\n", r)
    }
    // response from `TestHeaderIntegerBooleanStringEnums`: string
    fmt.Fprintf(os.Stdout, "Response from `HeaderAPI.TestHeaderIntegerBooleanStringEnums`: %v\n", resp)
}
```

### Path Parameters



### Other Parameters

Other parameters are passed through a pointer to a apiTestHeaderIntegerBooleanStringEnumsRequest struct via the builder pattern


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **integerHeader** | **int32** |  | 
 **booleanHeader** | **bool** |  | 
 **stringHeader** | **string** |  | 
 **enumNonrefStringHeader** | **string** |  | 
 **enumRefStringHeader** | [**StringEnumRef**](StringEnumRef.md) |  | 

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

