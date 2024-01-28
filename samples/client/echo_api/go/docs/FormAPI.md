# \FormAPI

All URIs are relative to *http://localhost:3000*

Method | HTTP request | Description
------------- | ------------- | -------------
[**TestFormIntegerBooleanString**](FormAPI.md#TestFormIntegerBooleanString) | **Post** /form/integer/boolean/string | Test form parameter(s)
[**TestFormOneof**](FormAPI.md#TestFormOneof) | **Post** /form/oneof | Test form parameter(s) for oneOf schema



## TestFormIntegerBooleanString

> string TestFormIntegerBooleanString(ctx).IntegerForm(integerForm).BooleanForm(booleanForm).StringForm(stringForm).Execute()

Test form parameter(s)



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
    integerForm := int32(56) // int32 |  (optional)
    booleanForm := true // bool |  (optional)
    stringForm := "stringForm_example" // string |  (optional)

    configuration := openapiclient.NewConfiguration()
    apiClient := openapiclient.NewAPIClient(configuration)
    resp, r, err := apiClient.FormAPI.TestFormIntegerBooleanString(context.Background()).IntegerForm(integerForm).BooleanForm(booleanForm).StringForm(stringForm).Execute()
    if err != nil {
        fmt.Fprintf(os.Stderr, "Error when calling `FormAPI.TestFormIntegerBooleanString``: %v\n", err)
        fmt.Fprintf(os.Stderr, "Full HTTP response: %v\n", r)
    }
    // response from `TestFormIntegerBooleanString`: string
    fmt.Fprintf(os.Stdout, "Response from `FormAPI.TestFormIntegerBooleanString`: %v\n", resp)
}
```

### Path Parameters



### Other Parameters

Other parameters are passed through a pointer to a apiTestFormIntegerBooleanStringRequest struct via the builder pattern


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **integerForm** | **int32** |  | 
 **booleanForm** | **bool** |  | 
 **stringForm** | **string** |  | 

### Return type

**string**

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: application/x-www-form-urlencoded
- **Accept**: text/plain

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints)
[[Back to Model list]](../README.md#documentation-for-models)
[[Back to README]](../README.md)


## TestFormOneof

> string TestFormOneof(ctx).Form1(form1).Form2(form2).Form3(form3).Form4(form4).Id(id).Name(name).Execute()

Test form parameter(s) for oneOf schema



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
    form1 := "form1_example" // string |  (optional)
    form2 := int32(56) // int32 |  (optional)
    form3 := "form3_example" // string |  (optional)
    form4 := true // bool |  (optional)
    id := int64(789) // int64 |  (optional)
    name := "name_example" // string |  (optional)

    configuration := openapiclient.NewConfiguration()
    apiClient := openapiclient.NewAPIClient(configuration)
    resp, r, err := apiClient.FormAPI.TestFormOneof(context.Background()).Form1(form1).Form2(form2).Form3(form3).Form4(form4).Id(id).Name(name).Execute()
    if err != nil {
        fmt.Fprintf(os.Stderr, "Error when calling `FormAPI.TestFormOneof``: %v\n", err)
        fmt.Fprintf(os.Stderr, "Full HTTP response: %v\n", r)
    }
    // response from `TestFormOneof`: string
    fmt.Fprintf(os.Stdout, "Response from `FormAPI.TestFormOneof`: %v\n", resp)
}
```

### Path Parameters



### Other Parameters

Other parameters are passed through a pointer to a apiTestFormOneofRequest struct via the builder pattern


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **form1** | **string** |  | 
 **form2** | **int32** |  | 
 **form3** | **string** |  | 
 **form4** | **bool** |  | 
 **id** | **int64** |  | 
 **name** | **string** |  | 

### Return type

**string**

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: application/x-www-form-urlencoded
- **Accept**: text/plain

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints)
[[Back to Model list]](../README.md#documentation-for-models)
[[Back to README]](../README.md)

