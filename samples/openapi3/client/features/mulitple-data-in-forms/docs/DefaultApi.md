# \DefaultApi

All URIs are relative to *http://example.com*

Method | HTTP request | Description
------------- | ------------- | -------------
[**UploadFiles**](DefaultApi.md#UploadFiles) | **Post** /uploadFiles | uploads two files



## UploadFiles

> ApiResponse UploadFiles(ctx).File(file).SecondFile(secondFile).Execute()

uploads two files

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
    file := os.NewFile(1234, "some_file") // *os.File | file to upload (optional)
    secondFile := os.NewFile(1234, "some_file") // *os.File | another file to upload (optional)

    configuration := openapiclient.NewConfiguration()
    api_client := openapiclient.NewAPIClient(configuration)
    resp, r, err := api_client.DefaultApi.UploadFiles(context.Background()).File(file).SecondFile(secondFile).Execute()
    if err != nil {
        fmt.Fprintf(os.Stderr, "Error when calling `DefaultApi.UploadFiles``: %v\n", err)
        fmt.Fprintf(os.Stderr, "Full HTTP response: %v\n", r)
    }
    // response from `UploadFiles`: ApiResponse
    fmt.Fprintf(os.Stdout, "Response from `DefaultApi.UploadFiles`: %v\n", resp)
}
```

### Path Parameters



### Other Parameters

Other parameters are passed through a pointer to a apiUploadFilesRequest struct via the builder pattern


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **file** | ***os.File** | file to upload | 
 **secondFile** | ***os.File** | another file to upload | 

### Return type

[**ApiResponse**](ApiResponse.md)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: multipart/form-data
- **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints)
[[Back to Model list]](../README.md#documentation-for-models)
[[Back to README]](../README.md)

