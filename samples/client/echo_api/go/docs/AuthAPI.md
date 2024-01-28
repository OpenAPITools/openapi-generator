# \AuthAPI

All URIs are relative to *http://localhost:3000*

Method | HTTP request | Description
------------- | ------------- | -------------
[**TestAuthHttpBasic**](AuthAPI.md#TestAuthHttpBasic) | **Post** /auth/http/basic | To test HTTP basic authentication



## TestAuthHttpBasic

> string TestAuthHttpBasic(ctx).Execute()

To test HTTP basic authentication



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

    configuration := openapiclient.NewConfiguration()
    apiClient := openapiclient.NewAPIClient(configuration)
    resp, r, err := apiClient.AuthAPI.TestAuthHttpBasic(context.Background()).Execute()
    if err != nil {
        fmt.Fprintf(os.Stderr, "Error when calling `AuthAPI.TestAuthHttpBasic``: %v\n", err)
        fmt.Fprintf(os.Stderr, "Full HTTP response: %v\n", r)
    }
    // response from `TestAuthHttpBasic`: string
    fmt.Fprintf(os.Stdout, "Response from `AuthAPI.TestAuthHttpBasic`: %v\n", resp)
}
```

### Path Parameters

This endpoint does not need any parameter.

### Other Parameters

Other parameters are passed through a pointer to a apiTestAuthHttpBasicRequest struct via the builder pattern


### Return type

**string**

### Authorization

[http_auth](../README.md#http_auth)

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: text/plain

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints)
[[Back to Model list]](../README.md#documentation-for-models)
[[Back to README]](../README.md)

