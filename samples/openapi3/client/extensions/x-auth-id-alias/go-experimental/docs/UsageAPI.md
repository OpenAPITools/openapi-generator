# \UsageAPI

All URIs are relative to *http://petstore.swagger.io:80/v2*

Method | HTTP request | Description
------------- | ------------- | -------------
[**AnyKey**](UsageAPI.md#AnyKey) | **Get** /any | Use any API key
[**BothKeys**](UsageAPI.md#BothKeys) | **Get** /both | Use both API keys
[**KeyInHeader**](UsageAPI.md#KeyInHeader) | **Get** /header | Use API key in header
[**KeyInQuery**](UsageAPI.md#KeyInQuery) | **Get** /query | Use API key in query



## AnyKey

> map[string]interface{} AnyKey(ctx).Execute()

Use any API key



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
    resp, r, err := apiClient.UsageAPI.AnyKey(context.Background()).Execute()
    if err != nil {
        fmt.Fprintf(os.Stderr, "Error when calling `UsageAPI.AnyKey``: %v\n", err)
        fmt.Fprintf(os.Stderr, "Full HTTP response: %v\n", r)
    }
    // response from `AnyKey`: map[string]interface{}
    fmt.Fprintf(os.Stdout, "Response from `UsageAPI.AnyKey`: %v\n", resp)
}
```

### Path Parameters

This endpoint does not need any parameter.

### Other Parameters

Other parameters are passed through a pointer to a apiAnyKeyRequest struct via the builder pattern


### Return type

**map[string]interface{}**

### Authorization

[api_key](../README.md#api_key), [api_key_query](../README.md#api_key_query)

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints)
[[Back to Model list]](../README.md#documentation-for-models)
[[Back to README]](../README.md)


## BothKeys

> map[string]interface{} BothKeys(ctx).Execute()

Use both API keys



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
    resp, r, err := apiClient.UsageAPI.BothKeys(context.Background()).Execute()
    if err != nil {
        fmt.Fprintf(os.Stderr, "Error when calling `UsageAPI.BothKeys``: %v\n", err)
        fmt.Fprintf(os.Stderr, "Full HTTP response: %v\n", r)
    }
    // response from `BothKeys`: map[string]interface{}
    fmt.Fprintf(os.Stdout, "Response from `UsageAPI.BothKeys`: %v\n", resp)
}
```

### Path Parameters

This endpoint does not need any parameter.

### Other Parameters

Other parameters are passed through a pointer to a apiBothKeysRequest struct via the builder pattern


### Return type

**map[string]interface{}**

### Authorization

[api_key](../README.md#api_key), [api_key_query](../README.md#api_key_query)

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints)
[[Back to Model list]](../README.md#documentation-for-models)
[[Back to README]](../README.md)


## KeyInHeader

> map[string]interface{} KeyInHeader(ctx).Execute()

Use API key in header



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
    resp, r, err := apiClient.UsageAPI.KeyInHeader(context.Background()).Execute()
    if err != nil {
        fmt.Fprintf(os.Stderr, "Error when calling `UsageAPI.KeyInHeader``: %v\n", err)
        fmt.Fprintf(os.Stderr, "Full HTTP response: %v\n", r)
    }
    // response from `KeyInHeader`: map[string]interface{}
    fmt.Fprintf(os.Stdout, "Response from `UsageAPI.KeyInHeader`: %v\n", resp)
}
```

### Path Parameters

This endpoint does not need any parameter.

### Other Parameters

Other parameters are passed through a pointer to a apiKeyInHeaderRequest struct via the builder pattern


### Return type

**map[string]interface{}**

### Authorization

[api_key](../README.md#api_key)

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints)
[[Back to Model list]](../README.md#documentation-for-models)
[[Back to README]](../README.md)


## KeyInQuery

> map[string]interface{} KeyInQuery(ctx).Execute()

Use API key in query



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
    resp, r, err := apiClient.UsageAPI.KeyInQuery(context.Background()).Execute()
    if err != nil {
        fmt.Fprintf(os.Stderr, "Error when calling `UsageAPI.KeyInQuery``: %v\n", err)
        fmt.Fprintf(os.Stderr, "Full HTTP response: %v\n", r)
    }
    // response from `KeyInQuery`: map[string]interface{}
    fmt.Fprintf(os.Stdout, "Response from `UsageAPI.KeyInQuery`: %v\n", resp)
}
```

### Path Parameters

This endpoint does not need any parameter.

### Other Parameters

Other parameters are passed through a pointer to a apiKeyInQueryRequest struct via the builder pattern


### Return type

**map[string]interface{}**

### Authorization

[api_key_query](../README.md#api_key_query)

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints)
[[Back to Model list]](../README.md#documentation-for-models)
[[Back to README]](../README.md)

