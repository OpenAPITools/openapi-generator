# \BodyAPI

All URIs are relative to *http://localhost:3000*

Method | HTTP request | Description
------------- | ------------- | -------------
[**TestBinaryGif**](BodyAPI.md#TestBinaryGif) | **Post** /binary/gif | Test binary (gif) response body
[**TestBodyApplicationOctetstreamBinary**](BodyAPI.md#TestBodyApplicationOctetstreamBinary) | **Post** /body/application/octetstream/binary | Test body parameter(s)
[**TestBodyMultipartFormdataArrayOfBinary**](BodyAPI.md#TestBodyMultipartFormdataArrayOfBinary) | **Post** /body/application/octetstream/array_of_binary | Test array of binary in multipart mime
[**TestEchoBodyFreeFormObjectResponseString**](BodyAPI.md#TestEchoBodyFreeFormObjectResponseString) | **Post** /echo/body/FreeFormObject/response_string | Test free form object
[**TestEchoBodyPet**](BodyAPI.md#TestEchoBodyPet) | **Post** /echo/body/Pet | Test body parameter(s)
[**TestEchoBodyPetResponseString**](BodyAPI.md#TestEchoBodyPetResponseString) | **Post** /echo/body/Pet/response_string | Test empty response body
[**TestEchoBodyTagResponseString**](BodyAPI.md#TestEchoBodyTagResponseString) | **Post** /echo/body/Tag/response_string | Test empty json (request body)



## TestBinaryGif

> *os.File TestBinaryGif(ctx).Execute()

Test binary (gif) response body



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
    resp, r, err := apiClient.BodyAPI.TestBinaryGif(context.Background()).Execute()
    if err != nil {
        fmt.Fprintf(os.Stderr, "Error when calling `BodyAPI.TestBinaryGif``: %v\n", err)
        fmt.Fprintf(os.Stderr, "Full HTTP response: %v\n", r)
    }
    // response from `TestBinaryGif`: *os.File
    fmt.Fprintf(os.Stdout, "Response from `BodyAPI.TestBinaryGif`: %v\n", resp)
}
```

### Path Parameters

This endpoint does not need any parameter.

### Other Parameters

Other parameters are passed through a pointer to a apiTestBinaryGifRequest struct via the builder pattern


### Return type

[***os.File**](*os.File.md)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: image/gif

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints)
[[Back to Model list]](../README.md#documentation-for-models)
[[Back to README]](../README.md)


## TestBodyApplicationOctetstreamBinary

> string TestBodyApplicationOctetstreamBinary(ctx).Body(body).Execute()

Test body parameter(s)



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
    body := os.NewFile(1234, "some_file") // *os.File |  (optional)

    configuration := openapiclient.NewConfiguration()
    apiClient := openapiclient.NewAPIClient(configuration)
    resp, r, err := apiClient.BodyAPI.TestBodyApplicationOctetstreamBinary(context.Background()).Body(body).Execute()
    if err != nil {
        fmt.Fprintf(os.Stderr, "Error when calling `BodyAPI.TestBodyApplicationOctetstreamBinary``: %v\n", err)
        fmt.Fprintf(os.Stderr, "Full HTTP response: %v\n", r)
    }
    // response from `TestBodyApplicationOctetstreamBinary`: string
    fmt.Fprintf(os.Stdout, "Response from `BodyAPI.TestBodyApplicationOctetstreamBinary`: %v\n", resp)
}
```

### Path Parameters



### Other Parameters

Other parameters are passed through a pointer to a apiTestBodyApplicationOctetstreamBinaryRequest struct via the builder pattern


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | ***os.File** |  | 

### Return type

**string**

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: application/octet-stream
- **Accept**: text/plain

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints)
[[Back to Model list]](../README.md#documentation-for-models)
[[Back to README]](../README.md)


## TestBodyMultipartFormdataArrayOfBinary

> string TestBodyMultipartFormdataArrayOfBinary(ctx).Files(files).Execute()

Test array of binary in multipart mime



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
    files := []*os.File{"TODO"} // []*os.File | 

    configuration := openapiclient.NewConfiguration()
    apiClient := openapiclient.NewAPIClient(configuration)
    resp, r, err := apiClient.BodyAPI.TestBodyMultipartFormdataArrayOfBinary(context.Background()).Files(files).Execute()
    if err != nil {
        fmt.Fprintf(os.Stderr, "Error when calling `BodyAPI.TestBodyMultipartFormdataArrayOfBinary``: %v\n", err)
        fmt.Fprintf(os.Stderr, "Full HTTP response: %v\n", r)
    }
    // response from `TestBodyMultipartFormdataArrayOfBinary`: string
    fmt.Fprintf(os.Stdout, "Response from `BodyAPI.TestBodyMultipartFormdataArrayOfBinary`: %v\n", resp)
}
```

### Path Parameters



### Other Parameters

Other parameters are passed through a pointer to a apiTestBodyMultipartFormdataArrayOfBinaryRequest struct via the builder pattern


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **files** | **[]*os.File** |  | 

### Return type

**string**

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: multipart/form-data
- **Accept**: text/plain

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints)
[[Back to Model list]](../README.md#documentation-for-models)
[[Back to README]](../README.md)


## TestEchoBodyFreeFormObjectResponseString

> string TestEchoBodyFreeFormObjectResponseString(ctx).Body(body).Execute()

Test free form object



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
    body := map[string]interface{}{ ... } // map[string]interface{} | Free form object (optional)

    configuration := openapiclient.NewConfiguration()
    apiClient := openapiclient.NewAPIClient(configuration)
    resp, r, err := apiClient.BodyAPI.TestEchoBodyFreeFormObjectResponseString(context.Background()).Body(body).Execute()
    if err != nil {
        fmt.Fprintf(os.Stderr, "Error when calling `BodyAPI.TestEchoBodyFreeFormObjectResponseString``: %v\n", err)
        fmt.Fprintf(os.Stderr, "Full HTTP response: %v\n", r)
    }
    // response from `TestEchoBodyFreeFormObjectResponseString`: string
    fmt.Fprintf(os.Stdout, "Response from `BodyAPI.TestEchoBodyFreeFormObjectResponseString`: %v\n", resp)
}
```

### Path Parameters



### Other Parameters

Other parameters are passed through a pointer to a apiTestEchoBodyFreeFormObjectResponseStringRequest struct via the builder pattern


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | **map[string]interface{}** | Free form object | 

### Return type

**string**

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: application/json
- **Accept**: text/plain

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints)
[[Back to Model list]](../README.md#documentation-for-models)
[[Back to README]](../README.md)


## TestEchoBodyPet

> Pet TestEchoBodyPet(ctx).Pet(pet).Execute()

Test body parameter(s)



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
    pet := *openapiclient.NewPet("doggie", []string{"PhotoUrls_example"}) // Pet | Pet object that needs to be added to the store (optional)

    configuration := openapiclient.NewConfiguration()
    apiClient := openapiclient.NewAPIClient(configuration)
    resp, r, err := apiClient.BodyAPI.TestEchoBodyPet(context.Background()).Pet(pet).Execute()
    if err != nil {
        fmt.Fprintf(os.Stderr, "Error when calling `BodyAPI.TestEchoBodyPet``: %v\n", err)
        fmt.Fprintf(os.Stderr, "Full HTTP response: %v\n", r)
    }
    // response from `TestEchoBodyPet`: Pet
    fmt.Fprintf(os.Stdout, "Response from `BodyAPI.TestEchoBodyPet`: %v\n", resp)
}
```

### Path Parameters



### Other Parameters

Other parameters are passed through a pointer to a apiTestEchoBodyPetRequest struct via the builder pattern


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **pet** | [**Pet**](Pet.md) | Pet object that needs to be added to the store | 

### Return type

[**Pet**](Pet.md)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: application/json
- **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints)
[[Back to Model list]](../README.md#documentation-for-models)
[[Back to README]](../README.md)


## TestEchoBodyPetResponseString

> string TestEchoBodyPetResponseString(ctx).Pet(pet).Execute()

Test empty response body



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
    pet := *openapiclient.NewPet("doggie", []string{"PhotoUrls_example"}) // Pet | Pet object that needs to be added to the store (optional)

    configuration := openapiclient.NewConfiguration()
    apiClient := openapiclient.NewAPIClient(configuration)
    resp, r, err := apiClient.BodyAPI.TestEchoBodyPetResponseString(context.Background()).Pet(pet).Execute()
    if err != nil {
        fmt.Fprintf(os.Stderr, "Error when calling `BodyAPI.TestEchoBodyPetResponseString``: %v\n", err)
        fmt.Fprintf(os.Stderr, "Full HTTP response: %v\n", r)
    }
    // response from `TestEchoBodyPetResponseString`: string
    fmt.Fprintf(os.Stdout, "Response from `BodyAPI.TestEchoBodyPetResponseString`: %v\n", resp)
}
```

### Path Parameters



### Other Parameters

Other parameters are passed through a pointer to a apiTestEchoBodyPetResponseStringRequest struct via the builder pattern


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **pet** | [**Pet**](Pet.md) | Pet object that needs to be added to the store | 

### Return type

**string**

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: application/json
- **Accept**: text/plain

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints)
[[Back to Model list]](../README.md#documentation-for-models)
[[Back to README]](../README.md)


## TestEchoBodyTagResponseString

> string TestEchoBodyTagResponseString(ctx).Tag(tag).Execute()

Test empty json (request body)



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
    tag := *openapiclient.NewTag() // Tag | Tag object (optional)

    configuration := openapiclient.NewConfiguration()
    apiClient := openapiclient.NewAPIClient(configuration)
    resp, r, err := apiClient.BodyAPI.TestEchoBodyTagResponseString(context.Background()).Tag(tag).Execute()
    if err != nil {
        fmt.Fprintf(os.Stderr, "Error when calling `BodyAPI.TestEchoBodyTagResponseString``: %v\n", err)
        fmt.Fprintf(os.Stderr, "Full HTTP response: %v\n", r)
    }
    // response from `TestEchoBodyTagResponseString`: string
    fmt.Fprintf(os.Stdout, "Response from `BodyAPI.TestEchoBodyTagResponseString`: %v\n", resp)
}
```

### Path Parameters



### Other Parameters

Other parameters are passed through a pointer to a apiTestEchoBodyTagResponseStringRequest struct via the builder pattern


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **tag** | [**Tag**](Tag.md) | Tag object | 

### Return type

**string**

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: application/json
- **Accept**: text/plain

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints)
[[Back to Model list]](../README.md#documentation-for-models)
[[Back to README]](../README.md)

