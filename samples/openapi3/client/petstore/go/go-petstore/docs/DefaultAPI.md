# \DefaultAPI

All URIs are relative to *http://petstore.swagger.io:80/v2*

Method | HTTP request | Description
------------- | ------------- | -------------
[**FooGet**](DefaultAPI.md#FooGet) | **Get** /foo | 
[**SomeOpsRequiringRefInt**](DefaultAPI.md#SomeOpsRequiringRefInt) | **Post** /fake/wrapped-integer-ref | 



## FooGet

> FooGetDefaultResponse FooGet(ctx).Execute()



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
	resp, r, err := apiClient.DefaultAPI.FooGet(context.Background()).Execute()
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error when calling `DefaultAPI.FooGet``: %v\n", err)
		fmt.Fprintf(os.Stderr, "Full HTTP response: %v\n", r)
	}
	// response from `FooGet`: FooGetDefaultResponse
	fmt.Fprintf(os.Stdout, "Response from `DefaultAPI.FooGet`: %v\n", resp)
}
```

### Path Parameters

This endpoint does not need any parameter.

### Other Parameters

Other parameters are passed through a pointer to a apiFooGetRequest struct via the builder pattern


### Return type

[**FooGetDefaultResponse**](FooGetDefaultResponse.md)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints)
[[Back to Model list]](../README.md#documentation-for-models)
[[Back to README]](../README.md)


## SomeOpsRequiringRefInt

> map[string]interface{} SomeOpsRequiringRefInt(ctx).SomeOpsRequiringRefIntRequest(someOpsRequiringRefIntRequest).Execute()



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
	someOpsRequiringRefIntRequest := *openapiclient.NewSomeOpsRequiringRefIntRequest() // SomeOpsRequiringRefIntRequest | 

	configuration := openapiclient.NewConfiguration()
	apiClient := openapiclient.NewAPIClient(configuration)
	resp, r, err := apiClient.DefaultAPI.SomeOpsRequiringRefInt(context.Background()).SomeOpsRequiringRefIntRequest(someOpsRequiringRefIntRequest).Execute()
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error when calling `DefaultAPI.SomeOpsRequiringRefInt``: %v\n", err)
		fmt.Fprintf(os.Stderr, "Full HTTP response: %v\n", r)
	}
	// response from `SomeOpsRequiringRefInt`: map[string]interface{}
	fmt.Fprintf(os.Stdout, "Response from `DefaultAPI.SomeOpsRequiringRefInt`: %v\n", resp)
}
```

### Path Parameters



### Other Parameters

Other parameters are passed through a pointer to a apiSomeOpsRequiringRefIntRequest struct via the builder pattern


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **someOpsRequiringRefIntRequest** | [**SomeOpsRequiringRefIntRequest**](SomeOpsRequiringRefIntRequest.md) |  | 

### Return type

**map[string]interface{}**

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: application/json
- **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints)
[[Back to Model list]](../README.md#documentation-for-models)
[[Back to README]](../README.md)

