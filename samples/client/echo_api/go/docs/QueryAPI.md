# \QueryAPI

All URIs are relative to *http://localhost:3000*

Method | HTTP request | Description
------------- | ------------- | -------------
[**TestEnumRefString**](QueryAPI.md#TestEnumRefString) | **Get** /query/enum_ref_string | Test query parameter(s)
[**TestQueryDatetimeDateString**](QueryAPI.md#TestQueryDatetimeDateString) | **Get** /query/datetime/date/string | Test query parameter(s)
[**TestQueryIntegerBooleanString**](QueryAPI.md#TestQueryIntegerBooleanString) | **Get** /query/integer/boolean/string | Test query parameter(s)
[**TestQueryStyleDeepObjectExplodeTrueObject**](QueryAPI.md#TestQueryStyleDeepObjectExplodeTrueObject) | **Get** /query/style_deepObject/explode_true/object | Test query parameter(s)
[**TestQueryStyleDeepObjectExplodeTrueObjectAllOf**](QueryAPI.md#TestQueryStyleDeepObjectExplodeTrueObjectAllOf) | **Get** /query/style_deepObject/explode_true/object/allOf | Test query parameter(s)
[**TestQueryStyleFormExplodeTrueArrayString**](QueryAPI.md#TestQueryStyleFormExplodeTrueArrayString) | **Get** /query/style_form/explode_true/array_string | Test query parameter(s)
[**TestQueryStyleFormExplodeTrueObject**](QueryAPI.md#TestQueryStyleFormExplodeTrueObject) | **Get** /query/style_form/explode_true/object | Test query parameter(s)
[**TestQueryStyleFormExplodeTrueObjectAllOf**](QueryAPI.md#TestQueryStyleFormExplodeTrueObjectAllOf) | **Get** /query/style_form/explode_true/object/allOf | Test query parameter(s)



## TestEnumRefString

> string TestEnumRefString(ctx).EnumNonrefStringQuery(enumNonrefStringQuery).EnumRefStringQuery(enumRefStringQuery).Execute()

Test query parameter(s)



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
    enumNonrefStringQuery := "enumNonrefStringQuery_example" // string |  (optional)
    enumRefStringQuery := openapiclient.StringEnumRef("success") // StringEnumRef |  (optional)

    configuration := openapiclient.NewConfiguration()
    apiClient := openapiclient.NewAPIClient(configuration)
    resp, r, err := apiClient.QueryAPI.TestEnumRefString(context.Background()).EnumNonrefStringQuery(enumNonrefStringQuery).EnumRefStringQuery(enumRefStringQuery).Execute()
    if err != nil {
        fmt.Fprintf(os.Stderr, "Error when calling `QueryAPI.TestEnumRefString``: %v\n", err)
        fmt.Fprintf(os.Stderr, "Full HTTP response: %v\n", r)
    }
    // response from `TestEnumRefString`: string
    fmt.Fprintf(os.Stdout, "Response from `QueryAPI.TestEnumRefString`: %v\n", resp)
}
```

### Path Parameters



### Other Parameters

Other parameters are passed through a pointer to a apiTestEnumRefStringRequest struct via the builder pattern


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **enumNonrefStringQuery** | **string** |  | 
 **enumRefStringQuery** | [**StringEnumRef**](StringEnumRef.md) |  | 

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


## TestQueryDatetimeDateString

> string TestQueryDatetimeDateString(ctx).DatetimeQuery(datetimeQuery).DateQuery(dateQuery).StringQuery(stringQuery).Execute()

Test query parameter(s)



### Example

```go
package main

import (
    "context"
    "fmt"
    "os"
    "time"
    openapiclient "github.com/GIT_USER_ID/GIT_REPO_ID"
)

func main() {
    datetimeQuery := time.Now() // time.Time |  (optional)
    dateQuery := time.Now() // string |  (optional)
    stringQuery := "stringQuery_example" // string |  (optional)

    configuration := openapiclient.NewConfiguration()
    apiClient := openapiclient.NewAPIClient(configuration)
    resp, r, err := apiClient.QueryAPI.TestQueryDatetimeDateString(context.Background()).DatetimeQuery(datetimeQuery).DateQuery(dateQuery).StringQuery(stringQuery).Execute()
    if err != nil {
        fmt.Fprintf(os.Stderr, "Error when calling `QueryAPI.TestQueryDatetimeDateString``: %v\n", err)
        fmt.Fprintf(os.Stderr, "Full HTTP response: %v\n", r)
    }
    // response from `TestQueryDatetimeDateString`: string
    fmt.Fprintf(os.Stdout, "Response from `QueryAPI.TestQueryDatetimeDateString`: %v\n", resp)
}
```

### Path Parameters



### Other Parameters

Other parameters are passed through a pointer to a apiTestQueryDatetimeDateStringRequest struct via the builder pattern


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **datetimeQuery** | **time.Time** |  | 
 **dateQuery** | **string** |  | 
 **stringQuery** | **string** |  | 

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


## TestQueryIntegerBooleanString

> string TestQueryIntegerBooleanString(ctx).IntegerQuery(integerQuery).BooleanQuery(booleanQuery).StringQuery(stringQuery).Execute()

Test query parameter(s)



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
    integerQuery := int32(56) // int32 |  (optional)
    booleanQuery := true // bool |  (optional)
    stringQuery := "stringQuery_example" // string |  (optional)

    configuration := openapiclient.NewConfiguration()
    apiClient := openapiclient.NewAPIClient(configuration)
    resp, r, err := apiClient.QueryAPI.TestQueryIntegerBooleanString(context.Background()).IntegerQuery(integerQuery).BooleanQuery(booleanQuery).StringQuery(stringQuery).Execute()
    if err != nil {
        fmt.Fprintf(os.Stderr, "Error when calling `QueryAPI.TestQueryIntegerBooleanString``: %v\n", err)
        fmt.Fprintf(os.Stderr, "Full HTTP response: %v\n", r)
    }
    // response from `TestQueryIntegerBooleanString`: string
    fmt.Fprintf(os.Stdout, "Response from `QueryAPI.TestQueryIntegerBooleanString`: %v\n", resp)
}
```

### Path Parameters



### Other Parameters

Other parameters are passed through a pointer to a apiTestQueryIntegerBooleanStringRequest struct via the builder pattern


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **integerQuery** | **int32** |  | 
 **booleanQuery** | **bool** |  | 
 **stringQuery** | **string** |  | 

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


## TestQueryStyleDeepObjectExplodeTrueObject

> string TestQueryStyleDeepObjectExplodeTrueObject(ctx).QueryObject(queryObject).Execute()

Test query parameter(s)



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
    queryObject := *openapiclient.NewPet("doggie", []string{"PhotoUrls_example"}) // Pet |  (optional)

    configuration := openapiclient.NewConfiguration()
    apiClient := openapiclient.NewAPIClient(configuration)
    resp, r, err := apiClient.QueryAPI.TestQueryStyleDeepObjectExplodeTrueObject(context.Background()).QueryObject(queryObject).Execute()
    if err != nil {
        fmt.Fprintf(os.Stderr, "Error when calling `QueryAPI.TestQueryStyleDeepObjectExplodeTrueObject``: %v\n", err)
        fmt.Fprintf(os.Stderr, "Full HTTP response: %v\n", r)
    }
    // response from `TestQueryStyleDeepObjectExplodeTrueObject`: string
    fmt.Fprintf(os.Stdout, "Response from `QueryAPI.TestQueryStyleDeepObjectExplodeTrueObject`: %v\n", resp)
}
```

### Path Parameters



### Other Parameters

Other parameters are passed through a pointer to a apiTestQueryStyleDeepObjectExplodeTrueObjectRequest struct via the builder pattern


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **queryObject** | [**Pet**](Pet.md) |  | 

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


## TestQueryStyleDeepObjectExplodeTrueObjectAllOf

> string TestQueryStyleDeepObjectExplodeTrueObjectAllOf(ctx).QueryObject(queryObject).Execute()

Test query parameter(s)



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
    queryObject := *openapiclient.NewTestQueryStyleDeepObjectExplodeTrueObjectAllOfQueryObjectParameter() // TestQueryStyleDeepObjectExplodeTrueObjectAllOfQueryObjectParameter |  (optional)

    configuration := openapiclient.NewConfiguration()
    apiClient := openapiclient.NewAPIClient(configuration)
    resp, r, err := apiClient.QueryAPI.TestQueryStyleDeepObjectExplodeTrueObjectAllOf(context.Background()).QueryObject(queryObject).Execute()
    if err != nil {
        fmt.Fprintf(os.Stderr, "Error when calling `QueryAPI.TestQueryStyleDeepObjectExplodeTrueObjectAllOf``: %v\n", err)
        fmt.Fprintf(os.Stderr, "Full HTTP response: %v\n", r)
    }
    // response from `TestQueryStyleDeepObjectExplodeTrueObjectAllOf`: string
    fmt.Fprintf(os.Stdout, "Response from `QueryAPI.TestQueryStyleDeepObjectExplodeTrueObjectAllOf`: %v\n", resp)
}
```

### Path Parameters



### Other Parameters

Other parameters are passed through a pointer to a apiTestQueryStyleDeepObjectExplodeTrueObjectAllOfRequest struct via the builder pattern


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **queryObject** | [**TestQueryStyleDeepObjectExplodeTrueObjectAllOfQueryObjectParameter**](TestQueryStyleDeepObjectExplodeTrueObjectAllOfQueryObjectParameter.md) |  | 

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


## TestQueryStyleFormExplodeTrueArrayString

> string TestQueryStyleFormExplodeTrueArrayString(ctx).QueryObject(queryObject).Execute()

Test query parameter(s)



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
    queryObject := *openapiclient.NewTestQueryStyleFormExplodeTrueArrayStringQueryObjectParameter() // TestQueryStyleFormExplodeTrueArrayStringQueryObjectParameter |  (optional)

    configuration := openapiclient.NewConfiguration()
    apiClient := openapiclient.NewAPIClient(configuration)
    resp, r, err := apiClient.QueryAPI.TestQueryStyleFormExplodeTrueArrayString(context.Background()).QueryObject(queryObject).Execute()
    if err != nil {
        fmt.Fprintf(os.Stderr, "Error when calling `QueryAPI.TestQueryStyleFormExplodeTrueArrayString``: %v\n", err)
        fmt.Fprintf(os.Stderr, "Full HTTP response: %v\n", r)
    }
    // response from `TestQueryStyleFormExplodeTrueArrayString`: string
    fmt.Fprintf(os.Stdout, "Response from `QueryAPI.TestQueryStyleFormExplodeTrueArrayString`: %v\n", resp)
}
```

### Path Parameters



### Other Parameters

Other parameters are passed through a pointer to a apiTestQueryStyleFormExplodeTrueArrayStringRequest struct via the builder pattern


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **queryObject** | [**TestQueryStyleFormExplodeTrueArrayStringQueryObjectParameter**](TestQueryStyleFormExplodeTrueArrayStringQueryObjectParameter.md) |  | 

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


## TestQueryStyleFormExplodeTrueObject

> string TestQueryStyleFormExplodeTrueObject(ctx).QueryObject(queryObject).Execute()

Test query parameter(s)



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
    queryObject := *openapiclient.NewPet("doggie", []string{"PhotoUrls_example"}) // Pet |  (optional)

    configuration := openapiclient.NewConfiguration()
    apiClient := openapiclient.NewAPIClient(configuration)
    resp, r, err := apiClient.QueryAPI.TestQueryStyleFormExplodeTrueObject(context.Background()).QueryObject(queryObject).Execute()
    if err != nil {
        fmt.Fprintf(os.Stderr, "Error when calling `QueryAPI.TestQueryStyleFormExplodeTrueObject``: %v\n", err)
        fmt.Fprintf(os.Stderr, "Full HTTP response: %v\n", r)
    }
    // response from `TestQueryStyleFormExplodeTrueObject`: string
    fmt.Fprintf(os.Stdout, "Response from `QueryAPI.TestQueryStyleFormExplodeTrueObject`: %v\n", resp)
}
```

### Path Parameters



### Other Parameters

Other parameters are passed through a pointer to a apiTestQueryStyleFormExplodeTrueObjectRequest struct via the builder pattern


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **queryObject** | [**Pet**](Pet.md) |  | 

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


## TestQueryStyleFormExplodeTrueObjectAllOf

> string TestQueryStyleFormExplodeTrueObjectAllOf(ctx).QueryObject(queryObject).Execute()

Test query parameter(s)



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
    queryObject := *openapiclient.NewDataQuery() // DataQuery |  (optional)

    configuration := openapiclient.NewConfiguration()
    apiClient := openapiclient.NewAPIClient(configuration)
    resp, r, err := apiClient.QueryAPI.TestQueryStyleFormExplodeTrueObjectAllOf(context.Background()).QueryObject(queryObject).Execute()
    if err != nil {
        fmt.Fprintf(os.Stderr, "Error when calling `QueryAPI.TestQueryStyleFormExplodeTrueObjectAllOf``: %v\n", err)
        fmt.Fprintf(os.Stderr, "Full HTTP response: %v\n", r)
    }
    // response from `TestQueryStyleFormExplodeTrueObjectAllOf`: string
    fmt.Fprintf(os.Stdout, "Response from `QueryAPI.TestQueryStyleFormExplodeTrueObjectAllOf`: %v\n", resp)
}
```

### Path Parameters



### Other Parameters

Other parameters are passed through a pointer to a apiTestQueryStyleFormExplodeTrueObjectAllOfRequest struct via the builder pattern


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **queryObject** | [**DataQuery**](DataQuery.md) |  | 

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

