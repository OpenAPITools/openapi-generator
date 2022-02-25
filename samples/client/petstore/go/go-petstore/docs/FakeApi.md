# \FakeApi

All URIs are relative to *http://petstore.swagger.io:80/v2*

Method | HTTP request | Description
------------- | ------------- | -------------
[**CreateXmlItem**](FakeApi.md#CreateXmlItem) | **Post** /fake/create_xml_item | creates an XmlItem
[**FakeOuterBooleanSerialize**](FakeApi.md#FakeOuterBooleanSerialize) | **Post** /fake/outer/boolean | 
[**FakeOuterCompositeSerialize**](FakeApi.md#FakeOuterCompositeSerialize) | **Post** /fake/outer/composite | 
[**FakeOuterNumberSerialize**](FakeApi.md#FakeOuterNumberSerialize) | **Post** /fake/outer/number | 
[**FakeOuterStringSerialize**](FakeApi.md#FakeOuterStringSerialize) | **Post** /fake/outer/string | 
[**TestBodyWithFileSchema**](FakeApi.md#TestBodyWithFileSchema) | **Put** /fake/body-with-file-schema | 
[**TestBodyWithQueryParams**](FakeApi.md#TestBodyWithQueryParams) | **Put** /fake/body-with-query-params | 
[**TestClientModel**](FakeApi.md#TestClientModel) | **Patch** /fake | To test \&quot;client\&quot; model
[**TestEndpointParameters**](FakeApi.md#TestEndpointParameters) | **Post** /fake | Fake endpoint for testing various parameters  假端點  偽のエンドポイント  가짜 엔드 포인트
[**TestEnumParameters**](FakeApi.md#TestEnumParameters) | **Get** /fake | To test enum parameters
[**TestGroupParameters**](FakeApi.md#TestGroupParameters) | **Delete** /fake | Fake endpoint to test group parameters (optional)
[**TestInlineAdditionalProperties**](FakeApi.md#TestInlineAdditionalProperties) | **Post** /fake/inline-additionalProperties | test inline additionalProperties
[**TestJsonFormData**](FakeApi.md#TestJsonFormData) | **Get** /fake/jsonFormData | test json serialization of form data
[**TestQueryParameterCollectionFormat**](FakeApi.md#TestQueryParameterCollectionFormat) | **Put** /fake/test-query-parameters | 



## CreateXmlItem

> CreateXmlItem(ctx).XmlItem(xmlItem).Execute()

creates an XmlItem



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
    xmlItem := *openapiclient.NewXmlItem() // XmlItem | XmlItem Body

    configuration := openapiclient.NewConfiguration()
    apiClient := openapiclient.NewAPIClient(configuration)
    resp, r, err := apiClient.FakeApi.CreateXmlItem(context.Background()).XmlItem(xmlItem).Execute()
    if err != nil {
        fmt.Fprintf(os.Stderr, "Error when calling `FakeApi.CreateXmlItem``: %v\n", err)
        fmt.Fprintf(os.Stderr, "Full HTTP response: %v\n", r)
    }
}
```

### Path Parameters



### Other Parameters

Other parameters are passed through a pointer to a apiCreateXmlItemRequest struct via the builder pattern


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **xmlItem** | [**XmlItem**](XmlItem.md) | XmlItem Body | 

### Return type

 (empty response body)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: application/xml, application/xml; charset=utf-8, application/xml; charset=utf-16, text/xml, text/xml; charset=utf-8, text/xml; charset=utf-16
- **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints)
[[Back to Model list]](../README.md#documentation-for-models)
[[Back to README]](../README.md)


## FakeOuterBooleanSerialize

> bool FakeOuterBooleanSerialize(ctx).Body(body).Execute()





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
    body := true // bool | Input boolean as post body (optional)

    configuration := openapiclient.NewConfiguration()
    apiClient := openapiclient.NewAPIClient(configuration)
    resp, r, err := apiClient.FakeApi.FakeOuterBooleanSerialize(context.Background()).Body(body).Execute()
    if err != nil {
        fmt.Fprintf(os.Stderr, "Error when calling `FakeApi.FakeOuterBooleanSerialize``: %v\n", err)
        fmt.Fprintf(os.Stderr, "Full HTTP response: %v\n", r)
    }
    // response from `FakeOuterBooleanSerialize`: bool
    fmt.Fprintf(os.Stdout, "Response from `FakeApi.FakeOuterBooleanSerialize`: %v\n", resp)
}
```

### Path Parameters



### Other Parameters

Other parameters are passed through a pointer to a apiFakeOuterBooleanSerializeRequest struct via the builder pattern


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | **bool** | Input boolean as post body | 

### Return type

**bool**

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: */*

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints)
[[Back to Model list]](../README.md#documentation-for-models)
[[Back to README]](../README.md)


## FakeOuterCompositeSerialize

> OuterComposite FakeOuterCompositeSerialize(ctx).Body(body).Execute()





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
    body := *openapiclient.NewOuterComposite() // OuterComposite | Input composite as post body (optional)

    configuration := openapiclient.NewConfiguration()
    apiClient := openapiclient.NewAPIClient(configuration)
    resp, r, err := apiClient.FakeApi.FakeOuterCompositeSerialize(context.Background()).Body(body).Execute()
    if err != nil {
        fmt.Fprintf(os.Stderr, "Error when calling `FakeApi.FakeOuterCompositeSerialize``: %v\n", err)
        fmt.Fprintf(os.Stderr, "Full HTTP response: %v\n", r)
    }
    // response from `FakeOuterCompositeSerialize`: OuterComposite
    fmt.Fprintf(os.Stdout, "Response from `FakeApi.FakeOuterCompositeSerialize`: %v\n", resp)
}
```

### Path Parameters



### Other Parameters

Other parameters are passed through a pointer to a apiFakeOuterCompositeSerializeRequest struct via the builder pattern


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | [**OuterComposite**](OuterComposite.md) | Input composite as post body | 

### Return type

[**OuterComposite**](OuterComposite.md)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: */*

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints)
[[Back to Model list]](../README.md#documentation-for-models)
[[Back to README]](../README.md)


## FakeOuterNumberSerialize

> float32 FakeOuterNumberSerialize(ctx).Body(body).Execute()





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
    body := float32(8.14) // float32 | Input number as post body (optional)

    configuration := openapiclient.NewConfiguration()
    apiClient := openapiclient.NewAPIClient(configuration)
    resp, r, err := apiClient.FakeApi.FakeOuterNumberSerialize(context.Background()).Body(body).Execute()
    if err != nil {
        fmt.Fprintf(os.Stderr, "Error when calling `FakeApi.FakeOuterNumberSerialize``: %v\n", err)
        fmt.Fprintf(os.Stderr, "Full HTTP response: %v\n", r)
    }
    // response from `FakeOuterNumberSerialize`: float32
    fmt.Fprintf(os.Stdout, "Response from `FakeApi.FakeOuterNumberSerialize`: %v\n", resp)
}
```

### Path Parameters



### Other Parameters

Other parameters are passed through a pointer to a apiFakeOuterNumberSerializeRequest struct via the builder pattern


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | **float32** | Input number as post body | 

### Return type

**float32**

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: */*

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints)
[[Back to Model list]](../README.md#documentation-for-models)
[[Back to README]](../README.md)


## FakeOuterStringSerialize

> string FakeOuterStringSerialize(ctx).Body(body).Execute()





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
    body := "body_example" // string | Input string as post body (optional)

    configuration := openapiclient.NewConfiguration()
    apiClient := openapiclient.NewAPIClient(configuration)
    resp, r, err := apiClient.FakeApi.FakeOuterStringSerialize(context.Background()).Body(body).Execute()
    if err != nil {
        fmt.Fprintf(os.Stderr, "Error when calling `FakeApi.FakeOuterStringSerialize``: %v\n", err)
        fmt.Fprintf(os.Stderr, "Full HTTP response: %v\n", r)
    }
    // response from `FakeOuterStringSerialize`: string
    fmt.Fprintf(os.Stdout, "Response from `FakeApi.FakeOuterStringSerialize`: %v\n", resp)
}
```

### Path Parameters



### Other Parameters

Other parameters are passed through a pointer to a apiFakeOuterStringSerializeRequest struct via the builder pattern


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | **string** | Input string as post body | 

### Return type

**string**

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: */*

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints)
[[Back to Model list]](../README.md#documentation-for-models)
[[Back to README]](../README.md)


## TestBodyWithFileSchema

> TestBodyWithFileSchema(ctx).Body(body).Execute()





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
    body := *openapiclient.NewFileSchemaTestClass() // FileSchemaTestClass | 

    configuration := openapiclient.NewConfiguration()
    apiClient := openapiclient.NewAPIClient(configuration)
    resp, r, err := apiClient.FakeApi.TestBodyWithFileSchema(context.Background()).Body(body).Execute()
    if err != nil {
        fmt.Fprintf(os.Stderr, "Error when calling `FakeApi.TestBodyWithFileSchema``: %v\n", err)
        fmt.Fprintf(os.Stderr, "Full HTTP response: %v\n", r)
    }
}
```

### Path Parameters



### Other Parameters

Other parameters are passed through a pointer to a apiTestBodyWithFileSchemaRequest struct via the builder pattern


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | [**FileSchemaTestClass**](FileSchemaTestClass.md) |  | 

### Return type

 (empty response body)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: application/json
- **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints)
[[Back to Model list]](../README.md#documentation-for-models)
[[Back to README]](../README.md)


## TestBodyWithQueryParams

> TestBodyWithQueryParams(ctx).Query(query).Body(body).Execute()



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
    query := "query_example" // string | 
    body := *openapiclient.NewUser() // User | 

    configuration := openapiclient.NewConfiguration()
    apiClient := openapiclient.NewAPIClient(configuration)
    resp, r, err := apiClient.FakeApi.TestBodyWithQueryParams(context.Background()).Query(query).Body(body).Execute()
    if err != nil {
        fmt.Fprintf(os.Stderr, "Error when calling `FakeApi.TestBodyWithQueryParams``: %v\n", err)
        fmt.Fprintf(os.Stderr, "Full HTTP response: %v\n", r)
    }
}
```

### Path Parameters



### Other Parameters

Other parameters are passed through a pointer to a apiTestBodyWithQueryParamsRequest struct via the builder pattern


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **query** | **string** |  | 
 **body** | [**User**](User.md) |  | 

### Return type

 (empty response body)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: application/json
- **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints)
[[Back to Model list]](../README.md#documentation-for-models)
[[Back to README]](../README.md)


## TestClientModel

> Client TestClientModel(ctx).Body(body).Execute()

To test \"client\" model



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
    body := *openapiclient.NewClient() // Client | client model

    configuration := openapiclient.NewConfiguration()
    apiClient := openapiclient.NewAPIClient(configuration)
    resp, r, err := apiClient.FakeApi.TestClientModel(context.Background()).Body(body).Execute()
    if err != nil {
        fmt.Fprintf(os.Stderr, "Error when calling `FakeApi.TestClientModel``: %v\n", err)
        fmt.Fprintf(os.Stderr, "Full HTTP response: %v\n", r)
    }
    // response from `TestClientModel`: Client
    fmt.Fprintf(os.Stdout, "Response from `FakeApi.TestClientModel`: %v\n", resp)
}
```

### Path Parameters



### Other Parameters

Other parameters are passed through a pointer to a apiTestClientModelRequest struct via the builder pattern


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | [**Client**](Client.md) | client model | 

### Return type

[**Client**](Client.md)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: application/json
- **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints)
[[Back to Model list]](../README.md#documentation-for-models)
[[Back to README]](../README.md)


## TestEndpointParameters

> TestEndpointParameters(ctx).Number(number).Double(double).PatternWithoutDelimiter(patternWithoutDelimiter).Byte_(byte_).Integer(integer).Int32_(int32_).Int64_(int64_).Float(float).String_(string_).Binary(binary).Date(date).DateTime(dateTime).Password(password).Callback(callback).Execute()

Fake endpoint for testing various parameters  假端點  偽のエンドポイント  가짜 엔드 포인트



### Example

```go
package main

import (
    "context"
    "fmt"
    "os"
    "time"
    openapiclient "./openapi"
)

func main() {
    number := float32(8.14) // float32 | None
    double := float64(1.2) // float64 | None
    patternWithoutDelimiter := "patternWithoutDelimiter_example" // string | None
    byte_ := string(BYTE_ARRAY_DATA_HERE) // string | None
    integer := int32(56) // int32 | None (optional)
    int32_ := int32(56) // int32 | None (optional)
    int64_ := int64(789) // int64 | None (optional)
    float := float32(3.4) // float32 | None (optional)
    string_ := "string__example" // string | None (optional)
    binary := os.NewFile(1234, "some_file") // *os.File | None (optional)
    date := time.Now() // string | None (optional)
    dateTime := time.Now() // time.Time | None (optional)
    password := "password_example" // string | None (optional)
    callback := "callback_example" // string | None (optional)

    configuration := openapiclient.NewConfiguration()
    apiClient := openapiclient.NewAPIClient(configuration)
    resp, r, err := apiClient.FakeApi.TestEndpointParameters(context.Background()).Number(number).Double(double).PatternWithoutDelimiter(patternWithoutDelimiter).Byte_(byte_).Integer(integer).Int32_(int32_).Int64_(int64_).Float(float).String_(string_).Binary(binary).Date(date).DateTime(dateTime).Password(password).Callback(callback).Execute()
    if err != nil {
        fmt.Fprintf(os.Stderr, "Error when calling `FakeApi.TestEndpointParameters``: %v\n", err)
        fmt.Fprintf(os.Stderr, "Full HTTP response: %v\n", r)
    }
}
```

### Path Parameters



### Other Parameters

Other parameters are passed through a pointer to a apiTestEndpointParametersRequest struct via the builder pattern


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **number** | **float32** | None | 
 **double** | **float64** | None | 
 **patternWithoutDelimiter** | **string** | None | 
 **byte_** | **string** | None | 
 **integer** | **int32** | None | 
 **int32_** | **int32** | None | 
 **int64_** | **int64** | None | 
 **float** | **float32** | None | 
 **string_** | **string** | None | 
 **binary** | ***os.File** | None | 
 **date** | **string** | None | 
 **dateTime** | **time.Time** | None | 
 **password** | **string** | None | 
 **callback** | **string** | None | 

### Return type

 (empty response body)

### Authorization

[http_basic_test](../README.md#http_basic_test)

### HTTP request headers

- **Content-Type**: application/x-www-form-urlencoded
- **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints)
[[Back to Model list]](../README.md#documentation-for-models)
[[Back to README]](../README.md)


## TestEnumParameters

> TestEnumParameters(ctx).EnumHeaderStringArray(enumHeaderStringArray).EnumHeaderString(enumHeaderString).EnumQueryStringArray(enumQueryStringArray).EnumQueryString(enumQueryString).EnumQueryInteger(enumQueryInteger).EnumQueryDouble(enumQueryDouble).EnumFormStringArray(enumFormStringArray).EnumFormString(enumFormString).Execute()

To test enum parameters



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
    enumHeaderStringArray := []string{"EnumHeaderStringArray_example"} // []string | Header parameter enum test (string array) (optional)
    enumHeaderString := "enumHeaderString_example" // string | Header parameter enum test (string) (optional) (default to "-efg")
    enumQueryStringArray := []string{"EnumQueryStringArray_example"} // []string | Query parameter enum test (string array) (optional)
    enumQueryString := "enumQueryString_example" // string | Query parameter enum test (string) (optional) (default to "-efg")
    enumQueryInteger := int32(56) // int32 | Query parameter enum test (double) (optional)
    enumQueryDouble := float64(1.2) // float64 | Query parameter enum test (double) (optional)
    enumFormStringArray := []string{"Inner_example"} // []string | Form parameter enum test (string array) (optional) (default to "$")
    enumFormString := "enumFormString_example" // string | Form parameter enum test (string) (optional) (default to "-efg")

    configuration := openapiclient.NewConfiguration()
    apiClient := openapiclient.NewAPIClient(configuration)
    resp, r, err := apiClient.FakeApi.TestEnumParameters(context.Background()).EnumHeaderStringArray(enumHeaderStringArray).EnumHeaderString(enumHeaderString).EnumQueryStringArray(enumQueryStringArray).EnumQueryString(enumQueryString).EnumQueryInteger(enumQueryInteger).EnumQueryDouble(enumQueryDouble).EnumFormStringArray(enumFormStringArray).EnumFormString(enumFormString).Execute()
    if err != nil {
        fmt.Fprintf(os.Stderr, "Error when calling `FakeApi.TestEnumParameters``: %v\n", err)
        fmt.Fprintf(os.Stderr, "Full HTTP response: %v\n", r)
    }
}
```

### Path Parameters



### Other Parameters

Other parameters are passed through a pointer to a apiTestEnumParametersRequest struct via the builder pattern


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **enumHeaderStringArray** | **[]string** | Header parameter enum test (string array) | 
 **enumHeaderString** | **string** | Header parameter enum test (string) | [default to &quot;-efg&quot;]
 **enumQueryStringArray** | **[]string** | Query parameter enum test (string array) | 
 **enumQueryString** | **string** | Query parameter enum test (string) | [default to &quot;-efg&quot;]
 **enumQueryInteger** | **int32** | Query parameter enum test (double) | 
 **enumQueryDouble** | **float64** | Query parameter enum test (double) | 
 **enumFormStringArray** | **[]string** | Form parameter enum test (string array) | [default to &quot;$&quot;]
 **enumFormString** | **string** | Form parameter enum test (string) | [default to &quot;-efg&quot;]

### Return type

 (empty response body)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: application/x-www-form-urlencoded
- **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints)
[[Back to Model list]](../README.md#documentation-for-models)
[[Back to README]](../README.md)


## TestGroupParameters

> TestGroupParameters(ctx).RequiredStringGroup(requiredStringGroup).RequiredBooleanGroup(requiredBooleanGroup).RequiredInt64Group(requiredInt64Group).StringGroup(stringGroup).BooleanGroup(booleanGroup).Int64Group(int64Group).Execute()

Fake endpoint to test group parameters (optional)



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
    requiredStringGroup := int32(56) // int32 | Required String in group parameters
    requiredBooleanGroup := true // bool | Required Boolean in group parameters
    requiredInt64Group := int64(789) // int64 | Required Integer in group parameters
    stringGroup := int32(56) // int32 | String in group parameters (optional)
    booleanGroup := true // bool | Boolean in group parameters (optional)
    int64Group := int64(789) // int64 | Integer in group parameters (optional)

    configuration := openapiclient.NewConfiguration()
    apiClient := openapiclient.NewAPIClient(configuration)
    resp, r, err := apiClient.FakeApi.TestGroupParameters(context.Background()).RequiredStringGroup(requiredStringGroup).RequiredBooleanGroup(requiredBooleanGroup).RequiredInt64Group(requiredInt64Group).StringGroup(stringGroup).BooleanGroup(booleanGroup).Int64Group(int64Group).Execute()
    if err != nil {
        fmt.Fprintf(os.Stderr, "Error when calling `FakeApi.TestGroupParameters``: %v\n", err)
        fmt.Fprintf(os.Stderr, "Full HTTP response: %v\n", r)
    }
}
```

### Path Parameters



### Other Parameters

Other parameters are passed through a pointer to a apiTestGroupParametersRequest struct via the builder pattern


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **requiredStringGroup** | **int32** | Required String in group parameters | 
 **requiredBooleanGroup** | **bool** | Required Boolean in group parameters | 
 **requiredInt64Group** | **int64** | Required Integer in group parameters | 
 **stringGroup** | **int32** | String in group parameters | 
 **booleanGroup** | **bool** | Boolean in group parameters | 
 **int64Group** | **int64** | Integer in group parameters | 

### Return type

 (empty response body)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints)
[[Back to Model list]](../README.md#documentation-for-models)
[[Back to README]](../README.md)


## TestInlineAdditionalProperties

> TestInlineAdditionalProperties(ctx).Param(param).Execute()

test inline additionalProperties

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
    param := map[string]string{"key": "Inner_example"} // map[string]string | request body

    configuration := openapiclient.NewConfiguration()
    apiClient := openapiclient.NewAPIClient(configuration)
    resp, r, err := apiClient.FakeApi.TestInlineAdditionalProperties(context.Background()).Param(param).Execute()
    if err != nil {
        fmt.Fprintf(os.Stderr, "Error when calling `FakeApi.TestInlineAdditionalProperties``: %v\n", err)
        fmt.Fprintf(os.Stderr, "Full HTTP response: %v\n", r)
    }
}
```

### Path Parameters



### Other Parameters

Other parameters are passed through a pointer to a apiTestInlineAdditionalPropertiesRequest struct via the builder pattern


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **param** | **map[string]string** | request body | 

### Return type

 (empty response body)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: application/json
- **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints)
[[Back to Model list]](../README.md#documentation-for-models)
[[Back to README]](../README.md)


## TestJsonFormData

> TestJsonFormData(ctx).Param(param).Param2(param2).Execute()

test json serialization of form data

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
    param := "param_example" // string | field1
    param2 := "param2_example" // string | field2

    configuration := openapiclient.NewConfiguration()
    apiClient := openapiclient.NewAPIClient(configuration)
    resp, r, err := apiClient.FakeApi.TestJsonFormData(context.Background()).Param(param).Param2(param2).Execute()
    if err != nil {
        fmt.Fprintf(os.Stderr, "Error when calling `FakeApi.TestJsonFormData``: %v\n", err)
        fmt.Fprintf(os.Stderr, "Full HTTP response: %v\n", r)
    }
}
```

### Path Parameters



### Other Parameters

Other parameters are passed through a pointer to a apiTestJsonFormDataRequest struct via the builder pattern


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **param** | **string** | field1 | 
 **param2** | **string** | field2 | 

### Return type

 (empty response body)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: application/x-www-form-urlencoded
- **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints)
[[Back to Model list]](../README.md#documentation-for-models)
[[Back to README]](../README.md)


## TestQueryParameterCollectionFormat

> TestQueryParameterCollectionFormat(ctx).Pipe(pipe).Ioutil(ioutil).Http(http).Url(url).Context(context).Execute()





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
    pipe := []string{"Inner_example"} // []string | 
    ioutil := []string{"Inner_example"} // []string | 
    http := []string{"Inner_example"} // []string | 
    url := []string{"Inner_example"} // []string | 
    context := []string{"Inner_example"} // []string | 

    configuration := openapiclient.NewConfiguration()
    apiClient := openapiclient.NewAPIClient(configuration)
    resp, r, err := apiClient.FakeApi.TestQueryParameterCollectionFormat(context.Background()).Pipe(pipe).Ioutil(ioutil).Http(http).Url(url).Context(context).Execute()
    if err != nil {
        fmt.Fprintf(os.Stderr, "Error when calling `FakeApi.TestQueryParameterCollectionFormat``: %v\n", err)
        fmt.Fprintf(os.Stderr, "Full HTTP response: %v\n", r)
    }
}
```

### Path Parameters



### Other Parameters

Other parameters are passed through a pointer to a apiTestQueryParameterCollectionFormatRequest struct via the builder pattern


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **pipe** | **[]string** |  | 
 **ioutil** | **[]string** |  | 
 **http** | **[]string** |  | 
 **url** | **[]string** |  | 
 **context** | **[]string** |  | 

### Return type

 (empty response body)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints)
[[Back to Model list]](../README.md#documentation-for-models)
[[Back to README]](../README.md)

