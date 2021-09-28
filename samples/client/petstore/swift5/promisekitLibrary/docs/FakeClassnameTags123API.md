# FakeClassnameTags123API

All URIs are relative to *http://petstore.swagger.io:80/v2*

Method | HTTP request | Description
------------- | ------------- | -------------
[**testClassname**](FakeClassnameTags123API.md#testclassname) | **PATCH** /fake_classname_test | To test class name in snake case


# **testClassname**
```swift
    open class func testClassname( body: Client) -> Promise<Client>
```

To test class name in snake case

To test class name in snake case

### Example
```swift
// The following code samples are still beta. For any issue, please report via http://github.com/OpenAPITools/openapi-generator/issues/new
import PetstoreClient

let body = Client(client: "client_example") // Client | client model

// To test class name in snake case
FakeClassnameTags123API.testClassname(body: body).then {
         // when the promise is fulfilled
     }.always {
         // regardless of whether the promise is fulfilled, or rejected
     }.catch { errorType in
         // when the promise is rejected
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | [**Client**](Client.md) | client model | 

### Return type

[**Client**](Client.md)

### Authorization

[api_key_query](../README.md#api_key_query)

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

