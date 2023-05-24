# AnotherFakeAPI

All URIs are relative to *http://petstore.swagger.io:80/v2*

Method | HTTP request | Description
------------- | ------------- | -------------
[**call123testSpecialTags**](AnotherFakeAPI.md#call123testspecialtags) | **PATCH** /another-fake/dummy | To test special tags


# **call123testSpecialTags**
```swift
    open class func call123testSpecialTags(body: Client, headers: HTTPHeaders = PetstoreClientAPI.customHeaders, beforeSend: (inout ClientRequest) throws -> () = { _ in }) -> EventLoopFuture<Call123testSpecialTags>
```

To test special tags

To test special tags and operation ID starting with number

### Example
```swift
// The following code samples are still beta. For any issue, please report via http://github.com/OpenAPITools/openapi-generator/issues/new
import PetstoreClient

let body = Client(client: "client_example") // Client | client model

// To test special tags
AnotherFakeAPI.call123testSpecialTags(body: body).whenComplete { result in
    switch result {
    case .failure(let error):
    // process error
    case .success(let response):
        switch response {
        // process decoded response value or raw ClientResponse
        case .http200(let value, let raw):
        case .http0(let value, let raw):
        }
    }
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | [**Client**](Client.md) | client model | 

### Return type

#### Call123testSpecialTags

```swift
public enum Call123testSpecialTags {
    case http200(value: Client?, raw: ClientResponse)
    case http0(value: Client?, raw: ClientResponse)
}
```

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

