# AnotherFakeAPI

All URIs are relative to *http://petstore.swagger.io:80/v2*

Method | HTTP request | Description
------------- | ------------- | -------------
[**call123testSpecialTags**](AnotherFakeAPI.md#call123testspecialtags) | **PATCH** /another-fake/dummy | To test special tags


# **call123testSpecialTags**
```swift
    open class func call123testSpecialTags(body: Client, completion: @escaping (_ data: Client?, _ error: Error?) -> Void)
```

To test special tags

To test special tags and operation ID starting with number

### Example
```swift
// The following code samples are still beta. For any issue, please report via http://github.com/OpenAPITools/openapi-generator/issues/new
import PetstoreClient

let body = Client(client: "client_example") // Client | client model

// To test special tags
AnotherFakeAPI.call123testSpecialTags(body: body) { (response, error) in
    guard error == nil else {
        print(error)
        return
    }

    if (response) {
        dump(response)
    }
}
```

### Parameters

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

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

