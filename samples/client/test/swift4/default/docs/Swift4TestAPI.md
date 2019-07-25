# Swift4TestAPI

All URIs are relative to *http://api.example.com/basePath*

Method | HTTP request | Description
------------- | ------------- | -------------
[**getAllModels**](Swift4TestAPI.md#getallmodels) | **GET** /allModels | Get all of the models


# **getAllModels**
```swift
    open class func getAllModels(clientId: String, completion: @escaping (_ data: GetAllModelsResult?, _ error: Error?) -> Void)
```

Get all of the models

This endpoint tests get a dictionary which contains examples of all of the models.

### Example 
```swift
// The following code samples are still beta. For any issue, please report via http://github.com/OpenAPITools/openapi-generator/issues/new
import TestClient

let clientId = "clientId_example" // String | id that represent the Api client

// Get all of the models
Swift4TestAPI.getAllModels(clientId: clientId) { (response, error) in
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
 **clientId** | **String** | id that represent the Api client | 

### Return type

[**GetAllModelsResult**](GetAllModelsResult.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

