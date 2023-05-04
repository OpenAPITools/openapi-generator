# PetsAPI

All URIs are relative to *http://localhost*

Method | HTTP request | Description
------------- | ------------- | -------------
[**petsGet**](PetsAPI.md#petsget) | **GET** /pets | 
[**showPetById**](PetsAPI.md#showpetbyid) | **GET** /pets/{petId} | Info for a specific pet


# **petsGet**
```swift
    open class func petsGet(completion: @escaping (_ data: PrefixPetSuffix?, _ error: Error?) -> Void)
```



### Example
```swift
// The following code samples are still beta. For any issue, please report via http://github.com/OpenAPITools/openapi-generator/issues/new
import PetstoreClient


PetsAPI.petsGet() { (response, error) in
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
This endpoint does not need any parameter.

### Return type

[**PrefixPetSuffix**](PrefixPetSuffix.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **showPetById**
```swift
    open class func showPetById(petId: String, completion: @escaping (_ data: AnyCodable?, _ error: Error?) -> Void)
```

Info for a specific pet

### Example
```swift
// The following code samples are still beta. For any issue, please report via http://github.com/OpenAPITools/openapi-generator/issues/new
import PetstoreClient

let petId = "petId_example" // String | The id of the pet to retrieve

// Info for a specific pet
PetsAPI.showPetById(petId: petId) { (response, error) in
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
 **petId** | **String** | The id of the pet to retrieve | 

### Return type

**AnyCodable**

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

