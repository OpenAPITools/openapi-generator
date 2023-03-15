# StoreAPI

All URIs are relative to *http://petstore.swagger.io:80/v2*

Method | HTTP request | Description
------------- | ------------- | -------------
[**deleteOrder**](StoreAPI.md#deleteorder) | **DELETE** /store/order/{order_id} | Delete purchase order by ID
[**getInventory**](StoreAPI.md#getinventory) | **GET** /store/inventory | Returns pet inventories by status
[**getOrderById**](StoreAPI.md#getorderbyid) | **GET** /store/order/{order_id} | Find purchase order by ID
[**placeOrder**](StoreAPI.md#placeorder) | **POST** /store/order | Place an order for a pet


# **deleteOrder**
```swift
    open class func deleteOrder(orderId: String, completion: @escaping (_ data: Void?, _ error: Error?) -> Void)
```

Delete purchase order by ID

For valid response try integer IDs with value < 1000. Anything above 1000 or nonintegers will generate API errors

### Example
```swift
// The following code samples are still beta. For any issue, please report via http://github.com/OpenAPITools/openapi-generator/issues/new
import PetstoreClient

let orderId = "orderId_example" // String | ID of the order that needs to be deleted

// Delete purchase order by ID
StoreAPI.deleteOrder(orderId: orderId) { (response, error) in
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
 **orderId** | **String** | ID of the order that needs to be deleted | 

### Return type

Void (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **getInventory**
```swift
    open class func getInventory(completion: @escaping (_ data: [String: Int]?, _ error: Error?) -> Void)
```

Returns pet inventories by status

Returns a map of status codes to quantities

### Example
```swift
// The following code samples are still beta. For any issue, please report via http://github.com/OpenAPITools/openapi-generator/issues/new
import PetstoreClient


// Returns pet inventories by status
StoreAPI.getInventory() { (response, error) in
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

**[String: Int]**

### Authorization

[api_key](../README.md#api_key)

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **getOrderById**
```swift
    open class func getOrderById(orderId: Int64, completion: @escaping (_ data: Order?, _ error: Error?) -> Void)
```

Find purchase order by ID

For valid response try integer IDs with value <= 5 or > 10. Other values will generated exceptions

### Example
```swift
// The following code samples are still beta. For any issue, please report via http://github.com/OpenAPITools/openapi-generator/issues/new
import PetstoreClient

let orderId = 987 // Int64 | ID of pet that needs to be fetched

// Find purchase order by ID
StoreAPI.getOrderById(orderId: orderId) { (response, error) in
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
 **orderId** | **Int64** | ID of pet that needs to be fetched | 

### Return type

[**Order**](Order.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/xml, application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **placeOrder**
```swift
    open class func placeOrder(body: Order, completion: @escaping (_ data: Order?, _ error: Error?) -> Void)
```

Place an order for a pet

### Example
```swift
// The following code samples are still beta. For any issue, please report via http://github.com/OpenAPITools/openapi-generator/issues/new
import PetstoreClient

let body = Order(_id: 123, petId: 123, quantity: 123, shipDate: Date(), status: "status_example", complete: false) // Order | order placed for purchasing the pet

// Place an order for a pet
StoreAPI.placeOrder(body: body) { (response, error) in
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
 **body** | [**Order**](Order.md) | order placed for purchasing the pet | 

### Return type

[**Order**](Order.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/xml, application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

