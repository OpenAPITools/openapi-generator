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
    open class func deleteOrder(orderId: String, headers: HTTPHeaders = PetstoreClientAPI.customHeaders, beforeSend: (inout ClientRequest) throws -> () = { _ in }) -> EventLoopFuture<DeleteOrder>
```

Delete purchase order by ID

For valid response try integer IDs with value < 1000. Anything above 1000 or nonintegers will generate API errors

### Example
```swift
// The following code samples are still beta. For any issue, please report via http://github.com/OpenAPITools/openapi-generator/issues/new
import PetstoreClient

let orderId = "orderId_example" // String | ID of the order that needs to be deleted

// Delete purchase order by ID
StoreAPI.deleteOrder(orderId: orderId).whenComplete { result in
    switch result {
    case .failure(let error):
    // process error
    case .success(let response):
        switch response {
        // process decoded response value or raw ClientResponse
        case .http400(let value, let raw):
        case .http404(let value, let raw):
        case .http0(let value, let raw):
        }
    }
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **orderId** | **String** | ID of the order that needs to be deleted | 

### Return type

#### DeleteOrder

```swift
public enum DeleteOrder {
    case http400(value: Void?, raw: ClientResponse)
    case http404(value: Void?, raw: ClientResponse)
    case http0(value: Void?, raw: ClientResponse)
}
```

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **getInventory**
```swift
    open class func getInventory(headers: HTTPHeaders = PetstoreClientAPI.customHeaders, beforeSend: (inout ClientRequest) throws -> () = { _ in }) -> EventLoopFuture<GetInventory>
```

Returns pet inventories by status

Returns a map of status codes to quantities

### Example
```swift
// The following code samples are still beta. For any issue, please report via http://github.com/OpenAPITools/openapi-generator/issues/new
import PetstoreClient


// Returns pet inventories by status
StoreAPI.getInventory().whenComplete { result in
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
This endpoint does not need any parameter.

### Return type

#### GetInventory

```swift
public enum GetInventory {
    case http200(value: [String: Int]?, raw: ClientResponse)
    case http0(value: [String: Int]?, raw: ClientResponse)
}
```

### Authorization

[api_key](../README.md#api_key)

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **getOrderById**
```swift
    open class func getOrderById(orderId: Int64, headers: HTTPHeaders = PetstoreClientAPI.customHeaders, beforeSend: (inout ClientRequest) throws -> () = { _ in }) -> EventLoopFuture<GetOrderById>
```

Find purchase order by ID

For valid response try integer IDs with value <= 5 or > 10. Other values will generated exceptions

### Example
```swift
// The following code samples are still beta. For any issue, please report via http://github.com/OpenAPITools/openapi-generator/issues/new
import PetstoreClient

let orderId = 987 // Int64 | ID of pet that needs to be fetched

// Find purchase order by ID
StoreAPI.getOrderById(orderId: orderId).whenComplete { result in
    switch result {
    case .failure(let error):
    // process error
    case .success(let response):
        switch response {
        // process decoded response value or raw ClientResponse
        case .http200(let value, let raw):
        case .http400(let value, let raw):
        case .http404(let value, let raw):
        case .http0(let value, let raw):
        }
    }
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **orderId** | **Int64** | ID of pet that needs to be fetched | 

### Return type

#### GetOrderById

```swift
public enum GetOrderById {
    case http200(value: Order?, raw: ClientResponse)
    case http400(value: Void?, raw: ClientResponse)
    case http404(value: Void?, raw: ClientResponse)
    case http0(value: Order?, raw: ClientResponse)
}
```

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/xml, application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **placeOrder**
```swift
    open class func placeOrder(body: Order, headers: HTTPHeaders = PetstoreClientAPI.customHeaders, beforeSend: (inout ClientRequest) throws -> () = { _ in }) -> EventLoopFuture<PlaceOrder>
```

Place an order for a pet

### Example
```swift
// The following code samples are still beta. For any issue, please report via http://github.com/OpenAPITools/openapi-generator/issues/new
import PetstoreClient

let body = Order(id: 123, petId: 123, quantity: 123, shipDate: Date(), status: "status_example", complete: false) // Order | order placed for purchasing the pet

// Place an order for a pet
StoreAPI.placeOrder(body: body).whenComplete { result in
    switch result {
    case .failure(let error):
    // process error
    case .success(let response):
        switch response {
        // process decoded response value or raw ClientResponse
        case .http200(let value, let raw):
        case .http400(let value, let raw):
        case .http0(let value, let raw):
        }
    }
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | [**Order**](Order.md) | order placed for purchasing the pet | 

### Return type

#### PlaceOrder

```swift
public enum PlaceOrder {
    case http200(value: Order?, raw: ClientResponse)
    case http400(value: Void?, raw: ClientResponse)
    case http0(value: Order?, raw: ClientResponse)
}
```

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/xml, application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

