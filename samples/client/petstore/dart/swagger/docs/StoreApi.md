# swagger.api.StoreApi

## Load the API package
```dart
import 'package:swagger/api.dart';
```

All URIs are relative to *http://petstore.swagger.io/v2*

Method | HTTP request | Description
------------- | ------------- | -------------
[**deleteOrder**](StoreApi.md#deleteOrder) | **DELETE** /store/order/{orderId} | Delete purchase order by ID
[**getInventory**](StoreApi.md#getInventory) | **GET** /store/inventory | Returns pet inventories by status
[**getOrderById**](StoreApi.md#getOrderById) | **GET** /store/order/{orderId} | Find purchase order by ID
[**placeOrder**](StoreApi.md#placeOrder) | **POST** /store/order | Place an order for a pet


# **deleteOrder**
> deleteOrder(orderId)

Delete purchase order by ID

For valid response try integer IDs with value < 1000. Anything above 1000 or nonintegers will generate API errors

### Example 
```dart
import 'package:swagger/api.dart';

var api_instance = new StoreApi();
var orderId = orderId_example; // String | ID of the order that needs to be deleted

try { 
    api_instance.deleteOrder(orderId);
} catch (e) {
    print("Exception when calling StoreApi->deleteOrder: $e\n");
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **orderId** | **String**| ID of the order that needs to be deleted | 

### Return type

void (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/xml, application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **getInventory**
> Map<String, int> getInventory()

Returns pet inventories by status

Returns a map of status codes to quantities

### Example 
```dart
import 'package:swagger/api.dart';
// TODO Configure API key authorization: api_key
//swagger.api.Configuration.apiKey{'api_key'} = 'YOUR_API_KEY';
// uncomment below to setup prefix (e.g. Bearer) for API key, if needed
//swagger.api.Configuration.apiKeyPrefix{'api_key'} = "Bearer";

var api_instance = new StoreApi();

try { 
    var result = api_instance.getInventory();
    print(result);
} catch (e) {
    print("Exception when calling StoreApi->getInventory: $e\n");
}
```

### Parameters
This endpoint does not need any parameter.

### Return type

**Map<String, int>**

### Authorization

[api_key](../README.md#api_key)

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **getOrderById**
> Order getOrderById(orderId)

Find purchase order by ID

For valid response try integer IDs with value <= 5 or > 10. Other values will generated exceptions

### Example 
```dart
import 'package:swagger/api.dart';

var api_instance = new StoreApi();
var orderId = 789; // int | ID of pet that needs to be fetched

try { 
    var result = api_instance.getOrderById(orderId);
    print(result);
} catch (e) {
    print("Exception when calling StoreApi->getOrderById: $e\n");
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **orderId** | **int**| ID of pet that needs to be fetched | 

### Return type

[**Order**](Order.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/xml, application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **placeOrder**
> Order placeOrder(body)

Place an order for a pet



### Example 
```dart
import 'package:swagger/api.dart';

var api_instance = new StoreApi();
var body = new Order(); // Order | order placed for purchasing the pet

try { 
    var result = api_instance.placeOrder(body);
    print(result);
} catch (e) {
    print("Exception when calling StoreApi->placeOrder: $e\n");
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | [**Order**](Order.md)| order placed for purchasing the pet | 

### Return type

[**Order**](Order.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/xml, application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

