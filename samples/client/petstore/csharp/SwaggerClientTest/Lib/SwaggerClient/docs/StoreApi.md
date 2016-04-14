# IO.Swagger.Api.StoreApi

All URIs are relative to *http://petstore.swagger.io/v2*

Method | HTTP request | Description
------------- | ------------- | -------------
[**DeleteOrder**](StoreApi.md#DeleteOrder) | **DELETE** /store/order/{orderId} | Delete purchase order by ID
[**FindOrdersByStatus**](StoreApi.md#FindOrdersByStatus) | **GET** /store/findByStatus | Finds orders by status
[**GetInventory**](StoreApi.md#GetInventory) | **GET** /store/inventory | Returns pet inventories by status
[**GetInventoryInObject**](StoreApi.md#GetInventoryInObject) | **GET** /store/inventory?response&#x3D;arbitrary_object | Fake endpoint to test arbitrary object return by &#39;Get inventory&#39;
[**GetOrderById**](StoreApi.md#GetOrderById) | **GET** /store/order/{orderId} | Find purchase order by ID
[**PlaceOrder**](StoreApi.md#PlaceOrder) | **POST** /store/order | Place an order for a pet


# **DeleteOrder**
> DeleteOrder(orderId)

Delete purchase order by ID

For valid response try integer IDs with value < 1000. Anything above 1000 or nonintegers will generate API errors

### Example 
```csharp
using System;
using System.Diagnostics;
using IO.Swagger.Api;
using IO.Swagger.Client;
using IO.Swagger.Module;

namespace Example
{
    public class DeleteOrderExample
    {
        public void main(){
            
            var apiInstance = new StoreApi();
            var orderId = orderId_example;  // string | ID of the order that needs to be deleted

            try {
                apiInstance.DeleteOrder(orderId);
            } catch (Exception e) {
                Debug.Print("Exception when calling StoreApi.DeleteOrder: " + e.Message );
            }
        }
    }
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **orderId** | **string**| ID of the order that needs to be deleted | 

### Return type

void (empty response body)

### Authorization

No authorization required

### HTTP reuqest headers

 - **Content-Type**: Not defined
 - **Accept**: application/json, application/xml

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **FindOrdersByStatus**
> List<Order> FindOrdersByStatus(status)

Finds orders by status

A single status value can be provided as a string

### Example 
```csharp
using System;
using System.Diagnostics;
using IO.Swagger.Api;
using IO.Swagger.Client;
using IO.Swagger.Module;

namespace Example
{
    public class FindOrdersByStatusExample
    {
        public void main(){
            
            // Configure API key authorization: test_api_client_id
            Configuration.Default.ApiKey.Add('x-test_api_client_id', 'YOUR_API_KEY');
            // Uncomment below to setup prefix (e.g. BEARER) for API key, if needed
            // Configuration.Default.ApiKeyPrefix.Add('x-test_api_client_id', 'BEARER');
            // Configure API key authorization: test_api_client_secret
            Configuration.Default.ApiKey.Add('x-test_api_client_secret', 'YOUR_API_KEY');
            // Uncomment below to setup prefix (e.g. BEARER) for API key, if needed
            // Configuration.Default.ApiKeyPrefix.Add('x-test_api_client_secret', 'BEARER');

            var apiInstance = new StoreApi();
            var status = status_example;  // string | Status value that needs to be considered for query

            try {
                List&lt;Order&gt; result = apiInstance.FindOrdersByStatus(status);
                Debug.WriteLine(result);
            } catch (Exception e) {
                Debug.Print("Exception when calling StoreApi.FindOrdersByStatus: " + e.Message );
            }
        }
    }
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **status** | **string**| Status value that needs to be considered for query | [optional] [default to placed]

### Return type

[**List<Order>**](Order.md)

### Authorization

[test_api_client_id](../README.md#test_api_client_id), [test_api_client_secret](../README.md#test_api_client_secret)

### HTTP reuqest headers

 - **Content-Type**: Not defined
 - **Accept**: application/json, application/xml

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **GetInventory**
> Dictionary<string, int?> GetInventory()

Returns pet inventories by status

Returns a map of status codes to quantities

### Example 
```csharp
using System;
using System.Diagnostics;
using IO.Swagger.Api;
using IO.Swagger.Client;
using IO.Swagger.Module;

namespace Example
{
    public class GetInventoryExample
    {
        public void main(){
            
            // Configure API key authorization: api_key
            Configuration.Default.ApiKey.Add('api_key', 'YOUR_API_KEY');
            // Uncomment below to setup prefix (e.g. BEARER) for API key, if needed
            // Configuration.Default.ApiKeyPrefix.Add('api_key', 'BEARER');

            var apiInstance = new StoreApi();

            try {
                Dictionary&lt;string, int?&gt; result = apiInstance.GetInventory();
                Debug.WriteLine(result);
            } catch (Exception e) {
                Debug.Print("Exception when calling StoreApi.GetInventory: " + e.Message );
            }
        }
    }
}
```

### Parameters
This endpoint does not need any parameter.

### Return type

**Dictionary<string, int?>**

### Authorization

[api_key](../README.md#api_key)

### HTTP reuqest headers

 - **Content-Type**: Not defined
 - **Accept**: application/json, application/xml

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **GetInventoryInObject**
> Object GetInventoryInObject()

Fake endpoint to test arbitrary object return by 'Get inventory'

Returns an arbitrary object which is actually a map of status codes to quantities

### Example 
```csharp
using System;
using System.Diagnostics;
using IO.Swagger.Api;
using IO.Swagger.Client;
using IO.Swagger.Module;

namespace Example
{
    public class GetInventoryInObjectExample
    {
        public void main(){
            
            // Configure API key authorization: api_key
            Configuration.Default.ApiKey.Add('api_key', 'YOUR_API_KEY');
            // Uncomment below to setup prefix (e.g. BEARER) for API key, if needed
            // Configuration.Default.ApiKeyPrefix.Add('api_key', 'BEARER');

            var apiInstance = new StoreApi();

            try {
                Object result = apiInstance.GetInventoryInObject();
                Debug.WriteLine(result);
            } catch (Exception e) {
                Debug.Print("Exception when calling StoreApi.GetInventoryInObject: " + e.Message );
            }
        }
    }
}
```

### Parameters
This endpoint does not need any parameter.

### Return type

**Object**

### Authorization

[api_key](../README.md#api_key)

### HTTP reuqest headers

 - **Content-Type**: Not defined
 - **Accept**: application/json, application/xml

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **GetOrderById**
> Order GetOrderById(orderId)

Find purchase order by ID

For valid response try integer IDs with value <= 5 or > 10. Other values will generated exceptions

### Example 
```csharp
using System;
using System.Diagnostics;
using IO.Swagger.Api;
using IO.Swagger.Client;
using IO.Swagger.Module;

namespace Example
{
    public class GetOrderByIdExample
    {
        public void main(){
            
            // Configure API key authorization: test_api_key_query
            Configuration.Default.ApiKey.Add('test_api_key_query', 'YOUR_API_KEY');
            // Uncomment below to setup prefix (e.g. BEARER) for API key, if needed
            // Configuration.Default.ApiKeyPrefix.Add('test_api_key_query', 'BEARER');
            // Configure API key authorization: test_api_key_header
            Configuration.Default.ApiKey.Add('test_api_key_header', 'YOUR_API_KEY');
            // Uncomment below to setup prefix (e.g. BEARER) for API key, if needed
            // Configuration.Default.ApiKeyPrefix.Add('test_api_key_header', 'BEARER');

            var apiInstance = new StoreApi();
            var orderId = orderId_example;  // string | ID of pet that needs to be fetched

            try {
                Order result = apiInstance.GetOrderById(orderId);
                Debug.WriteLine(result);
            } catch (Exception e) {
                Debug.Print("Exception when calling StoreApi.GetOrderById: " + e.Message );
            }
        }
    }
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **orderId** | **string**| ID of pet that needs to be fetched | 

### Return type

[**Order**](Order.md)

### Authorization

[test_api_key_query](../README.md#test_api_key_query), [test_api_key_header](../README.md#test_api_key_header)

### HTTP reuqest headers

 - **Content-Type**: Not defined
 - **Accept**: application/json, application/xml

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **PlaceOrder**
> Order PlaceOrder(body)

Place an order for a pet



### Example 
```csharp
using System;
using System.Diagnostics;
using IO.Swagger.Api;
using IO.Swagger.Client;
using IO.Swagger.Module;

namespace Example
{
    public class PlaceOrderExample
    {
        public void main(){
            
            // Configure API key authorization: test_api_client_id
            Configuration.Default.ApiKey.Add('x-test_api_client_id', 'YOUR_API_KEY');
            // Uncomment below to setup prefix (e.g. BEARER) for API key, if needed
            // Configuration.Default.ApiKeyPrefix.Add('x-test_api_client_id', 'BEARER');
            // Configure API key authorization: test_api_client_secret
            Configuration.Default.ApiKey.Add('x-test_api_client_secret', 'YOUR_API_KEY');
            // Uncomment below to setup prefix (e.g. BEARER) for API key, if needed
            // Configuration.Default.ApiKeyPrefix.Add('x-test_api_client_secret', 'BEARER');

            var apiInstance = new StoreApi();
            var body = new Order(); // Order | order placed for purchasing the pet

            try {
                Order result = apiInstance.PlaceOrder(body);
                Debug.WriteLine(result);
            } catch (Exception e) {
                Debug.Print("Exception when calling StoreApi.PlaceOrder: " + e.Message );
            }
        }
    }
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | [**Order**](Order.md)| order placed for purchasing the pet | [optional] 

### Return type

[**Order**](Order.md)

### Authorization

[test_api_client_id](../README.md#test_api_client_id), [test_api_client_secret](../README.md#test_api_client_secret)

### HTTP reuqest headers

 - **Content-Type**: Not defined
 - **Accept**: application/json, application/xml

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

