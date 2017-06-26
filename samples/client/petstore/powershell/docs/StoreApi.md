# IO.Swagger.IO.Swagger\API.StoreApi

All URIs are relative to *http://petstore.swagger.io/v2*

Method | HTTP request | Description
------------- | ------------- | -------------
[**DeleteOrder**](StoreApi.md#deleteorder) | **DELETE** /store/order/{orderId} | Delete purchase order by ID
[**GetInventory**](StoreApi.md#getinventory) | **GET** /store/inventory | Returns pet inventories by status
[**GetOrderById**](StoreApi.md#getorderbyid) | **GET** /store/order/{orderId} | Find purchase order by ID
[**PlaceOrder**](StoreApi.md#placeorder) | **POST** /store/order | Place an order for a pet


<a name="deleteorder"></a>
# **DeleteOrder**
> void DeleteOrder (String orderId)

Delete purchase order by ID

For valid response try integer IDs with value < 1000. Anything above 1000 or nonintegers will generate API errors

### Example
```csharp
using System;
using System.Diagnostics;
using IO.Swagger.IO.Swagger\API;
using IO.Swagger.Client;
using IO.Swagger.IO.Swagger\Model;

namespace Example
{
    public class DeleteOrderExample
    {
        public void main()
        {
            var apiInstance = new StoreApi();
            var orderId = orderId_example;  // String | ID of the order that needs to be deleted

            try
            {
                // Delete purchase order by ID
                apiInstance.DeleteOrder(orderId);
            }
            catch (Exception e)
            {
                Debug.Print("Exception when calling StoreApi.DeleteOrder: " + e.Message );
            }
        }
    }
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

<a name="getinventory"></a>
# **GetInventory**
> {String, Int32} GetInventory ()

Returns pet inventories by status

Returns a map of status codes to quantities

### Example
```csharp
using System;
using System.Diagnostics;
using IO.Swagger.IO.Swagger\API;
using IO.Swagger.Client;
using IO.Swagger.IO.Swagger\Model;

namespace Example
{
    public class GetInventoryExample
    {
        public void main()
        {
            // Configure API key authorization: api_key
            Configuration.Default.ApiKey.Add("api_key", "YOUR_API_KEY");
            // Uncomment below to setup prefix (e.g. Bearer) for API key, if needed
            // Configuration.Default.ApiKeyPrefix.Add("api_key", "Bearer");

            var apiInstance = new StoreApi();

            try
            {
                // Returns pet inventories by status
                {String, Int32} result = apiInstance.GetInventory();
                Debug.WriteLine(result);
            }
            catch (Exception e)
            {
                Debug.Print("Exception when calling StoreApi.GetInventory: " + e.Message );
            }
        }
    }
}
```

### Parameters
This endpoint does not need any parameter.

### Return type

[**{String, Int32}**](Map.md)

### Authorization

[api_key](../README.md#api_key)

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

<a name="getorderbyid"></a>
# **GetOrderById**
> IO.Swagger.Model.Order GetOrderById (Int64 orderId)

Find purchase order by ID

For valid response try integer IDs with value <= 5 or > 10. Other values will generated exceptions

### Example
```csharp
using System;
using System.Diagnostics;
using IO.Swagger.IO.Swagger\API;
using IO.Swagger.Client;
using IO.Swagger.IO.Swagger\Model;

namespace Example
{
    public class GetOrderByIdExample
    {
        public void main()
        {
            var apiInstance = new StoreApi();
            var orderId = 789;  // Int64 | ID of pet that needs to be fetched

            try
            {
                // Find purchase order by ID
                IO.Swagger.Model.Order result = apiInstance.GetOrderById(orderId);
                Debug.WriteLine(result);
            }
            catch (Exception e)
            {
                Debug.Print("Exception when calling StoreApi.GetOrderById: " + e.Message );
            }
        }
    }
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **orderId** | **Int64**| ID of pet that needs to be fetched | 

### Return type

[**IO.Swagger.Model.Order**](Order.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/xml, application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

<a name="placeorder"></a>
# **PlaceOrder**
> IO.Swagger.Model.Order PlaceOrder (Order body)

Place an order for a pet



### Example
```csharp
using System;
using System.Diagnostics;
using IO.Swagger.IO.Swagger\API;
using IO.Swagger.Client;
using IO.Swagger.IO.Swagger\Model;

namespace Example
{
    public class PlaceOrderExample
    {
        public void main()
        {
            var apiInstance = new StoreApi();
            var body = new Order(); // Order | order placed for purchasing the pet

            try
            {
                // Place an order for a pet
                IO.Swagger.Model.Order result = apiInstance.PlaceOrder(body);
                Debug.WriteLine(result);
            }
            catch (Exception e)
            {
                Debug.Print("Exception when calling StoreApi.PlaceOrder: " + e.Message );
            }
        }
    }
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | [**Order**](Order.md)| order placed for purchasing the pet | 

### Return type

[**IO.Swagger.Model.Order**](Order.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/xml, application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

