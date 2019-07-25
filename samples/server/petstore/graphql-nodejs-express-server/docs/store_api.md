# store_api

All URIs are relative to *http://petstore.swagger.io/v2*

Method | HTTP request | Description
------------- | ------------- | -------------
[**DeleteOrder**](store_api.md#DeleteOrder) | **DELETE** /store/order/{orderId} | Delete purchase order by ID
[**GetInventory**](store_api.md#GetInventory) | **GET** /store/inventory | Returns pet inventories by status
[**GetOrderById**](store_api.md#GetOrderById) | **GET** /store/order/{orderId} | Find purchase order by ID
[**PlaceOrder**](store_api.md#PlaceOrder) | **POST** /store/order | Place an order for a pet


<a name="DeleteOrder"></a>
# **DeleteOrder**
> DeleteOrder(orderId)

Delete purchase order by ID

For valid response try integer IDs with value &lt; 1000. Anything above 1000 or nonintegers will generate API errors
<a name="GetInventory"></a>
# **GetInventory**
> Int! GetInventory()

Returns pet inventories by status

Returns a map of status codes to quantities
<a name="GetOrderById"></a>
# **GetOrderById**
> Order GetOrderById(orderId)

Find purchase order by ID

For valid response try integer IDs with value &lt;&#x3D; 5 or &gt; 10. Other values will generated exceptions
<a name="PlaceOrder"></a>
# **PlaceOrder**
> Order PlaceOrder(body)

Place an order for a pet
