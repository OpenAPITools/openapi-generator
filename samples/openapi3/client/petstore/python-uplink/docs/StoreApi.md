# petstore_api.StoreApi

All URIs are relative to *http://petstore.swagger.io/v2*

Method | HTTP request | Description
------------- | ------------- | -------------
[**delete_order**](StoreApi.md#delete_order) | **DELETE** /store/order/{orderId} | Delete purchase order by ID
[**get_inventory**](StoreApi.md#get_inventory) | **GET** /store/inventory | Returns pet inventories by status
[**get_order_by_id**](StoreApi.md#get_order_by_id) | **GET** /store/order/{orderId} | Find purchase order by ID
[**place_order**](StoreApi.md#place_order) | **POST** /store/order | Place an order for a pet


# **delete_order**
> delete_order(order_id)

Delete purchase order by ID

For valid response try integer IDs with value < 1000. Anything above 1000 or nonintegers will generate API errors

### Example
```python
from __future__ import print_function
import time
import petstore_api
from petstore_api import security
from pprint import pprint

# create an instance of the API class
api_client = petstore_api.()
order_id = 'order_id_example' # str | ID of the order that needs to be deleted

try:
    # Delete purchase order by ID
    api_client.delete_order(order_id)
except ApiException as e:
    print("Exception when calling StoreApi->delete_order: %s\n" % e)
```


### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **order_id** | **str**| ID of the order that needs to be deleted | 

### Return type

void (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **get_inventory**
> dict(str, int) get_inventory()

Returns pet inventories by status

Returns a map of status codes to quantities

### Example
```python
from __future__ import print_function
import time
import petstore_api
from petstore_api import security
from pprint import pprint


# Configure API key authorization: api_key
auth = security.ApiKeySecurity('YOUR_API_KEY')
# If you need multiple auth methods, string them together with MultiAuth
# auth = security.MultiAuth(auth, security.ProxyAuth('PROXY_USERNAME', 'PROXY_PASSWORD'))


# create an instance of the API class
api_client = petstore_api.(auth=auth)

try:
    # Returns pet inventories by status
    api_response = api_client.get_inventory()
    pprint(api_response)
except ApiException as e:
    print("Exception when calling StoreApi->get_inventory: %s\n" % e)
```


### Parameters
This endpoint does not need any parameter.

### Return type

**dict(str, int)**

### Authorization

[api_key](../README.md#api_key)

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **get_order_by_id**
> Order get_order_by_id(order_id)

Find purchase order by ID

For valid response try integer IDs with value <= 5 or > 10. Other values will generated exceptions

### Example
```python
from __future__ import print_function
import time
import petstore_api
from petstore_api import security
from pprint import pprint

# create an instance of the API class
api_client = petstore_api.()
order_id = 56 # int | ID of pet that needs to be fetched

try:
    # Find purchase order by ID
    api_response = api_client.get_order_by_id(order_id)
    pprint(api_response)
except ApiException as e:
    print("Exception when calling StoreApi->get_order_by_id: %s\n" % e)
```


### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **order_id** | **int**| ID of pet that needs to be fetched | 

### Return type

[**Order**](Order.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/xml, application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **place_order**
> Order place_order(order)

Place an order for a pet

### Example
```python
from __future__ import print_function
import time
import petstore_api
from petstore_api import security
from pprint import pprint

# create an instance of the API class
api_client = petstore_api.()
order = petstore_api.Order() # Order | order placed for purchasing the pet

try:
    # Place an order for a pet
    api_response = api_client.place_order(order)
    pprint(api_response)
except ApiException as e:
    print("Exception when calling StoreApi->place_order: %s\n" % e)
```


### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **order** | [**Order**](Order.md)| order placed for purchasing the pet | 

### Return type

[**Order**](Order.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: application/xml, application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

